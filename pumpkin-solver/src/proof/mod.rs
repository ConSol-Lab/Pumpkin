//! Pumpkin supports proof logging for SAT and CP problems. During search, the solver produces a
//! [`ProofLog`], which is a list of deductions made by the solver.
//!
//! Proof logging for CP is supported in the DRCP format. This format explicitly supports usage
//! where the solver logs a proof scaffold which later processed into a full proof after search
//! has completed.
mod dimacs;
mod finalizer;
mod proof_literals;

use std::fs::File;
use std::num::NonZero;
use std::num::NonZeroU64;
use std::path::Path;
use std::path::PathBuf;

use drcp_format::writer::ProofWriter;
pub use drcp_format::Format;
pub(crate) use finalizer::*;

use self::dimacs::DimacsProof;
use self::proof_literals::ProofLiterals;
use crate::predicates::Predicate;
use crate::variable_names::VariableNames;
use crate::variables::Literal;
#[cfg(doc)]
use crate::Solver;

/// A proof log which logs the proof steps necessary to prove unsatisfiability or optimality. We
/// allow the following types of proofs:
/// - A CP proof log - This can be created using [`ProofLog::cp`].
/// - A DIMACS proof log - This can be created using [`ProofLog::dimacs`].
///
/// When a proof log should not be generated, use the implementation of [`Default`].
#[derive(Debug, Default)]
pub struct ProofLog {
    internal_proof: Option<ProofImpl>,
}

/// A dummy proof step ID. Used when there is proof logging is not enabled.
const DUMMY_STEP_ID: NonZeroU64 = NonZeroU64::new(1).unwrap();

impl ProofLog {
    /// Create a CP proof logger.
    pub fn cp(
        file_path: &Path,
        format: Format,
        log_inferences: bool,
        log_hints: bool,
    ) -> std::io::Result<ProofLog> {
        let definitions_path = file_path.with_extension("lits");
        let file = File::create(file_path)?;

        let writer = ProofWriter::new(format, file, ProofLiterals::default());

        Ok(ProofLog {
            internal_proof: Some(ProofImpl::CpProof {
                writer,
                log_inferences,
                definitions_path,
                propagation_order_hint: if log_hints { Some(vec![]) } else { None },
            }),
        })
    }

    /// Create a dimacs proof logger.
    pub fn dimacs(file_path: &Path) -> std::io::Result<ProofLog> {
        let file = File::create(file_path)?;
        Ok(ProofLog {
            internal_proof: Some(ProofImpl::DimacsProof(DimacsProof::new(file))),
        })
    }

    /// Log an inference to the proof.
    pub(crate) fn log_inference(
        &mut self,
        constraint_tag: Option<NonZero<u32>>,
        premises: impl IntoIterator<Item = Predicate>,
        propagated: Option<Predicate>,
    ) -> std::io::Result<NonZeroU64> {
        let Some(ProofImpl::CpProof {
            writer,
            log_inferences: true,
            propagation_order_hint,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(DUMMY_STEP_ID);
        };

        // TODO: Log the inference label.
        let id = writer.log_inference(constraint_tag, None, premises, propagated)?;

        if let Some(hints) = propagation_order_hint {
            hints.push(id);
        }

        Ok(id)
    }

    /// Record that a step has been used in the derivation of the next nogood.
    ///
    /// Inferences are automatically added as a propagation hint when they are logged, this is
    /// therefore only necessary when nogoods are used in a propagation.
    pub(crate) fn add_propagation(&mut self, step_id: NonZeroU64) {
        let Some(ProofImpl::CpProof {
            propagation_order_hint: Some(ref mut hints),
            ..
        }) = self.internal_proof.as_mut()
        else {
            return;
        };

        hints.push(step_id);
    }

    /// Log a learned clause to the proof.
    ///
    /// The inferences and marked propagations are assumed to be recorded in reverse-application
    /// order.
    pub(crate) fn log_learned_clause(
        &mut self,
        literals: impl IntoIterator<Item = Predicate>,
        variable_names: &VariableNames,
    ) -> std::io::Result<NonZeroU64> {
        match &mut self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                propagation_order_hint,
                ..
            }) => {
                let propagation_hints = propagation_order_hint
                    .as_ref()
                    .map(|vec| vec.iter().rev().copied());
                let id = writer.log_nogood_clause(literals, propagation_hints)?;

                // Clear the hints for the next nogood.
                if let Some(hints) = propagation_order_hint.as_mut() {
                    hints.clear();
                }

                Ok(id)
            }

            Some(ProofImpl::DimacsProof(writer)) => writer.learned_clause(literals, variable_names),

            None => Ok(DUMMY_STEP_ID),
        }
    }

    pub(crate) fn unsat(self, variable_names: &VariableNames) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                definitions_path,
                ..
            }) => {
                let literals = writer.unsat()?;
                let file = File::create(definitions_path)?;
                literals.write(file, variable_names)
            }
            Some(ProofImpl::DimacsProof(mut writer)) => writer
                .learned_clause(std::iter::empty(), variable_names)
                .map(|_| ()),
            None => Ok(()),
        }
    }

    pub(crate) fn optimal(
        self,
        objective_bound: Predicate,
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                definitions_path,
                ..
            }) => {
                let literals = writer.optimal(objective_bound)?;
                let file = File::create(definitions_path)?;
                literals.write(file, variable_names)
            }

            Some(ProofImpl::DimacsProof(_)) => {
                panic!("Cannot conclude optimality in DIMACS proof")
            }

            None => Ok(()),
        }
    }

    pub(crate) fn is_logging_inferences(&self) -> bool {
        matches!(
            self.internal_proof,
            Some(ProofImpl::CpProof {
                log_inferences: true,
                ..
            })
        )
    }

    pub(crate) fn reify_predicate(&mut self, literal: Literal, predicate: Predicate) {
        let Some(ProofImpl::CpProof { ref mut writer, .. }) = self.internal_proof else {
            return;
        };

        writer.literals_mut().reify_predicate(literal, predicate);
    }
}

#[derive(Debug)]
enum ProofImpl {
    CpProof {
        writer: ProofWriter<File, ProofLiterals>,
        log_inferences: bool,
        definitions_path: PathBuf,
        // If propagation hints are enabled, this is a buffer used to record propagations in the
        // order they can be applied to derive the next nogood.
        propagation_order_hint: Option<Vec<NonZeroU64>>,
    },
    DimacsProof(DimacsProof<File>),
}
