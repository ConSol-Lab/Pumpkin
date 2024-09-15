mod dimacs;
mod proof_literals;

use std::fs::File;
use std::num::NonZero;
use std::num::NonZeroU64;
use std::path::Path;
use std::path::PathBuf;

pub use drcp_format::Format;
use drcp_format::ProofWriter;

use self::dimacs::DimacsProof;
use self::proof_literals::ProofLiterals;
use super::variables::Literal;
use super::VariableLiteralMappings;
use crate::variable_names::VariableNames;
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
// Safety: Unwrapping an option is not stable, so we cannot get a NonZero<T> safely in a const
// context.
const DUMMY_STEP_ID: NonZeroU64 = unsafe { NonZeroU64::new_unchecked(1) };

impl ProofLog {
    /// Create a CP proof logger.
    pub fn cp(file_path: &Path, format: Format, log_inferences: bool) -> std::io::Result<ProofLog> {
        let definitions_path = file_path.with_extension("lits");
        let file = File::create(file_path)?;

        let writer = ProofWriter::new(format, file, ProofLiterals::default());

        Ok(ProofLog {
            internal_proof: Some(ProofImpl::CpProof {
                writer,
                log_inferences,
                definitions_path,
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
        premises: impl IntoIterator<Item = Literal>,
        propagated: Literal,
    ) -> std::io::Result<NonZeroU64> {
        let Some(ProofImpl::CpProof {
            writer,
            log_inferences: true,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(DUMMY_STEP_ID);
        };

        // TODO: Log the inference label.
        writer.log_inference(constraint_tag, None, premises, propagated)
    }

    /// Log a learned clause to the proof.
    pub(crate) fn log_learned_clause(
        &mut self,
        literals: impl IntoIterator<Item = Literal>,
    ) -> std::io::Result<NonZeroU64> {
        match &mut self.internal_proof {
            Some(ProofImpl::CpProof { writer, .. }) => writer.log_nogood_clause(literals),

            Some(ProofImpl::DimacsProof(writer)) => writer.learned_clause(literals),

            None => Ok(DUMMY_STEP_ID),
        }
    }

    pub(crate) fn unsat(
        self,
        variable_names: &VariableNames,
        variable_literal_mapping: &VariableLiteralMappings,
    ) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                log_inferences: _,
                definitions_path,
            }) => {
                let literals = writer.unsat()?;
                let file = File::create(definitions_path)?;
                literals.write(file, variable_names, variable_literal_mapping)
            }
            Some(ProofImpl::DimacsProof(mut writer)) => {
                writer.learned_clause(std::iter::empty()).map(|_| ())
            }
            None => Ok(()),
        }
    }

    pub(crate) fn optimal(
        self,
        objective_bound: Literal,
        variable_names: &VariableNames,
        variable_literal_mapping: &VariableLiteralMappings,
    ) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                log_inferences: _,
                definitions_path,
            }) => {
                let literals = writer.optimal(objective_bound)?;
                let file = File::create(definitions_path)?;
                literals.write(file, variable_names, variable_literal_mapping)
            }

            Some(ProofImpl::DimacsProof(_)) => {
                panic!("Cannot conclude optimality in DIMACS proof")
            }

            None => Ok(()),
        }
    }
}

#[derive(Debug)]
enum ProofImpl {
    CpProof {
        writer: ProofWriter<File, ProofLiterals>,
        log_inferences: bool,
        definitions_path: PathBuf,
    },
    DimacsProof(DimacsProof<File>),
}
