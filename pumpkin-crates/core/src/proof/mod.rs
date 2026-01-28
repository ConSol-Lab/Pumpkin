//! Pumpkin supports proof logging for SAT and CP problems. During search, the solver produces a
//! [`ProofLog`], which is a list of deductions made by the solver.
//!
//! Proof logging for CP is supported in the DRCP format. This format explicitly supports usage
//! where the solver logs a proof scaffold which later processed into a full proof after search
//! has completed.
mod dimacs;
mod finalizer;
mod inference_code;
mod proof_atomics;

use std::fs::File;
use std::io::Write;
use std::path::Path;

use dimacs::DimacsProof;
use drcp_format::Deduction;
use drcp_format::Inference;
use drcp_format::writer::ProofWriter;
pub(crate) use finalizer::*;
pub use inference_code::*;
use proof_atomics::ProofAtomics;

#[cfg(doc)]
use crate::Solver;
use crate::containers::HashMap;
use crate::containers::KeyGenerator;
use crate::engine::Assignments;
use crate::engine::variable_names::VariableNames;
use crate::predicates::Predicate;
use crate::variables::Literal;

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

impl ProofLog {
    /// Create a CP proof logger.
    pub fn cp(file_path: &Path, log_hints: bool) -> std::io::Result<ProofLog> {
        let file = File::create(file_path)?;

        let sink = if file_path.extension().is_some_and(|ext| ext == ".gz") {
            Sink::GzippedFile(flate2::write::GzEncoder::new(
                file,
                flate2::Compression::fast(),
            ))
        } else {
            Sink::File(file)
        };

        let writer = ProofWriter::new(sink);

        Ok(ProofLog {
            internal_proof: Some(ProofImpl::CpProof {
                writer,
                propagation_order_hint: if log_hints { Some(vec![]) } else { None },
                logged_domain_inferences: HashMap::default(),
                proof_atomics: ProofAtomics::default(),
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
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        inference_code: InferenceCode,
        premises: impl IntoIterator<Item = Predicate>,
        propagated: Option<Predicate>,
        variable_names: &VariableNames,
        assignments: &Assignments,
    ) -> std::io::Result<ConstraintTag> {
        let inference_tag = constraint_tags.next_key();

        let Some(ProofImpl::CpProof {
            writer,
            propagation_order_hint: Some(propagation_sequence),
            proof_atomics,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(inference_tag);
        };

        if let Some(propagated) = propagated {
            assert!(!assignments.is_initial_bound(propagated));
        }

        let inference = Inference {
            constraint_id: inference_tag.into(),
            premises: premises
                .into_iter()
                .filter(|&predicate| !assignments.is_initial_bound(predicate))
                .map(|premise| proof_atomics.map_predicate_to_proof_atomic(premise, variable_names))
                .collect(),
            consequent: propagated.map(|predicate| {
                proof_atomics.map_predicate_to_proof_atomic(predicate, variable_names)
            }),
            generated_by: Some(inference_code.tag().into()),
            label: Some(inference_code.label()),
        };

        writer.log_inference(inference)?;

        propagation_sequence.push(Some(inference_tag));

        Ok(inference_tag)
    }

    /// Log an inference that claims the given predicate is part of the initial domain.
    pub(crate) fn log_domain_inference(
        &mut self,
        predicate: Predicate,
        variable_names: &VariableNames,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        assignments: &Assignments,
    ) -> std::io::Result<Option<ConstraintTag>> {
        assert!(assignments.is_initial_bound(predicate));

        let domain = predicate.get_domain();
        if assignments.get_initial_lower_bound(domain)
            == assignments.get_initial_upper_bound(domain)
            && variable_names.get_int_name(domain).is_none()
        {
            // The predicate is over a constant variable. We assume we do not want to
            // log these if they have no name.

            return Ok(None);
        }

        let inference_tag = constraint_tags.next_key();

        let Some(ProofImpl::CpProof {
            writer,
            propagation_order_hint: Some(propagation_sequence),
            logged_domain_inferences,
            proof_atomics,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(Some(inference_tag));
        };

        if let Some(hint_idx) = logged_domain_inferences.get(&predicate).copied() {
            let tag = propagation_sequence[hint_idx]
                .take()
                .expect("the logged_domain_inferences always points to some index");
            propagation_sequence.push(Some(tag));

            let _ = logged_domain_inferences.insert(predicate, propagation_sequence.len() - 1);

            return Ok(Some(tag));
        }

        let inference = Inference {
            constraint_id: inference_tag.into(),
            premises: vec![],
            consequent: Some(
                proof_atomics.map_predicate_to_proof_atomic(predicate, variable_names),
            ),
            generated_by: None,
            label: Some("initial_domain"),
        };

        writer.log_inference(inference)?;

        propagation_sequence.push(Some(inference_tag));

        let _ = logged_domain_inferences.insert(predicate, propagation_sequence.len() - 1);

        Ok(Some(inference_tag))
    }

    /// Log a deduction (learned nogood) to the proof.
    ///
    /// The inferences and marked propagations are assumed to be recorded in reverse-application
    /// order.
    pub(crate) fn log_deduction(
        &mut self,
        premises: impl IntoIterator<Item = Predicate>,
        variable_names: &VariableNames,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        assignments: &Assignments,
    ) -> std::io::Result<ConstraintTag> {
        let constraint_tag = constraint_tags.next_key();

        match &mut self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                propagation_order_hint,
                proof_atomics,
                logged_domain_inferences,
                ..
            }) => {
                // Reset the logged domain inferences.
                logged_domain_inferences.clear();

                let deduction = Deduction {
                    constraint_id: constraint_tag.into(),
                    premises: premises
                        .into_iter()
                        .filter(|&predicate| assignments.is_initial_bound(predicate))
                        .map(|premise| {
                            proof_atomics.map_predicate_to_proof_atomic(premise, variable_names)
                        })
                        .collect(),
                    sequence: propagation_order_hint
                        .as_ref()
                        .iter()
                        .flat_map(|vec| vec.iter().rev().copied())
                        .flatten()
                        .map(|tag| tag.into())
                        .collect(),
                };

                writer.log_deduction(deduction)?;

                // Clear the hints for the next nogood.
                if let Some(hints) = propagation_order_hint.as_mut() {
                    hints.clear();
                }

                Ok(constraint_tag)
            }

            Some(ProofImpl::DimacsProof(writer)) => {
                let clause = premises.into_iter().map(|predicate| !predicate);
                writer.learned_clause(clause, variable_names)?;
                Ok(constraint_tag)
            }

            None => Ok(constraint_tag),
        }
    }

    pub(crate) fn unsat(self, variable_names: &VariableNames) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof { mut writer, .. }) => {
                writer.log_conclusion::<&str>(drcp_format::Conclusion::Unsat)
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
                mut writer,
                mut proof_atomics,
                ..
            }) => {
                let atomic =
                    proof_atomics.map_predicate_to_proof_atomic(objective_bound, variable_names);

                writer.log_conclusion::<&str>(drcp_format::Conclusion::DualBound(atomic))
            }

            Some(ProofImpl::DimacsProof(_)) => {
                panic!("Cannot conclude optimality in DIMACS proof")
            }

            None => Ok(()),
        }
    }

    pub fn is_logging_inferences(&self) -> bool {
        matches!(
            self.internal_proof,
            Some(ProofImpl::CpProof {
                propagation_order_hint: Some(_),
                ..
            })
        )
    }

    pub(crate) fn reify_predicate(&mut self, literal: Literal, predicate: Predicate) {
        let Some(ProofImpl::CpProof {
            ref mut proof_atomics,
            ..
        }) = self.internal_proof
        else {
            return;
        };

        proof_atomics.reify_predicate(literal, predicate);
    }

    pub(crate) fn is_logging_proof(&self) -> bool {
        self.internal_proof.is_some()
    }
}

/// A wrapper around either a file or a gzipped file.
///
/// Whether or not we will gzip on the fly is a runtime decision, and this wrapper is the [`Write`]
/// implementation that [`ProofWriter`] will write to.
#[derive(Debug)]
enum Sink {
    File(File),
    GzippedFile(flate2::write::GzEncoder<File>),
}

impl Write for Sink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Sink::File(file) => file.write(buf),
            Sink::GzippedFile(gz_encoder) => gz_encoder.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Sink::File(file) => file.flush(),
            Sink::GzippedFile(gz_encoder) => gz_encoder.flush(),
        }
    }
}

#[derive(Debug)]
#[allow(
    clippy::large_enum_variant,
    reason = "there will only ever be one per solver"
)]
#[allow(
    variant_size_differences,
    reason = "there will only ever be one per solver"
)]
enum ProofImpl {
    CpProof {
        writer: ProofWriter<Sink, i32>,
        // If propagation hints are enabled, this is a buffer used to record propagations in the
        // order they can be applied to derive the next nogood.
        //
        // Every element is optional, because when we log a domain inference multiple
        // times, we have to move the corresponding constraint tag to the end of the hint.
        // We do this by replacing the existing value with `None` and appending `Some` at
        // the end.
        propagation_order_hint: Option<Vec<Option<ConstraintTag>>>,
        proof_atomics: ProofAtomics,
        /// The domain inferences that are logged for the next deduction. For each
        /// inference we keep the index in the propagation order hint.
        logged_domain_inferences: HashMap<Predicate, usize>,
    },
    DimacsProof(DimacsProof<File>),
}
