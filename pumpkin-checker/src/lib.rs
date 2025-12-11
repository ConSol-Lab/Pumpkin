use std::collections::BTreeMap;
use std::io::BufRead;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::reader::ProofReader;

pub mod deductions;
pub mod inferences;
mod state;

pub mod model;

use model::*;

/// The errors that can be returned by the checker.
#[derive(Debug, thiserror::Error)]
pub enum CheckError {
    /// The inference with the given [`ConstraintId`] is invalid due to
    /// [`inferences::InvalidInference`].
    #[error("inference {0} is invalid: {1}")]
    InvalidInference(ConstraintId, inferences::InvalidInference),

    /// The inference with the given [`ConstraintId`] is invalid due to
    /// [`deductions::InvalidDeduction`].
    #[error("deduction {0} is invalid: {1}")]
    InvalidDeduction(ConstraintId, deductions::InvalidDeduction),

    /// The proof did not contain a conclusion line.
    #[error("the proof was not terminated with a conclusion")]
    MissingConclusion,

    /// The conclusion does not follow from any deduction in the proof.
    #[error("the conclusion is not present as a proof step")]
    InvalidConclusion,

    /// An I/O error prevented us from reading all the input.
    #[error("failed to read next proof line: {0}")]
    ProofReadError(#[from] drcp_format::reader::Error),
}

/// Verify whether the given proof is valid w.r.t. the model.
pub fn verify_proof<Source: BufRead>(
    mut model: Model,
    mut proof: ProofReader<Source, i32>,
) -> Result<(), CheckError> {
    // To check a proof we iterate over every step.
    // - If the step is an inference, it is checked. If it is valid, then the inference is stored in
    //   the fact database to be used in the next deduction. Otherwise, an error is returned
    //   indicating that the step is invalid.
    // - If the step is a deduction, it is checked with respect to all the inferences in the fact
    //   database. If the deduction is valid, the fact database is cleared and the deduction is
    //   added to the model to be used in future inferences.

    let mut fact_database = BTreeMap::new();

    loop {
        let next_step = proof.next_step()?;

        let Some(step) = next_step else {
            // The loop stops when a conclusion is found, so at this point we know the proof does
            // not contain a conclusion.
            return Err(CheckError::MissingConclusion);
        };

        match step {
            drcp_format::Step::Inference(inference) => {
                let fact = inferences::verify_inference(&model, &inference)
                    .map_err(|err| CheckError::InvalidInference(inference.constraint_id, err))?;

                let _ = fact_database.insert(inference.constraint_id, fact);
            }

            drcp_format::Step::Deduction(deduction) => {
                let derived_constraint = deductions::verify_deduction(&deduction, &fact_database)
                    .map_err(|err| {
                    CheckError::InvalidDeduction(deduction.constraint_id, err)
                })?;

                let new_constraint_added = model.add_constraint(
                    deduction.constraint_id,
                    Constraint::Nogood(derived_constraint),
                );

                if !new_constraint_added {
                    return Err(CheckError::InvalidDeduction(
                        deduction.constraint_id,
                        deductions::InvalidDeduction::DuplicateConstraintId(
                            deduction.constraint_id,
                        ),
                    ));
                }

                // Forget the stored inferences.
                fact_database.clear();
            }

            drcp_format::Step::Conclusion(conclusion) => {
                if verify_conclusion(&model, &conclusion) {
                    return Ok(());
                } else {
                    return Err(CheckError::InvalidConclusion);
                }
            }
        }
    }
}

fn verify_conclusion(model: &Model, conclusion: &drcp_format::Conclusion<Rc<str>, i32>) -> bool {
    // First we ensure the conclusion type matches the solve item in the model.
    match (&model.objective, conclusion) {
        (Some(_), drcp_format::Conclusion::Unsat)
        | (None, drcp_format::Conclusion::DualBound(_)) => return false,

        _ => {}
    }

    // We iterate in reverse order, since it is likely that the conclusion is based on a constraint
    // towards the end of the proof.
    model.iter_constraints().rev().any(|(_, constraint)| {
        let Constraint::Nogood(nogood) = constraint else {
            return false;
        };

        match conclusion {
            drcp_format::Conclusion::Unsat => nogood.as_ref().is_empty(),
            drcp_format::Conclusion::DualBound(atomic) => nogood.as_ref() == [!atomic.clone()],
        }
    })
}
