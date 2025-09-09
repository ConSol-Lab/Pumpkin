use std::{collections::BTreeMap, io::BufRead};

use drcp_format::{reader::ProofReader, ConstraintId};

mod inferences;
mod state;

pub mod model;

use model::*;

#[derive(Debug, thiserror::Error)]
pub enum CheckError {
    #[error("inference {0} is invalid: {1}")]
    InvalidInference(ConstraintId, inferences::InvalidInference),

    #[error("invalid deduction: {0}")]
    InvalidDeduction(#[from] InvalidDeduction),

    #[error("the proof was not terminated with a conclusion")]
    MissingConclusion,

    #[error("the conclusion is not present as a proof step")]
    InvalidConclusion,

    #[error("failed to read next proof line: {0}")]
    ProofReadError(#[from] drcp_format::reader::Error),
}

#[derive(thiserror::Error, Debug)]
#[error("invalid deduction")]
pub enum InvalidDeduction {
    #[error("constraint id {0} already in use")]
    DuplicateConstraintId(ConstraintId),
}

/// Verify whether the given proof is valid w.r.t. the model.
pub fn verify_proof<Source: BufRead>(
    mut model: Model,
    mut proof: ProofReader<Source, i32>,
) -> Result<(), CheckError> {
    let mut fact_database = BTreeMap::new();

    loop {
        let next_step = proof.next_step()?;

        let Some(step) = next_step else {
            return Err(CheckError::MissingConclusion);
        };

        match step {
            drcp_format::Step::Inference(inference) => {
                let fact = inferences::verify_inference(&model, &inference)
                    .map_err(|err| CheckError::InvalidInference(inference.constraint_id, err))?;

                let _ = fact_database.insert(inference.constraint_id, fact);
            }

            drcp_format::Step::Deduction(deduction) => {
                let derived_constraint = verify_deduction(&deduction)?;

                let constraint_id_was_used = model.add_constraint(
                    deduction.constraint_id,
                    Constraint::Nogood(derived_constraint),
                );

                if constraint_id_was_used {
                    return Err(
                        InvalidDeduction::DuplicateConstraintId(deduction.constraint_id).into(),
                    );
                }
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

fn verify_conclusion(
    model: &Model,
    conclusion: &drcp_format::Conclusion<std::rc::Rc<str>, i32>,
) -> bool {
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

fn verify_deduction(
    deduction: &drcp_format::Deduction<std::rc::Rc<str>, i32>,
) -> Result<Nogood, InvalidDeduction> {
    todo!()
}
