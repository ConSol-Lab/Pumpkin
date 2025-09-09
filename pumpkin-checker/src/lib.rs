use std::collections::BTreeMap;
use std::io::BufRead;
use std::rc::Rc;

use drcp_format::reader::ProofReader;
use drcp_format::ConstraintId;
use drcp_format::IntAtomic;

mod inferences;
mod state;

pub mod model;

use model::*;

use crate::inferences::Fact;
use crate::state::VariableState;

#[derive(Debug, thiserror::Error)]
pub enum CheckError {
    #[error("inference {0} is invalid: {1}")]
    InvalidInference(ConstraintId, inferences::InvalidInference),

    #[error("deduction {0} is invalid: {1}")]
    InvalidDeduction(ConstraintId, InvalidDeduction),

    #[error("the proof was not terminated with a conclusion")]
    MissingConclusion,

    #[error("the conclusion is not present as a proof step")]
    InvalidConclusion,

    #[error("failed to read next proof line: {0}")]
    ProofReadError(#[from] drcp_format::reader::Error),
}

#[derive(thiserror::Error, Debug)]
#[error("invalid deduction")]
#[allow(
    variant_size_differences,
    reason = "this is the error path, so no need to worry about variant sizes"
)]
pub enum InvalidDeduction {
    #[error("constraint id {0} already in use")]
    DuplicateConstraintId(ConstraintId),

    #[error("inference {0} does not exist")]
    UnknownInference(ConstraintId),

    #[error("no conflict was derived after applying all inferences")]
    NoConflict(Vec<(ConstraintId, Vec<IntAtomic<String, i32>>)>),

    #[error("the deduction contains inconsistent premises")]
    InconsistentPremises,
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
                let derived_constraint = verify_deduction(&deduction, &fact_database)
                    .map_err(|err| CheckError::InvalidDeduction(deduction.constraint_id, err))?;

                let new_constraint_added = model.add_constraint(
                    deduction.constraint_id,
                    Constraint::Nogood(derived_constraint),
                );

                if !new_constraint_added {
                    return Err(CheckError::InvalidDeduction(
                        deduction.constraint_id,
                        InvalidDeduction::DuplicateConstraintId(deduction.constraint_id),
                    ));
                }

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
    deduction: &drcp_format::Deduction<Rc<str>, i32>,
    facts: &BTreeMap<ConstraintId, Fact>,
) -> Result<Nogood, InvalidDeduction> {
    let mut variable_state = VariableState::default();
    let mut unused_inferences = Vec::new();

    for atomic in deduction.premises.iter() {
        if !variable_state.apply(atomic.clone()) {
            return Err(InvalidDeduction::InconsistentPremises);
        }
    }

    for constraint_id in deduction.sequence.iter() {
        let fact = facts
            .get(constraint_id)
            .ok_or(InvalidDeduction::UnknownInference(*constraint_id))?;

        let unsatisfied_premises = fact
            .premises
            .iter()
            .filter_map(|premise| {
                if variable_state.is_true(premise.clone()) {
                    None
                } else {
                    Some(IntAtomic {
                        name: premise.name.as_ref().to_owned(),
                        comparison: premise.comparison,
                        value: premise.value,
                    })
                }
            })
            .collect::<Vec<_>>();

        if !unsatisfied_premises.is_empty() {
            unused_inferences.push((*constraint_id, unsatisfied_premises));
            continue;
        }

        match &fact.consequent {
            Some(consequent) => {
                if !variable_state.apply(consequent.clone()) {
                    return Ok(Nogood::from(deduction.premises.clone()));
                }
            }
            None => return Ok(Nogood::from(deduction.premises.clone())),
        }
    }

    Err(InvalidDeduction::NoConflict(unused_inferences))
}
