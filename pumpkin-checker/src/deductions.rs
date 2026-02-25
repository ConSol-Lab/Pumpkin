use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;
use pumpkin_checking::SupportingInference;

use crate::inferences::Fact;
use crate::model::Atomic;
use crate::model::Nogood;

/// An inference that was ignored when checking a deduction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IgnoredInference {
    /// The ID of the ignored inference.
    pub constraint_id: ConstraintId,

    /// The premises that were not satisfied when the inference was evaluated.
    pub unsatisfied_premises: Vec<IntAtomic<String, i32>>,
}

/// A deduction is rejected by the checker.
#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("invalid deduction")]
pub enum InvalidDeduction {
    /// The constraint ID of the deduction is already used by an existing constraint.
    #[error("constraint id {0} already in use")]
    DuplicateConstraintId(ConstraintId),

    /// An inference in the deduction sequence does not exist in the proof stage.
    #[error("inference {0} does not exist")]
    UnknownInference(ConstraintId),

    /// The inferences in the proof stage do not derive an empty domain or an explicit
    /// conflict.
    #[error("no conflict was derived after applying all inferences")]
    NoConflict(Vec<IgnoredInference>),

    /// The premise contains mutually exclusive atomic constraints.
    #[error("the deduction contains inconsistent premises")]
    InconsistentPremises,
}

/// Verify that a deduction is valid given the inferences in the proof stage.
pub fn verify_deduction(
    deduction: &drcp_format::Deduction<Rc<str>, i32>,
    facts_in_proof_stage: &BTreeMap<ConstraintId, Fact>,
) -> Result<Nogood, InvalidDeduction> {
    // First we convert the deduction sequence to the types from the checking library.
    let inferences = deduction
        .sequence
        .iter()
        .map(|cid| {
            facts_in_proof_stage
                .get(cid)
                .map(|fact| SupportingInference {
                    premises: fact.premises.clone(),
                    consequent: fact.consequent.clone(),
                })
                .ok_or(InvalidDeduction::UnknownInference(*cid))
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Then we convert the deduction premise to the types from the checking library.
    let premises = deduction.premises.iter().cloned().map(Atomic::IntAtomic);

    match pumpkin_checking::verify_deduction(premises.clone(), inferences) {
        Ok(_) => Ok(Nogood::from(premises)),
        Err(error) => Err(convert_error(error, facts_in_proof_stage)),
    }
}

fn convert_error(
    error: pumpkin_checking::InvalidDeduction<Atomic>,
    facts_in_proof_stage: &BTreeMap<ConstraintId, Fact>,
) -> InvalidDeduction {
    match error {
        pumpkin_checking::InvalidDeduction::NoConflict(ignored_inferences) => {
            let mapped_ignored_inferences = ignored_inferences
                .into_iter()
                .map(|ignored_inference| {
                    convert_ignored_inferences(ignored_inference, facts_in_proof_stage)
                })
                .collect();

            InvalidDeduction::NoConflict(mapped_ignored_inferences)
        }
        pumpkin_checking::InvalidDeduction::InconsistentPremises => {
            InvalidDeduction::InconsistentPremises
        }
    }
}

fn convert_ignored_inferences(
    ignored_inference: pumpkin_checking::IgnoredInference<Atomic>,
    facts_in_proof_stage: &BTreeMap<ConstraintId, Fact>,
) -> IgnoredInference {
    IgnoredInference {
        constraint_id: facts_in_proof_stage
            .iter()
            .find_map(|(constraint_id, inference)| {
                let constraint_id = *constraint_id;
                let checker_inference = inference.clone();
                let ignored_inference_as_checker = Fact::from(ignored_inference.inference.clone());

                if checker_inference == ignored_inference_as_checker {
                    Some(constraint_id)
                } else {
                    None
                }
            })
            .expect("one of these will match"),

        unsatisfied_premises: ignored_inference
            .unsatisfied_premises
            .into_iter()
            .map(|premise| match premise {
                Atomic::True | Atomic::False => unreachable!(),

                Atomic::IntAtomic(int_atomic) => IntAtomic {
                    // Note: String is required here since the error type needs to
                    // implement `Send`. By default we use `Rc<str>` everywhere, which
                    // does not implement `Send`.
                    name: String::from(int_atomic.name.as_ref()),
                    comparison: int_atomic.comparison,
                    value: int_atomic.value,
                },
            })
            .collect(),
    }
}
