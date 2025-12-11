use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;

use crate::inferences::Fact;
use crate::model::Nogood;
use crate::state::VariableState;

/// An inference that was ignored when checking a deduction.
#[derive(Clone, Debug)]
pub struct IgnoredInference {
    /// The ID of the ignored inference.
    pub constraint_id: ConstraintId,

    /// The premises that were not satisfied when the inference was evaluated.
    pub unsatisfied_premises: Vec<IntAtomic<String, i32>>,
}

/// A deduction is rejected by the checker.
#[derive(thiserror::Error, Debug)]
#[error("invalid deduction")]
pub enum InvalidDeduction {
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
    // To verify a deduction, we assume that the premises are true. Then we go over all the
    // facts in the sequence, and if all the premises are satisfied, we apply the consequent.
    // At some point, this should either reach a fact without a consequent or derive an
    // inconsistent domain.

    let mut variable_state = VariableState::prepare_for_conflict_check(&deduction.premises, None)
        .ok_or(InvalidDeduction::InconsistentPremises)?;

    let mut unused_inferences = Vec::new();

    for constraint_id in deduction.sequence.iter() {
        // Get the fact associated with the constraint ID from the sequence.
        let fact = facts_in_proof_stage
            .get(constraint_id)
            .ok_or(InvalidDeduction::UnknownInference(*constraint_id))?;

        // Collect all premises that do not evaluate to `true` under the current variable
        // state.
        let unsatisfied_premises: Vec<IntAtomic<String, i32>> = fact
            .premises
            .iter()
            .filter_map::<IntAtomic<String, i32>, _>(|premise| {
                if variable_state.is_true(premise) {
                    None
                } else {
                    // We need to convert the premise name from a `Rc<str>` to a
                    // `String`. The former does not implement `Send`, but that is
                    // required for our error type to be used with anyhow.
                    Some(IntAtomic {
                        name: String::from(premise.name.as_ref()),
                        comparison: premise.comparison,
                        value: premise.value,
                    })
                }
            })
            .collect::<Vec<_>>();

        // If at least one premise is unassigned, this fact is ignored for the conflict
        // check and recorded as unused.
        if !unsatisfied_premises.is_empty() {
            unused_inferences.push(IgnoredInference {
                constraint_id: *constraint_id,
                unsatisfied_premises,
            });

            continue;
        }

        // At this point the premises are satisfied so we handle the consequent of the
        // inference.
        match &fact.consequent {
            Some(consequent) => {
                if !variable_state.apply(consequent) {
                    // If applying the consequent yields an empty domain for a
                    // variable, then the deduction is valid.
                    return Ok(Nogood::from(deduction.premises.clone()));
                }
            }
            // If the consequent is explicitly false, then the deduction is valid.
            None => return Ok(Nogood::from(deduction.premises.clone())),
        }
    }

    // Reaching this point means that the conjunction of inferences did not yield to a
    // conflict. Therefore the deduction is invalid.
    Err(InvalidDeduction::NoConflict(unused_inferences))
}
