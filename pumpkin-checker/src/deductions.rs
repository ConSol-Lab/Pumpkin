use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::VariableState;

use crate::inferences::Fact;
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
    // To verify a deduction, we assume that the premises are true. Then we go over all the
    // facts in the sequence, and if all the premises are satisfied, we apply the consequent.
    // At some point, this should either reach a fact without a consequent or derive an
    // inconsistent domain.

    let mut variable_state = VariableState::prepare_for_conflict_check(
        deduction.premises.iter().cloned().map(Into::into),
        None,
    )
    .map_err(|_| InvalidDeduction::InconsistentPremises)?;

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
                        name: String::from(premise.identifier().as_ref()),
                        comparison: match premise.comparison() {
                            pumpkin_checking::Comparison::GreaterEqual => {
                                drcp_format::IntComparison::GreaterEqual
                            }
                            pumpkin_checking::Comparison::LessEqual => {
                                drcp_format::IntComparison::LessEqual
                            }
                            pumpkin_checking::Comparison::Equal => {
                                drcp_format::IntComparison::Equal
                            }
                            pumpkin_checking::Comparison::NotEqual => {
                                drcp_format::IntComparison::NotEqual
                            }
                        },
                        value: premise.value(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::atomic;
    use crate::fact;
    use crate::test_utils::constraint_id;
    use crate::test_utils::deduction;

    macro_rules! facts {
        {$($k: expr => $v: expr),* $(,)?} => {
            ::std::collections::BTreeMap::from([$(($crate::test_utils::constraint_id($k), $v),)*])
        };
    }

    #[test]
    fn a_sequence_is_correctly_traversed() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2, 3]);

        let facts_in_proof_stage = facts! {
            1 => fact!([x >= 5] -> [y <= 4]),
            2 => fact!([y <= 7] -> [z != 10]),
            3 => fact!([y <= 5] & [z != 10] -> [x <= 4]),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn an_inference_implying_false_is_a_valid_stopping_condition() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2, 3]);

        let facts_in_proof_stage = facts! {
            1 => fact!([x >= 5] -> [y <= 4]),
            2 => fact!([y <= 7] -> [z != 10]),
            3 => fact!([y <= 5] & [z != 10] -> false),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn inference_order_does_not_need_to_be_by_constraint_id() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1, 4]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn inconsistent_premises_are_identified() {
        let premises = vec![atomic!([x >= 5]), atomic!([x <= 4])];
        let deduction = deduction(5, premises.clone(), [2]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x == 5] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("inconsistent premises");
        assert_eq!(InvalidDeduction::InconsistentPremises, error);
    }

    #[test]
    fn all_inferences_in_sequence_must_be_in_fact_database() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x == 5] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(InvalidDeduction::UnknownInference(constraint_id(1)), error);
    }

    #[test]
    fn sequence_that_does_not_terminate_in_conflict_is_rejected() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(InvalidDeduction::NoConflict(vec![]), error);
    }

    #[test]
    fn inferences_with_unsatisfied_premises_are_ignored() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] & [x >= 6] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(
            InvalidDeduction::NoConflict(vec![IgnoredInference {
                constraint_id: constraint_id(1),
                unsatisfied_premises: vec![atomic!([x string >= 6])],
            }]),
            error
        );
    }
}
