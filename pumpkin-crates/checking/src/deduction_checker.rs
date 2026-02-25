use crate::AtomicConstraint;
use crate::VariableState;

/// An inference that was ignored when checking a deduction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IgnoredInference<Atomic> {
    /// The inference that was ignored.
    pub inference: SupportingInference<Atomic>,

    /// The premises that were not satisfied when the inference was evaluated.
    pub unsatisfied_premises: Vec<Atomic>,
}

/// A deduction is rejected by the checker.
#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("invalid deduction")]
pub enum InvalidDeduction<Atomic> {
    /// The inferences in the proof stage do not derive an empty domain or an explicit
    /// conflict.
    #[error("no conflict was derived after applying all inferences")]
    NoConflict(Vec<IgnoredInference<Atomic>>),

    /// The premise contains mutually exclusive atomic constraints.
    #[error("the deduction contains inconsistent premises")]
    InconsistentPremises,
}

/// An inference used to support a deduction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SupportingInference<Atomic> {
    /// The premises of the inference.
    pub premises: Vec<Atomic>,
    /// The consequent of the inference.
    ///
    /// [`None`] represents the literal false. I.e., if the consequent is [`None`], then the
    /// premises imply false.
    pub consequent: Option<Atomic>,
}

/// Verify that a deduction is valid given the inferences in the proof stage.
///
/// The `inferences` are considered in the order they are provided.
pub fn verify_deduction<Atomic>(
    premises: impl IntoIterator<Item = Atomic>,
    inferences: impl IntoIterator<Item = SupportingInference<Atomic>>,
) -> Result<(), InvalidDeduction<Atomic>>
where
    Atomic: AtomicConstraint,
{
    // To verify a deduction, we assume that the premises are true. Then we go over all the
    // facts in the sequence, and if all the premises are satisfied, we apply the consequent.
    // At some point, this should either reach a fact without a consequent or derive an
    // inconsistent domain.

    let mut variable_state = VariableState::prepare_for_conflict_check(premises, None)
        .map_err(|_| InvalidDeduction::InconsistentPremises)?;

    let mut unused_inferences = Vec::new();

    for inference in inferences.into_iter() {
        // Collect all premises that do not evaluate to `true` under the current variable
        // state.
        let unsatisfied_premises = inference
            .premises
            .iter()
            .filter(|premise| !variable_state.is_true(premise))
            .cloned()
            .collect::<Vec<_>>();

        // If at least one premise is unassigned, this fact is ignored for the conflict
        // check and recorded as unused.
        if !unsatisfied_premises.is_empty() {
            unused_inferences.push(IgnoredInference {
                inference,
                unsatisfied_premises,
            });

            continue;
        }

        // At this point the premises are satisfied so we handle the consequent of the
        // inference.
        match &inference.consequent {
            Some(consequent) => {
                if !variable_state.apply(consequent) {
                    // If applying the consequent yields an empty domain for a
                    // variable, then the deduction is valid.
                    return Ok(());
                }
            }
            // If the consequent is explicitly false, then the deduction is valid.
            None => return Ok(()),
        }
    }

    // Reaching this point means that the conjunction of inferences did not yield to a
    // conflict. Therefore the deduction is invalid.
    Err(InvalidDeduction::NoConflict(unused_inferences))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_atomic;

    /// Create a [`SupportingInference`] in a DSL.
    ///
    /// # Example
    /// ```
    /// inference!([x >= 5] & [y <= 10] -> [z == 5]);
    /// inference!([x >= 5] & [y <= 10] -> false);
    /// ```
    #[macro_export]
    macro_rules! inference {
        // Case: consequent is an Atomic
        (
            $($prem:tt)&+ -> [$($cons:tt)+]
        ) => {
            SupportingInference {
                premises: vec![$( test_atomic!($prem) ),+],
                consequent: Some(test_atomic!([$($cons)+])),
            }
        };

        // Case: consequent is false (i.e., None)
        (
            $($prem:tt)&+ -> false
        ) => {
            SupportingInference {
                premises: vec![$( test_atomic!($prem) ),+],
                consequent: None,
            }
        };
    }

    #[test]
    fn a_sequence_is_correctly_traversed() {
        let premises = vec![test_atomic!([x >= 5])];

        let inferences = vec![
            inference!([x >= 5] -> [y <= 4]),
            inference!([y <= 7] -> [z != 10]),
            inference!([y <= 5] & [z != 10] -> [x <= 4]),
        ];

        verify_deduction(premises, inferences).expect("valid deduction");
    }

    #[test]
    fn an_inference_implying_false_is_a_valid_stopping_condition() {
        let premises = vec![test_atomic!([x >= 5])];

        let inferences = vec![
            inference!([x >= 5] -> [y <= 4]),
            inference!([y <= 7] -> [z != 10]),
            inference!([y <= 5] & [z != 10] -> false),
        ];

        verify_deduction(premises, inferences).expect("valid deduction");
    }

    #[test]
    fn inconsistent_premises_are_identified() {
        let premises = vec![test_atomic!([x >= 5]), test_atomic!([x <= 4])];

        let inferences = vec![inference!([x == 5] -> false)];

        let error = verify_deduction(premises, inferences).expect_err("inconsistent premises");
        assert_eq!(InvalidDeduction::InconsistentPremises, error);
    }

    #[test]
    fn sequence_that_does_not_terminate_in_conflict_is_rejected() {
        let premises = vec![test_atomic!([x >= 5])];

        let inferences = vec![
            inference!([x >= 5] -> [y <= 4]),
            inference!([y <= 7] -> [z != 10]),
        ];

        let error = verify_deduction(premises, inferences).expect_err("conflict is not reached");
        assert_eq!(InvalidDeduction::NoConflict(vec![]), error);
    }

    #[test]
    fn inferences_with_unsatisfied_premises_are_ignored() {
        let premises = vec![test_atomic!([x >= 5])];

        let inferences = vec![
            inference!([x >= 5] -> [y <= 4]),
            inference!([y <= 7] & [x >= 6] -> [z != 10]),
            inference!([y <= 5] & [z != 10] -> false),
        ];

        let error = verify_deduction(premises, inferences).expect_err("premises are not satisfied");
        assert_eq!(
            InvalidDeduction::NoConflict(vec![
                IgnoredInference {
                    inference: inference!([y <= 7] & [x >= 6] -> [z != 10]),
                    unsatisfied_premises: vec![test_atomic!([x >= 6])],
                },
                IgnoredInference {
                    inference: inference!([y <= 5] & [z != 10] -> false),
                    unsatisfied_premises: vec![test_atomic!([z != 10])],
                }
            ]),
            error
        );
    }
}
