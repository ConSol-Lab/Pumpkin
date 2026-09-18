use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_propagators::arithmetic::LinearLessOrEqualChecker;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::model::Linear;
use crate::model::Term;

/// Verify a `linear_bounds` inference.
///
/// The inference is sound for linear inequalites and linear equalities.
pub(super) fn verify_linear_bounds(
    fact: &Fact,
    generated_by: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    match generated_by {
        Constraint::LinearLeq(linear) => verify_linear_inference(linear, fact, state),

        Constraint::LinearEq(linear) => {
            let try_upper_bound = verify_linear_inference(linear, fact, state.clone());

            let inverted_linear = Linear {
                terms: linear
                    .terms
                    .iter()
                    .map(|term| Term {
                        weight: -term.weight,
                        variable: term.variable.clone(),
                    })
                    .collect(),
                bound: -linear.bound,
            };
            let try_lower_bound = verify_linear_inference(&inverted_linear, fact, state);

            match (try_lower_bound, try_upper_bound) {
                (Ok(_), Ok(_)) => panic!("This should not happen."),
                (Ok(fact), Err(_)) | (Err(_), Ok(fact)) => Ok(fact),
                (Err(_), Err(_)) => Err(InvalidInference::Unsound),
            }
        }

        _ => Err(InvalidInference::ConstraintLabelMismatch),
    }
}

fn verify_linear_inference(
    linear: &Linear,
    fact: &Fact,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let checker = LinearLessOrEqualChecker::new(linear.terms.clone().into(), linear.bound);

    if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;
    use std::rc::Rc;

    use drcp_format::IntAtomic;
    use drcp_format::IntComparison::*;
    use fzn_rs::VariableExpr::*;

    use super::*;
    use crate::model::Atomic;

    #[test]
    fn linear_1() {
        // x1 - x2 <= -7
        let linear = Linear {
            terms: vec![
                Term {
                    weight: NonZero::new(1).unwrap(),
                    variable: Identifier(Rc::from("x1")).into(),
                },
                Term {
                    weight: NonZero::new(-1).unwrap(),
                    variable: Identifier(Rc::from("x2")).into(),
                },
            ],
            bound: -7,
        };

        let premises = vec![Atomic::IntAtomic(IntAtomic {
            name: "x2".into(),
            comparison: LessEqual,
            value: 37,
        })];

        let consequent = Some(Atomic::IntAtomic(IntAtomic {
            name: "x1".into(),
            comparison: LessEqual,
            value: 30,
        }));

        let variable_state =
            VariableState::prepare_for_conflict_check(premises.clone(), consequent.clone())
                .expect("no mutually exclusive atomics");

        verify_linear_inference(
            &linear,
            &Fact {
                premises,
                consequent,
            },
            variable_state,
        )
        .expect("valid inference");
    }
}
