use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_propagators::arithmetic::LinearLessOrEqualInferenceChecker;

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
    _: &Fact,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let checker = LinearLessOrEqualInferenceChecker::new(linear.terms.clone().into(), linear.bound);

    if InferenceChecker::<Atomic>::check(&checker, state) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}

#[cfg(test)]
mod tests {
    use drcp_format::IntComparison::*;
    use fzn_rs::VariableExpr::*;

    use super::*;
    use crate::model::Atomic;

    #[test]
    fn linear_1() {
        // x1 - x2 <= -7
        let linear = Linear {
            terms: vec![(1, Identifier("x1".into())), (-1, Identifier("x2".into()))],
            bound: -7,
        };

        let premises = vec![Atomic {
            name: "x2".into(),
            comparison: LessEqual,
            value: 37,
        }];

        let consequent = Some(Atomic {
            name: "x1".into(),
            comparison: LessEqual,
            value: 30,
        });

        verify_linear_inference(
            &linear,
            &Fact {
                premises,
                consequent,
            },
        )
        .expect("valid inference");
    }
}
