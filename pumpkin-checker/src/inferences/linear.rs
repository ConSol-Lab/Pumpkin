use drcp_format::ConstraintId;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Constraint;
use crate::model::Linear;
use crate::model::Model;
use crate::state::I32Ext;
use crate::state::VariableState;

/// Verify a `linear_bounds` inference.
///
/// The inference is sound for linear inequalites and linear equalities.
pub(super) fn verify_linear_bounds(
    model: &Model,
    fact: &Fact,
    generated_by: ConstraintId,
) -> Result<(), InvalidInference> {
    match model.get_constraint(generated_by) {
        Some(Constraint::LinearLeq(linear)) => verify_linear_inference(linear, fact),

        Some(Constraint::LinearEq(linear)) => {
            let try_upper_bound = verify_linear_inference(linear, fact);

            let inverted_linear = Linear {
                terms: linear
                    .terms
                    .iter()
                    .map(|(weight, variable)| (-weight, variable.clone()))
                    .collect(),
                bound: -linear.bound,
            };
            let try_lower_bound = verify_linear_inference(&inverted_linear, fact);

            match (try_lower_bound, try_upper_bound) {
                (Ok(_), Ok(_)) => panic!("This should not happen."),
                (Ok(fact), Err(_)) | (Err(_), Ok(fact)) => Ok(fact),
                (Err(_), Err(_)) => Err(InvalidInference::Unsound),
            }
        }

        Some(_) => Err(InvalidInference::ConstraintLabelMismatch),

        None => Err(InvalidInference::UndefinedConstraint),
    }
}

fn verify_linear_inference(linear: &Linear, fact: &Fact) -> Result<(), InvalidInference> {
    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    // Next, we evaluate the linear inequality. The lower bound of the
    // left-hand side must exceed the bound in the constraint.
    let left_hand_side = linear.terms.iter().fold(None, |acc, (weight, variable)| {
        let lower_bound = if *weight >= 0 {
            variable_state.lower_bound(variable)
        } else {
            variable_state.upper_bound(variable)
        };

        match acc {
            None => match lower_bound {
                I32Ext::I32(value) => Some(weight * value),
                I32Ext::NegativeInf => None,
                I32Ext::PositiveInf => None,
            },

            Some(v1) => match lower_bound {
                I32Ext::I32(v2) => Some(v1 + weight * v2),
                I32Ext::NegativeInf => Some(v1),
                I32Ext::PositiveInf => Some(v1),
            },
        }
    });

    if left_hand_side.is_some_and(|value| value > linear.bound) {
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
