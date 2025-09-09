use drcp_format::ConstraintId;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::model::Linear;
use crate::model::Model;
use crate::state::I32Ext;
use crate::state::VariableState;

pub(super) fn verify_linear_bounds(
    model: &Model,
    premises: &[Atomic],
    consequent: Option<Atomic>,
    generated_by: ConstraintId,
) -> Result<Fact, InvalidInference> {
    match model.get_constraint(generated_by) {
        Some(Constraint::LinearLeq(linear)) => {
            verify_linear_inference(linear, premises, consequent)
        }

        Some(Constraint::LinearEq(linear)) => {
            let try_upper_bound = verify_linear_inference(linear, premises, consequent.clone());

            let inverted_linear = Linear {
                terms: linear
                    .terms
                    .iter()
                    .map(|(weight, variable)| (-weight, variable.clone()))
                    .collect(),
                bound: -linear.bound,
            };
            let try_lower_bound = verify_linear_inference(&inverted_linear, premises, consequent);

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

fn verify_linear_inference(
    linear: &Linear,
    premises: &[Atomic],
    consequent: Option<Atomic>,
) -> Result<Fact, InvalidInference> {
    // First, we apply all the premises and the negation of the consequent to a
    // variable state.
    let mut variable_state = VariableState::default();

    for premise in premises {
        if !variable_state.apply(premise.clone()) {
            return Err(InvalidInference::InconsistentPremises);
        }
    }

    if let Some(consequent) = consequent.clone() {
        if !variable_state.apply(consequent.clone()) {
            return Err(InvalidInference::InconsistentPremises);
        }
    }

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
        Ok(Fact {
            premises: premises.to_vec(),
            consequent: consequent.clone(),
        })
    } else {
        Err(InvalidInference::Unsound)
    }
}
