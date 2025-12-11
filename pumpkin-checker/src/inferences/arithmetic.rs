use std::collections::BTreeSet;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntComparison;
use fzn_rs::VariableExpr;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::AllDifferent;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::model::Model;
use crate::state::I32Ext;
use crate::state::VariableState;

/// Verify a `binary_equals` inference.
///
/// The checker accepts inferences for binary equality constraints. The difference with the general
/// `linear_bounds` inference is that in the binary case, we can certify holes in the domain as
/// well.
pub(crate) fn verify_binary_equals(
    model: &Model,
    premises: &[Atomic],
    consequent: Option<Atomic>,
    generated_by: ConstraintId,
) -> Result<Fact, InvalidInference> {
    let Some(constraint) = model.get_constraint(generated_by) else {
        return Err(InvalidInference::UndefinedConstraint);
    };

    let Constraint::LinearEq(linear) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    if linear.terms.len() != 2 || linear.terms[0].0 != 1 || linear.terms[1].0 != -1 {
        return Err(InvalidInference::Unsound);
    }

    let mut variable_state =
        VariableState::prepare_for_conflict_check(premises, consequent.as_ref())
            .ok_or(InvalidInference::InconsistentPremises)?;

    let var_expr1 = &linear.terms[0].1;
    let var_expr2 = &linear.terms[1].1;

    // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
    // the step is unsound!
    let is_consistent = match var_expr1 {
        VariableExpr::Identifier(var1) => {
            let mut consistent = true;

            if let I32Ext::I32(value) = variable_state.upper_bound(var_expr2) {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::LessEqual,
                    value: linear.bound + value,
                });
            }

            if let I32Ext::I32(value) = variable_state.lower_bound(var_expr2) {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::GreaterEqual,
                    value: linear.bound + value,
                });
            }

            for value in variable_state.holes(var_expr2).collect::<Vec<_>>() {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::NotEqual,
                    value: linear.bound + value,
                });
            }

            consistent
        }

        VariableExpr::Constant(value) => match var_expr2 {
            VariableExpr::Identifier(var2) => variable_state.apply(&Atomic {
                name: Rc::clone(var2),
                comparison: IntComparison::NotEqual,
                value: linear.bound + *value,
            }),
            VariableExpr::Constant(_) => panic!("Would be weird to end up here."),
        },
    };

    if is_consistent {
        return Err(InvalidInference::Unsound);
    }

    Ok(Fact {
        premises: premises.to_vec(),
        consequent,
    })
}

/// Verify a `binary_not_equals` inference.
///
/// Tests that the premise of the inference and the negation of the conclusion force the linear sum
/// to equal the right-hand side of the not equals constraint.
pub(crate) fn verify_binary_not_equals(
    model: &Model,
    premises: &[Atomic],
    consequent: Option<Atomic>,
    generated_by: ConstraintId,
) -> Result<Fact, InvalidInference> {
    let Some(constraint) = model.get_constraint(generated_by) else {
        return Err(InvalidInference::UndefinedConstraint);
    };

    let Constraint::AllDifferent(AllDifferent { variables }) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(premises, consequent.as_ref())
        .ok_or(InvalidInference::InconsistentPremises)?;

    let mut values = BTreeSet::new();
    for variable in variables {
        let Some(value) = variable_state.fixed_value(variable) else {
            continue;
        };

        if !values.insert(value) {
            return Ok(Fact {
                premises: premises.to_vec(),
                consequent,
            });
        }
    }

    Err(InvalidInference::Unsound)
}
