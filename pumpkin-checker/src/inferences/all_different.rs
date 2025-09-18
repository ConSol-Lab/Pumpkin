use std::collections::HashSet;

use drcp_format::ConstraintId;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::model::Model;
use crate::state::VariableState;

pub(crate) fn verify_all_different(
    model: &Model,
    premises: &[Atomic],
    consequent: Option<Atomic>,
    generated_by: ConstraintId,
) -> Result<Fact, InvalidInference> {
    let Some(constraint) = model.get_constraint(generated_by) else {
        return Err(InvalidInference::UndefinedConstraint);
    };

    let Constraint::AllDifferent(all_different) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // First, we apply all the premises of the constraint that generated the inference.
    let mut variable_state = VariableState::default();

    for premise in premises {
        if !variable_state.apply(premise.clone()) {
            return Err(InvalidInference::InconsistentPremises);
        }
    }

    if let Some(consequent) = consequent.clone() {
        if !variable_state.apply(!consequent.clone()) {
            return Err(InvalidInference::InconsistentPremises);
        }
    }

    let (variables, values) = all_different
        .variables
        .iter()
        .filter_map(|variable| {
            variable_state
                .fixed_value(variable)
                .map(|value| (variable, value))
        })
        .collect::<(Vec<_>, HashSet<_>)>();

    if values.len() < variables.len() {
        Ok(Fact {
            premises: premises.to_vec(),
            consequent,
        })
    } else {
        Err(InvalidInference::Unsound)
    }
}
