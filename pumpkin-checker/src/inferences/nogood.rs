use drcp_format::ConstraintId;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::model::Model;
use crate::state::VariableState;

pub(crate) fn verify_nogood(
    model: &Model,
    premises: &[Atomic],
    consequent: Option<Atomic>,
    generated_by: ConstraintId,
) -> Result<Fact, InvalidInference> {
    let Some(constraint) = model.get_constraint(generated_by) else {
        return Err(InvalidInference::UndefinedConstraint);
    };

    let Constraint::Nogood(nogood) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // First, we apply all the premises of the constraint that generated the inference.
    let mut variable_state = VariableState::default();

    for premise in nogood.as_ref().iter() {
        assert!(
            variable_state.apply(premise.clone()),
            "this can only fail if the nogood contains inconsistent atomics"
        );
    }

    for premise in premises {
        if !variable_state.is_true(premise.clone()) {
            return Err(InvalidInference::Unsound);
        }
    }

    if let Some(consequent) = consequent.clone() {
        if !variable_state.is_true(!consequent) {
            return Err(InvalidInference::Unsound);
        }
    }

    Ok(Fact {
        premises: premises.to_vec(),
        consequent,
    })
}
