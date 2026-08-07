use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::circuit::CircuitConstructor;

use crate::all_different;

pub fn circuit<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    Circuit {
        successors: variables.into(),
        constraint_tag,
    }
}

struct Circuit<Var> {
    successors: Box<[Var]>,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> Constraint for Circuit<Var> {
    fn post(
        self,
        solver: &mut pumpkin_core::Solver,
    ) -> Result<(), pumpkin_core::ConstraintOperationError> {
        all_different(self.successors.clone(), self.constraint_tag).post(solver)?;

        CircuitConstructor {
            successors: self.successors,
            constraint_tag: self.constraint_tag,
        }
        .post(solver)
    }

    fn implied_by(
        self,
        solver: &mut pumpkin_core::Solver,
        reification_literal: pumpkin_core::variables::Literal,
    ) -> Result<(), pumpkin_core::ConstraintOperationError> {
        all_different(self.successors.clone(), self.constraint_tag)
            .implied_by(solver, reification_literal)?;

        CircuitConstructor {
            successors: self.successors,
            constraint_tag: self.constraint_tag,
        }
        .implied_by(solver, reification_literal)
    }
}
