use log::warn;

use super::Constraint;
use super::NegatableConstraint;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// A temporary structure which is responsible for actually adding created constraints to the
/// solver. For an example on how to use this, see [`Solver::add_constraint`].
#[derive(Debug)]
pub struct ConstraintPoster<'solver, ConstraintImpl> {
    solver: &'solver mut Solver,
    constraint: Option<ConstraintImpl>,
}

impl<'a, ConstraintImpl> ConstraintPoster<'a, ConstraintImpl> {
    pub(crate) fn new(solver: &'a mut Solver, constraint: ConstraintImpl) -> Self {
        ConstraintPoster {
            solver,
            constraint: Some(constraint),
        }
    }
}

impl<ConstraintImpl: Constraint> ConstraintPoster<'_, ConstraintImpl> {
    /// Add the constraint to the solver.
    pub fn post(mut self) -> Result<(), ConstraintOperationError> {
        self.constraint.take().unwrap().post(self.solver)
    }

    /// Add the half-reified version of the constraint to the solver. I.e. post the constraint
    /// `r -> Self` where `r` is a reification literal.
    pub fn implied_by(
        mut self,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.constraint
            .take()
            .unwrap()
            .implied_by(self.solver, reification_literal)
    }
}

impl<ConstraintImpl: NegatableConstraint> ConstraintPoster<'_, ConstraintImpl> {
    /// Add the reified version of the constraint to the solver. I.e. post the constraint
    /// `r <-> Self` where `r` is a reification literal.
    pub fn reify(mut self, reification_literal: Literal) -> Result<(), ConstraintOperationError> {
        self.constraint
            .take()
            .unwrap()
            .reify(self.solver, reification_literal)
    }
}

impl<ConstraintImpl> Drop for ConstraintPoster<'_, ConstraintImpl> {
    fn drop(&mut self) {
        if self.constraint.is_some() {
            warn!("A constraint poster is never used, this is likely a mistake.");
        }
    }
}
