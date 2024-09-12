use std::num::NonZero;

use log::warn;

use super::Constraint;
use super::NegatableConstraint;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// A structure which is responsible for adding the created [`Constraint`]s to the
/// [`Solver`]. For an example on how to use this, see [`crate::constraints`].
#[derive(Debug)]
pub struct ConstraintPoster<'solver, ConstraintImpl> {
    solver: &'solver mut Solver,
    constraint: Option<ConstraintImpl>,
    tag: Option<NonZero<u32>>,
}

impl<'a, ConstraintImpl> ConstraintPoster<'a, ConstraintImpl> {
    pub(crate) fn new(solver: &'a mut Solver, constraint: ConstraintImpl) -> Self {
        ConstraintPoster {
            solver,
            constraint: Some(constraint),
            tag: None,
        }
    }
}

impl<ConstraintImpl: Constraint> ConstraintPoster<'_, ConstraintImpl> {
    /// Tag the constraint with an integer. This tag is used in the proof to identify which
    /// constraints trigger particular inferences.
    pub fn with_tag(mut self, tag: NonZero<u32>) -> Self {
        self.tag = Some(tag);

        self
    }

    /// Add the [`Constraint`] to the [`Solver`].
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    pub fn post(mut self) -> Result<(), ConstraintOperationError> {
        self.constraint.take().unwrap().post(self.solver, self.tag)
    }

    /// Add the half-reified version of the [`Constraint`] to the [`Solver`]; i.e. post the
    /// constraint `r -> constraint` where `r` is a reification literal.
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    pub fn implied_by(
        mut self,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.constraint
            .take()
            .unwrap()
            .implied_by(self.solver, reification_literal, self.tag)
    }
}

impl<ConstraintImpl: NegatableConstraint> ConstraintPoster<'_, ConstraintImpl> {
    /// Add the reified version of the [`Constraint`] to the [`Solver`]; i.e. post the constraint
    /// `r <-> constraint` where `r` is a reification literal.
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    pub fn reify(mut self, reification_literal: Literal) -> Result<(), ConstraintOperationError> {
        self.constraint
            .take()
            .unwrap()
            .reify(self.solver, reification_literal, self.tag)
    }
}

impl<ConstraintImpl> Drop for ConstraintPoster<'_, ConstraintImpl> {
    fn drop(&mut self) {
        if self.constraint.is_some() {
            warn!("A constraint poster is never used, this is likely a mistake.");
        }
    }
}
