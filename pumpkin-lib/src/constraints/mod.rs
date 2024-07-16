//! Defines the constraints which Pumpkin provides out of the box.
//!
//! A constraint is a relation over variables. In the solver, constraints are enforced through
//! propagators, and therefore constraints can be viewed as a collection of propagators.
//!
//! # Note
//! At the moment, the API for posting propagators is not yet publicly accessible as it is
//! highly unstable. Consumers of the Pumpkin library can therefore only define constraints by
//! decomposing them into the constraints that are predefined in the library. Once the
//! propagator API is stabilized, it will become part of the public API.

mod all_different;
mod arithmetic;
mod boolean;
mod clause;
mod cumulative;

pub use all_different::*;
pub use arithmetic::*;
pub use boolean::*;
pub use clause::*;
pub use cumulative::*;

use crate::engine::propagation::PropagatorConstructor;
use crate::propagators::element::ElementConstructor;
use crate::propagators::ReifiedPropagatorConstructor;
use crate::variables::IntegerVariable;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// A [`Constraint`] is a relation over variables.
pub trait Constraint {
    /// Post the constraint to the solver.
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError>;

    /// Post the half-reified version of the constraint to the solver. I.e. post the constraint
    /// `r -> Self` where `r` is a reification literal.
    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError>;
}

impl<Constructor> Constraint for Constructor
where
    Constructor: PropagatorConstructor,
    Constructor::Propagator: 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(self)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(ReifiedPropagatorConstructor {
            propagator: self,
            reification_literal,
        })
    }
}

impl<C: Constraint> Constraint for Vec<C> {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        self.into_iter().try_for_each(|c| c.post(solver))
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.into_iter()
            .try_for_each(|c| c.implied_by(solver, reification_literal))
    }
}

/// A [`Constraint`] which has a well-defined negation. Having a negation means the constraint can
/// be fully reified. I.e., a constraint `C` can be turned into `r <-> C` for a Boolean `r`.
pub trait NegatableConstraint: Constraint {
    type NegatedConstraint: NegatableConstraint + 'static;

    fn negation(&self) -> Self::NegatedConstraint;

    /// Post the reified version of the constraint to the solver. I.e. post the constraint
    /// `r <-> Self` where `r` is a reification literal.
    fn reify(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError>
    where
        Self: Sized,
    {
        let negation = self.negation();

        self.implied_by(solver, reification_literal)?;
        negation.implied_by(solver, !reification_literal)
    }
}

/// Creates the [element](https://sofdem.github.io/gccat/gccat/Celement.html) constraint which states that `array[index] = rhs`.
pub fn element<ElementVar: IntegerVariable + 'static>(
    index: impl IntegerVariable + 'static,
    array: impl Into<Box<[ElementVar]>>,
    rhs: impl IntegerVariable + 'static,
) -> impl Constraint {
    ElementConstructor {
        index,
        array: array.into(),
        rhs,
    }
}
