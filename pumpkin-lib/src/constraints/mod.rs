//! Defines the constraints that Pumpkin provides out of the box which can be added to the
//! [`Solver`].
//!
//! A constraint is a relation over variables. In the solver, constraints are enforced through
//! propagators, and therefore constraints can be viewed as a collection of propagators.
//!
//! # Example
//! ```
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::Solver;
//! let mut solver = Solver::default();
//!
//! let a = solver.new_bounded_integer(0, 3);
//! let b = solver.new_bounded_integer(0, 3);
//!
//! solver.add_constraint(constraints::equals([a, b], 0)).post();
//! ```
//!
//! # Note
//! At the moment, the API for posting propagators is not yet publicly accessible as it is
//! unstable. Consumers of the Pumpkin library can therefore only define constraints by
//! decomposing them into the constraints that are predefined in the library. Once the
//! propagator API is stabilized, it will become part of the public API.

mod all_different;
mod arithmetic;
mod boolean;
mod clause;
mod constraint_poster;
mod cumulative;
mod element;

use std::num::NonZero;

pub use all_different::*;
pub use arithmetic::*;
pub use boolean::*;
pub use clause::*;
pub use constraint_poster::*;
pub use cumulative::*;
pub use element::*;

use crate::engine::propagation::Propagator;
use crate::propagators::ReifiedPropagator;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// A [`Constraint`] is a relation over variables. It disqualifies certain partial assignments of
/// making it into a solution of the problem.
///
/// For example, the constraint `a = b` over two variables `a` and `b` only allows assignments to
/// `a` and `b` of the same value, and rejects any assignment where `a` and `b` differ.
pub trait Constraint {
    /// Add the [`Constraint`] to the [`Solver`].
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    ///
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn post(
        &self,
        solver: &mut Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError>;

    /// Add the half-reified version of the [`Constraint`] to the [`Solver`]; i.e. post the
    /// constraint `r -> constraint` where `r` is a reification literal.
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    ///
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn implied_by(
        &self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError>;

    /// Clone the constraint as a trait object.
    ///
    /// This allows cloning of `Box<dyn Constraint>`, which is not possible through the regular
    /// `Clone` trait.
    fn boxed_clone(&self) -> Box<dyn Constraint>;
}

impl<ConcretePropagator> Constraint for ConcretePropagator
where
    ConcretePropagator: Propagator + Clone + 'static,
{
    fn post(
        &self,
        solver: &mut Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        if let Some(tag) = tag {
            solver.add_tagged_propagator(self.clone(), tag)
        } else {
            solver.add_propagator(self.clone())
        }
    }

    fn implied_by(
        &self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        if let Some(tag) = tag {
            solver.add_tagged_propagator(
                ReifiedPropagator::new(self.clone(), reification_literal),
                tag,
            )
        } else {
            solver.add_propagator(ReifiedPropagator::new(self.clone(), reification_literal))
        }
    }

    fn boxed_clone(&self) -> Box<dyn Constraint> {
        todo!()
    }
}

/// A [`Constraint`] which has a well-defined negation.
///
/// Having a negation means the [`Constraint`] can be fully reified; i.e., a constraint `C` can be
/// turned into `r <-> C` where `r` is a reification literal.
///
/// For example, the negation of the [`Constraint`] `a = b` is (well-)defined as `a != b`.
pub trait NegatableConstraint: Constraint {
    type NegatedConstraint: NegatableConstraint + 'static;

    fn negation(&self) -> Self::NegatedConstraint;

    /// Add the reified version of the [`Constraint`] to the [`Solver`]; i.e. post the constraint
    /// `r <-> constraint` where `r` is a reification literal.
    ///
    /// This method returns a [`ConstraintOperationError`] if the addition of the [`Constraint`] led
    /// to a root-level conflict.
    ///
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn reify(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError>
    where
        Self: Sized,
    {
        let negation = self.negation();

        self.implied_by(solver, reification_literal, tag)?;
        negation.implied_by(solver, !reification_literal, tag)
    }
}
