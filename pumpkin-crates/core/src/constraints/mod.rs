//! Defines the main building blocks of constraints.
use crate::Solver;
use crate::propagation::PropagatorConstructor;
use crate::propagators::reified_propagator::ReifiedPropagatorArgs;
use crate::variables::Literal;

mod constraint_poster;
pub use constraint_poster::ConstraintPoster;

/// A [`Constraint`] is a relation over variables. It disqualifies certain partial assignments of
/// making it into a solution of the problem.
///
/// For example, the constraint `a = b` over two variables `a` and `b` only allows assignments to
/// `a` and `b` of the same value, and rejects any assignment where `a` and `b` differ.
pub trait Constraint {
    /// Add the [`Constraint`] to the [`Solver`].
    ///
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn post(self, solver: &mut Solver);

    /// Add the half-reified version of the [`Constraint`] to the [`Solver`]; i.e. post the
    /// constraint `r -> constraint` where `r` is a reification literal.
    ///
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn implied_by(self, solver: &mut Solver, reification_literal: Literal);
}

impl<ConcretePropagator> Constraint for ConcretePropagator
where
    ConcretePropagator: PropagatorConstructor + 'static,
{
    fn post(self, solver: &mut Solver) {
        let _ = solver.add_propagator(self);
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        let _ = solver.add_propagator(ReifiedPropagatorArgs {
            propagator: self,
            reification_literal,
        });
    }
}

impl<C: Constraint> Constraint for Vec<C> {
    fn post(self, solver: &mut Solver) {
        self.into_iter().for_each(|c| c.post(solver))
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        self.into_iter()
            .for_each(|c| c.implied_by(solver, reification_literal))
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
    /// The `tag` allows inferences to be traced to the constraint that implies them. They will
    /// show up in the proof log.
    fn reify(self, solver: &mut Solver, reification_literal: Literal)
    where
        Self: Sized,
    {
        let negation = self.negation();

        self.implied_by(solver, reification_literal);
        negation.implied_by(solver, !reification_literal)
    }
}
