use crate::constraints::Constraint;
use crate::constraints::NegatableConstraint;
use crate::propagators::linear_less_or_equal::linear_less_or_equal_constructor::IncrementalLinearLessOrEqual;
use crate::propagators::linear_less_or_equal::linear_less_or_equal_constructor::LinearLessOrEqual;
use crate::variables::IntegerVariable;
use crate::ConstraintOperationError;
use crate::Solver;

/// Create the constraint `\sum terms_i <= rhs`.
pub fn less_than_or_equals<Var: IntegerVariable + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
) -> impl NegatableConstraint {
    Inequality {
        terms: terms.into(),
        rhs,
    }
}

/// Creates the constraint `lhs <= rhs`.
pub fn binary_less_than_or_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
) -> impl NegatableConstraint {
    less_than_or_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
}

/// Creates the constraint `lhs < rhs`.
pub fn binary_less_than<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
) -> impl NegatableConstraint {
    binary_less_than_or_equals(lhs.scaled(1), rhs.offset(-1))
}

struct Inequality<Var> {
    terms: Box<[Var]>,
    rhs: i32,
}

/// If the number of terms in an [`Inequality`] is larger than this threshold, the
/// [`IncrementalLinearLessOrEqual`] propagator is used, otherwise the [`LinearLessOrEqual`]
/// propagator is used.
const TERM_THRESHOLD: usize = 30;

impl<Var: IntegerVariable + 'static> Constraint for Inequality<Var> {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        if self.terms.len() >= TERM_THRESHOLD {
            // Heuristic way in which to select the propagator to post
            IncrementalLinearLessOrEqual::new(self.terms, self.rhs).post(solver)
        } else {
            LinearLessOrEqual::new(self.terms, self.rhs).post(solver)
        }
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: crate::variables::Literal,
    ) -> Result<(), ConstraintOperationError> {
        if self.terms.len() >= TERM_THRESHOLD {
            // Heuristic way in which to select the propagator to post
            IncrementalLinearLessOrEqual::new(self.terms, self.rhs)
                .implied_by(solver, reification_literal)
        } else {
            LinearLessOrEqual::new(self.terms, self.rhs).implied_by(solver, reification_literal)
        }
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for Inequality<Var> {
    type NegatedConstraint = Inequality<Var::AffineView>;

    fn negation(&self) -> Self::NegatedConstraint {
        Inequality {
            terms: self.terms.iter().map(|term| term.scaled(-1)).collect(),
            rhs: -self.rhs - 1,
        }
    }
}
