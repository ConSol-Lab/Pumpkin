use super::less_than_or_equals;
use crate::constraints::Constraint;
use crate::constraints::NegatableConstraint;
use crate::propagators::linear_not_equal::LinearNotEqualConstructor;
use crate::variables::IntegerVariable;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the [`NegatableConstraint`] `\sum terms_i = rhs`.
///
/// Its negation is [`not_equals`].
pub fn equals<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
) -> impl NegatableConstraint {
    EqualConstraint {
        terms: terms.into(),
        rhs,
    }
}

/// Creates the [`NegatableConstraint`] `lhs = rhs`.
///
/// Its negation is [`binary_not_equals`].
pub fn binary_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
) -> impl NegatableConstraint {
    equals([lhs.scaled(1), rhs.scaled(-1)], 0)
}

/// Create the [`NegatableConstraint`] `\sum terms_i != rhs`.
///
/// Its negation is [`equals`].
pub fn not_equals<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
) -> impl NegatableConstraint {
    equals(terms, rhs).negation()
}

/// Creates the [`NegatableConstraint`] `lhs != rhs`.
///
/// Its negation is [`binary_equals`].
pub fn binary_not_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
) -> impl NegatableConstraint {
    not_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
}

struct EqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
}

impl<Var> Constraint for EqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        less_than_or_equals(self.terms.clone(), self.rhs).post(solver)?;

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs).post(solver)?;

        Ok(())
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        less_than_or_equals(self.terms.clone(), self.rhs)
            .implied_by(solver, reification_literal)?;

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs).implied_by(solver, reification_literal)?;

        Ok(())
    }
}

impl<Var> NegatableConstraint for EqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    type NegatedConstraint = NotEqualConstraint<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        NotEqualConstraint {
            terms: self.terms.clone(),
            rhs: self.rhs,
        }
    }
}

struct NotEqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
}

impl<Var> Constraint for NotEqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        LinearNotEqualConstructor::new(self.terms, self.rhs).post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        LinearNotEqualConstructor::new(self.terms, self.rhs).implied_by(solver, reification_literal)
    }
}

impl<Var> NegatableConstraint for NotEqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    type NegatedConstraint = EqualConstraint<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        EqualConstraint {
            terms: self.terms.clone(),
            rhs: self.rhs,
        }
    }
}
