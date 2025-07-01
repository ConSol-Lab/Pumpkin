use super::less_than_or_equals;
use crate::constraints::Constraint;
use crate::constraints::NegatableConstraint;
use crate::proof::ConstraintTag;
use crate::propagators::binary::BinaryEqualsPropagatorArgs;
use crate::propagators::binary::BinaryNotEqualsPropagatorArgs;
use crate::propagators::linear_not_equal::LinearNotEqualPropagatorArgs;
use crate::propagators::ReifiedPropagatorArgs;
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
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    EqualConstraint {
        terms: terms.into(),
        rhs,
        constraint_tag,
    }
}

/// Creates the [`NegatableConstraint`] `lhs = rhs`.
///
/// Its negation is [`binary_not_equals`].
pub fn binary_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    BinaryEqualConstraint {
        a: lhs,
        b: rhs,
        constraint_tag,
    }
}

/// Create the [`NegatableConstraint`] `\sum terms_i != rhs`.
///
/// Its negation is [`equals`].
pub fn not_equals<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    equals(terms, rhs, constraint_tag).negation()
}

/// Creates the [`NegatableConstraint`] `lhs != rhs`.
///
/// Its negation is [`binary_equals`].
pub fn binary_not_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    BinaryNotEqualsConstraint {
        a: lhs,
        b: rhs,
        constraint_tag,
    }
}

struct BinaryEqualConstraint<Var> {
    a: Var,
    b: Var,
    constraint_tag: ConstraintTag,
}

impl<Var> Constraint for BinaryEqualConstraint<Var>
where
    Var: IntegerVariable + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(BinaryEqualsPropagatorArgs {
            a: self.a,
            b: self.b,
            constraint_tag: self.constraint_tag,
        })
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(ReifiedPropagatorArgs {
            propagator: BinaryEqualsPropagatorArgs {
                a: self.a,
                b: self.b,
                constraint_tag: self.constraint_tag,
            },
            reification_literal,
        })
    }
}

impl<Var> NegatableConstraint for BinaryEqualConstraint<Var>
where
    Var: IntegerVariable + 'static,
{
    type NegatedConstraint = BinaryNotEqualsConstraint<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        BinaryNotEqualsConstraint {
            a: self.a.clone(),
            b: self.b.clone(),
            constraint_tag: self.constraint_tag,
        }
    }
}

struct BinaryNotEqualsConstraint<Var> {
    a: Var,
    b: Var,
    constraint_tag: ConstraintTag,
}

impl<Var> Constraint for BinaryNotEqualsConstraint<Var>
where
    Var: IntegerVariable + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(BinaryNotEqualsPropagatorArgs {
            a: self.a,
            b: self.b,
            constraint_tag: self.constraint_tag,
        })
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        solver.add_propagator(ReifiedPropagatorArgs {
            propagator: BinaryNotEqualsPropagatorArgs {
                a: self.a,
                b: self.b,
                constraint_tag: self.constraint_tag,
            },
            reification_literal,
        })
    }
}

impl<Var> NegatableConstraint for BinaryNotEqualsConstraint<Var>
where
    Var: IntegerVariable + 'static,
{
    type NegatedConstraint = BinaryEqualConstraint<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        BinaryEqualConstraint {
            a: self.a.clone(),
            b: self.b.clone(),
            constraint_tag: self.constraint_tag,
        }
    }
}

struct EqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

impl<Var> Constraint for EqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag).post(solver)?;

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs, self.constraint_tag).post(solver)?;

        Ok(())
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal)?;

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal)?;

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
            constraint_tag: self.constraint_tag,
        }
    }
}

struct NotEqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

impl<Var> Constraint for NotEqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
        } = self;

        LinearNotEqualPropagatorArgs {
            terms: terms.into(),
            rhs,
            constraint_tag,
        }
        .post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
        } = self;

        LinearNotEqualPropagatorArgs {
            terms: terms.into(),
            rhs,
            constraint_tag,
        }
        .implied_by(solver, reification_literal)
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
            constraint_tag: self.constraint_tag,
        }
    }
}
