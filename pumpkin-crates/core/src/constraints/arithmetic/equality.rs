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
use crate::variables::TransformableVariable;
use crate::ConstraintOperationError;
use crate::Solver;

struct EqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

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
    EqualConstraint {
        terms: [lhs.scaled(1), rhs.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
    }
}

struct NotEqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
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
    NotEqualConstraint {
        terms: [lhs.scaled(1), rhs.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
    }
}

impl<Var> Constraint for EqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        if self.terms.len() == 2 {
            solver.add_propagator(BinaryEqualsPropagatorArgs {
                a: self.terms[0].clone(),
                b: self.terms[1].scaled(-1).offset(self.rhs),
                constraint_tag: self.constraint_tag,
            })?;
        } else {
            less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag).post(solver)?;

            let negated = self
                .terms
                .iter()
                .map(|var| var.scaled(-1))
                .collect::<Box<[_]>>();
            less_than_or_equals(negated, -self.rhs, self.constraint_tag).post(solver)?;
        }

        Ok(())
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        if self.terms.len() == 2 {
            solver.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryEqualsPropagatorArgs {
                    a: self.terms[0].clone(),
                    b: self.terms[1].scaled(-1).offset(self.rhs),
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            })?;
        } else {
            less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag)
                .implied_by(solver, reification_literal)?;

            let negated = self
                .terms
                .iter()
                .map(|var| var.scaled(-1))
                .collect::<Box<[_]>>();
            less_than_or_equals(negated, -self.rhs, self.constraint_tag)
                .implied_by(solver, reification_literal)?;
        }

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

        if terms.len() == 2 {
            solver.add_propagator(BinaryNotEqualsPropagatorArgs {
                a: terms[0].clone(),
                b: terms[1].scaled(-1).offset(self.rhs),
                constraint_tag: self.constraint_tag,
            })
        } else {
            LinearNotEqualPropagatorArgs {
                terms: terms.into(),
                rhs,
                constraint_tag,
            }
            .post(solver)
        }
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

        if terms.len() == 2 {
            solver.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryNotEqualsPropagatorArgs {
                    a: terms[0].clone(),
                    b: terms[1].scaled(-1).offset(self.rhs),
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            })
        } else {
            LinearNotEqualPropagatorArgs {
                terms: terms.into(),
                rhs,
                constraint_tag,
            }
            .implied_by(solver, reification_literal)
        }
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
