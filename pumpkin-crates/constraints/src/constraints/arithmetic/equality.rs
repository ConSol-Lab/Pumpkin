use pumpkin_core::Solver;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::options::ReifiedPropagatorArgs;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_propagators::arithmetic::BinaryEqualsDescription;
use pumpkin_propagators::arithmetic::BinaryEqualsPropagatorArgs;
use pumpkin_propagators::arithmetic::BinaryNotEqualsDescription;
use pumpkin_propagators::arithmetic::BinaryNotEqualsPropagatorArgs;
use pumpkin_propagators::arithmetic::LinearNotEqualDescription;
use pumpkin_propagators::arithmetic::LinearNotEqualPropagatorArgs;

use super::less_than_or_equals;

struct EqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

/// Creates the [`NegatableConstraint`] `∑ terms_i = rhs`.
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

/// Create the [`NegatableConstraint`] `∑ terms_i != rhs`.
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
    fn post(self, solver: &mut Solver) {
        if self.terms.len() == 2 {
            let _ = solver.add_propagator(BinaryEqualsPropagatorArgs {
                constraint_description: BinaryEqualsDescription {
                    a: self.terms[0].clone(),
                    b: self.terms[1].scaled(-1).offset(self.rhs),
                },
                constraint_tag: self.constraint_tag,
            });
        } else {
            EqualityByBounds::from(self).post(solver);
        }
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        if self.terms.len() == 2 {
            let _ = solver.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryEqualsPropagatorArgs {
                    constraint_description: BinaryEqualsDescription {
                        a: self.terms[0].clone(),
                        b: self.terms[1].scaled(-1).offset(self.rhs),
                    },
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            });
        } else {
            EqualityByBounds::from(self).implied_by(solver, reification_literal);
        }
    }
}

/// The equality `∑ terms_i = rhs` as the two linear inequalities `∑ terms_i <= rhs` and
/// `∑ -terms_i <= -rhs`, for any number of terms.
struct EqualityByBounds<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

impl<Var> From<EqualConstraint<Var>> for EqualityByBounds<Var> {
    fn from(equality: EqualConstraint<Var>) -> Self {
        EqualityByBounds {
            terms: equality.terms,
            rhs: equality.rhs,
            constraint_tag: equality.constraint_tag,
        }
    }
}

/// Creates the [`Constraint`] `∑ terms_i = rhs` as the two linear inequalities
/// `∑ terms_i <= rhs` and `∑ -terms_i <= -rhs`, whose inferences are those of the linear bounds
/// rule, also when there are two terms, for which [`equals`] posts the propagator of
/// [`binary_equals`].
pub fn equals_by_bounds<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    EqualityByBounds {
        terms: terms.into(),
        rhs,
        constraint_tag,
    }
}

impl<Var> Constraint for EqualityByBounds<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, solver: &mut Solver) {
        less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag).post(solver);

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs, self.constraint_tag).post(solver);
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal);

        let negated = self
            .terms
            .iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<[_]>>();
        less_than_or_equals(negated, -self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal);
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
    fn post(self, solver: &mut Solver) {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
        } = self;

        if terms.len() == 2 {
            let _ = solver.add_propagator(BinaryNotEqualsPropagatorArgs {
                constraint_description: BinaryNotEqualsDescription {
                    a: terms[0].clone(),
                    b: terms[1].scaled(-1).offset(self.rhs),
                },
                constraint_tag: self.constraint_tag,
            });
        } else {
            LinearNotEqualPropagatorArgs {
                constraint_description: LinearNotEqualDescription {
                    terms: terms.into(),
                    rhs,
                },
                constraint_tag,
            }
            .post(solver)
        }
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
        } = self;

        if terms.len() == 2 {
            let _ = solver.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryNotEqualsPropagatorArgs {
                    constraint_description: BinaryNotEqualsDescription {
                        a: terms[0].clone(),
                        b: terms[1].scaled(-1).offset(self.rhs),
                    },
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            });
        } else {
            LinearNotEqualPropagatorArgs {
                constraint_description: LinearNotEqualDescription {
                    terms: terms.into(),
                    rhs,
                },
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_hole_at_the_fixed_value_makes_the_reified_equality_false() {
        let mut solver = Solver::default();
        let constraint_tag = solver.new_constraint_tag();
        let x = solver.new_sparse_integer(vec![1, 3]);
        let c = solver.new_bounded_integer(2, 2);
        let r = solver.new_literal();

        binary_equals(x, c, constraint_tag).reify(&mut solver, r);

        let _ = solver.propagate_to_fixpoint();
        assert_eq!(solver.get_literal_value(r), Some(false));
    }
}
