use pumpkin_core::ConstraintOperationError;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::options::ReifiedPropagatorArgs;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::state::State;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_propagators::arithmetic::BinaryEqualsPropagatorArgs;
use pumpkin_propagators::arithmetic::BinaryNotEqualsPropagatorArgs;
use pumpkin_propagators::arithmetic::LinearNotEqualPropagatorArgs;

use super::less_than_or_equals;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EqualityConsistency {
    Bound,
    Domain,
}

struct EqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
}

/// Creates the [`NegatableConstraint`] `∑ terms_i = rhs`.
///
/// Its negation is [`not_equals`].
pub fn equals<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
) -> impl NegatableConstraint {
    EqualConstraint {
        terms: terms.into(),
        rhs,
        constraint_tag,
        consistency,
    }
}

/// Creates the [`NegatableConstraint`] `lhs = rhs`.
///
/// Its negation is [`binary_not_equals`].
pub fn binary_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
) -> impl NegatableConstraint {
    EqualConstraint {
        terms: [lhs.scaled(1), rhs.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
        consistency,
    }
}

struct NotEqualConstraint<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
}

/// Create the [`NegatableConstraint`] `∑ terms_i != rhs`.
///
/// Its negation is [`equals`].
pub fn not_equals<Var: IntegerVariable + Clone + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
) -> impl NegatableConstraint {
    equals(terms, rhs, constraint_tag, consistency).negation()
}

/// Creates the [`NegatableConstraint`] `lhs != rhs`.
///
/// Its negation is [`binary_equals`].
pub fn binary_not_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
    consistency: EqualityConsistency,
) -> impl NegatableConstraint {
    NotEqualConstraint {
        terms: [lhs.scaled(1), rhs.scaled(-1)].into(),
        rhs: 0,
        constraint_tag,
        consistency,
    }
}

impl<Var> Constraint for EqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, state: &mut State) {
        if self.terms.len() == 2 && self.consistency == EqualityConsistency::Domain {
            let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
                a: self.terms[0].clone(),
                b: self.terms[1].scaled(-1).offset(self.rhs),
                constraint_tag: self.constraint_tag,
            });
        } else {
            less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag).post(state);

            let negated = self
                .terms
                .iter()
                .map(|var| var.scaled(-1))
                .collect::<Box<[_]>>();
            less_than_or_equals(negated, -self.rhs, self.constraint_tag).post(state);
        }
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        if self.terms.len() == 2 && self.consistency == EqualityConsistency::Domain {
            let _ = state.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryEqualsPropagatorArgs {
                    a: self.terms[0].clone(),
                    b: self.terms[1].scaled(-1).offset(self.rhs),
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            });
        } else {
            less_than_or_equals(self.terms.clone(), self.rhs, self.constraint_tag)
                .implied_by(state, reification_literal);

            let negated = self
                .terms
                .iter()
                .map(|var| var.scaled(-1))
                .collect::<Box<[_]>>();
            less_than_or_equals(negated, -self.rhs, self.constraint_tag)
                .implied_by(state, reification_literal);
        }
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
            consistency: self.consistency,
        }
    }
}

impl<Var> Constraint for NotEqualConstraint<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(self, state: &mut State) {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
            consistency: _,
        } = self;

        if terms.len() == 2 {
            let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
                a: terms[0].clone(),
                b: terms[1].scaled(-1).offset(self.rhs),
                constraint_tag: self.constraint_tag,
            });
        } else {
            LinearNotEqualPropagatorArgs {
                terms: terms.into(),
                rhs,
                constraint_tag,
            }
            .post(state)
        }
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        let NotEqualConstraint {
            terms,
            rhs,
            constraint_tag,
            consistency: _,
        } = self;

        if terms.len() == 2 {
            let _ = state.add_propagator(ReifiedPropagatorArgs {
                propagator: BinaryNotEqualsPropagatorArgs {
                    a: terms[0].clone(),
                    b: terms[1].scaled(-1).offset(self.rhs),
                    constraint_tag: self.constraint_tag,
                },
                reification_literal,
            });
        } else {
            LinearNotEqualPropagatorArgs {
                terms: terms.into(),
                rhs,
                constraint_tag,
            }
            .implied_by(state, reification_literal)
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
            consistency: self.consistency,
        }
    }
}
