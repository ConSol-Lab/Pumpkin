use implementation::propagators::linear_propagator::LinearConstructor;
use pumpkin_core::ConstraintOperationError;
use pumpkin_core::Solver;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

/// Create the [`NegatableConstraint`] `∑ terms_i <= rhs`.
///
/// Its negation is `∑ terms_i > rhs`
pub fn less_than_or_equals<Var: IntegerVariable + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    Inequality {
        terms: terms.into(),
        rhs,
        constraint_tag,
    }
}

/// Create the [`NegatableConstraint`] `∑ terms_i < rhs`.
///
/// Its negation is `∑ terms_i <= rhs`
pub fn less_than<Var: IntegerVariable + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    less_than_or_equals(terms, rhs - 1, constraint_tag)
}

/// Create the [`NegatableConstraint`] `∑ terms_i > rhs`.
///
/// Its negation is `∑ terms_i <= rhs`
pub fn greater_than<Var: IntegerVariable + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    greater_than_or_equals(terms, rhs + 1, constraint_tag)
}

/// Create the [`NegatableConstraint`] `∑ terms_i >= rhs`.
///
/// Its negation is `∑ terms_i < rhs`
pub fn greater_than_or_equals<Var: IntegerVariable + 'static>(
    terms: impl Into<Box<[Var]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    let terms: Box<[_]> = terms.into().iter().map(|var| var.scaled(-1)).collect();
    less_than_or_equals(terms, -rhs, constraint_tag)
}

/// Creates the [`NegatableConstraint`] `lhs <= rhs`.
///
/// Its negation is `lhs > rhs`.
pub fn binary_less_than_or_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    less_than_or_equals([lhs.scaled(1), rhs.scaled(-1)], 0, constraint_tag)
}

/// Creates the [`NegatableConstraint`] `lhs < rhs`.
///
/// Its negation is `lhs >= rhs`.
pub fn binary_less_than<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    binary_less_than_or_equals(lhs.scaled(1), rhs.offset(-1), constraint_tag)
}

/// Creates the [`NegatableConstraint`] `lhs >= rhs`.
///
/// Its negation is `lhs < rhs`.
pub fn binary_greater_than_or_equals<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    binary_less_than_or_equals(lhs.scaled(-1), rhs.scaled(-1), constraint_tag)
}

/// Creates the [`NegatableConstraint`] `lhs > rhs`.
///
/// Its negation is `lhs <= rhs`.
pub fn binary_greater_than<Var: IntegerVariable + 'static>(
    lhs: Var,
    rhs: Var,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    binary_less_than(lhs.scaled(-1), rhs.scaled(-1), constraint_tag)
}

struct Inequality<Var> {
    terms: Box<[Var]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> Constraint for Inequality<Var> {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        LinearConstructor {
            x: self.terms,
            c: self.rhs,
            constraint_tag: self.constraint_tag,
        }
        .post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        LinearConstructor {
            x: self.terms,
            c: self.rhs,
            constraint_tag: self.constraint_tag,
        }
        .implied_by(solver, reification_literal)
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for Inequality<Var> {
    type NegatedConstraint = Inequality<Var::AffineView>;

    fn negation(&self) -> Self::NegatedConstraint {
        Inequality {
            terms: self.terms.iter().map(|term| term.scaled(-1)).collect(),
            rhs: -self.rhs - 1,
            constraint_tag: self.constraint_tag,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn less_than_conflict() {
        let mut solver = Solver::default();

        let constraint_tag = solver.new_constraint_tag();
        let x = solver.new_named_bounded_integer(0, 0, "x");

        let result = less_than([x], 0, constraint_tag).post(&mut solver);
        assert_eq!(
            result,
            Err(ConstraintOperationError::InfeasiblePropagator),
            "Expected {result:?} to be an `InfeasiblePropagator` error"
        );
    }

    #[test]
    fn greater_than_conflict() {
        let mut solver = Solver::default();

        let constraint_tag = solver.new_constraint_tag();
        let x = solver.new_named_bounded_integer(0, 0, "x");

        let result = greater_than([x], 0, constraint_tag).post(&mut solver);
        assert_eq!(
            result,
            Err(ConstraintOperationError::InfeasiblePropagator),
            "Expected {result:?} to be an `InfeasiblePropagator` error"
        );
    }
}
