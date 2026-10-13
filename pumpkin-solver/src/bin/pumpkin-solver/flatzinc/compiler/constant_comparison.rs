//! Comparisons between an integer variable and a constant, e.g. the `x <= 5` in
//! `int_le_reif(x, 5, r)`, which can be represented by a single [`Literal`].

use std::rc::Rc;

use pumpkin_solver::core::predicates::Predicate;
use pumpkin_solver::core::predicates::PredicateType;
use pumpkin_solver::core::variables::Literal;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;

/// A comparison `variable op value` between an integer variable and a constant, where the
/// comparison is that of the corresponding [`Predicate`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ConstantComparison {
    variable: Rc<str>,
    predicate_type: PredicateType,
    value: i32,
}

/// The comparison `a op b` of an integer comparison constraint such as `int_le_reif(a, b, r)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum IntComparison {
    Equal,
    NotEqual,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,
}

impl IntComparison {
    /// Returns the comparison `op'` such that `a op b` is equivalent to `b op' a`.
    fn mirrored(self) -> IntComparison {
        match self {
            IntComparison::Equal => IntComparison::Equal,
            IntComparison::NotEqual => IntComparison::NotEqual,
            IntComparison::LessThanOrEqual => IntComparison::GreaterThanOrEqual,
            IntComparison::LessThan => IntComparison::GreaterThan,
            IntComparison::GreaterThanOrEqual => IntComparison::LessThanOrEqual,
            IntComparison::GreaterThan => IntComparison::LessThan,
        }
    }
}

impl ConstantComparison {
    /// Creates the comparison `lhs comparison rhs` if exactly one of `lhs` and `rhs` is a
    /// constant and the other is an integer variable; otherwise returns [`None`].
    pub(crate) fn new(
        comparison: IntComparison,
        lhs: &flatzinc::Expr,
        rhs: &flatzinc::Expr,
        context: &CompilationContext,
    ) -> Option<ConstantComparison> {
        let (variable, comparison, value) = match (
            resolve_constant(lhs, context),
            resolve_constant(rhs, context),
        ) {
            // `x op c`
            (None, Some(value)) => (resolve_variable(lhs, context)?, comparison, value),
            // `c op x`, which is `x op' c` with the comparison mirrored
            (Some(value), None) => (
                resolve_variable(rhs, context)?,
                comparison.mirrored(),
                value,
            ),
            _ => return None,
        };

        let (predicate_type, value) = match comparison {
            IntComparison::Equal => (PredicateType::Equal, value),
            IntComparison::NotEqual => (PredicateType::NotEqual, value),
            IntComparison::LessThanOrEqual => (PredicateType::UpperBound, value),
            IntComparison::LessThan => (PredicateType::UpperBound, value.checked_sub(1)?),
            IntComparison::GreaterThanOrEqual => (PredicateType::LowerBound, value),
            IntComparison::GreaterThan => (PredicateType::LowerBound, value.checked_add(1)?),
        };

        Some(ConstantComparison {
            variable,
            predicate_type,
            value,
        })
    }

    /// The identifier of the integer variable in the comparison.
    pub(crate) fn variable(&self) -> &str {
        &self.variable
    }

    /// Creates the literal which is true if and only if the comparison holds.
    ///
    /// The domain of the integer variable should already have been created.
    pub(crate) fn to_literal(
        &self,
        context: &mut CompilationContext,
    ) -> Result<Literal, FlatZincError> {
        let variable = context.resolve_integer_variable_from_identifier(&self.variable)?;

        Ok(Literal::new(Predicate::new(
            variable,
            self.predicate_type,
            self.value,
        )))
    }
}

fn resolve_constant(expr: &flatzinc::Expr, context: &CompilationContext) -> Option<i32> {
    match expr {
        flatzinc::Expr::Int(value) => i32::try_from(*value).ok(),
        flatzinc::Expr::VarParIdentifier(id) => {
            context.integer_parameters.get(id.as_str()).copied()
        }
        _ => None,
    }
}

fn resolve_variable(expr: &flatzinc::Expr, context: &CompilationContext) -> Option<Rc<str>> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) if context.equivalences.is_defined(id) => {
            Some(Rc::from(id.as_str()))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_solver::Solver;

    use super::*;

    #[test]
    fn comparisons_with_a_constant_are_normalised() {
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        context
            .equivalences
            .create_equivalence_class("x".into(), 0, 10);
        let _ = context.integer_parameters.insert("c".into(), 7);

        let x = flatzinc::Expr::VarParIdentifier("x".into());
        let five = flatzinc::Expr::Int(5);
        let c = flatzinc::Expr::VarParIdentifier("c".into());
        let expected = |predicate_type, value| {
            Some(ConstantComparison {
                variable: "x".into(),
                predicate_type,
                value,
            })
        };

        assert_eq!(
            ConstantComparison::new(IntComparison::LessThanOrEqual, &x, &five, &context),
            expected(PredicateType::UpperBound, 5)
        );
        assert_eq!(
            ConstantComparison::new(IntComparison::LessThan, &five, &x, &context),
            expected(PredicateType::LowerBound, 6)
        );
        assert_eq!(
            ConstantComparison::new(IntComparison::GreaterThan, &x, &five, &context),
            expected(PredicateType::LowerBound, 6)
        );
        assert_eq!(
            ConstantComparison::new(IntComparison::GreaterThanOrEqual, &five, &x, &context),
            expected(PredicateType::UpperBound, 5)
        );
        assert_eq!(
            ConstantComparison::new(IntComparison::NotEqual, &x, &c, &context),
            expected(PredicateType::NotEqual, 7)
        );
    }

    #[test]
    fn comparisons_without_exactly_one_constant_are_not_created() {
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        context
            .equivalences
            .create_equivalence_class("x".into(), 0, 10);
        context
            .equivalences
            .create_equivalence_class("y".into(), 0, 10);

        let x = flatzinc::Expr::VarParIdentifier("x".into());
        let y = flatzinc::Expr::VarParIdentifier("y".into());
        let one = flatzinc::Expr::Int(1);
        let two = flatzinc::Expr::Int(2);

        assert_eq!(
            ConstantComparison::new(IntComparison::LessThanOrEqual, &x, &y, &context),
            None
        );
        assert_eq!(
            ConstantComparison::new(IntComparison::LessThanOrEqual, &one, &two, &context),
            None
        );
    }
}
