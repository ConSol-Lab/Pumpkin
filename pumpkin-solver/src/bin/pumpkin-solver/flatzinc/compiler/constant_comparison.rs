//! Comparisons between an integer variable and a constant, e.g. the `x <= 5` in
//! `int_le_reif(x, 5, r)`, which can be represented by a single predicate.

use pumpkin_solver::core::predicate;
use pumpkin_solver::core::predicates::Predicate;

use super::collect_domains::get_or_create_domain;
use super::context::CompilationContext;

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

/// Returns the predicate for `lhs comparison rhs` if exactly one of `lhs` and `rhs` is a
/// constant and the other is an integer variable; otherwise returns [`None`].
///
/// The domain of the integer variable is created if it does not exist yet.
pub(crate) fn constant_comparison_predicate(
    comparison: IntComparison,
    lhs: &flatzinc::Expr,
    rhs: &flatzinc::Expr,
    context: &mut CompilationContext,
) -> Option<Predicate> {
    let (variable, comparison, value) = match (
        context.resolve_integer_constant_from_expr(lhs).ok(),
        context.resolve_integer_constant_from_expr(rhs).ok(),
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

    let variable = get_or_create_domain(context, variable);

    Some(match comparison {
        IntComparison::Equal => predicate![variable == value],
        IntComparison::NotEqual => predicate![variable != value],
        IntComparison::LessThanOrEqual => predicate![variable <= value],
        IntComparison::LessThan => predicate![variable <= value.checked_sub(1)?],
        IntComparison::GreaterThanOrEqual => predicate![variable >= value],
        IntComparison::GreaterThan => predicate![variable >= value.checked_add(1)?],
    })
}

fn resolve_variable<'a>(expr: &'a flatzinc::Expr, context: &CompilationContext) -> Option<&'a str> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) if context.equivalences.is_defined(id) => Some(id),
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
        let domain = get_or_create_domain(&mut context, "x");

        let x = flatzinc::Expr::VarParIdentifier("x".into());
        let five = flatzinc::Expr::Int(5);
        let c = flatzinc::Expr::VarParIdentifier("c".into());

        for (comparison, lhs, rhs, expected) in [
            (
                IntComparison::LessThanOrEqual,
                &x,
                &five,
                predicate![domain <= 5],
            ),
            (IntComparison::LessThan, &five, &x, predicate![domain >= 6]),
            (
                IntComparison::GreaterThan,
                &x,
                &five,
                predicate![domain >= 6],
            ),
            (
                IntComparison::GreaterThanOrEqual,
                &five,
                &x,
                predicate![domain <= 5],
            ),
            (IntComparison::NotEqual, &x, &c, predicate![domain != 7]),
        ] {
            assert_eq!(
                constant_comparison_predicate(comparison, lhs, rhs, &mut context),
                Some(expected)
            );
        }
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
            constant_comparison_predicate(IntComparison::LessThanOrEqual, &x, &y, &mut context),
            None
        );
        assert_eq!(
            constant_comparison_predicate(IntComparison::LessThanOrEqual, &one, &two, &mut context),
            None
        );
    }
}
