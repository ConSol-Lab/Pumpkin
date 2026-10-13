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

impl ConstantComparison {
    /// Parses the comparison of a reified (`_reif`) or half-reified (`_imp`) integer comparison
    /// constraint, e.g. `int_le_reif(a, b, r)`.
    ///
    /// Returns [`None`] if the constraint is not such a comparison, or if it does not compare an
    /// integer variable with a constant.
    pub(crate) fn from_constraint(
        constraint_name: &str,
        exprs: &[flatzinc::Expr],
        context: &CompilationContext,
    ) -> Option<ConstantComparison> {
        let operator = constraint_name
            .strip_prefix("int_")?
            .strip_suffix("_reif")
            .or_else(|| constraint_name.strip_prefix("int_")?.strip_suffix("_imp"))?;
        let [lhs, rhs, _] = exprs else {
            return None;
        };

        match (
            resolve_constant(lhs, context),
            resolve_constant(rhs, context),
        ) {
            // `x op c`
            (None, Some(value)) => Self::new(resolve_variable(lhs, context)?, operator, value),
            // `c op x`, which is `x op' c` with the operator mirrored
            (Some(value), None) => {
                let mirrored = match operator {
                    "le" => "ge",
                    "lt" => "gt",
                    "ge" => "le",
                    "gt" => "lt",
                    symmetric => symmetric,
                };
                Self::new(resolve_variable(rhs, context)?, mirrored, value)
            }
            _ => None,
        }
    }

    fn new(variable: Rc<str>, operator: &str, value: i32) -> Option<ConstantComparison> {
        let (predicate_type, value) = match operator {
            "eq" => (PredicateType::Equal, value),
            "ne" => (PredicateType::NotEqual, value),
            "le" => (PredicateType::UpperBound, value),
            "lt" => (PredicateType::UpperBound, value.checked_sub(1)?),
            "ge" => (PredicateType::LowerBound, value),
            "gt" => (PredicateType::LowerBound, value.checked_add(1)?),
            _ => return None,
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

    fn comparison(
        context: &CompilationContext,
        name: &str,
        lhs: flatzinc::Expr,
        rhs: flatzinc::Expr,
    ) -> Option<ConstantComparison> {
        let r = flatzinc::Expr::VarParIdentifier("r".into());
        ConstantComparison::from_constraint(name, &[lhs, rhs, r], context)
    }

    #[test]
    fn comparisons_with_a_constant_are_normalised() {
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        context
            .equivalences
            .create_equivalence_class("x".into(), 0, 10);
        let _ = context.integer_parameters.insert("c".into(), 7);

        let x = || flatzinc::Expr::VarParIdentifier("x".into());
        let expected = |predicate_type, value| {
            Some(ConstantComparison {
                variable: "x".into(),
                predicate_type,
                value,
            })
        };

        assert_eq!(
            comparison(&context, "int_le_reif", x(), flatzinc::Expr::Int(5)),
            expected(PredicateType::UpperBound, 5)
        );
        assert_eq!(
            comparison(&context, "int_lt_reif", flatzinc::Expr::Int(5), x()),
            expected(PredicateType::LowerBound, 6)
        );
        assert_eq!(
            comparison(&context, "int_gt_imp", x(), flatzinc::Expr::Int(5)),
            expected(PredicateType::LowerBound, 6)
        );
        assert_eq!(
            comparison(&context, "int_ge_imp", flatzinc::Expr::Int(5), x()),
            expected(PredicateType::UpperBound, 5)
        );
        assert_eq!(
            comparison(
                &context,
                "int_ne_reif",
                x(),
                flatzinc::Expr::VarParIdentifier("c".into())
            ),
            expected(PredicateType::NotEqual, 7)
        );
    }

    #[test]
    fn comparisons_without_exactly_one_constant_are_not_parsed() {
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        context
            .equivalences
            .create_equivalence_class("x".into(), 0, 10);
        context
            .equivalences
            .create_equivalence_class("y".into(), 0, 10);

        let x = || flatzinc::Expr::VarParIdentifier("x".into());
        let y = || flatzinc::Expr::VarParIdentifier("y".into());

        assert_eq!(comparison(&context, "int_le_reif", x(), y()), None);
        assert_eq!(
            comparison(
                &context,
                "int_le_reif",
                flatzinc::Expr::Int(1),
                flatzinc::Expr::Int(2)
            ),
            None
        );
        assert_eq!(
            comparison(&context, "int_lin_le_reif", x(), flatzinc::Expr::Int(2)),
            None
        );
    }
}
