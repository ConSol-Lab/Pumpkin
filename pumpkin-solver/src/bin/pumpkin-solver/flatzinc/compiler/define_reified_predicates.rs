//! Defines the boolean variables which reify a comparison between an integer variable and a
//! constant as the predicate over that comparison.
//!
//! For example, `int_le_reif(x, 5, r)` defines `r` as the predicate `[x <= 5]`. The boolean then
//! does not need a domain of its own, and the defining constraint is removed.

use super::constant_comparison::IntComparison;
use super::constant_comparison::constant_comparison_predicate;
use super::context::CompilationContext;
use super::context::Domain;
use crate::FlatZincOptions;
use crate::ProofType;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if matches!(options.proof_type, Some(ProofType::Full)) {
        // The removed constraints would be missing from the proof.
        return Ok(());
    }

    // The constraints are taken out since defining the predicates requires the whole context.
    let mut constraints = std::mem::take(&mut context.constraints);
    constraints.retain(|(_, constraint)| !define_reified_predicate(constraint, context));
    context.constraints = constraints;

    Ok(())
}

/// Returns whether the constraint defines its reified boolean as a predicate, in which case the
/// constraint can be removed.
fn define_reified_predicate(
    constraint: &flatzinc::ConstraintItem,
    context: &mut CompilationContext,
) -> bool {
    // First, we check whether it is a constraint that we know can define reification.
    let comparison = match constraint.id.as_str() {
        "int_eq_reif" => IntComparison::Equal,
        "int_ne_reif" => IntComparison::NotEqual,
        "int_le_reif" => IntComparison::LessThanOrEqual,
        "int_lt_reif" => IntComparison::LessThan,
        _ => return false,
    };

    // First, we retrieve the expressions in the constraint, where it should be a
    // `VarParIdentifier` representing a variable.
    let [lhs, rhs, flatzinc::Expr::VarParIdentifier(boolean)] = constraint.exprs.as_slice() else {
        return false;
    };

    let equivalences = &context.equivalences;

    // Parameters (i.e., constants) do not have an equivalence class, only variables can be defined.
    if !equivalences.is_defined(boolean) {
        return false;
    }

    // A boolean which is merged with an integer variable cannot be defined, since the integer
    // still needs a domain.
    //
    // For example, consider `int_le_reif(x, 5, r)` and `bool2int(r, i)`. The `bool2int` constraint
    // is removed by merging `r` and `i` into one equivalence class, which is normally represented
    // by a single 0-1 domain `d`, where `r` is `[d >= 1]` and `i` is `d`. If `r` were defined as
    // `[x <= 5]`, then the `int_le_reif` constraint would be removed as well. However, `i` is an
    // integer, so it would still be represented by `d`. Since no constraint is left which links
    // `d` to `[x <= 5]`, `i` could take any value, regardless of whether `x <= 5` holds.
    if equivalences.contains_integer(boolean) {
        return false;
    }

    // The predicate would not enforce the value of a fixed boolean.
    if equivalences.domain(boolean) != Domain::from_lower_bound_and_upper_bound(0, 1) {
        return false;
    }

    // A boolean already defined by another constraint keeps that definition; this constraint is
    // instead posted as clauses linking the two predicates.
    //
    // For example, consider `int_le_reif(x, 5, r)` followed by `int_ne_reif(y, 1, r)`. The first
    // constraint defines `r` as `[x <= 5]` and is removed. The boolean cannot also be defined as
    // `[y != 1]`, as a boolean is represented by a single predicate. Instead, the second constraint
    // is kept and posted as the clauses `[x <= 5] -> [y != 1]` and `[y != 1] -> [x <= 5]`.
    let representative = equivalences.representative(boolean);
    if context.boolean_predicates.contains_key(&representative) {
        return false;
    }

    let Some(predicate) = constant_comparison_predicate(comparison, lhs, rhs, context) else {
        return false;
    };
    let _ = context.boolean_predicates.insert(representative, predicate);

    true
}

#[cfg(test)]
mod tests {
    use flatzinc::ConstraintItem;
    use flatzinc::Expr;
    use flatzinc::SolveItem;
    use pumpkin_solver::Solver;

    use super::*;
    use crate::flatzinc::ast::FlatZincAst;
    use crate::flatzinc::ast::SingleVarDecl;

    #[test]
    fn only_booleans_without_integer_view_or_earlier_definition_are_defined() {
        let mut ast_builder = FlatZincAst::builder();

        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "x".into(),
            lb: 0,
            ub: 10,
            expr: None,
            annos: vec![],
        });
        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "i".into(),
            lb: 0,
            ub: 1,
            expr: None,
            annos: vec![],
        });
        for id in ["r", "s"] {
            ast_builder.add_variable_decl(SingleVarDecl::Bool {
                id: id.into(),
                expr: None,
                annos: vec![],
            });
        }

        let var = |id: &str| Expr::VarParIdentifier(id.into());
        for (id, exprs) in [
            // Defines `r`.
            ("int_le_reif", vec![var("x"), Expr::Int(5), var("r")]),
            // `r` is already defined.
            ("int_ne_reif", vec![var("x"), Expr::Int(1), var("r")]),
            // `s` is merged with the integer `i`.
            ("int_eq_reif", vec![Expr::Int(3), var("x"), var("s")]),
            ("bool2int", vec![var("s"), var("i")]),
        ] {
            ast_builder.add_constraint(ConstraintItem {
                id: id.into(),
                exprs,
                annos: vec![],
            });
        }
        ast_builder.set_solve_item(SolveItem {
            goal: flatzinc::Goal::Satisfy,
            annotations: vec![],
        });

        let ast = ast_builder.build().expect("valid ast");
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        let options = FlatZincOptions::default();

        super::super::reserve_constraint_tags::run(&ast, &mut context)
            .expect("step should not fail");
        super::super::prepare_variables::run(&ast, &mut context).expect("step should not fail");
        super::super::merge_equivalences::run(&ast, &mut context, &options)
            .expect("step should not fail");
        run(&mut context, &options).expect("step should not fail");

        let defined = context
            .boolean_predicates
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(defined, vec![context.equivalences.representative("r")]);

        let remaining = context
            .constraints
            .iter()
            .map(|(_, constraint)| constraint.id.as_str())
            .collect::<Vec<_>>();
        assert_eq!(remaining, vec!["int_ne_reif", "int_eq_reif"]);
    }
}
