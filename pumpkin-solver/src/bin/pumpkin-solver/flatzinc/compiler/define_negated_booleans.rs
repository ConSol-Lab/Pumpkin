//! Defines the boolean variables which are the negation of another boolean variable as the negated
//! predicate of that boolean.
//!
//! For example, `bool_not(a, b)` defines `b` as `!a`. The boolean `b` then does not need a domain
//! of its own, and the defining constraint is removed. If `a` is defined as the predicate `[x <=
//! 5]` (see `define_reified_predicates`), then `b` is the predicate `[x >= 6]`.

use super::collect_domains::get_or_create_predicate;
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
    constraints.retain(|(_, constraint)| !define_negated_boolean(constraint, context));
    context.constraints = constraints;

    Ok(())
}

/// Returns whether the constraint defines one of its booleans as the negation of the other, in
/// which case the constraint can be removed.
fn define_negated_boolean(
    constraint: &flatzinc::ConstraintItem,
    context: &mut CompilationContext,
) -> bool {
    if constraint.id != "bool_not" {
        return false;
    }

    let [
        flatzinc::Expr::VarParIdentifier(a),
        flatzinc::Expr::VarParIdentifier(b),
    ] = constraint.exprs.as_slice()
    else {
        return false;
    };

    // Parameters do not have an equivalence class, only variables can be defined.
    if !context.equivalences.is_defined(a) || !context.equivalences.is_defined(b) {
        return false;
    }

    // If `b` cannot be defined (e.g. it is already defined as a predicate), then `a` can still be
    // defined as its negation.
    define_negation(b, a, context) || define_negation(a, b, context)
}

/// Defines `boolean` as the negation of the predicate of `negated`, if possible.
fn define_negation(boolean: &str, negated: &str, context: &mut CompilationContext) -> bool {
    let representative = context.equivalences.representative(boolean);

    // The boolean can only be defined as the negation if:
    // - it is not in the same equivalence class as `negated`;
    // - its domain is not fixed, as the negation would not enforce that value;
    // - it is not already defined as a predicate;
    // - it does not have a variable yet, which is the case when its equivalence class (e.g. through
    //   `bool2int`) is the integer of a comparison, or when it is negated by another boolean.
    //
    // As the predicate of `negated` is created, `negated` has a variable afterwards and cannot be
    // defined as a negation itself; this prevents cycles such as `bool_not(a, b)` and
    // `bool_not(b, a)`, where neither boolean would have a domain.
    let can_be_defined = representative != context.equivalences.representative(negated)
        && context.equivalences.domain(boolean) == (Domain::IntervalDomain { lb: 0, ub: 1 })
        && !context.boolean_predicates.contains_key(&representative)
        && !context.variable_map.contains_key(&representative);

    if can_be_defined {
        let predicate = !get_or_create_predicate(context, negated);
        let _ = context.boolean_predicates.insert(representative, predicate);
    }

    can_be_defined
}

#[cfg(test)]
mod tests {
    use flatzinc::ConstraintItem;
    use flatzinc::Expr;
    use flatzinc::SolveItem;
    use pumpkin_solver::Solver;
    use pumpkin_solver::core::predicate;

    use super::*;
    use crate::flatzinc::ast::FlatZincAst;
    use crate::flatzinc::ast::SingleVarDecl;

    #[test]
    fn negations_are_defined_unless_they_form_a_cycle_or_are_already_defined() {
        let mut ast_builder = FlatZincAst::builder();

        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "x".into(),
            lb: 0,
            ub: 10,
            expr: None,
            annos: vec![],
        });
        for id in ["a", "b", "c", "d", "e"] {
            ast_builder.add_variable_decl(SingleVarDecl::Bool {
                id: id.into(),
                expr: None,
                annos: vec![],
            });
        }

        let var = |id: &str| Expr::VarParIdentifier(id.into());
        for (id, exprs) in [
            // Defines `e` as `[x <= 5]`.
            ("int_le_reif", vec![var("x"), Expr::Int(5), var("e")]),
            // Defines `b` as `!a`.
            ("bool_not", vec![var("a"), var("b")]),
            // Defines `c` as `!b`.
            ("bool_not", vec![var("b"), var("c")]),
            // Would define `a` as `!c`, which is a cycle.
            ("bool_not", vec![var("c"), var("a")]),
            // `e` is already defined, so `d` is defined as `!e` instead.
            ("bool_not", vec![var("e"), var("d")]),
            // Both `d` and `e` are already defined.
            ("bool_not", vec![var("d"), var("e")]),
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
        super::super::define_reified_predicates::run(&mut context, &options)
            .expect("step should not fail");
        run(&mut context, &options).expect("step should not fail");

        let a = context.variable_map[&context.equivalences.representative("a")];
        let x = context.variable_map[&context.equivalences.representative("x")];
        let predicate =
            |id: &str| context.boolean_predicates[&context.equivalences.representative(id)];
        assert_eq!(predicate("b"), predicate![a <= 0]);
        assert_eq!(predicate("c"), predicate![a >= 1]);
        assert_eq!(predicate("d"), predicate![x >= 6]);

        let remaining = context
            .constraints
            .iter()
            .map(|(_, constraint)| &constraint.exprs[..])
            .collect::<Vec<_>>();
        assert_eq!(
            remaining,
            vec![&[var("c"), var("a")][..], &[var("d"), var("e")][..]]
        );
    }
}
