//! Defines the boolean variables which reify a comparison between an integer variable and a
//! constant as a literal over that comparison.
//!
//! For example, `int_le_reif(x, 5, r)` defines `r` as the literal `[x <= 5]`. The boolean then
//! does not need a domain of its own, and the defining constraint is removed.

use std::rc::Rc;

use pumpkin_solver::core::containers::HashSet;

use super::constant_comparison::ConstantComparison;
use super::constant_comparison::IntComparison;
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

    // The equivalence classes of the integer variables in the comparisons of the definitions.
    let mut compared_classes = HashSet::default();

    // The constraints are taken out since defining the literals requires the whole context.
    let mut constraints = std::mem::take(&mut context.constraints);
    constraints.retain(|(_, constraint)| {
        !define_reified_literal(constraint, context, &mut compared_classes)
    });
    context.constraints = constraints;

    Ok(())
}

/// Returns whether the constraint defines its reified boolean as a literal, in which case the
/// constraint can be removed.
fn define_reified_literal(
    constraint: &flatzinc::ConstraintItem,
    context: &mut CompilationContext,
    compared_classes: &mut HashSet<Rc<str>>,
) -> bool {
    let comparison = match constraint.id.as_str() {
        "int_eq_reif" => IntComparison::Equal,
        "int_ne_reif" => IntComparison::NotEqual,
        "int_le_reif" => IntComparison::LessThanOrEqual,
        "int_lt_reif" => IntComparison::LessThan,
        _ => return false,
    };

    // The reified boolean should be a variable; parameters do not have an equivalence class.
    if let [lhs, rhs, flatzinc::Expr::VarParIdentifier(boolean)] = constraint.exprs.as_slice()
        && context.equivalences.is_defined(boolean)
        && let Some(comparison) = ConstantComparison::new(comparison, lhs, rhs, context)
    {
        let representative = context.equivalences.representative(boolean);
        let compared_class = context.equivalences.representative(comparison.variable());
        // The boolean can only be defined as the literal of the comparison if:
        // - its domain is exactly {0, 1}; a fixed domain would not be enforced by the literal. This
        //   also holds for an integer merged with the boolean (e.g. through `bool2int`), which then
        //   becomes a 0-1 view of the literal;
        // - it is not already defined by another constraint; that constraint will instead be posted
        //   as clauses linking the two literals;
        // - no class is both defined as a literal and compared in the definition of a literal,
        //   which also excludes a comparison over the boolean itself. This means that the literals
        //   do not depend on each other, so the order in which they are created (see
        //   `collect_domains`) does not matter.
        let can_be_defined = context.equivalences.domain(boolean)
            == Domain::IntervalDomain { lb: 0, ub: 1 }
            && !context.reified_literals.contains_key(&representative)
            && !compared_classes.contains(&representative)
            && !context.reified_literals.contains_key(&compared_class)
            && compared_class != representative;

        if can_be_defined {
            let _ = context.reified_literals.insert(representative, comparison);
            let _ = compared_classes.insert(compared_class);
        }

        can_be_defined
    } else {
        false
    }
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
    fn only_booleans_without_earlier_definition_or_chained_comparison_are_defined() {
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
        for id in ["r", "s", "t"] {
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
            // Defines `s`, which is merged with the integer `i` that becomes a view of the
            // literal.
            ("int_eq_reif", vec![Expr::Int(3), var("x"), var("s")]),
            ("bool2int", vec![var("s"), var("i")]),
            // `i` is in the class of `s`, which is already defined as a literal.
            ("int_eq_reif", vec![var("i"), Expr::Int(1), var("t")]),
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

        let mut defined = context.reified_literals.keys().cloned().collect::<Vec<_>>();
        defined.sort();
        let mut expected = vec![
            context.equivalences.representative("r"),
            context.equivalences.representative("s"),
        ];
        expected.sort();
        assert_eq!(defined, expected);

        let remaining = context
            .constraints
            .iter()
            .map(|(_, constraint)| constraint.id.as_str())
            .collect::<Vec<_>>();
        assert_eq!(remaining, vec!["int_ne_reif", "int_eq_reif"]);
        assert_eq!(context.constraints[1].1.exprs[2], var("t"));
    }
}
