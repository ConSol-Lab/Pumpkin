//! Defines the boolean variables which are the negation of another boolean variable as the negated
//! predicate of that boolean.
//!
//! For example, `bool_not(a, b)` defines `b` as `!a`. The boolean `b` then does not need a domain
//! of its own, and the defining constraint is removed. If `a` is defined as the predicate `[x <=
//! 5]` (see `define_reified_predicates`), then `b` is the predicate `[x >= 6]`.

use std::collections::VecDeque;
use std::rc::Rc;

use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::containers::HashSet;

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

    // The `bool_not` constraints form a graph over the booleans (by the representatives of their
    // equivalence classes), where every boolean has the constraints it occurs in together with the
    // other boolean of that constraint.
    let mut neighbours: HashMap<Rc<str>, Vec<(usize, Rc<str>)>> = HashMap::default();
    let mut booleans = Vec::new();
    for (index, (_, constraint)) in context.constraints.iter().enumerate() {
        let Some((a, b)) = negated_booleans(constraint, context) else {
            continue;
        };

        neighbours
            .entry(Rc::clone(&a))
            .or_default()
            .push((index, Rc::clone(&b)));
        neighbours
            .entry(Rc::clone(&b))
            .or_default()
            .push((index, Rc::clone(&a)));
        booleans.extend([a, b]);
    }

    // Every boolean which is reached from another boolean in the graph is defined as the negation
    // of that boolean, and the constraint between them is removed. The traversal first starts from
    // the booleans which cannot be defined (e.g. because they are defined as a predicate), such
    // that their predicates are passed along chains of negations regardless of the order of the
    // constraints. For example, with `[x <= 5]` defining `a`, the constraints `bool_not(b, c)` and
    // `bool_not(a, b)` define `b` as `[x >= 6]` and `c` as `[x <= 5]`.
    //
    // Afterwards, the traversal starts from the remaining booleans, each of which gets a domain
    // that defines the other booleans of its connected component.
    //
    // As a boolean has a variable once its negation is created, it cannot be defined afterwards;
    // this prevents cycles such as `bool_not(a, b)` and `bool_not(b, a)`, where neither boolean
    // would have a domain.
    let (roots, others): (Vec<_>, Vec<_>) = booleans
        .into_iter()
        .partition(|boolean| !can_be_defined(boolean, context));

    let mut removed = vec![false; context.constraints.len()];
    let mut visited: HashSet<Rc<str>> = HashSet::default();
    let mut queue = VecDeque::new();
    for start in roots.into_iter().chain(others) {
        if !visited.insert(Rc::clone(&start)) {
            continue;
        }
        queue.push_back(start);

        while let Some(boolean) = queue.pop_front() {
            for (index, neighbour) in &neighbours[&boolean] {
                if !can_be_defined(neighbour, context) {
                    continue;
                }

                let predicate = !get_or_create_predicate(context, &boolean);
                let _ = context
                    .boolean_predicates
                    .insert(Rc::clone(neighbour), predicate);
                removed[*index] = true;

                let _ = visited.insert(Rc::clone(neighbour));
                queue.push_back(Rc::clone(neighbour));
            }
        }
    }

    let mut removed = removed.into_iter();
    context
        .constraints
        .retain(|_| !removed.next().expect("every constraint has an entry"));

    Ok(())
}

/// Returns the representatives of the two booleans of a `bool_not` constraint, if both are
/// variables in different equivalence classes.
fn negated_booleans(
    constraint: &flatzinc::ConstraintItem,
    context: &CompilationContext,
) -> Option<(Rc<str>, Rc<str>)> {
    if constraint.id != "bool_not" {
        return None;
    }

    let [
        flatzinc::Expr::VarParIdentifier(a),
        flatzinc::Expr::VarParIdentifier(b),
    ] = constraint.exprs.as_slice()
    else {
        return None;
    };

    // Parameters do not have an equivalence class, only variables can be defined.
    if !context.equivalences.is_defined(a) || !context.equivalences.is_defined(b) {
        return None;
    }

    // A boolean cannot be defined as its own negation; the constraint is posted instead.
    let a = context.equivalences.representative(a);
    let b = context.equivalences.representative(b);
    (a != b).then_some((a, b))
}

/// Returns whether the boolean (by the representative of its equivalence class) can be defined as
/// the negation of another boolean, which is the case if:
/// - its domain is not fixed, as the negation would not enforce that value;
/// - it is not already defined as a predicate;
/// - it does not have a variable yet, which is the case when its equivalence class (e.g. through
///   `bool2int`) is the integer of a comparison, or when it is negated by another boolean.
fn can_be_defined(representative: &Rc<str>, context: &CompilationContext) -> bool {
    context.equivalences.domain(representative) == (Domain::IntervalDomain { lb: 0, ub: 1 })
        && !context.boolean_predicates.contains_key(representative)
        && !context.variable_map.contains_key(representative)
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
    fn negations_are_defined_along_chains_but_not_cycles() {
        let mut ast_builder = FlatZincAst::builder();

        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "x".into(),
            lb: 0,
            ub: 10,
            expr: None,
            annos: vec![],
        });
        for id in ["a", "b", "c", "d", "e", "f", "g"] {
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
            // Defines `g` as `!f`, which is `[x <= 5]` through the next constraint.
            ("bool_not", vec![var("f"), var("g")]),
            // Defines `f` as `!e`.
            ("bool_not", vec![var("e"), var("f")]),
            // Defines `b` and `c` as `!a`; the constraint between `b` and `c` is kept.
            ("bool_not", vec![var("a"), var("b")]),
            ("bool_not", vec![var("b"), var("c")]),
            ("bool_not", vec![var("c"), var("a")]),
            // Defines `d` as `!e`; the constraint between `d` and `f` is kept.
            ("bool_not", vec![var("e"), var("d")]),
            ("bool_not", vec![var("d"), var("f")]),
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
        assert_eq!(predicate("c"), predicate![a <= 0]);
        assert_eq!(predicate("d"), predicate![x >= 6]);
        assert_eq!(predicate("f"), predicate![x >= 6]);
        assert_eq!(predicate("g"), predicate![x <= 5]);

        let remaining = context
            .constraints
            .iter()
            .map(|(_, constraint)| &constraint.exprs[..])
            .collect::<Vec<_>>();
        assert_eq!(
            remaining,
            vec![&[var("b"), var("c")][..], &[var("d"), var("f")][..]]
        );
    }
}
