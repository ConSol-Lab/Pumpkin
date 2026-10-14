//! The value symmetry steps of a croc proof: for two values interchangeable for a group of
//! variables, the step that adds their value precedence. The checker has no rule for value
//! precedence, so no inference can cite the constraint a step adds; under proof logging the
//! constraint is therefore not posted.

use super::super::breaking_steps::log;
use super::super::breaking_steps::log_order_key;
use super::super::breaking_steps::take_identifier;
use super::super::context::CompilationContext;
use super::super::context::Domain;
use super::canonical_form::Constant;
use super::canonical_form::Graph;
use super::canonical_form::ModelIndex;
use super::canonical_form::SlotKey;
use super::canonical_form::VarIndex;
use super::value_symmetry::InterchangeableValues;
use crate::core::predicate;
use crate::core::proof::HeadField;

fn contains(domain: &Domain, value: i64) -> bool {
    match domain {
        Domain::IntervalDomain { lb, ub } => i64::from(*lb) <= value && value <= i64::from(*ub),
        Domain::SparseDomain { values } => values.iter().any(|&v| i64::from(v) == value),
    }
}

fn lower_bound(domain: &Domain) -> i32 {
    match domain {
        Domain::IntervalDomain { lb, .. } => *lb,
        Domain::SparseDomain { values } => values.iter().copied().min().unwrap_or(0),
    }
}

/// Whether exchanging `a` and `b` in the variables of `group` keeps every constraint over them
/// by one of the forms the checker reads, and the initial domains hold at both values or at
/// neither.
pub(super) fn is_kept_by_exchange(
    graph: &Graph,
    index: &ModelIndex,
    group: &[VarIndex],
    a: i64,
    b: i64,
) -> bool {
    let in_group = |var: VarIndex| group.contains(&var);
    if group.iter().any(|&var| {
        contains(&index.variables[var].domain, a) != contains(&index.variables[var].domain, b)
    }) || index.objective.is_some_and(in_group)
    {
        return false;
    }
    let mut constraints: Vec<usize> = group
        .iter()
        .flat_map(|&var| graph.incident[var].iter().map(|&(position, _)| position))
        .collect();
    constraints.sort_unstable();
    constraints.dedup();

    // `x - y = 0` or `!= 0` over two exchanged variables, or one exchanged variable compared with
    // a constant neither value meets.
    let comparison_is_kept =
        |variables: &[VarIndex], constants: &[i64]| match (variables, constants) {
            ([x, y], []) => in_group(*x) && in_group(*y),
            ([x], [c]) => in_group(*x) && *c != a && *c != b,
            _ => false,
        };

    constraints.into_iter().all(|position| {
        let constraint = &graph.constraints[position];
        let variables_in = |selected: &dyn Fn(SlotKey) -> bool| -> Vec<VarIndex> {
            constraint
                .slots
                .iter()
                .filter(|&&(key, _)| selected(key))
                .map(|&(_, var)| var)
                .collect()
        };
        let integers_in = |selected: &dyn Fn(SlotKey) -> bool| -> Option<Vec<i64>> {
            constraint
                .constants
                .iter()
                .filter(|&&(key, _)| selected(key))
                .map(|(_, constant)| match constant {
                    Constant::Int(value) => Some(*value),
                    _ => None,
                })
                .collect()
        };
        match constraint.id.as_ref() {
            "fzn_all_different_int" | "pumpkin_all_different" | "all_different_int" => {
                constraint.constants.is_empty()
                    && constraint.slots.iter().all(|&(_, var)| in_group(var))
            }
            "int_eq" | "int_ne" => integers_in(&|_| true)
                .is_some_and(|constants| comparison_is_kept(&variables_in(&|_| true), &constants)),
            "int_eq_reif" | "int_ne_reif" | "int_eq_imp" | "int_ne_imp" => {
                let compared = |key: SlotKey| matches!(key, SlotKey::Commutative { group: 0 });
                let indicator = |key: SlotKey| matches!(key, SlotKey::Positional { arg: 2, .. });
                variables_in(&indicator).iter().all(|&var| !in_group(var))
                    && integers_in(&compared).is_some_and(|constants| {
                        comparison_is_kept(&variables_in(&compared), &constants)
                    })
            }
            "array_var_int_element" => {
                let index_slot = |key: SlotKey| matches!(key, SlotKey::Positional { arg: 0, .. });
                let entries = |key: SlotKey| matches!(key, SlotKey::Positional { arg: 1, .. });
                let result = |key: SlotKey| matches!(key, SlotKey::Positional { arg: 2, .. });
                variables_in(&index_slot).iter().all(|&var| !in_group(var))
                    && integers_in(&entries).is_some_and(|constants| constants.is_empty())
                    && integers_in(&result).is_some_and(|constants| constants.is_empty())
                    && variables_in(&entries).iter().all(|&var| in_group(var))
                    && variables_in(&result).iter().all(|&var| in_group(var))
            }
            _ => constraint.slots.iter().all(|&(_, var)| !in_group(var)),
        }
    })
}

/// Whether every pair of consecutive values of the group is kept by its exchange.
pub(super) fn is_logged(graph: &Graph, index: &ModelIndex, found: &InterchangeableValues) -> bool {
    found
        .values
        .windows(2)
        .all(|pair| is_kept_by_exchange(graph, index, &found.variables, pair[0], pair[1]))
}

/// Write the order, a key preferring smaller values for every variable of a logged group in
/// variable order, and a `u` step for each pair of consecutive values of each group.
pub(super) fn log_steps(
    context: &mut CompilationContext,
    index: &ModelIndex,
    logged: &[&InterchangeableValues],
) {
    let mut keys: Vec<VarIndex> = logged
        .iter()
        .flat_map(|found| found.variables.iter().copied())
        .collect();
    keys.sort_unstable();
    keys.dedup();
    for var in keys {
        let domain = index.domain_ids[var];
        let lower = lower_bound(&index.variables[var].domain);
        log_order_key(
            context,
            vec![
                HeadField::Text("p ".to_owned()),
                HeadField::Name(domain),
                HeadField::Text(" smaller ".to_owned()),
                HeadField::Atom(predicate![domain <= lower - 1]),
                HeadField::Text(" c:0 l:".to_owned()),
                HeadField::Label("initial_domain"),
            ],
        );
    }
    for found in logged {
        for pair in found.values.windows(2) {
            let tag = context.solver.new_constraint_tag();
            let Some(identifier) = take_identifier(context, tag) else {
                return;
            };
            let mut fields = vec![HeadField::Text(format!(
                "u {identifier} {} {}",
                pair[0], pair[1]
            ))];
            for &var in &found.variables {
                fields.push(HeadField::Text(" ".to_owned()));
                fields.push(HeadField::Name(index.domain_ids[var]));
            }
            log(context, fields);
        }
    }
}
