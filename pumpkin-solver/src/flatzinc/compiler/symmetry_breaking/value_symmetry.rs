//! Detection of interchangeable values.
//!
//! Values `a` and `b` are interchangeable for a group `X` of integer variables when exchanging
//! them in the values of every variable of `X` maps solutions to solutions. The pass considers
//! variables that occur only where exchanging values has a known effect: compared by equality or
//! disequality (plain, reified or half-reified), in `all_different`, in element constraints over
//! variable arrays, and as the index of an element constraint over a constant array. A comparison
//! with a constant maps to the comparison with the exchanged constant; an element over a constant
//! array maps to the element over the array with the entries at positions `a` and `b` exchanged.
//!
//! The literal `r` of a reified comparison `r <-> x = a` maps to the literal `r'` of
//! `r' <-> x = b`, and the exchange of `r` and `r'` is followed through the constraints that
//! use them: each such constraint is matched with its image in the model, which fixes the images
//! of the variables it defines. The resulting permutation is then verified like a variable
//! symmetry: every affected constraint must map onto a constraint of the model.
//!
//! For example, take `x`, `y` and `z` in `1..3` with `x != y` and `y != z`. Every constraint on
//! them is a disequality and the domains are equal, so exchanging any two values maps the model to
//! itself. The exchanges of `1, 2` and of `2, 3` are verified, which makes the values `1, 2, 3`
//! interchangeable. If the model also contained `x <= 2`, `x` would be compared with a constant by
//! order, so no values would be interchangeable.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::time::Instant;

use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::containers::HashSet;

use super::super::context::Domain;
use super::canonical_form::CanonicalConstraint;
use super::canonical_form::Constant;
use super::canonical_form::Graph;
use super::canonical_form::SlotKey;
use super::canonical_form::VarIndex;
use super::canonical_form::VarKind;
use super::canonical_form::Variable;

/// How a constraint treats a slot, as far as exchanging values is concerned.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ValueRole {
    /// Compared by (dis)equality; exchanged values map to exchanged values.
    Value,
    /// The literal of a reified comparison with a constant.
    Literal,
    /// The index of an element constraint over a variable array; values cannot be exchanged.
    VariableArrayIndex,
    /// The index of an element constraint over a constant array.
    ConstantArrayIndex,
}

fn value_role(id: &str, key: SlotKey) -> Option<ValueRole> {
    match (id, key) {
        ("int_eq" | "int_ne", SlotKey::Commutative { group: 0 }) => Some(ValueRole::Value),
        (
            "int_eq_reif" | "int_ne_reif" | "int_eq_imp" | "int_ne_imp",
            SlotKey::Commutative { group: 0 },
        ) => Some(ValueRole::Value),
        (
            "int_eq_reif" | "int_ne_reif" | "int_eq_imp" | "int_ne_imp",
            SlotKey::Positional { arg: 2, .. },
        ) => Some(ValueRole::Literal),
        (
            "fzn_all_different_int" | "pumpkin_all_different" | "all_different_int",
            SlotKey::Unordered { arg: 0 },
        ) => Some(ValueRole::Value),
        ("array_var_int_element", SlotKey::Positional { arg: 0, .. }) => {
            Some(ValueRole::VariableArrayIndex)
        }
        ("array_var_int_element", SlotKey::Positional { arg: 1 | 2, .. }) => Some(ValueRole::Value),
        ("array_int_element" | "array_bool_element", SlotKey::Positional { arg: 0, .. }) => {
            Some(ValueRole::ConstantArrayIndex)
        }
        _ => None,
    }
}

/// Whether a variable in this slot can have its values exchanged.
fn allows_exchange(id: &str, key: SlotKey) -> bool {
    matches!(
        value_role(id, key),
        Some(ValueRole::Value | ValueRole::ConstantArrayIndex)
    )
}

/// A group of variables and a chain of values that are pairwise interchangeable for it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct InterchangeableValues {
    /// In increasing variable order.
    pub(crate) variables: Vec<VarIndex>,
    /// In increasing order; consecutive values are exchanged by a verified symmetry.
    pub(crate) values: Vec<i64>,
}

fn contains(domain: &Domain, value: i64) -> bool {
    match domain {
        Domain::IntervalDomain { lb, ub } => i64::from(*lb) <= value && value <= i64::from(*ub),
        Domain::SparseDomain { values } => values.iter().any(|&v| i64::from(v) == value),
    }
}

fn domain_values(domain: &Domain) -> Vec<i64> {
    match domain {
        Domain::IntervalDomain { lb, ub } => (i64::from(*lb)..=i64::from(*ub)).collect(),
        Domain::SparseDomain { values } => values.iter().map(|&v| i64::from(v)).collect(),
    }
}

fn exchanged(value: i64, a: i64, b: i64) -> i64 {
    if value == a {
        b
    } else if value == b {
        a
    } else {
        value
    }
}

/// The exchange of values `a` and `b` for the variables `acting`, together with the variable
/// permutation `mapping` it induces on literals and the variables defined from them.
struct Exchange<'a> {
    graph: &'a Graph,
    acting: HashSet<VarIndex>,
    a: i64,
    b: i64,
    mapping: HashMap<VarIndex, VarIndex>,
}

impl Exchange<'_> {
    fn image_of_var(&self, var: VarIndex) -> VarIndex {
        self.mapping.get(&var).copied().unwrap_or(var)
    }

    /// The constants of the image of a constraint, or `None` if the exchange moves an element
    /// array entry to a position before the start of the array; such an exchange maps a feasible
    /// index to an infeasible one and is not a symmetry.
    fn image_constants(
        &self,
        constraint: &CanonicalConstraint,
    ) -> Option<Vec<(SlotKey, Constant)>> {
        let compares = constraint.slots.iter().any(|&(key, var)| {
            self.acting.contains(&var) && value_role(&constraint.id, key) == Some(ValueRole::Value)
        });
        let indexes = constraint.slots.iter().any(|&(key, var)| {
            self.acting.contains(&var)
                && value_role(&constraint.id, key) == Some(ValueRole::ConstantArrayIndex)
        });
        constraint
            .constants
            .iter()
            .map(|(key, constant)| match (key, constant) {
                (_, Constant::Int(value))
                    if compares && value_role(&constraint.id, *key) == Some(ValueRole::Value) =>
                {
                    Some((*key, Constant::Int(exchanged(*value, self.a, self.b))))
                }
                // Element arrays are indexed from 1.
                (SlotKey::Positional { arg: 1, index }, _) if indexes => {
                    let position = exchanged(i64::from(*index) + 1, self.a, self.b) - 1;
                    let index = u32::try_from(position).ok()?;
                    Some((SlotKey::Positional { arg: 1, index }, constant.clone()))
                }
                _ => Some((*key, constant.clone())),
            })
            .collect()
    }

    fn image(&self, constraint: &CanonicalConstraint) -> Option<CanonicalConstraint> {
        let slots = constraint
            .slots
            .iter()
            .map(|&(key, var)| (key, self.image_of_var(var)))
            .collect();
        Some(CanonicalConstraint::new(
            Rc::clone(&constraint.id),
            self.image_constants(constraint)?,
            slots,
        ))
    }

    /// Record `from -> to` and `to -> from`; false if that contradicts the mapping so far.
    fn map_pair(&mut self, variables: &[Variable], from: VarIndex, to: VarIndex) -> bool {
        if self.acting.contains(&from) || self.acting.contains(&to) {
            return from == to;
        }
        if variables[from] != variables[to] {
            return false;
        }
        for (x, y) in [(from, to), (to, from)] {
            match self.mapping.get(&x) {
                Some(&existing) if existing != y => return false,
                _ => {
                    let _ = self.mapping.insert(x, y);
                }
            }
        }
        true
    }
}

/// The outcome of matching one constraint with its image.
enum Matching {
    /// Every variable of the constraint is mapped.
    Complete,
    /// The images of some variables are not determined yet.
    Deferred,
    /// No constraint of the model can be the image.
    Impossible,
}

/// Match a constraint with a constraint of the model that agrees with its image on every
/// variable mapped so far, and map its remaining variables to the corresponding ones. Variables
/// are only mapped when the correspondence is unique; otherwise the constraint is deferred.
fn match_constraint(
    exchange: &mut Exchange<'_>,
    variables: &[Variable],
    by_tag: &HashMap<u64, Vec<usize>>,
    index: usize,
    newly_mapped: &mut Vec<VarIndex>,
) -> Matching {
    let graph = exchange.graph;
    let constraint = &graph.constraints[index];
    let is_known = |v: &VarIndex| exchange.mapping.contains_key(v) || exchange.acting.contains(v);
    let unmapped: Vec<(SlotKey, VarIndex)> = constraint
        .slots
        .iter()
        .copied()
        .filter(|(_, v)| !is_known(v))
        .collect();
    if unmapped.is_empty() {
        return Matching::Complete;
    }
    let known: Vec<(SlotKey, VarIndex)> = constraint
        .slots
        .iter()
        .copied()
        .filter(|(_, v)| is_known(v))
        .map(|(key, v)| (key, exchange.image_of_var(v)))
        .collect();
    let Some(constants) = exchange.image_constants(constraint) else {
        return Matching::Impossible;
    };
    let tag =
        CanonicalConstraint::new(Rc::clone(&constraint.id), constants.clone(), vec![]).tag_hash();
    let matches: Vec<usize> = by_tag
        .get(&tag)
        .into_iter()
        .flatten()
        .copied()
        .filter(|&candidate| {
            let other = &graph.constraints[candidate];
            other.id == constraint.id
                && other.constants == constants
                && other.slots.len() == constraint.slots.len()
                && contains_all(&other.slots, &known)
        })
        .collect();
    let chosen = match matches.as_slice() {
        [] => return Matching::Impossible,
        [only] => *only,
        _ => return Matching::Deferred,
    };

    // The slots of the match that are not taken by a mapped variable.
    let mut free: Vec<(SlotKey, VarIndex)> = graph.constraints[chosen].slots.clone();
    for known_slot in &known {
        if let Some(position) = free.iter().position(|slot| slot == known_slot) {
            let _ = free.swap_remove(position);
        }
    }
    let mut pairs = Vec::with_capacity(unmapped.len());
    for &(key, from) in &unmapped {
        let mut options = free.iter().filter(|(k, _)| *k == key).map(|&(_, v)| v);
        match (options.next(), options.next()) {
            (None, _) => return Matching::Impossible,
            (Some(to), None) => pairs.push((from, to)),
            (Some(_), Some(_)) => return Matching::Deferred,
        }
    }
    for (from, to) in pairs {
        if !exchange.map_pair(variables, from, to) {
            return Matching::Impossible;
        }
        newly_mapped.push(from);
        newly_mapped.push(to);
    }
    Matching::Complete
}

/// Extend the mapping from the variables in `start` through the constraints that use them,
/// until no constraint determines further images. Variables whose image is never determined
/// keep their place; the final verification decides whether that is a symmetry. Returns false
/// if some constraint cannot have an image.
fn follow_literals(
    exchange: &mut Exchange<'_>,
    variables: &[Variable],
    by_tag: &HashMap<u64, Vec<usize>>,
    start: Vec<VarIndex>,
) -> bool {
    let graph = exchange.graph;
    let mut complete: HashSet<usize> = HashSet::default();
    let mut pending: BTreeSet<usize> = start
        .iter()
        .flat_map(|&var| graph.incident[var].iter().map(|&(index, _)| index))
        .collect();
    loop {
        let mut deferred = Vec::new();
        let mut progress = false;
        while let Some(index) = pending.pop_first() {
            let mut newly_mapped = Vec::new();
            match match_constraint(exchange, variables, by_tag, index, &mut newly_mapped) {
                Matching::Complete => {
                    let _ = complete.insert(index);
                    for var in newly_mapped {
                        progress = true;
                        pending.extend(
                            graph.incident[var]
                                .iter()
                                .map(|&(other, _)| other)
                                .filter(|other| !complete.contains(other)),
                        );
                    }
                }
                Matching::Deferred => deferred.push(index),
                Matching::Impossible => return false,
            }
        }
        if deferred.is_empty() || !progress {
            return true;
        }
        pending.extend(deferred);
    }
}

fn contains_all(slots: &[(SlotKey, VarIndex)], required: &[(SlotKey, VarIndex)]) -> bool {
    let mut remaining = slots.to_vec();
    required.iter().all(|slot| {
        remaining
            .iter()
            .position(|other| other == slot)
            .map(|position| remaining.swap_remove(position))
            .is_some()
    })
}

/// Whether exchanging `a` and `b` for the variables of `group` that can take them is a symmetry
/// of the model.
#[allow(
    clippy::too_many_arguments,
    reason = "the model, its indexes and the exchange are passed separately"
)]
fn is_value_symmetry(
    graph: &Graph,
    variables: &[Variable],
    objective: Option<VarIndex>,
    store: &HashSet<&CanonicalConstraint>,
    by_tag: &HashMap<u64, Vec<usize>>,
    group: &[VarIndex],
    a: i64,
    b: i64,
) -> bool {
    let takes =
        |var: VarIndex| contains(&variables[var].domain, a) || contains(&variables[var].domain, b);
    let acting: HashSet<VarIndex> = group.iter().copied().filter(|&var| takes(var)).collect();
    if acting.is_empty()
        || acting
            .iter()
            .any(|&var| contains(&variables[var].domain, a) != contains(&variables[var].domain, b))
        || objective.is_some_and(|objective| acting.contains(&objective))
    {
        return false;
    }
    // A variable compared with an acting variable must act as well if it can take `a` or `b`.
    for &var in &acting {
        for &(index, _) in &graph.incident[var] {
            let constraint = &graph.constraints[index];
            if constraint.slots.iter().any(|&(key, other)| {
                value_role(&constraint.id, key) == Some(ValueRole::Value)
                    && !acting.contains(&other)
                    && takes(other)
            }) {
                return false;
            }
        }
    }

    let mut exchange = Exchange {
        graph,
        acting,
        a,
        b,
        mapping: HashMap::default(),
    };

    // The literal of each reified comparison of an acting variable with `a` or `b`.
    let mut literal_of: HashMap<(Rc<str>, VarIndex, i64), VarIndex> = HashMap::default();
    for &var in &exchange.acting {
        for &(index, _) in &graph.incident[var] {
            let constraint = &graph.constraints[index];
            let constant = constraint.constants.iter().find_map(|(key, constant)| {
                match (value_role(&constraint.id, *key), constant) {
                    (Some(ValueRole::Value), Constant::Int(value))
                        if *value == a || *value == b =>
                    {
                        Some(*value)
                    }
                    _ => None,
                }
            });
            let literal = constraint.slots.iter().find_map(|&(key, slot_var)| {
                (value_role(&constraint.id, key) == Some(ValueRole::Literal)).then_some(slot_var)
            });
            if let (Some(constant), Some(literal)) = (constant, literal)
                && literal_of
                    .insert((Rc::clone(&constraint.id), var, constant), literal)
                    .is_some()
            {
                return false;
            }
        }
    }
    let mut start = Vec::new();
    let pairs: Vec<(VarIndex, VarIndex)> = literal_of
        .iter()
        .filter(|((_, _, constant), _)| *constant == a)
        .map(|((id, var, _), &literal)| {
            let image = literal_of.get(&(Rc::clone(id), *var, b)).copied();
            (literal, image.unwrap_or(usize::MAX))
        })
        .collect();
    if pairs.len() * 2 != literal_of.len() {
        return false;
    }
    for (from, to) in pairs {
        if to == usize::MAX || !exchange.map_pair(variables, from, to) {
            return false;
        }
        start.push(from);
        start.push(to);
    }
    if !follow_literals(&mut exchange, variables, by_tag, start) {
        return false;
    }
    if objective.is_some_and(|objective| exchange.mapping.contains_key(&objective)) {
        return false;
    }

    let mut affected: Vec<usize> = exchange
        .acting
        .iter()
        .chain(exchange.mapping.keys())
        .flat_map(|&var| graph.incident[var].iter().map(|&(index, _)| index))
        .collect();
    affected.sort_unstable();
    affected.dedup();
    affected.into_iter().all(|index| {
        exchange
            .image(&graph.constraints[index])
            .is_some_and(|image| store.contains(&image))
    })
}

/// Groups whose variables together have more values than this are skipped.
const MAX_VALUES: usize = 1000;

/// Integer variables that occur only in slots where values can be exchanged, split into the
/// groups linked by value comparisons. The union of all groups comes first, since values are
/// often interchangeable for all such variables together.
fn candidate_groups(graph: &Graph, variables: &[Variable]) -> Vec<Vec<VarIndex>> {
    let eligible: Vec<VarIndex> = (0..graph.num_variables())
        .filter(|&var| {
            variables[var].kind == VarKind::Int
                && !graph.incident[var].is_empty()
                && graph.incident[var]
                    .iter()
                    .all(|&(index, key)| allows_exchange(&graph.constraints[index].id, key))
        })
        .collect();
    let is_eligible: HashSet<VarIndex> = eligible.iter().copied().collect();

    let mut parent: HashMap<VarIndex, VarIndex> = eligible.iter().map(|&v| (v, v)).collect();
    fn find(parent: &HashMap<VarIndex, VarIndex>, mut x: VarIndex) -> VarIndex {
        while parent[&x] != x {
            x = parent[&x];
        }
        x
    }
    for constraint in &graph.constraints {
        let linked: Vec<VarIndex> = constraint
            .slots
            .iter()
            .filter(|&&(key, var)| {
                is_eligible.contains(&var)
                    && value_role(&constraint.id, key) == Some(ValueRole::Value)
            })
            .map(|&(_, var)| var)
            .collect();
        for pair in linked.windows(2) {
            let (x, y) = (find(&parent, pair[0]), find(&parent, pair[1]));
            let _ = parent.insert(x, y);
        }
    }
    let mut components: BTreeMap<VarIndex, Vec<VarIndex>> = BTreeMap::new();
    for &var in &eligible {
        let root = find(&parent, var);
        components.entry(root).or_default().push(var);
    }

    let mut groups = vec![eligible];
    if components.len() > 1 {
        groups.extend(
            components
                .into_values()
                .filter(|component| component.len() > 1),
        );
    }
    groups.retain(|group| group.len() > 1);
    groups
}

/// Every group with a chain of at least two interchangeable values, as far as found before
/// `deadline`. Once the union of all candidate variables has a chain, the separate groups are not
/// tried, so the chains found never act on overlapping variables with different groups.
pub(crate) fn interchangeable_values(
    graph: &Graph,
    variables: &[Variable],
    objective: Option<VarIndex>,
    deadline: Instant,
) -> Vec<InterchangeableValues> {
    let store: HashSet<&CanonicalConstraint> = graph.constraints.iter().collect();
    let mut by_tag: HashMap<u64, Vec<usize>> = HashMap::default();
    for (index, &tag) in graph.tag_hashes.iter().enumerate() {
        by_tag.entry(tag).or_default().push(index);
    }

    let mut found = Vec::new();
    for (position, group) in candidate_groups(graph, variables).into_iter().enumerate() {
        if position == 1 && !found.is_empty() {
            break;
        }
        let too_many = group.iter().any(|&var| match &variables[var].domain {
            Domain::IntervalDomain { lb, ub } => {
                i64::from(*ub) - i64::from(*lb) >= MAX_VALUES as i64
            }
            Domain::SparseDomain { values } => values.len() > MAX_VALUES,
        });
        if too_many {
            continue;
        }
        let mut values: Vec<i64> = group
            .iter()
            .flat_map(|&var| domain_values(&variables[var].domain))
            .collect();
        values.sort_unstable();
        values.dedup();

        let mut chain: Vec<i64> = Vec::new();
        for pair in values.windows(2) {
            if Instant::now() > deadline {
                break;
            }
            let (a, b) = (pair[0], pair[1]);
            if is_value_symmetry(graph, variables, objective, &store, &by_tag, &group, a, b) {
                if chain.last() != Some(&a) {
                    chain = vec![a];
                }
                chain.push(b);
                continue;
            }
            if chain.len() > 1 {
                found.push(InterchangeableValues {
                    variables: group.clone(),
                    values: std::mem::take(&mut chain),
                });
            }
            chain.clear();
        }
        if chain.len() > 1 {
            found.push(InterchangeableValues {
                variables: group,
                values: chain,
            });
        }
    }
    found
}
