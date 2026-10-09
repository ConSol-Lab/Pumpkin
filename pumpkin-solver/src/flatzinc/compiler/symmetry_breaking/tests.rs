//! Unit tests on the pure parts of symmetry detection: refinement, candidate proposal and
//! verification over hand-built graphs. Posting is covered by the integration tests in
//! `tests/mzn_symmetry_test.rs`.

use std::rc::Rc;
use std::time::Duration;

use super::automorphism_check::constraint_set;
use super::automorphism_check::is_automorphism;
use super::candidate_permutations::Budget;
use super::candidate_permutations::Permutation;
use super::candidate_permutations::candidate_swaps;
use super::canonical_form::CanonicalConstraint;
use super::canonical_form::Constant;
use super::canonical_form::Graph;
use super::canonical_form::SlotKey;
use super::canonical_form::VarIndex;
use super::canonical_form::VarKind;
use super::canonical_form::Variable;
use super::colour_refinement::colour_classes;
use super::colour_refinement::individualised_colouring;
use super::colour_refinement::initial_colouring;
use super::colour_refinement::refine;
use crate::flatzinc::compiler::context::Domain;

fn int_var(lb: i32, ub: i32) -> Variable {
    Variable {
        kind: VarKind::Int,
        domain: Domain::IntervalDomain { lb, ub },
    }
}

fn coefficient(coefficient: i64) -> SlotKey {
    SlotKey::Coefficient {
        arg: 1,
        coefficient,
    }
}

fn rhs(value: i64) -> (SlotKey, Constant) {
    (
        SlotKey::Positional { arg: 2, index: 0 },
        Constant::Int(value),
    )
}

/// `sum(coefficient_i * var_i) <= bound`
fn linear_le(terms: &[(i64, VarIndex)], bound: i64) -> CanonicalConstraint {
    CanonicalConstraint::new(
        Rc::from("int_lin_le"),
        vec![rhs(bound)],
        terms.iter().map(|&(c, v)| (coefficient(c), v)).collect(),
    )
}

fn budget() -> Budget {
    Budget::new(Duration::from_secs(10), 16)
}

fn far() -> std::time::Instant {
    std::time::Instant::now() + Duration::from_secs(10)
}

fn check(
    graph: &Graph,
    variables: &[Variable],
    objective: Option<VarIndex>,
    permutation: &Permutation,
) -> bool {
    is_automorphism(
        graph,
        &constraint_set(graph),
        variables,
        objective,
        permutation,
    )
}

fn detect(
    variables: &[Variable],
    objective: Option<VarIndex>,
    constraints: Vec<CanonicalConstraint>,
) -> (Graph, Vec<Permutation>) {
    let graph = Graph::new(variables.len(), constraints);
    let refinement = refine(&graph, initial_colouring(variables, objective), far());
    let classes = colour_classes(&refinement.colouring());
    let candidates = candidate_swaps(&graph, &refinement, &classes, &budget());
    (graph, candidates)
}

fn swap(num_variables: usize, a: VarIndex, b: VarIndex) -> Permutation {
    let mut permutation = Permutation::identity(num_variables);
    permutation.map[a] = b;
    permutation.map[b] = a;
    permutation
}

#[test]
fn two_independent_pairs_yield_their_swaps() {
    // x0 + x1 <= 5 and x2 + x3 <= 5: (x0 x1) and (x2 x3) are symmetries.
    let variables = vec![int_var(0, 5); 4];
    let (graph, candidates) = detect(
        &variables,
        None,
        vec![
            linear_le(&[(1, 0), (1, 1)], 5),
            linear_le(&[(1, 2), (1, 3)], 5),
        ],
    );

    assert!(!candidates.is_empty());
    for candidate in &candidates {
        assert!(check(&graph, &variables, None, candidate));
    }
    assert!(
        candidates.contains(&swap(4, 0, 1)),
        "the exchange of x0 and x1 should be proposed, got {candidates:?}"
    );
}

#[test]
fn objective_variable_is_never_moved() {
    // Same model, but x0 is the objective: the exchange (x0 x1) must not be proposed or
    // accepted, while (x2 x3) still is.
    let variables = vec![int_var(0, 5); 4];
    let (graph, candidates) = detect(
        &variables,
        Some(0),
        vec![
            linear_le(&[(1, 0), (1, 1)], 5),
            linear_le(&[(1, 2), (1, 3)], 5),
        ],
    );

    assert!(candidates.iter().all(|candidate| candidate.map[0] == 0));
    assert!(candidates.contains(&swap(4, 2, 3)));
    assert!(!check(&graph, &variables, Some(0), &swap(4, 0, 1)));
}

#[test]
fn different_coefficients_are_not_interchangeable() {
    // x0 + 2*x1 <= 5: x0 and x1 are distinguished by their coefficient.
    let variables = vec![int_var(0, 5); 2];
    let (graph, candidates) = detect(&variables, None, vec![linear_le(&[(1, 0), (2, 1)], 5)]);

    assert!(candidates.is_empty());
    assert!(!check(&graph, &variables, None, &swap(2, 0, 1)));
}

#[test]
fn different_domains_are_not_interchangeable() {
    let variables = vec![int_var(0, 5), int_var(0, 6)];
    let (graph, candidates) = detect(&variables, None, vec![linear_le(&[(1, 0), (1, 1)], 5)]);

    assert!(candidates.is_empty());
    assert!(!check(&graph, &variables, None, &swap(2, 0, 1)));
}

#[test]
fn verification_rejects_a_permutation_that_does_not_preserve_the_store() {
    // x0 + x1 <= 5 and x0 + 2*x2 <= 5. Exchanging x1 and x2 maps the second constraint to
    // x0 + 2*x1 <= 5, which is not in the model.
    let variables = vec![int_var(0, 5); 3];
    let graph = Graph::new(
        3,
        vec![
            linear_le(&[(1, 0), (1, 1)], 5),
            linear_le(&[(1, 0), (2, 2)], 5),
        ],
    );

    assert!(!check(&graph, &variables, None, &swap(3, 1, 2)));
}

#[test]
fn verification_rejects_non_bijections() {
    let variables = vec![int_var(0, 5); 2];
    let graph = Graph::new(2, vec![linear_le(&[(1, 0), (1, 1)], 5)]);
    let collapse = Permutation { map: vec![0, 0] };

    assert!(!check(&graph, &variables, None, &collapse));
}

#[test]
fn a_column_symmetry_moves_whole_columns() {
    // Two "rows" r0, r1 over three interchangeable "columns": every row is an at-most-one over
    // its three cells, and every column is constrained to a shared bound. Exchanging two
    // columns must move both cells of each.
    //
    //   cells: c[r][k] = 3*r + k
    let variables = vec![int_var(0, 1); 6];
    let mut constraints = Vec::new();
    for row in 0..2 {
        constraints.push(linear_le(
            &[(1, 3 * row), (1, 3 * row + 1), (1, 3 * row + 2)],
            1,
        ));
    }
    for column in 0..3 {
        constraints.push(linear_le(&[(1, column), (1, 3 + column)], 1));
    }
    let (graph, candidates) = detect(&variables, None, constraints);

    let verified: Vec<_> = candidates
        .iter()
        .filter(|candidate| check(&graph, &variables, None, candidate))
        .collect();
    assert!(!verified.is_empty());
    for permutation in &verified {
        assert_eq!(
            permutation.support().len(),
            4,
            "a column exchange moves two cells per row: {permutation:?}"
        );
    }

    // The generators exchange consecutive columns, so their lex-leader constraints form a
    // chain. Exchanging columns 1 and 2 is only proposed when pairs are consecutive.
    let supports: Vec<Vec<VarIndex>> = verified.iter().map(|p| p.support()).collect();
    assert!(supports.contains(&vec![0, 1, 3, 4]), "{supports:?}");
    assert!(supports.contains(&vec![1, 2, 4, 5]), "{supports:?}");
}

#[test]
fn support_is_in_increasing_variable_order() {
    let permutation = swap(5, 4, 1);
    assert_eq!(permutation.support(), vec![1, 4]);
}

/// The partition of a colouring as a set of classes, independent of the colour names.
fn partition(colouring: &[u32]) -> Vec<Vec<VarIndex>> {
    let mut classes: std::collections::BTreeMap<u32, Vec<VarIndex>> = Default::default();
    for (var, &colour) in colouring.iter().enumerate() {
        classes.entry(colour).or_default().push(var);
    }
    let mut classes: Vec<_> = classes.into_values().collect();
    classes.sort();
    classes
}

/// Colour refinement without incrementality: every round recolours every variable.
fn full_refinement(graph: &Graph, variables: &[Variable]) -> Vec<Vec<VarIndex>> {
    full_refinement_from(graph, initial_colouring(variables, None))
}

fn full_refinement_from(graph: &Graph, start: Vec<u64>) -> Vec<Vec<VarIndex>> {
    use std::hash::Hash;
    use std::hash::Hasher;
    let hash = |value: &dyn Fn(&mut std::collections::hash_map::DefaultHasher)| {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        value(&mut hasher);
        hasher.finish()
    };
    let mut colours = start;
    loop {
        let constraint_colours: Vec<u64> = graph
            .constraints
            .iter()
            .enumerate()
            .map(|(index, constraint)| {
                let mut slots: Vec<_> = constraint
                    .slots
                    .iter()
                    .map(|&(key, var)| (key, colours[var]))
                    .collect();
                slots.sort();
                hash(&|h| {
                    graph.tag_hashes[index].hash(h);
                    slots.hash(h);
                })
            })
            .collect();
        let next: Vec<u64> = (0..graph.num_variables())
            .map(|var| {
                let mut neighbours: Vec<_> = graph.incident[var]
                    .iter()
                    .map(|&(index, key)| (constraint_colours[index], key))
                    .collect();
                neighbours.sort();
                hash(&|h| {
                    colours[var].hash(h);
                    neighbours.hash(h);
                })
            })
            .collect();
        let before = partition(&normalised(&colours)).len();
        let after = partition(&normalised(&next)).len();
        colours = next;
        if before == after {
            return partition(&normalised(&colours));
        }
    }
}

fn normalised(hashes: &[u64]) -> Vec<u32> {
    let mut unique = hashes.to_vec();
    unique.sort_unstable();
    unique.dedup();
    hashes
        .iter()
        .map(|h| unique.binary_search(h).expect("present") as u32)
        .collect()
}

#[test]
fn incremental_refinement_gives_the_same_partition_as_full_refinement() {
    // A path x0 - x1 - ... - x9 of disequalities plus a linear constraint over the ends, which
    // distinguishes variables by their distance to the ends only after several rounds.
    let variables = vec![int_var(0, 5); 10];
    let mut constraints: Vec<CanonicalConstraint> = (0..9)
        .map(|i| linear_le(&[(1, i), (-1, i + 1)], 0))
        .collect();
    constraints.push(linear_le(&[(2, 0), (3, 9)], 7));
    let graph = Graph::new(10, constraints);

    let incremental =
        partition(&refine(&graph, initial_colouring(&variables, None), far()).colouring());
    assert_eq!(incremental, full_refinement(&graph, &variables));
}

#[test]
fn incremental_refinement_matches_full_refinement_on_a_grid() {
    // A 4x4 grid of cells with an at-most-one per row and per column, and one corner cell
    // singled out by a unary bound.
    let variables = vec![int_var(0, 1); 16];
    let mut constraints = Vec::new();
    for r in 0..4 {
        constraints.push(linear_le(
            &[(1, 4 * r), (1, 4 * r + 1), (1, 4 * r + 2), (1, 4 * r + 3)],
            1,
        ));
        constraints.push(linear_le(&[(1, r), (1, 4 + r), (1, 8 + r), (1, 12 + r)], 1));
    }
    constraints.push(linear_le(&[(1, 0)], 0));
    let graph = Graph::new(16, constraints);

    let incremental =
        partition(&refine(&graph, initial_colouring(&variables, None), far()).colouring());
    assert_eq!(incremental, full_refinement(&graph, &variables));
}

#[test]
fn incremental_individualisation_matches_full_refinement() {
    // A 3x4 grid with at-most-one per row and per column: after individualising a cell, the
    // refinement separates its row, its column and the rest.
    let variables = vec![int_var(0, 1); 12];
    let mut constraints = Vec::new();
    for r in 0..3 {
        constraints.push(linear_le(
            &[(1, 4 * r), (1, 4 * r + 1), (1, 4 * r + 2), (1, 4 * r + 3)],
            1,
        ));
    }
    for c in 0..4 {
        constraints.push(linear_le(&[(1, c), (1, 4 + c), (1, 8 + c)], 1));
    }
    let graph = Graph::new(12, constraints);
    let converged = refine(&graph, initial_colouring(&variables, None), far());
    let stable = converged.colouring();
    for var in 0..12 {
        let incremental = partition(&individualised_colouring(&graph, &converged, var, far()));
        let mut start: Vec<u64> = stable.iter().map(|&c| u64::from(c)).collect();
        start[var] = u64::MAX;
        assert_eq!(
            incremental,
            full_refinement_from(&graph, start),
            "individualising {var}"
        );
    }
}
