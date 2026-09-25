//! Unit tests on the pure parts of symmetry detection: refinement, candidate proposal and
//! verification over hand-built graphs. Posting is covered by the integration tests in
//! `tests/mzn_symmetry_test.rs`.

use std::rc::Rc;
use std::time::Duration;

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

fn detect(
    variables: &[Variable],
    objective: Option<VarIndex>,
    constraints: Vec<CanonicalConstraint>,
) -> (Graph, Vec<Permutation>) {
    let graph = Graph::new(variables.len(), constraints);
    let colouring = refine(&graph, initial_colouring(variables, objective));
    let classes = colour_classes(&colouring);
    let candidates = candidate_swaps(&graph, &colouring, &classes, &budget());
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
        assert!(is_automorphism(&graph, &variables, None, candidate));
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
    assert!(!is_automorphism(
        &graph,
        &variables,
        Some(0),
        &swap(4, 0, 1)
    ));
}

#[test]
fn different_coefficients_are_not_interchangeable() {
    // x0 + 2*x1 <= 5: x0 and x1 are distinguished by their coefficient.
    let variables = vec![int_var(0, 5); 2];
    let (graph, candidates) = detect(&variables, None, vec![linear_le(&[(1, 0), (2, 1)], 5)]);

    assert!(candidates.is_empty());
    assert!(!is_automorphism(&graph, &variables, None, &swap(2, 0, 1)));
}

#[test]
fn different_domains_are_not_interchangeable() {
    let variables = vec![int_var(0, 5), int_var(0, 6)];
    let (graph, candidates) = detect(&variables, None, vec![linear_le(&[(1, 0), (1, 1)], 5)]);

    assert!(candidates.is_empty());
    assert!(!is_automorphism(&graph, &variables, None, &swap(2, 0, 1)));
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

    assert!(!is_automorphism(&graph, &variables, None, &swap(3, 1, 2)));
}

#[test]
fn verification_rejects_non_bijections() {
    let variables = vec![int_var(0, 5); 2];
    let graph = Graph::new(2, vec![linear_le(&[(1, 0), (1, 1)], 5)]);
    let collapse = Permutation { map: vec![0, 0] };

    assert!(!is_automorphism(&graph, &variables, None, &collapse));
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
        .filter(|candidate| is_automorphism(&graph, &variables, None, candidate))
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
