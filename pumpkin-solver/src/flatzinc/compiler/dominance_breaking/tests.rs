//! Unit tests on the linear forms and the dominance order over hand-built linear
//! models.

use std::collections::BTreeMap;

use super::dominance_order::pairs_to_post;
use super::linear_forms::Row;
use super::linear_forms::rows_of;
use super::linear_model::Candidate;
use super::linear_model::LinearModel;
use super::linear_model::Relation;
use crate::flatzinc::compiler::context::Domain;
use crate::flatzinc::compiler::symmetry_breaking::canonical_form::Constant;
use crate::flatzinc::compiler::symmetry_breaking::canonical_form::Element;
use crate::flatzinc::compiler::symmetry_breaking::canonical_form::Resolved;
use crate::flatzinc::compiler::symmetry_breaking::canonical_form::VarKind;
use crate::flatzinc::compiler::symmetry_breaking::canonical_form::Variable;

fn binary() -> Variable {
    Variable {
        kind: VarKind::Int,
        domain: Domain::IntervalDomain { lb: 0, ub: 1 },
    }
}

fn candidate(var: usize, terms: &[(usize, i64)], objective_weight: i64) -> Candidate {
    Candidate {
        var,
        terms: terms.iter().copied().collect::<BTreeMap<_, _>>(),
        objective_weight,
    }
}

fn pairs(
    candidates: Vec<Candidate>,
    relations: Vec<Relation>,
    variables: Vec<Variable>,
) -> Vec<(usize, usize)> {
    let model = LinearModel {
        candidates,
        relations,
    };
    pairs_to_post(&model, &variables, 100).0
}

#[test]
fn smaller_weight_dominates() {
    // x0 + 2*x1 <= 5, equal objective weights: x0 dominates x1.
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[(0, 2)], 3)],
        vec![Relation::LessOrEqual],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(0, 1)]);
}

#[test]
fn larger_objective_weight_dominates() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 2), candidate(1, &[(0, 1)], 5)],
        vec![Relation::LessOrEqual],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(1, 0)]);
}

#[test]
fn conflicting_weight_and_objective_is_not_dominance() {
    // x0 is lighter but less profitable than x1.
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 2), candidate(1, &[(0, 3)], 5)],
        vec![Relation::LessOrEqual],
        vec![binary(); 2],
    );
    assert!(found.is_empty());
}

#[test]
fn equality_requires_equal_coefficients() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[(0, 2)], 3)],
        vec![Relation::Equal],
        vec![binary(); 2],
    );
    assert!(found.is_empty());
}

#[test]
fn a_missing_term_counts_as_zero() {
    // x0 occurs in a <= constraint that x1 does not occur in, so x0 has the larger coefficient
    // there and x1 dominates x0.
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[], 3)],
        vec![Relation::LessOrEqual],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(1, 0)]);
}

#[test]
fn identical_variables_are_ordered_in_one_direction_only() {
    // Posting both directions would force x0 = x1 and lose solutions that select one of them.
    let found = pairs(
        vec![candidate(0, &[(0, 5)], 3), candidate(1, &[(0, 5)], 3)],
        vec![Relation::LessOrEqual],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(0, 1)]);
}

#[test]
fn only_the_transitive_reduction_is_posted() {
    // Coefficients 1, 2, 3: x0 > x1 > x2, and x0 > x2 is implied.
    let found = pairs(
        vec![
            candidate(0, &[(0, 1)], 3),
            candidate(1, &[(0, 2)], 3),
            candidate(2, &[(0, 3)], 3),
        ],
        vec![Relation::LessOrEqual],
        vec![binary(); 3],
    );
    assert_eq!(found, vec![(0, 1), (1, 2)]);
}

#[test]
fn different_domains_are_never_compared() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[(0, 2)], 3)],
        vec![Relation::LessOrEqual],
        vec![
            binary(),
            Variable {
                kind: VarKind::Int,
                domain: Domain::IntervalDomain { lb: 0, ub: 2 },
            },
        ],
    );
    assert!(found.is_empty());
}

// ---- Linear forms ----------------------------------------------------------------------------

fn var(index: usize) -> Element {
    Element::Var(index)
}

fn int(value: i64) -> Element {
    Element::Const(Constant::Int(value))
}

fn scalar(element: Element) -> Resolved {
    Resolved::Scalar(element)
}

fn array(elements: Vec<Element>) -> Resolved {
    Resolved::Array(elements)
}

/// Variables 0 and 1 are Booleans, variables 2 and 3 are in `0..10`.
fn test_bounds(var: usize) -> (i64, i64) {
    if var < 2 { (0, 1) } else { (0, 10) }
}

fn less_or_equal(terms: &[(usize, i64)], rhs: i64) -> Row {
    Row {
        relation: Relation::LessOrEqual,
        terms: terms.to_vec(),
        rhs,
    }
}

#[test]
fn a_clause_is_one_row() {
    // bool_clause([x0], [x1]) is x0 \/ ~x1, the row -x0 + x1 <= 0.
    let rows = rows_of(
        "bool_clause",
        &[array(vec![var(0)]), array(vec![var(1)])],
        &test_bounds,
    );
    assert_eq!(rows, Some(vec![less_or_equal(&[(0, -1), (1, 1)], 0)]));
}

#[test]
fn a_half_reification_uses_a_big_m_from_the_bounds() {
    // x0 -> x2 <= 4 with x2 in 0..10 is x2 + 6 x0 <= 10.
    let rows = rows_of(
        "int_lin_le_imp",
        &[
            array(vec![int(1)]),
            array(vec![var(2)]),
            scalar(int(4)),
            scalar(var(0)),
        ],
        &test_bounds,
    );
    assert_eq!(rows, Some(vec![less_or_equal(&[(0, 6), (2, 1)], 10)]));
}

#[test]
fn a_full_reification_adds_the_negated_direction() {
    // x0 <-> x2 <= 4: also ~x0 -> x2 >= 5, which is -x2 - 5 x0 <= -5.
    let rows = rows_of(
        "int_le_reif",
        &[scalar(var(2)), scalar(int(4)), scalar(var(0))],
        &test_bounds,
    );
    assert_eq!(
        rows,
        Some(vec![
            less_or_equal(&[(0, 6), (2, 1)], 10),
            less_or_equal(&[(0, -5), (2, -1)], -5),
        ])
    );
}

#[test]
fn an_implication_that_the_bounds_satisfy_is_entailed() {
    // x0 -> x2 <= 10 holds for every x2 in 0..10.
    let rows = rows_of(
        "int_lin_le_imp",
        &[
            array(vec![int(1)]),
            array(vec![var(2)]),
            scalar(int(10)),
            scalar(var(0)),
        ],
        &test_bounds,
    )
    .expect("has a linear form");
    assert!(rows.iter().all(|row| row.is_entailed(&test_bounds)));
}

#[test]
fn a_merged_channel_holds_trivially() {
    let rows = rows_of("bool2int", &[scalar(var(0)), scalar(var(0))], &test_bounds);
    assert_eq!(rows, Some(vec![]));
}

#[test]
fn a_non_linear_constraint_has_no_linear_form() {
    let rows = rows_of(
        "int_times",
        &[scalar(var(2)), scalar(var(3)), scalar(var(2))],
        &test_bounds,
    );
    assert_eq!(rows, None);
}
