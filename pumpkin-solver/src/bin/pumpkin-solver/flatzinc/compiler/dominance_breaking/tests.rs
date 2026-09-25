//! Unit tests on the dominance order over hand-built linear models. Extraction from FlatZinc and
//! posting are covered by the integration tests in `tests/mzn_dominance_test.rs`.

use std::collections::BTreeMap;

use super::dominance_order::pairs_to_post;
use super::linear_model::Candidate;
use super::linear_model::LinearModel;
use super::linear_model::Relation;
use crate::flatzinc::compiler::context::Domain;
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
    relations: Vec<Option<Relation>>,
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
        vec![Some(Relation::LessOrEqual)],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(0, 1)]);
}

#[test]
fn larger_objective_weight_dominates() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 2), candidate(1, &[(0, 1)], 5)],
        vec![Some(Relation::LessOrEqual)],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(1, 0)]);
}

#[test]
fn conflicting_weight_and_objective_is_not_dominance() {
    // x0 is lighter but less profitable than x1.
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 2), candidate(1, &[(0, 3)], 5)],
        vec![Some(Relation::LessOrEqual)],
        vec![binary(); 2],
    );
    assert!(found.is_empty());
}

#[test]
fn equality_requires_equal_coefficients() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[(0, 2)], 3)],
        vec![Some(Relation::Equal)],
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
        vec![Some(Relation::LessOrEqual)],
        vec![binary(); 2],
    );
    assert_eq!(found, vec![(1, 0)]);
}

#[test]
fn identical_variables_are_ordered_in_one_direction_only() {
    // Posting both directions would force x0 = x1 and lose solutions that select one of them.
    let found = pairs(
        vec![candidate(0, &[(0, 5)], 3), candidate(1, &[(0, 5)], 3)],
        vec![Some(Relation::LessOrEqual)],
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
        vec![Some(Relation::LessOrEqual)],
        vec![binary(); 3],
    );
    assert_eq!(found, vec![(0, 1), (1, 2)]);
}

#[test]
fn different_domains_are_never_compared() {
    let found = pairs(
        vec![candidate(0, &[(0, 1)], 3), candidate(1, &[(0, 2)], 3)],
        vec![Some(Relation::LessOrEqual)],
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
