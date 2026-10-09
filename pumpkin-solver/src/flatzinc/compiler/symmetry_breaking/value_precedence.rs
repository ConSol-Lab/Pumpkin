//! Posting value-precedence constraints for interchangeable values.
//!
//! For values `v_1 < ... < v_k` that are pairwise interchangeable for the variables
//! `x_1, ..., x_n`, the constraint `value_precede_chain([v_1, ..., v_k], [x_1, ..., x_n])` keeps
//! exactly the solutions in which the first occurrence of `v_i` comes before the first occurrence
//! of `v_{i+1}` (Law and Lee, 2004). Every orbit under permutations of the values contains such a
//! solution: rename the values in the order in which they first occur.
//!
//! Each consecutive pair `(s, t)` is decomposed with literals `seen_j <-> seen_{j-1} \/ x_j = s`
//! ("`s` occurs among `x_1, ..., x_j`") and clauses `x_j = t -> seen_{j-1}`, where `seen_0` is
//! false.
//!
//! For example, for the values `1, 2, 3` and the variables `x, y, z`, the chain requires that `x`
//! is 1, that `y` is not 3 unless 2 occurs before it, and so on. The solution `x = 1, y = 2,
//! z = 3` is kept, and its images such as `x = 2, y = 1, z = 3` are removed.

use pumpkin_solver::Solver;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::variables::Literal;

use super::canonical_form::ModelIndex;
use super::value_symmetry::InterchangeableValues;

pub(super) fn post(solver: &mut Solver, index: &ModelIndex, found: &InterchangeableValues) {
    for pair in found.values.windows(2) {
        post_pair(solver, index, &found.variables, pair[0], pair[1]);
    }
}

/// `value_precede(s, t, variables)`: `t` does not occur before the first occurrence of `s`.
fn post_pair(solver: &mut Solver, index: &ModelIndex, variables: &[usize], s: i64, t: i64) {
    let (Ok(s), Ok(t)) = (i32::try_from(s), i32::try_from(t)) else {
        return;
    };
    let tag = solver.new_constraint_tag();
    // `None` stands for the literal false.
    let mut seen: Option<Literal> = None;
    for (position, &var) in variables.iter().enumerate() {
        let x = index.domain_ids[var];

        // x_j = t -> seen_{j-1}
        match seen {
            None => solver.add_clause([predicate![x != t]], tag),
            Some(seen) => solver.add_clause([predicate![x != t], seen.get_true_predicate()], tag),
        }

        if position + 1 == variables.len() {
            break;
        }

        // seen_j <-> seen_{j-1} \/ x_j = s
        let next = solver.new_literal();
        solver.add_clause(
            [next.get_false_predicate(), predicate![x == s]]
                .into_iter()
                .chain(seen.map(|seen| seen.get_true_predicate()))
                .collect::<Vec<_>>(),
            tag,
        );
        solver.add_clause([predicate![x != s], next.get_true_predicate()], tag);
        if let Some(seen) = seen {
            solver.add_clause([seen.get_false_predicate(), next.get_true_predicate()], tag);
        }
        seen = Some(next);
    }
}
