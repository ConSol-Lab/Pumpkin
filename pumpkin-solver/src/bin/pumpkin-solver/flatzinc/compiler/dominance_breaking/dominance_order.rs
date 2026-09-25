//! The dominance relation on candidate variables and the constraints to post.
//!
//! `better` dominates `worse` when exchanging their values, in any solution where `worse` has the
//! larger value, gives a solution that is at least as good. For candidates this holds when:
//!
//! - both have the same kind and domain;
//! - in every `<=` constraint, the coefficient of `better` is at most that of `worse`, and in every
//!   `=` or `!=` constraint the two coefficients are equal (a missing term counts as 0);
//! - the objective weight of `better` is at least that of `worse`.
//!
//! Each condition is a preorder, so dominance is one. Ties (mutual dominance) are broken by
//! variable index, which gives a strict partial order `better < worse`. Posting `better >= worse`
//! for every pair of that order is sound: from any optimal solution, repeatedly exchanging the
//! values of a violated pair keeps it feasible and optimal, and terminates because each exchange
//! moves the larger value to the variable that is earlier in a linear extension of the order.
//! Only the transitive reduction is posted; the remaining pairs are implied by it.
//!
//! For example, in the knapsack `maximise 5a + 5b + 4c` subject to `2a + 3b + 3c <= 5`, `a`
//! dominates `b` (coefficients `2 <= 3`, objective weights `5 >= 5`), `b` dominates `c` (`3 <= 3`,
//! `5 >= 4`), and `a` dominates `c`. Only `a >= b` and `b >= c` are posted, because `a >= c`
//! follows from them. If `c` had the same coefficient and objective weight as `b`, each would
//! dominate the other. The tie is broken by index and only `b >= c` is posted: posting `c >= b`
//! as well would force `b = c` and remove the solutions that select exactly one of them.

use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::super::symmetry_breaking::canonical_form::Variable;
use super::linear_model::Candidate;
use super::linear_model::LinearModel;
use super::linear_model::Relation;

/// Whether exchanging the values of `better` and `worse`, in a solution where `worse` has the
/// larger value, preserves every constraint and does not make the objective worse. Domains are
/// compared by the caller.
pub(super) fn dominates(
    better: &Candidate,
    worse: &Candidate,
    relations: &[Option<Relation>],
) -> bool {
    if better.objective_weight < worse.objective_weight {
        return false;
    }
    better
        .terms
        .keys()
        .chain(worse.terms.keys())
        .all(|&constraint| {
            let a = better.terms.get(&constraint).copied().unwrap_or(0);
            let b = worse.terms.get(&constraint).copied().unwrap_or(0);
            match relations[constraint] {
                Some(Relation::LessOrEqual) => a <= b,
                Some(Relation::Equal) => a == b,
                None => false,
            }
        })
}

/// The pairs `(better, worse)` of the transitive reduction of the dominance order, and the number
/// of candidates left out because their domain group exceeded `max_group`.
pub(super) fn pairs_to_post(
    model: &LinearModel,
    variables: &[Variable],
    max_group: usize,
) -> (Vec<(VarIndex, VarIndex)>, usize) {
    // Dominance requires equal domains, so only candidates within one group are compared.
    let mut groups: Vec<(&Variable, Vec<&Candidate>)> = Vec::new();
    for candidate in &model.candidates {
        let variable = &variables[candidate.var];
        match groups.iter_mut().find(|(key, _)| *key == variable) {
            Some((_, members)) => members.push(candidate),
            None => groups.push((variable, vec![candidate])),
        }
    }

    let mut pairs = Vec::new();
    let mut skipped = 0;
    for (_, members) in &groups {
        if members.len() > max_group {
            skipped += members.len();
            continue;
        }
        pairs.extend(reduced_order(members, &model.relations));
    }
    pairs.sort_unstable();
    (pairs, skipped)
}

/// The transitive reduction of the strict dominance order on `members`.
fn reduced_order(
    members: &[&Candidate],
    relations: &[Option<Relation>],
) -> Vec<(VarIndex, VarIndex)> {
    let n = members.len();
    let dominance: Vec<Vec<bool>> = (0..n)
        .map(|i| {
            (0..n)
                .map(|j| i != j && dominates(members[i], members[j], relations))
                .collect()
        })
        .collect();

    let words = n.div_ceil(64);
    let mut successors = vec![vec![0_u64; words]; n];
    let mut predecessors = vec![vec![0_u64; words]; n];
    for i in 0..n {
        for j in 0..n {
            let precedes = dominance[i][j] && (!dominance[j][i] || members[i].var < members[j].var);
            if precedes {
                successors[i][j / 64] |= 1 << (j % 64);
                predecessors[j][i / 64] |= 1 << (i % 64);
            }
        }
    }

    let mut reduced = Vec::new();
    for i in 0..n {
        for j in 0..n {
            if successors[i][j / 64] & (1 << (j % 64)) == 0 {
                continue;
            }
            let has_intermediate = successors[i]
                .iter()
                .zip(&predecessors[j])
                .any(|(s, p)| s & p != 0);
            if !has_intermediate {
                reduced.push((members[i].var, members[j].var));
            }
        }
    }
    reduced
}
