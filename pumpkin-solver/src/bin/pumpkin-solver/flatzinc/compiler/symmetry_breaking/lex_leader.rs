//! Posting the lex-leader constraint for a verified symmetry.
//!
//! For a symmetry `sigma` with support `S = [s_1, ..., s_k]` in increasing variable order, the
//! constraint `lex_lesseq([s_1, ..., s_k], [sigma(s_1), ..., sigma(s_k)])` excludes every
//! solution that is not the lexicographically smallest of its orbit under `sigma`. Every
//! symmetry uses the same global variable order, so the constraints for several symmetries are
//! jointly sound (Crawford, Ginsberg, Luks and Roy, 1996).
//!
//! For example, if `x`, `y` and `z` are interchangeable, the exchanges of `x, y` and of `y, z`
//! are symmetries. Their supports are `[x, y]` and `[y, z]`, and the constraints
//! `lex_lesseq([x, y], [y, x])` and `lex_lesseq([y, z], [z, y])` are equivalent to `x <= y` and
//! `y <= z`. Together they keep exactly the sorted solution of each orbit, such as `(1, 2, 3)` out
//! of the six permutations of `1, 2, 3`.
//!
//! The constraint is decomposed: `eq_i <-> (x_i = y_i)`, prefix literals `p_i <-> eq_0 /\ ...
//! /\ eq_{i-1}`, and `p_i -> x_i <= y_i`. This uses only reified equalities, half-reified
//! inequalities and clauses, all of which already exist in the solver.

use pumpkin_solver::Solver;
use pumpkin_solver::core::constraints::Constraint;
use pumpkin_solver::core::constraints::NegatableConstraint;
use pumpkin_solver::core::variables::IntegerVariable;
use pumpkin_solver::core::variables::Literal;

use super::candidate_permutations::Permutation;
use super::canonical_form::ModelIndex;
use super::canonical_form::VarKind;

/// Post `lex_lesseq(support, sigma(support))`.
pub(super) fn post(solver: &mut Solver, index: &ModelIndex, permutation: &Permutation) {
    let support = permutation.support();
    let mut prefix: Option<Literal> = None;

    for (position, &var) in support.iter().enumerate() {
        let image = permutation.map[var];
        let is_last = position + 1 == support.len();
        let x = index.domain_ids[var];
        let y = index.domain_ids[image];

        prefix = match index.variables[var].kind {
            VarKind::Bool => post_pair(solver, Literal::new(x), Literal::new(y), prefix, is_last),
            VarKind::Int => post_pair(solver, x, y, prefix, is_last),
        };
    }
}

/// Post the constraints for one position of the lex chain and return the prefix literal for the
/// next position: `prefix -> x <= y` and, unless this is the last position, `next <-> prefix /\
/// (x = y)`. A `None` prefix stands for `true`.
fn post_pair<Var: IntegerVariable + Clone + 'static>(
    solver: &mut Solver,
    x: Var,
    y: Var,
    prefix: Option<Literal>,
    is_last: bool,
) -> Option<Literal> {
    let tag = solver.new_constraint_tag();
    let less_or_equal = pumpkin_constraints::binary_less_than_or_equals(x.clone(), y.clone(), tag);
    match prefix {
        None => less_or_equal.post(solver),
        Some(prefix) => less_or_equal.implied_by(solver, prefix),
    }

    if is_last {
        return None;
    }

    let equal = solver.new_literal();
    let tag = solver.new_constraint_tag();
    pumpkin_constraints::binary_equals(x, y, tag).reify(solver, equal);

    let Some(prefix) = prefix else {
        return Some(equal);
    };

    let next = solver.new_literal();
    let tag = solver.new_constraint_tag();
    solver.add_clause(
        [next.get_false_predicate(), prefix.get_true_predicate()],
        tag,
    );
    solver.add_clause(
        [next.get_false_predicate(), equal.get_true_predicate()],
        tag,
    );
    solver.add_clause(
        [
            prefix.get_false_predicate(),
            equal.get_false_predicate(),
            next.get_true_predicate(),
        ],
        tag,
    );
    Some(next)
}
