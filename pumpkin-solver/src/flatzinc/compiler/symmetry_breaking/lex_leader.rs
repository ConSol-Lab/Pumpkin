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
//! The constraint is posted as one `lex_less_or_equal` constraint, whose bounds propagator is in
//! `pumpkin-propagators`. Boolean variables take part through their 0/1 domains, so `false`
//! comes before `true`.

use super::candidate_permutations::Permutation;
use super::canonical_form::ModelIndex;
use crate::Solver;
use crate::core::constraints::Constraint;
use crate::core::variables::DomainId;

/// Post `lex_lesseq(support, sigma(support))`.
pub(super) fn post(solver: &mut Solver, index: &ModelIndex, permutation: &Permutation) {
    let support = permutation.support();
    let x: Vec<DomainId> = support.iter().map(|&var| index.domain_ids[var]).collect();
    let y: Vec<DomainId> = support
        .iter()
        .map(|&var| index.domain_ids[permutation.map[var]])
        .collect();
    let tag = solver.new_constraint_tag();
    pumpkin_constraints::lex_less_or_equal(x, y, tag).post(solver);
}
