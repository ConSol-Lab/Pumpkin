//! Dual fixing: fix a variable to a bound when moving it towards that bound can never violate a
//! constraint or make the objective worse.
//!
//! A candidate can be lowered without violating any row when it occurs in no `=` or `!=` row and
//! has no negative coefficient in a `<=` row. If in addition its objective weight is at most 0,
//! lowering it to its lower bound in any solution gives a solution that is at least as good, so
//! it is fixed to its lower bound. Symmetrically for the upper bound. The witness is the
//! assignment of the bound to the variable. Several fixings are jointly sound: each condition
//! only concerns the variable's own coefficients, so the fixings can be applied to an optimal
//! solution one after another.
//!
//! For example, in
//!
//! ```text
//! minimise 3x + y   subject to   x + y >= 2,   z - y <= 4,   x, y, z in 0..5
//! ```
//!
//! the constraints are the rows `-x - y <= -2` and `z - y <= 4`. `z` has only a positive
//! coefficient and does not occur in the objective, so it is fixed to 0. `x` could be lowered
//! only if it had no negative coefficient, which it has, so it is not fixed.

use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::linear_model::Candidate;
use super::linear_model::LinearModel;
use super::linear_model::Relation;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum FixedBound {
    Lower,
    Upper,
}

/// The bound a candidate can be fixed to, if any.
pub(super) fn fixed_bound(candidate: &Candidate, relations: &[Relation]) -> Option<FixedBound> {
    let mut has_negative = false;
    let mut has_positive = false;
    for (&row, &coefficient) in &candidate.terms {
        if relations[row] == Relation::Equal {
            return None;
        }
        has_negative |= coefficient < 0;
        has_positive |= coefficient > 0;
    }
    if !has_negative && candidate.objective_weight <= 0 {
        Some(FixedBound::Lower)
    } else if !has_positive && candidate.objective_weight >= 0 {
        Some(FixedBound::Upper)
    } else {
        None
    }
}

/// Every candidate that can be fixed, with its bound.
pub(super) fn fixings(model: &LinearModel) -> Vec<(VarIndex, FixedBound)> {
    model
        .candidates
        .iter()
        .filter_map(|candidate| {
            fixed_bound(candidate, &model.relations).map(|bound| (candidate.var, bound))
        })
        .collect()
}
