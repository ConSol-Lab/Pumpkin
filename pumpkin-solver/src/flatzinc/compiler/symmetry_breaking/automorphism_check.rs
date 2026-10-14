//! Verification that a proposed permutation is a symmetry of the model.
//!
//! A permutation `sigma` is a symmetry when it is a bijection on the variables, maps every
//! variable to one with the same kind and domain, fixes the objective, and maps every
//! constraint to a constraint that is present in the model. The last condition is checked
//! syntactically on canonical forms, so it never involves propagation or search.
//!
//! For example, in the model `x + y <= 4`, `x != z`, `y != z`, the exchange of `x` and `y` maps
//! `x + y <= 4` to `y + x <= 4`, which has the same canonical form, and maps `x != z` and
//! `y != z` onto each other, so it is accepted. If the model also contained `x < z`, its image
//! `y < z` would not be in the model, and the exchange would be rejected.

use super::candidate_permutations::Permutation;
use super::canonical_form::CanonicalConstraint;
use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::canonical_form::Variable;
use crate::core::containers::HashSet;

/// The constraints of the model, for membership tests; built once per pass.
pub(super) fn constraint_set(graph: &Graph) -> HashSet<&CanonicalConstraint> {
    graph.constraints.iter().collect()
}

pub(super) fn is_automorphism(
    graph: &Graph,
    store: &HashSet<&CanonicalConstraint>,
    variables: &[Variable],
    objective: Option<VarIndex>,
    permutation: &Permutation,
) -> bool {
    if permutation.map.len() != variables.len() || !permutation.is_bijection() {
        return false;
    }
    if objective.is_some_and(|objective| permutation.map[objective] != objective) {
        return false;
    }
    let support = permutation.support();
    if support
        .iter()
        .any(|&var| variables[var] != variables[permutation.map[var]])
    {
        return false;
    }

    // A constraint without a moved variable maps to itself, so only the constraints around the
    // support need to be checked.
    let mut affected: Vec<usize> = support
        .iter()
        .flat_map(|&var| graph.incident[var].iter().map(|&(index, _)| index))
        .collect();
    affected.sort_unstable();
    affected.dedup();
    affected
        .into_iter()
        .all(|index| store.contains(&graph.constraints[index].map(&permutation.map)))
}
