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

use pumpkin_solver::core::containers::HashSet;

use super::candidate_permutations::Permutation;
use super::canonical_form::CanonicalConstraint;
use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::canonical_form::Variable;

pub(super) fn is_automorphism(
    graph: &Graph,
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
    if permutation
        .map
        .iter()
        .enumerate()
        .any(|(var, &image)| variables[var] != variables[image])
    {
        return false;
    }

    let store: HashSet<&CanonicalConstraint> = graph.constraints.iter().collect();
    graph
        .constraints
        .iter()
        .all(|constraint| store.contains(&constraint.map(&permutation.map)))
}
