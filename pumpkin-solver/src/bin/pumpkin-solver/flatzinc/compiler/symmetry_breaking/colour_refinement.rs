//! Colour refinement (1-dimensional Weisfeiler-Leman) over the variable/constraint graph.
//!
//! Variables that end in the same colour class cannot be told apart by their constraint
//! structure. That is a necessary condition for being interchangeable, not a sufficient one, so
//! every permutation proposed from these classes is verified separately.
//!
//! For example, take `x`, `y` and `z` in `0..3` with `x + y <= 4`, `x != z` and `y != z`. The
//! three variables start with the same colour, because they have the same domain. After one
//! round, `x` and `y` each occur in the linear constraint and in one `!=` constraint, while `z`
//! occurs in two `!=` constraints, so the classes are `{x, y}` and `{z}`. The next round splits
//! no class, and refinement stops.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::canonical_form::Variable;

/// A colour per variable, normalised to `0..num_classes`.
pub(super) type Colouring = Vec<u32>;

pub(super) fn initial_colouring(variables: &[Variable], objective: Option<VarIndex>) -> Colouring {
    let hashes = variables
        .iter()
        .enumerate()
        .map(|(var, variable)| {
            let mut hasher = DefaultHasher::new();
            variable.hash_into(&mut hasher);
            (objective == Some(var)).hash(&mut hasher);
            hasher.finish()
        })
        .collect();
    normalise(hashes).0
}

/// Refine until the partition is stable. Refinement only ever splits classes, so the partition
/// is stable exactly when the number of classes stops growing.
pub(super) fn refine(graph: &Graph, mut colouring: Colouring) -> Colouring {
    let mut num_classes = count_classes(&colouring);
    loop {
        let constraint_colours: Vec<u64> = graph
            .constraints
            .iter()
            .zip(&graph.tag_hashes)
            .map(|(constraint, &tag_hash)| {
                let mut slots: Vec<_> = constraint
                    .slots
                    .iter()
                    .map(|&(key, var)| (key, colouring[var]))
                    .collect();
                slots.sort();
                let mut hasher = DefaultHasher::new();
                tag_hash.hash(&mut hasher);
                slots.hash(&mut hasher);
                hasher.finish()
            })
            .collect();

        let variable_hashes = graph
            .incident
            .iter()
            .enumerate()
            .map(|(var, occurrences)| {
                let mut neighbours: Vec<_> = occurrences
                    .iter()
                    .map(|&(constraint, key)| (constraint_colours[constraint], key))
                    .collect();
                neighbours.sort();
                let mut hasher = DefaultHasher::new();
                colouring[var].hash(&mut hasher);
                neighbours.hash(&mut hasher);
                hasher.finish()
            })
            .collect();

        let (refined, refined_classes) = normalise(variable_hashes);
        if refined_classes == num_classes {
            return refined;
        }
        colouring = refined;
        num_classes = refined_classes;
    }
}

/// The colouring in which `var` alone receives a fresh colour, refined to stability.
pub(super) fn individualise(graph: &Graph, converged: &Colouring, var: VarIndex) -> Colouring {
    let mut colouring = converged.clone();
    colouring[var] = count_classes(converged) as u32;
    refine(graph, colouring)
}

/// Non-singleton colour classes, each sorted, in order of their colour.
pub(super) fn colour_classes(colouring: &Colouring) -> Vec<Vec<VarIndex>> {
    let mut classes = vec![Vec::new(); count_classes(colouring)];
    for (var, &colour) in colouring.iter().enumerate() {
        classes[colour as usize].push(var);
    }
    classes.retain(|class| class.len() > 1);
    classes
}

fn count_classes(colouring: &Colouring) -> usize {
    colouring.iter().max().map_or(0, |&max| max as usize + 1)
}

/// Map hashes to dense colours `0..k`, ordered by hash value so that the result depends only on
/// the multiset of hashes and not on variable order.
fn normalise(hashes: Vec<u64>) -> (Colouring, usize) {
    let mut unique = hashes.clone();
    unique.sort_unstable();
    unique.dedup();
    let colouring = hashes
        .iter()
        .map(|hash| unique.binary_search(hash).expect("hash is present") as u32)
        .collect();
    (colouring, unique.len())
}
