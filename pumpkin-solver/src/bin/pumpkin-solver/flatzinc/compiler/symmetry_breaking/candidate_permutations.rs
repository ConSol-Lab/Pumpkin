//! Proposing variable permutations from the refined colouring.
//!
//! For two variables `a` and `b` in the same colour class, refine once with `a` individualised
//! and once with `b` individualised. If exchanging `a` and `b` is an automorphism, the two
//! colourings are images of each other: every colour class of the first is mapped onto the class
//! of the same colour in the second. Variables common to both classes are fixed; where the
//! classes differ in exactly one variable each, those two are exchanged. Anything else is
//! ambiguous and the pair is skipped. The result is only a candidate and is verified afterwards.
//!
//! For example, in the model `x + y <= 4`, `x != z`, `y != z`, the class `{x, y}` gives two
//! refinements. Individualising `x` gives the classes `{x}`, `{y}`, `{z}`, and individualising
//! `y` gives `{y}`, `{x}`, `{z}`, listed in the same colour order. Matching them class by class
//! proposes `x -> y`, `y -> x`, `z -> z`.

use std::time::Duration;
use std::time::Instant;

use pumpkin_solver::core::containers::HashSet;

use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::colour_refinement::Colouring;
use super::colour_refinement::individualise;

/// A permutation of the variables, as `map[var] = image`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct Permutation {
    pub(super) map: Vec<VarIndex>,
}

impl Permutation {
    pub(super) fn identity(num_variables: usize) -> Self {
        Permutation {
            map: (0..num_variables).collect(),
        }
    }

    pub(super) fn is_identity(&self) -> bool {
        self.map
            .iter()
            .enumerate()
            .all(|(var, &image)| var == image)
    }

    /// The moved variables, in increasing index order.
    pub(super) fn support(&self) -> Vec<VarIndex> {
        self.map
            .iter()
            .enumerate()
            .filter(|&(var, &image)| var != image)
            .map(|(var, _)| var)
            .collect()
    }

    pub(super) fn is_bijection(&self) -> bool {
        let mut seen = vec![false; self.map.len()];
        self.map
            .iter()
            .all(|&image| image < seen.len() && !std::mem::replace(&mut seen[image], true))
    }
}

/// Limits on the search for candidates: a deadline and a cap on how many colour classes are
/// used as seeds.
pub(super) struct Budget {
    deadline: Instant,
    max_seed_classes: usize,
}

impl Budget {
    pub(super) fn new(time_limit: Duration, max_seed_classes: usize) -> Self {
        Budget {
            deadline: Instant::now() + time_limit,
            max_seed_classes,
        }
    }

    fn expired(&self) -> bool {
        Instant::now() >= self.deadline
    }
}

/// Candidate permutations, each exchanging two consecutive members of a colour class (and
/// whatever else that forces). Pairing consecutive members, rather than the first member with
/// every other one, makes the posted lex-leader constraints a chain `c_1 <=lex c_2 <=lex ...`,
/// which breaks a group of fully interchangeable columns completely instead of only fixing its
/// minimum. Duplicates are removed.
pub(super) fn candidate_swaps(
    graph: &Graph,
    converged: &Colouring,
    classes: &[Vec<VarIndex>],
    budget: &Budget,
) -> Vec<Permutation> {
    let mut found: Vec<Permutation> = Vec::new();
    let mut seen: HashSet<Vec<VarIndex>> = HashSet::default();

    for class in classes.iter().take(budget.max_seed_classes) {
        if budget.expired() {
            break;
        }
        let mut previous = individualise(graph, converged, class[0]);
        for &var in &class[1..] {
            if budget.expired() {
                break;
            }
            let next = individualise(graph, converged, var);
            if let Some(permutation) = permutation_between(&previous, &next)
                && seen.insert(permutation.map.clone())
            {
                found.push(permutation);
            }
            previous = next;
        }
    }
    found
}

/// The permutation mapping colouring `first` onto colouring `second`, if it is unambiguous.
fn permutation_between(first: &Colouring, second: &Colouring) -> Option<Permutation> {
    let classes_first = all_classes(first);
    let classes_second = all_classes(second);
    if classes_first.len() != classes_second.len() {
        return None;
    }

    let mut permutation = Permutation::identity(first.len());
    for (class_first, class_second) in classes_first.iter().zip(&classes_second) {
        let only_first: Vec<VarIndex> = class_first
            .iter()
            .copied()
            .filter(|var| class_second.binary_search(var).is_err())
            .collect();
        let only_second: Vec<VarIndex> = class_second
            .iter()
            .copied()
            .filter(|var| class_first.binary_search(var).is_err())
            .collect();
        match (only_first.as_slice(), only_second.as_slice()) {
            ([], []) => {}
            ([u], [w]) => permutation.map[*u] = *w,
            _ => return None,
        }
    }

    if permutation.is_identity() || !permutation.is_bijection() {
        return None;
    }
    Some(permutation)
}

/// Every colour class including singletons, each sorted, in order of colour.
fn all_classes(colouring: &Colouring) -> Vec<Vec<VarIndex>> {
    let num_classes = colouring.iter().max().map_or(0, |&max| max as usize + 1);
    let mut classes = vec![Vec::new(); num_classes];
    for (var, &colour) in colouring.iter().enumerate() {
        classes[colour as usize].push(var);
    }
    classes
}
