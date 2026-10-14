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

#[cfg(test)]
use std::time::Duration;
use std::time::Instant;

use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::colour_refinement::Refinement;
use super::colour_refinement::individualise;
use crate::core::containers::HashMap;
use crate::core::containers::HashSet;

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
    pub(super) deadline: Instant,
    max_seed_classes: usize,
}

impl Budget {
    #[cfg(test)]
    pub(super) fn new(time_limit: Duration, max_seed_classes: usize) -> Self {
        Self::until(Instant::now() + time_limit, max_seed_classes)
    }

    pub(super) fn until(deadline: Instant, max_seed_classes: usize) -> Self {
        Budget {
            deadline,
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
    converged: &Refinement,
    classes: &[Vec<VarIndex>],
    budget: &Budget,
) -> Vec<Permutation> {
    let mut found: Vec<Permutation> = Vec::new();
    let mut seen: HashSet<Vec<VarIndex>> = HashSet::default();
    // Individualisation changes this copy and restores it afterwards.
    let mut work = converged.clone();

    // Stop once this many seed classes in a row have produced no candidate; on models without
    // symmetry this ends the search long before the deadline.
    const MAX_UNPRODUCTIVE_CLASSES: usize = 32;
    let mut unproductive = 0;
    for class in classes.iter().take(budget.max_seed_classes) {
        if budget.expired() || unproductive >= MAX_UNPRODUCTIVE_CLASSES {
            break;
        }
        let found_before = found.len();
        let mut previous = individualise(graph, &mut work, class[0], budget.deadline);
        for &var in &class[1..] {
            if budget.expired() {
                break;
            }
            let next = individualise(graph, &mut work, var, budget.deadline);
            if let Some(permutation) = permutation_between(&work, &previous, &next)
                && seen.insert(permutation.map.clone())
            {
                found.push(permutation);
            }
            previous = next;
        }
        if found.len() == found_before {
            unproductive += 1;
        } else {
            unproductive = 0;
        }
    }
    found
}

/// The permutation mapping the colouring after the changes `first` onto the colouring after the
/// changes `second`, both relative to `converged`, if it is unambiguous: every colour class of
/// the first must equal the class of the same colour in the second except for at most one
/// variable each, and those variables are exchanged. Only variables whose colour changed can
/// differ, so only those are compared.
fn permutation_between(
    converged: &Refinement,
    first: &[(VarIndex, u64)],
    second: &[(VarIndex, u64)],
) -> Option<Permutation> {
    let colour_in = |changes: &[(VarIndex, u64)], var: VarIndex| {
        changes
            .binary_search_by_key(&var, |&(changed, _)| changed)
            .map_or_else(|_| converged.colour(var), |position| changes[position].1)
    };
    let mut differing: Vec<VarIndex> = first.iter().chain(second).map(|&(var, _)| var).collect();
    differing.sort_unstable();
    differing.dedup();

    // For each colour, the variables that have it in one colouring but not in the other.
    let mut only_first: HashMap<u64, Vec<VarIndex>> = HashMap::default();
    let mut only_second: HashMap<u64, Vec<VarIndex>> = HashMap::default();
    for var in differing {
        let (a, b) = (colour_in(first, var), colour_in(second, var));
        if a != b {
            only_first.entry(a).or_default().push(var);
            only_second.entry(b).or_default().push(var);
        }
    }

    let mut permutation = Permutation::identity(converged.num_variables());
    for (colour, from) in &only_first {
        match (from.as_slice(), only_second.get(colour).map(Vec::as_slice)) {
            ([u], Some([w])) => permutation.map[*u] = *w,
            _ => return None,
        }
    }
    if only_second
        .keys()
        .any(|colour| !only_first.contains_key(colour))
    {
        return None;
    }

    if permutation.is_identity() || !permutation.is_bijection() {
        return None;
    }
    Some(permutation)
}
