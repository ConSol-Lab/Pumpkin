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
//!
//! Refinement is incremental. A round recomputes only the constraints that contain a variable
//! whose colour changed, and re-examines only the classes of the variables in those constraints.
//! When a class splits, its largest part keeps the old colour, so the variables of that part do
//! not trigger further work; this is the "smaller half" rule of partition refinement. Colours are
//! hashes computed from the structure only, so two refinements of isomorphic situations assign
//! the same colours, which [`candidate_permutations`](super::candidate_permutations) relies on
//! when it compares the colourings obtained by individualising two different variables.

use std::hash::Hash;
use std::hash::Hasher;
use std::time::Instant;

use pumpkin_solver::core::containers::HashMap;

use super::canonical_form::Graph;
use super::canonical_form::VarIndex;
use super::canonical_form::Variable;

/// A colour per variable, normalised to `0..num_classes`.
pub(super) type Colouring = Vec<u32>;

/// A stable (or, after the deadline, partially refined) colouring: a hash colour per variable
/// and per constraint, and the members of each variable colour.
#[derive(Clone)]
pub(super) struct Refinement {
    variable_colours: Vec<u64>,
    constraint_colours: Vec<u64>,
    members: HashMap<u64, Vec<VarIndex>>,
    /// Number of rounds performed so far, including those before an individualisation. A part
    /// split off a class gets a colour computed from the class colour, its signature and the
    /// round, so parts split off the same class in different rounds never share a colour.
    round: u64,
    /// While individualising, the previous state of everything changed, so that the refinement
    /// can be restored to the converged colouring afterwards instead of being copied.
    journal: Option<Vec<Change>>,
}

#[derive(Clone)]
enum Change {
    Variable(VarIndex, u64),
    Constraint(usize, u64),
    Members(u64, Option<Vec<VarIndex>>),
}

impl Refinement {
    /// The colouring with colours normalised to `0..num_classes`.
    pub(super) fn colouring(&self) -> Colouring {
        normalise(&self.variable_colours)
    }

    /// The colour of a variable.
    pub(super) fn colour(&self, var: VarIndex) -> u64 {
        self.variable_colours[var]
    }

    pub(super) fn num_variables(&self) -> usize {
        self.variable_colours.len()
    }

    fn set_variable_colour(&mut self, var: VarIndex, colour: u64) {
        if let Some(journal) = &mut self.journal {
            journal.push(Change::Variable(var, self.variable_colours[var]));
        }
        self.variable_colours[var] = colour;
    }

    fn set_constraint_colour(&mut self, index: usize, colour: u64) {
        if let Some(journal) = &mut self.journal {
            journal.push(Change::Constraint(index, self.constraint_colours[index]));
        }
        self.constraint_colours[index] = colour;
    }

    fn remember_members(&mut self, colour: u64) {
        if let Some(journal) = &mut self.journal {
            journal.push(Change::Members(colour, self.members.get(&colour).cloned()));
        }
    }

    fn remove_members(&mut self, colour: u64) -> Option<Vec<VarIndex>> {
        self.remember_members(colour);
        self.members.remove(&colour)
    }

    fn insert_members(&mut self, colour: u64, members: Vec<VarIndex>) {
        self.remember_members(colour);
        let _ = self.members.insert(colour, members);
    }

    fn extend_members(&mut self, colour: u64, members: Vec<VarIndex>) {
        self.remember_members(colour);
        self.members.entry(colour).or_default().extend(members);
    }

    /// Undo every change recorded in `journal`, most recent first.
    fn restore(&mut self, journal: Vec<Change>) {
        for change in journal.into_iter().rev() {
            match change {
                Change::Variable(var, colour) => self.variable_colours[var] = colour,
                Change::Constraint(index, colour) => self.constraint_colours[index] = colour,
                Change::Members(colour, Some(members)) => {
                    let _ = self.members.insert(colour, members);
                }
                Change::Members(colour, None) => {
                    let _ = self.members.remove(&colour);
                }
            }
        }
    }
}

pub(super) fn initial_colouring(variables: &[Variable], objective: Option<VarIndex>) -> Vec<u64> {
    variables
        .iter()
        .enumerate()
        .map(|(var, variable)| {
            let mut hasher = FastHasher::default();
            variable.hash_into(&mut hasher);
            (objective == Some(var)).hash(&mut hasher);
            hasher.finish()
        })
        .collect()
}

/// Refine `initial` until the partition is stable or the deadline passes.
pub(super) fn refine(graph: &Graph, initial: Vec<u64>, deadline: Instant) -> Refinement {
    let mut members: HashMap<u64, Vec<VarIndex>> = HashMap::default();
    for (var, &colour) in initial.iter().enumerate() {
        members.entry(colour).or_default().push(var);
    }
    let mut refinement = Refinement {
        constraint_colours: (0..graph.constraints.len())
            .map(|index| constraint_colour(graph, index, &initial))
            .collect(),
        variable_colours: initial,
        members,
        round: 0,
        journal: None,
    };
    let all: Vec<VarIndex> = (0..graph.num_variables()).collect();
    let changed = split_classes(graph, &mut refinement, &all);
    propagate(graph, &mut refinement, changed, deadline);
    refinement
}

/// Refine the colouring in which `var` alone receives a fresh colour, and return the variables
/// whose colour differs from `converged` with their new colours, sorted by variable. `converged`
/// is restored before returning.
pub(super) fn individualise(
    graph: &Graph,
    converged: &mut Refinement,
    var: VarIndex,
    deadline: Instant,
) -> Vec<(VarIndex, u64)> {
    let round = converged.round;
    converged.journal = Some(Vec::new());

    let old = converged.variable_colours[var];
    let fresh = combine(old, u64::MAX);
    if let Some(mut class) = converged.remove_members(old) {
        class.retain(|&member| member != var);
        converged.insert_members(old, class);
    }
    converged.extend_members(fresh, vec![var]);
    converged.set_variable_colour(var, fresh);
    propagate(graph, converged, vec![var], deadline);

    let journal = converged.journal.take().expect("journal was started");
    let mut changed: Vec<VarIndex> = journal
        .iter()
        .filter_map(|change| match change {
            Change::Variable(var, _) => Some(*var),
            _ => None,
        })
        .collect();
    changed.sort_unstable();
    changed.dedup();
    let changed = changed
        .into_iter()
        .map(|var| (var, converged.variable_colours[var]))
        .collect();
    converged.restore(journal);
    converged.round = round;
    changed
}

/// The full colouring after individualising `var`; for tests.
#[cfg(test)]
pub(super) fn individualised_colouring(
    graph: &Graph,
    converged: &Refinement,
    var: VarIndex,
    deadline: Instant,
) -> Colouring {
    let mut refinement = converged.clone();
    let changed = individualise(graph, &mut refinement, var, deadline);
    let mut colours = refinement.variable_colours.clone();
    for (var, colour) in changed {
        colours[var] = colour;
    }
    normalise(&colours)
}

/// Recompute the constraints around `changed` and split the classes they affect, until no
/// colour changes.
fn propagate(
    graph: &Graph,
    refinement: &mut Refinement,
    mut changed: Vec<VarIndex>,
    deadline: Instant,
) {
    while !changed.is_empty() && Instant::now() < deadline {
        refinement.round += 1;
        let mut constraints: Vec<usize> = changed
            .iter()
            .flat_map(|&var| graph.incident[var].iter().map(|&(index, _)| index))
            .collect();
        constraints.sort_unstable();
        constraints.dedup();

        let mut affected: Vec<VarIndex> = Vec::new();
        for index in constraints {
            let colour = constraint_colour(graph, index, &refinement.variable_colours);
            if colour != refinement.constraint_colours[index] {
                refinement.set_constraint_colour(index, colour);
                affected.extend(graph.constraints[index].slots.iter().map(|&(_, var)| var));
            }
        }
        changed = split_classes(graph, refinement, &affected);
    }
}

/// Split every class that contains a variable of `affected` by the colours of the constraints
/// around its members. Returns the variables whose colour changed.
fn split_classes(
    graph: &Graph,
    refinement: &mut Refinement,
    affected: &[VarIndex],
) -> Vec<VarIndex> {
    let mut classes: Vec<u64> = affected
        .iter()
        .map(|&var| refinement.variable_colours[var])
        .collect();
    classes.sort_unstable();
    classes.dedup();

    let mut changed = Vec::new();
    for colour in classes {
        let Some(class) = refinement.remove_members(colour) else {
            continue;
        };
        if class.len() == 1 {
            refinement.insert_members(colour, class);
            continue;
        }
        let mut by_signature: HashMap<u64, Vec<VarIndex>> = HashMap::default();
        for &var in &class {
            by_signature
                .entry(signature(graph, var, &refinement.constraint_colours))
                .or_default()
                .push(var);
        }
        if by_signature.len() == 1 {
            refinement.insert_members(colour, class);
            continue;
        }
        let mut parts: Vec<(u64, Vec<VarIndex>)> = by_signature.into_iter().collect();
        // The largest part keeps the colour; ties are broken by signature, which is canonical.
        parts.sort_unstable_by(|(a, part_a), (b, part_b)| {
            part_b.len().cmp(&part_a.len()).then(a.cmp(b))
        });
        let mut parts = parts.into_iter();
        let (_, kept) = parts.next().expect("at least two parts");
        refinement.insert_members(colour, kept);
        for (signature, part) in parts {
            let fresh = combine(combine(colour, signature), refinement.round);
            for &var in &part {
                refinement.set_variable_colour(var, fresh);
            }
            changed.extend_from_slice(&part);
            refinement.extend_members(fresh, part);
        }
    }
    changed
}

fn combine(a: u64, b: u64) -> u64 {
    let mut hasher = FastHasher::default();
    a.hash(&mut hasher);
    b.hash(&mut hasher);
    hasher.finish()
}

fn constraint_colour(graph: &Graph, index: usize, variable_colours: &[u64]) -> u64 {
    let mut slots: Vec<_> = graph.constraints[index]
        .slots
        .iter()
        .map(|&(key, var)| (key, variable_colours[var]))
        .collect();
    slots.sort_unstable();
    let mut hasher = FastHasher::default();
    graph.tag_hashes[index].hash(&mut hasher);
    slots.hash(&mut hasher);
    hasher.finish()
}

fn signature(graph: &Graph, var: VarIndex, constraint_colours: &[u64]) -> u64 {
    let mut neighbours: Vec<_> = graph.incident[var]
        .iter()
        .map(|&(index, key)| (constraint_colours[index], key))
        .collect();
    neighbours.sort_unstable();
    let mut hasher = FastHasher::default();
    neighbours.hash(&mut hasher);
    hasher.finish()
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
fn normalise(hashes: &[u64]) -> Colouring {
    let mut unique = hashes.to_vec();
    unique.sort_unstable();
    unique.dedup();
    hashes
        .iter()
        .map(|hash| unique.binary_search(hash).expect("hash is present") as u32)
        .collect()
}

/// A fast non-cryptographic hasher for colours, combining one machine word at a time (the
/// multiply-rotate scheme of FxHash). A collision can only merge two colour classes, which loses
/// candidates but never soundness, since every candidate is verified.
#[derive(Default)]
pub(super) struct FastHasher(u64);

impl FastHasher {
    fn add(&mut self, word: u64) {
        self.0 = (self.0.rotate_left(5) ^ word).wrapping_mul(0x51_7c_c1_b7_27_22_0a_95);
    }
}

impl Hasher for FastHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for chunk in bytes.chunks(8) {
            let mut word = [0_u8; 8];
            word[..chunk.len()].copy_from_slice(chunk);
            self.add(u64::from_le_bytes(word));
        }
    }

    fn write_u8(&mut self, value: u8) {
        self.add(u64::from(value));
    }

    fn write_u32(&mut self, value: u32) {
        self.add(u64::from(value));
    }

    fn write_u64(&mut self, value: u64) {
        self.add(value);
    }

    fn write_i64(&mut self, value: i64) {
        self.add(value as u64);
    }

    fn write_usize(&mut self, value: usize) {
        self.add(value as u64);
    }
}
