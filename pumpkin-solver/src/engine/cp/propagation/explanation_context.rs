use std::sync::LazyLock;

use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyValueHeap;
use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::Assignments;
use crate::predicates::Predicate;

/// The context that is available when lazily explaining propagations.
///
/// See [`pumpkin_solver::engine::propagation::Propagator`] for more information.
pub(crate) struct ExplanationContext<'a> {
    assignments: &'a Assignments,
    current_nogood: CurrentNogood<'a>,
}

impl<'a> From<&'a Assignments> for ExplanationContext<'a> {
    fn from(value: &'a Assignments) -> Self {
        ExplanationContext {
            assignments: value,
            current_nogood: CurrentNogood::empty(),
        }
    }
}

impl<'a> ExplanationContext<'a> {
    pub(crate) fn new(assignments: &'a Assignments, current_nogood: CurrentNogood<'a>) -> Self {
        ExplanationContext {
            assignments,
            current_nogood,
        }
    }

    /// Get the underlying assignments.
    #[deprecated = "using the assignments directly is not ideal, and we should develop this context API further instead"]
    pub(crate) fn assignments(&self) -> &'a Assignments {
        self.assignments
    }

    /// Get the current working nogood.
    ///
    /// The working nogood does not necessarily contain the predicate that is being explained.
    /// However, the explanation will be used to either resolve with the working nogood or minimize
    /// it some other way.
    #[allow(unused, reason = "it will be part of the public API at some point")]
    pub(crate) fn working_nogood(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.current_nogood.iter()
    }
}

impl HasAssignments for ExplanationContext<'_> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }
}

static EMPTY_HEAP: KeyValueHeap<PredicateId, u32> = KeyValueHeap::new();

static EMPTY_PREDICATE_IDS: LazyLock<PredicateIdGenerator> =
    LazyLock::new(PredicateIdGenerator::default);

static EMPTY_PREDICATES: [Predicate; 0] = [];

pub(crate) struct CurrentNogood<'a> {
    heap: &'a KeyValueHeap<PredicateId, u32>,
    visited: &'a [Predicate],
    ids: &'a PredicateIdGenerator,
}

impl<'a> CurrentNogood<'a> {
    pub(crate) fn new(
        heap: &'a KeyValueHeap<PredicateId, u32>,
        visited: &'a [Predicate],
        ids: &'a PredicateIdGenerator,
    ) -> Self {
        Self { heap, visited, ids }
    }

    pub(crate) fn empty() -> CurrentNogood<'a> {
        // The variable here is necessary for lifetime coersion.
        let reference: &[Predicate] = &EMPTY_PREDICATES;
        Self::from(reference)
    }

    fn iter<'this, 'ids>(&'this self) -> impl Iterator<Item = Predicate> + 'this
    where
        'ids: 'this,
    {
        self.heap
            .keys()
            .map(|id| self.ids.get_predicate(id).unwrap())
            .chain(self.visited.iter().copied())
    }
}

impl<'a> From<&'a [Predicate]> for CurrentNogood<'a> {
    fn from(value: &'a [Predicate]) -> Self {
        CurrentNogood {
            heap: &EMPTY_HEAP,
            visited: value,
            ids: &EMPTY_PREDICATE_IDS,
        }
    }
}
