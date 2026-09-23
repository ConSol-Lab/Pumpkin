use pumpkin_checking::BoxedRetentionChecker;

use crate::checkers::Scope;
use crate::containers::KeyedBitSet;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::PropagatorId;
use crate::variables::DomainId;

/// Holds the retention checkers in the solver.
///
/// Also responsible for enqueueing the checkers and dispatching them
/// when instructed via [`RetentionCheckerStore::run`].
#[derive(Clone, Debug, Default)]
pub struct RetentionCheckerStore {
    /// The checkers in the store.
    store: KeyedVec<CheckerId, Entry>,
    /// Map from [`DomainId`] to the relevant checkers via their ID.
    watch_list: KeyedVec<DomainId, Vec<CheckerId>>,
    /// The checkers to run the next time.
    queue: Vec<CheckerId>,
    /// Marks which checkers are enqueued to prevent duplicate checkers in
    /// [`RetentionCheckerStore::queue`].
    enqueued: KeyedBitSet<CheckerId>,
    /// The propagators whose checkers are discarded when they are registered.
    excluded: KeyedVec<PropagatorId, bool>,
}

/// A checker together with what it is attached to.
#[derive(Clone, Debug)]
struct Entry {
    scope: Scope,
    checker: BoxedRetentionChecker<Predicate>,
    /// The propagator whose rule the checker describes.
    propagator: PropagatorId,
}

/// Which checkers are consulted when a fixpoint is reached.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RetentionCoverage {
    /// Only the checkers watching a domain that changed since the previous fixpoint.
    ///
    /// The others were consulted when their domains last changed.
    Notified,
    /// Every checker in the store, whether or not its domains changed.
    All,
}

/// The checker which reported that its propagator has something left to propagate.
#[derive(Clone, Debug)]
pub struct RetentionFailure {
    /// The propagator whose rule the checker describes.
    pub propagator: PropagatorId,
    /// The variables the checker watches.
    pub variables: Vec<DomainId>,
}

impl RetentionCheckerStore {
    /// Discard the checkers of this propagator instead of storing them.
    ///
    /// Used to restrict checking to a chosen set of propagators.
    /// It has to be called before the checkers of that propagator are registered.
    pub fn exclude(&mut self, propagator: PropagatorId) {
        self.excluded.accomodate(propagator, false);
        self.excluded[propagator] = true;
    }

    /// Add a new `checker` to the store with the given `scope`.
    ///
    /// The checker is dropped when its propagator has been excluded.
    pub fn register(
        &mut self,
        scope: Scope,
        checker: BoxedRetentionChecker<Predicate>,
        propagator: PropagatorId,
    ) {
        if self.is_excluded(propagator) {
            return;
        }

        let checker_slot = self.store.new_slot();

        for (_, domain) in scope.domains() {
            self.watch_list.accomodate(domain, vec![]);
            self.watch_list[domain].push(checker_slot.key());
        }

        let _ = checker_slot.populate(Entry {
            scope,
            checker,
            propagator,
        });
    }

    /// Called when the domain is modified.
    ///
    /// Causes the checkers for this domain to be enqueued.
    pub fn on_domain_event(&mut self, domain_id: DomainId) {
        let Some(list) = self.watch_list.get(domain_id) else {
            return;
        };

        for &checker_id in list {
            if !self.enqueued.insert(checker_id) {
                continue;
            }

            self.queue.push(checker_id);
        }
    }

    /// Run the checkers selected by `coverage`,
    /// stopping at the first one that reports that its propagator is not finished.
    pub fn run(
        &mut self,
        coverage: RetentionCoverage,
        domains: Domains<'_>,
    ) -> Result<(), RetentionFailure> {
        match coverage {
            RetentionCoverage::Notified => {
                while let Some(checker_id) = self.queue.pop() {
                    assert!(self.enqueued.remove(checker_id));
                    self.check(checker_id, &domains)?;
                }
            }
            RetentionCoverage::All => {
                self.clear_queue();

                for index in 0..self.store.len() {
                    self.check(CheckerId::create_from_index(index), &domains)?;
                }
            }
        }

        Ok(())
    }

    /// Clear the queue of retention checkers.
    pub fn clear_queue(&mut self) {
        self.queue.clear();
        self.enqueued.clear();
    }

    fn check(&self, checker_id: CheckerId, domains: &Domains<'_>) -> Result<(), RetentionFailure> {
        let entry = &self.store[checker_id];

        if entry
            .checker
            .check_retention(&entry.scope.snapshot(domains))
        {
            return Ok(());
        }

        Err(RetentionFailure {
            propagator: entry.propagator,
            variables: entry.scope.domains().map(|(_, domain)| domain).collect(),
        })
    }

    fn is_excluded(&self, propagator: PropagatorId) -> bool {
        self.excluded.get(propagator).copied().unwrap_or(false)
    }
}

/// An identifier for added checkers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct CheckerId(u32);

impl StorageKey for CheckerId {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        CheckerId(index as u32)
    }
}
