use crate::checkers::BoxedConsistencyChecker;
use crate::checkers::Scope;
use crate::containers::KeyedBitSet;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::propagation::Domains;
use crate::variables::DomainId;

/// Holds the consistency checkers in the solver.
///
/// Also responsible for enqueueing the checkers and dispatching them when instructed via
/// [`ConsistencyCheckerStore::run_enqueued`].
#[derive(Clone, Debug, Default)]
pub struct ConsistencyCheckerStore {
    /// The checkers in the store.
    store: KeyedVec<CheckerId, (Scope, BoxedConsistencyChecker)>,
    /// Map from [`DomainId`] to the relevant checkers via their ID.
    watch_list: KeyedVec<DomainId, Vec<CheckerId>>,
    /// The checkers to run the next time.
    queue: Vec<CheckerId>,
    /// Marks which checkers are enqueued to prevent duplicate checkers in
    /// [`ConsistencyCheckerStore::queue`].
    enqueued: KeyedBitSet<CheckerId>,
}

impl ConsistencyCheckerStore {
    /// Add a new `checker` to the store with the given `scope`.
    pub fn register(&mut self, scope: Scope, checker: BoxedConsistencyChecker) {
        let checker_slot = self.store.new_slot();

        for (_, domain) in scope.domains() {
            self.watch_list.accomodate(domain, vec![]);
            self.watch_list[domain].push(checker_slot.key());
        }

        let _ = checker_slot.populate((scope, checker));
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

    /// Run the enqueued consistency checkers.
    pub fn run_enqueued(&mut self, mut domains: Domains<'_>) -> bool {
        for checker_id in self.queue.drain(..) {
            assert!(self.enqueued.remove(checker_id));

            let (scope, checker) = &mut self.store[checker_id];

            if !checker.check_consistency(scope, domains.reborrow()) {
                return false;
            }
        }

        true
    }

    /// Clear the queue of consistency checkers.
    pub fn clear_queue(&mut self) {
        self.queue.clear();
        self.enqueued.clear();
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
