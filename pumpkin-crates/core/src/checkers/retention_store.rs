use pumpkin_checking::BoxedRetentionChecker;
use pumpkin_checking::RetentionCheck;

use crate::checkers::Scope;
use crate::containers::KeyedBitSet;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::propagation::Domains;
use crate::propagation::PropagatorId;
use crate::variables::DomainId;

/// Holds the retention checkers of the solver, and keeps the checkers whose domains changed pending
/// until their propagator reports a fixpoint ([`RetentionCheckerStore::run_pending_of`]) or
/// propagation reaches one ([`RetentionCheckerStore::run`]).
#[derive(Clone, Debug, Default)]
pub struct RetentionCheckerStore {
    /// The checkers in the store; `None` for a checker that was removed.
    store: KeyedVec<RetentionCheckerId, Option<Entry>>,
    /// The checkers that watch each domain.
    watch_list: KeyedVec<DomainId, Vec<RetentionCheckerId>>,
    /// For each propagator, its checkers whose domains changed since they last ran.
    pending: KeyedVec<PropagatorId, Vec<RetentionCheckerId>>,
    /// The checkers in [`RetentionCheckerStore::pending`], to keep each there at most once.
    enqueued: KeyedBitSet<RetentionCheckerId>,
}

/// A checker together with what it is attached to.
#[derive(Clone, Debug)]
struct Entry {
    scope: Scope,
    checker: BoxedRetentionChecker<Predicate>,
    /// The propagator that implements the rule of the checker.
    propagator: PropagatorId,
    /// The inference code of the rule and the constraint that the checker belongs to.
    inference_code: InferenceCode,
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
    /// The propagator that implements the rule of the checker.
    pub propagator: PropagatorId,
    /// The inference code of the rule and the constraint that the checker belongs to.
    pub inference_code: InferenceCode,
    /// The variables the checker watches.
    pub variables: Vec<DomainId>,
}

impl RetentionCheckerStore {
    /// Add a new `checker` to the store with the given `scope`, for the rule and constraint of
    /// `inference_code`, for a constraint that is never removed.
    pub fn register(
        &mut self,
        scope: Scope,
        checker: BoxedRetentionChecker<Predicate>,
        propagator: PropagatorId,
        inference_code: InferenceCode,
    ) {
        let _ = self.register_removable(scope, checker, propagator, inference_code);
    }

    /// Add a new `checker` as with [`RetentionCheckerStore::register`], for a constraint that can
    /// be removed later.
    ///
    /// Returns the identifier through which [`RetentionCheckerStore::remove`] removes the checker.
    pub fn register_removable(
        &mut self,
        scope: Scope,
        checker: BoxedRetentionChecker<Predicate>,
        propagator: PropagatorId,
        inference_code: InferenceCode,
    ) -> RetentionCheckerId {
        let checker_slot = self.store.new_slot();
        let checker_id = checker_slot.key();

        for domain in scope.domains() {
            self.watch_list.accomodate(domain, vec![]);
            self.watch_list[domain].push(checker_id);
        }

        let _ = checker_slot.populate(Some(Entry {
            scope,
            checker,
            propagator,
            inference_code,
        }));

        // The checker is pending from the start, so that it checks the state after the first call
        // of its propagator even when no domain of its scope changes.
        let _ = self.enqueued.insert(checker_id);
        self.pending.accomodate(propagator, vec![]);
        self.pending[propagator].push(checker_id);

        checker_id
    }

    /// Remove the checker with the given identifier, for a constraint that no longer exists.
    ///
    /// It is dropped from the watch lists of its domains the next time they change.
    pub fn remove(&mut self, checker: RetentionCheckerId) {
        self.store[checker] = None;
    }

    /// Make the checkers that watch `domain_id` pending, if they are not already.
    pub fn on_domain_event(&mut self, domain_id: DomainId) {
        let Self {
            store,
            watch_list,
            pending,
            enqueued,
        } = self;

        let Some(list) = watch_list.get_mut(domain_id) else {
            return;
        };

        list.retain(|&checker_id| store[checker_id].is_some());

        for &checker_id in list.iter() {
            let Some(entry) = &store[checker_id] else {
                continue;
            };
            if !enqueued.insert(checker_id) {
                continue;
            }

            pending.accomodate(entry.propagator, vec![]);
            pending[entry.propagator].push(checker_id);
        }
    }

    /// Run the pending checkers of `propagator`, which reports that it is at a fixpoint, stopping
    /// at the first one that reports that the propagator is not finished.
    pub fn run_pending_of(
        &mut self,
        propagator: PropagatorId,
        domains: Domains<'_>,
    ) -> Result<(), RetentionFailure> {
        let Some(pending) = self.pending.get_mut(propagator) else {
            return Ok(());
        };

        for checker_id in std::mem::take(pending) {
            assert!(self.enqueued.remove(checker_id));
            self.check(checker_id, &domains)?;
        }

        Ok(())
    }

    /// Run the checkers selected by `coverage` when propagation reaches a fixpoint, stopping at the
    /// first one that reports that its propagator is not finished.
    pub fn run(
        &mut self,
        coverage: RetentionCoverage,
        domains: Domains<'_>,
    ) -> Result<(), RetentionFailure> {
        match coverage {
            RetentionCoverage::Notified => {
                for index in 0..self.pending.len() {
                    for checker_id in
                        std::mem::take(&mut self.pending[PropagatorId::create_from_index(index)])
                    {
                        assert!(self.enqueued.remove(checker_id));
                        self.check(checker_id, &domains)?;
                    }
                }
            }
            RetentionCoverage::All => {
                self.clear_queue();

                for index in 0..self.store.len() {
                    self.check(RetentionCheckerId::create_from_index(index), &domains)?;
                }
            }
        }

        Ok(())
    }

    pub fn clear_queue(&mut self) {
        self.pending.iter_mut().for_each(Vec::clear);
        self.enqueued.clear();
    }

    fn check(
        &self,
        checker_id: RetentionCheckerId,
        domains: &Domains<'_>,
    ) -> Result<(), RetentionFailure> {
        // A removed checker can still be pending, since it only leaves the pending list when it
        // runs.
        let Some(entry) = &self.store[checker_id] else {
            return Ok(());
        };

        match entry.checker.check_retention(domains.assignments) {
            RetentionCheck::NothingToPropagate => Ok(()),
            RetentionCheck::PropagationMissed => Err(RetentionFailure {
                propagator: entry.propagator,
                inference_code: entry.inference_code,
                variables: entry.scope.domains().collect(),
            }),
        }
    }
}

/// Identifies a checker in the [`RetentionCheckerStore`], for instance so that it can be removed.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RetentionCheckerId(u32);

impl StorageKey for RetentionCheckerId {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        RetentionCheckerId(index as u32)
    }
}
