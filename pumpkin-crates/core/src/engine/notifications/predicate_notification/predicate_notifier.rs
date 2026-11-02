use bit_set::BitSet;

use super::PredicateIdAssignments;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::predicate_notification::predicate_tracker::PredicateTracker;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// An orchestrating struct which serves as the main contact point for the solver with
/// [`PredicateNotifier`].
///
/// Contains method for creating [`PredicateId`]s for [`Predicate`]s, retrieving the list of
/// updated [`Predicate`]s, and allowing propagators to indicate that the polarity of a
/// [`Predicate`] should be tracked (i.e. adding a [`Predicate`] to the scope of
/// [`PredicateNotifier`]).
///
/// It also contains the [`PredicateIdAssignments`] which serves as a (lazy) structure for
/// retrieving the polarity of [`Predicate`]s (represented by [`PredicateId`]s).
#[derive(Default, Debug, Clone)]
pub(crate) struct PredicateNotifier {
    /// Maps a [`Predicate`] to a [`PredicateId`]
    pub(crate) predicate_to_id: PredicateIdGenerator,
    /// Tracks the current status for [`PredicateId`]s.
    pub(crate) predicate_id_assignments: PredicateIdAssignments,
    /// Contains the [`PredicateTracker`] for each [`DomainId`]
    domain_id_to_predicate_tracker: KeyedVec<DomainId, PredicateTracker>,
    /// The [`PredicateId`]s (by index) which have been added to the scope of their
    /// [`PredicateTracker`].
    ///
    /// Predicates are re-tracked frequently (e.g., when a watch list of the nogood propagator
    /// becomes non-empty again), and determining whether a [`Predicate`] is tracked by the
    /// [`PredicateTracker`] itself requires traversing its values.
    ///
    /// It is also used by the [`PredicateTracker`] to determine which predicates to update when a
    /// disequality [`Predicate`] has been posted.
    is_tracked: BitSet,
    /// The number of [`PredicateId`]s in `is_tracked`.
    num_tracked: usize,
    /// The [`PredicateId`]s (by index) which are tracked but are no longer watched by any
    /// propagator; these can be removed from the scope of their [`PredicateTracker`] at the root
    /// level (see [`PredicateNotifier::untrack_unwatched_predicates`]).
    ///
    /// A [`PredicateId`] is removed from this set once it is watched again.
    untrack_candidates: BitSet,
    /// The number of [`PredicateId`]s in `untrack_candidates`.
    num_untrack_candidates: usize,
    /// The number of [`PredicateId`]s in `untrack_candidates` which could not be removed from the
    /// scope of their [`PredicateTracker`] during the last cleanup (see
    /// [`PredicateNotifier::untrack_unwatched_predicates`]).
    ///
    /// These are not taken into account when determining whether a cleanup should take place, to
    /// avoid repeatedly attempting to remove them.
    num_blocked_untrack_candidates: usize,
    pub(crate) statistics: PredicateNotifierStatistics,
}

create_statistics_struct!(PredicateNotifierStatistics {
    /// The number of times that predicates which are not watched anymore have been removed from
    /// the scope of their [`PredicateTracker`].
    num_cleanups: u64,
    /// The number of values (i.e., nodes) which have been removed from the [`PredicateTracker`]s
    /// due to cleanups.
    num_untracked_values: u64,
});

/// A cleanup of the [`PredicateTracker`]s is only attempted once at least `1 /
/// UNTRACK_CANDIDATES_RATIO_DENOMINATOR` of the tracked predicates have become unwatched since
/// the previous cleanup.
const UNTRACK_CANDIDATES_RATIO_DENOMINATOR: usize = 10;

impl PredicateNotifier {
    pub(crate) fn debug_empty_clone(&self) -> Self {
        let predicate_id_assignments = self.predicate_id_assignments.debug_empty_clone();
        Self {
            predicate_to_id: self.predicate_to_id.clone(),
            predicate_id_assignments,
            ..Default::default()
        }
    }

    pub(crate) fn debug_create_from_assignments(&mut self, assignments: &Assignments) {
        self.predicate_id_assignments
            .debug_create_from_assignments(assignments, &self.predicate_to_id);
    }

    /// Returns the satisfied predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.predicate_id_assignments.drain_satisfied_predicates()
    }

    /// Returns the [`Predicate`] corresponding to a [`PredicateId`].
    ///
    /// This method will panic if there is no [`Predicate`] for the provided [`PredicateId`].
    pub(crate) fn get_predicate(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_to_id.get_predicate(predicate_id)
    }

    /// Method which is called when an update to a [`DomainId`] has taken place
    ///
    /// This method will pass it along to the correct [`PredicateNotifier::on_update`]
    /// corresponding to the [`DomainId`] for which the update took place.
    pub(crate) fn on_update(
        &mut self,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
        event: DomainEvent,
        domain: DomainId,
    ) {
        if self.domain_id_to_predicate_tracker.len() <= domain.index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        let predicate_type: PredicateType = event.into();

        let predicate_tracker = &mut self.domain_id_to_predicate_tracker[domain];
        if predicate_type.is_disequality() {
            if predicate_tracker.can_be_updated_by_disequality()
                && !predicate_tracker.is_fixed(trailed_values)
            {
                for removed_value in assignments.get_holes_at_current_checkpoint(domain) {
                    let predicate =
                        predicate_type.into_predicate(domain, assignments, Some(removed_value));

                    predicate_tracker.on_update(
                        predicate,
                        trailed_values,
                        &self.predicate_to_id,
                        &self.is_tracked,
                        &mut self.predicate_id_assignments,
                    );
                }
            }
        } else {
            // Otherwise we update the structures
            predicate_tracker.on_update(
                predicate_type.into_predicate(domain, assignments, None),
                trailed_values,
                &self.predicate_to_id,
                &self.is_tracked,
                &mut self.predicate_id_assignments,
            );
        }
    }

    /// This method will extend the scope of [`PredicateNotifier`] by adding the provided
    /// [`Predicate`] to its scope.
    pub(crate) fn track_predicate(
        &mut self,
        id: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        if self.untrack_candidates.remove(id.index()) {
            // The predicate is watched again, so it should not be removed from its tracker
            //
            // Note that we do not know whether it was blocked, so we only ensure that there are
            // not more blocked candidates than candidates
            self.num_untrack_candidates -= 1;
            self.num_blocked_untrack_candidates = self
                .num_blocked_untrack_candidates
                .min(self.num_untrack_candidates);
        }

        if !self.is_tracked.insert(id.index()) {
            // The predicate is already in the scope of its tracker
            return;
        }
        self.num_tracked += 1;

        let predicate = self.predicate_to_id.get_predicate(id);

        // First, we resize the number of DomainIds for which we store predicate trackers
        if self.domain_id_to_predicate_tracker.len() <= predicate.get_domain().index() {
            self.domain_id_to_predicate_tracker
                .resize(predicate.get_domain().index() + 1, PredicateTracker::new());
        }

        // Now we initialise the predicate tracker; this does not add it to the scope yet but it
        // initialises the structures
        self.domain_id_to_predicate_tracker[predicate.get_domain()].initialise(
            assignments.get_initial_lower_bound(predicate.get_domain()),
            assignments.get_initial_upper_bound(predicate.get_domain()),
            trailed_values,
        );

        // Now we add it to the scope of the tracker
        let added =
            self.domain_id_to_predicate_tracker[predicate.get_domain()].track(predicate, id);
        pumpkin_assert_simple!(added, "Expected {predicate:?} to not be tracked yet");
    }

    /// Indicates that the provided [`PredicateId`] is not watched by any propagator anymore, and
    /// can thus be removed from the scope of its [`PredicateTracker`].
    ///
    /// If enough such [`PredicateId`]s have accumulated, then this method attempts to remove
    /// them (see [`PredicateNotifier::untrack_unwatched_predicates`]).
    pub(crate) fn mark_as_untrack_candidate(
        &mut self,
        id: PredicateId,
        trailed_values: &TrailedValues,
    ) {
        pumpkin_assert_moderate!(self.is_tracked.contains(id.index()));
        if !self.untrack_candidates.insert(id.index()) {
            return;
        }
        self.num_untrack_candidates += 1;

        let num_new_candidates = self.num_untrack_candidates - self.num_blocked_untrack_candidates;
        if num_new_candidates * UNTRACK_CANDIDATES_RATIO_DENOMINATOR >= self.num_tracked {
            self.untrack_unwatched_predicates(trailed_values);
        }
    }

    /// Removes the [`Predicate`]s which were marked using
    /// [`PredicateNotifier::mark_as_untrack_candidate`] (and which have not been watched since)
    /// from the scope of their [`PredicateTracker`].
    ///
    /// The nodes of a [`PredicateTracker`] can only be moved if its assigned indices point to the
    /// sentinels (see [`PredicateTracker::are_assigned_indices_at_sentinels`]); the candidates of
    /// the other [`PredicateTracker`]s remain candidates and are considered in a later cleanup.
    fn untrack_unwatched_predicates(&mut self, trailed_values: &TrailedValues) {
        let mut updated_domains = Vec::new();
        for index in self.untrack_candidates.iter() {
            let domain = self
                .predicate_to_id
                .get_predicate(PredicateId::create_from_index(index))
                .get_domain();
            if self.domain_id_to_predicate_tracker[domain]
                .are_assigned_indices_at_sentinels(trailed_values)
            {
                let removed = self.is_tracked.remove(index);
                pumpkin_assert_simple!(removed, "Expected an untrack candidate to be tracked");
                updated_domains.push(domain);
            }
        }

        // The candidates which have been untracked are no longer candidates
        let num_untracked = updated_domains.len();
        self.untrack_candidates.intersect_with(&self.is_tracked);
        self.num_tracked -= num_untracked;
        self.num_untrack_candidates -= num_untracked;
        self.num_blocked_untrack_candidates = self.num_untrack_candidates;

        if num_untracked == 0 {
            return;
        }

        updated_domains.sort_unstable_by_key(|domain| domain.index());
        updated_domains.dedup();
        for domain in updated_domains {
            let num_removed = self.domain_id_to_predicate_tracker[domain]
                .remove_untracked_predicates(&self.is_tracked, trailed_values);
            self.statistics.num_untracked_values += num_removed as u64;
        }
        self.statistics.num_cleanups += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::containers::StorageKey;
    use crate::engine::Assignments;
    use crate::engine::TrailedValues;
    use crate::engine::notifications::PredicateNotifier;
    use crate::predicate;

    #[test]
    fn candidates_are_untracked_once_enough_have_accumulated() {
        let mut assignments = Assignments::default();
        let mut trailed_values = TrailedValues::default();
        let mut notifier = PredicateNotifier::default();

        let x = assignments.grow(0, 30);
        let ids = (1..=20)
            .map(|value| notifier.predicate_to_id.get_id(predicate!(x >= value)))
            .collect::<Vec<_>>();
        for &id in &ids {
            notifier.track_predicate(id, &mut trailed_values, &assignments);
        }

        // A candidate which is watched again is not a candidate anymore
        notifier.mark_as_untrack_candidate(ids[0], &trailed_values);
        notifier.track_predicate(ids[0], &mut trailed_values, &assignments);

        // A single candidate is not enough to trigger a cleanup for 20 tracked predicates...
        notifier.mark_as_untrack_candidate(ids[1], &trailed_values);
        assert_eq!(notifier.statistics.num_cleanups, 0);

        // ...but two candidates are
        notifier.mark_as_untrack_candidate(ids[2], &trailed_values);
        assert_eq!(notifier.statistics.num_cleanups, 1);
        assert_eq!(notifier.statistics.num_untracked_values, 2);

        assert!(notifier.is_tracked.contains(ids[0].index()));
        assert!(!notifier.is_tracked.contains(ids[1].index()));
        assert!(!notifier.is_tracked.contains(ids[2].index()));
        assert_eq!(notifier.num_tracked, 18);
        assert_eq!(notifier.num_untrack_candidates, 0);
    }

    #[test]
    fn blocked_candidates_are_untracked_once_their_tracker_is_at_the_sentinels() {
        let mut assignments = Assignments::default();
        let mut trailed_values = TrailedValues::default();
        let mut notifier = PredicateNotifier::default();

        let x = assignments.grow(0, 30);
        let ids = (1..=20)
            .map(|value| notifier.predicate_to_id.get_id(predicate!(x >= value)))
            .collect::<Vec<_>>();
        for &id in &ids {
            notifier.track_predicate(id, &mut trailed_values, &assignments);
        }

        // The lower-bound passes some of the tracked values, so the nodes cannot be moved
        trailed_values.new_checkpoint();
        notifier.domain_id_to_predicate_tracker[x].on_update(
            predicate!(x >= 5),
            &mut trailed_values,
            &notifier.predicate_to_id,
            &notifier.is_tracked,
            &mut notifier.predicate_id_assignments,
        );

        // The cleanup is attempted, but the candidates are blocked
        notifier.mark_as_untrack_candidate(ids[10], &trailed_values);
        notifier.mark_as_untrack_candidate(ids[11], &trailed_values);
        assert_eq!(notifier.statistics.num_cleanups, 0);
        assert!(notifier.is_tracked.contains(ids[10].index()));
        assert!(notifier.is_tracked.contains(ids[11].index()));
        assert_eq!(notifier.num_untrack_candidates, 2);
        assert_eq!(notifier.num_blocked_untrack_candidates, 2);

        // The blocked candidates do not count towards triggering another cleanup
        notifier.mark_as_untrack_candidate(ids[12], &trailed_values);
        assert_eq!(notifier.num_blocked_untrack_candidates, 2);
        assert!(notifier.is_tracked.contains(ids[12].index()));

        // After backtracking, the tracker is at the sentinels again, and all of the candidates are
        // removed by the next cleanup
        trailed_values.synchronise(0);
        notifier.mark_as_untrack_candidate(ids[13], &trailed_values);
        assert_eq!(notifier.statistics.num_cleanups, 1);
        assert_eq!(notifier.statistics.num_untracked_values, 4);
        assert_eq!(notifier.num_tracked, 16);
        assert_eq!(notifier.num_untrack_candidates, 0);
        assert_eq!(notifier.num_blocked_untrack_candidates, 0);
        for id in &ids[10..14] {
            assert!(!notifier.is_tracked.contains(id.index()));
        }
    }
}
