use super::predicate_trackers::DisequalityTracker;
use super::predicate_trackers::DomainTracker;
use super::predicate_trackers::DomainTrackerInformation;
use super::predicate_trackers::EqualityTracker;
use super::predicate_trackers::LowerBoundTracker;
use super::predicate_trackers::UpperBoundTracker;
use crate::basic_types::PredicateId;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::variables::DomainId;

/// A structure for managing the trackers of the polarity for a specific [`DomainId`].
#[derive(Debug, Clone)]
pub(crate) struct PredicateTrackerForDomain {
    /// A tracker for the lower-bound [`Predicate`]s which are tracked.
    lower_bound: LowerBoundTracker,
    /// A tracker for the upper-bound [`Predicate`]s which are tracked.
    upper_bound: UpperBoundTracker,
    /// A tracker for the disequality [`Predicate`]s which are tracked.
    disequality: DisequalityTracker,
    /// A tracker for the equality [`Predicate`]s which are tracked.
    equality: EqualityTracker,
}

impl PredicateTrackerForDomain {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            lower_bound: LowerBoundTracker::new(trailed_values),
            upper_bound: UpperBoundTracker::new(trailed_values),
            disequality: DisequalityTracker::new(trailed_values),
            equality: EqualityTracker::new(trailed_values),
        }
    }
}

impl PredicateTrackerForDomain {
    /// This method will extend the scope of [`PredicateTrackerForDomain`] by adding the provided
    /// [`Predicate`] to the scope of the correct tracker.
    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        id: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        let value = predicate.get_right_hand_side();
        if predicate.is_lower_bound_predicate() {
            self.lower_bound
                .track(value, id, trailed_values, assignments)
        } else if predicate.is_upper_bound_predicate() {
            self.upper_bound
                .track(value, id, trailed_values, assignments)
        } else if predicate.is_not_equal_predicate() {
            self.disequality
                .track(value, id, trailed_values, assignments)
        } else if predicate.is_equality_predicate() {
            self.equality.track(value, id, trailed_values, assignments)
        } else {
            panic!()
        }
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [`Predicate`]).
    ///
    /// This method will pass it along to the correct tracker; if no [`Predicate`] are being
    /// watched by the corresponding tracker then no updates will take place.
    pub(crate) fn on_update(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    ) {
        if !self.lower_bound.is_empty() {
            self.lower_bound.on_update(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                None,
            );
        }

        if !self.upper_bound.is_empty() {
            self.upper_bound.on_update(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                None,
            );
        }

        if !self.disequality.is_empty() {
            self.disequality.on_update(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                predicate_id,
            );
        }

        if !self.equality.is_empty() {
            self.equality.on_update(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                None,
            );
        }
    }

    /// Initialises all of the trackers.
    pub(crate) fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
    ) {
        self.lower_bound
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
        self.upper_bound
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
        self.disequality
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
        self.equality
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
    }
}
