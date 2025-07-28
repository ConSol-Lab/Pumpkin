use super::predicate_trackers::DisequalityTracker;
use super::predicate_trackers::DomainTracker;
use super::predicate_trackers::DomainTrackerInformation;
use super::predicate_trackers::EqualityTracker;
use super::predicate_trackers::LowerBoundTracker;
use super::predicate_trackers::UpperBoundTracker;
use super::PredicateIdAssignments;
use crate::basic_types::PredicateId;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
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
    pub(crate) fn new() -> Self {
        Self {
            lower_bound: LowerBoundTracker::new(),
            upper_bound: UpperBoundTracker::new(),
            disequality: DisequalityTracker::new(),
            equality: EqualityTracker::new(),
        }
    }
}

impl PredicateTrackerForDomain {
    /// This method will extend the scope of [`PredicateTrackerForDomain`] by adding the provided
    /// [`Predicate`] to the scope of the correct tracker.
    ///
    /// Returns true if it was not already tracked and false otherwise.
    pub(crate) fn watch_predicate(&mut self, predicate: Predicate, id: PredicateId) -> bool {
        let value = predicate.get_right_hand_side();
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => self.lower_bound.track(value, id),
            PredicateType::UpperBound => self.upper_bound.track(value, id),
            PredicateType::NotEqual => self.disequality.track(value, id),
            PredicateType::Equal => self.equality.track(value, id),
        }
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [`Predicate`]).
    ///
    /// This method will pass it along to the correct tracker; if no [`Predicate`] are being
    /// watched by the corresponding tracker then no updates will take place.
    #[allow(clippy::too_many_arguments, reason = "Should be refactored")]
    pub(crate) fn on_update(
        &mut self,
        domain: DomainId,
        predicate_type: PredicateType,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        // We check three things:
        // 1. Is the lower-bound tracker tracking anything?
        // 2. Is the update a lower-bound or upper-bound update?
        // 3. Can updates still take place?
        if !self.lower_bound.is_empty()
            && (predicate_type.is_lower_bound() || predicate_type.is_upper_bound())
            && !self.lower_bound.is_fixed(trailed_values)
        {
            self.lower_bound.on_update(
                predicate_type.into_predicate(domain, assignments, None),
                trailed_values,
                predicate_id_assignments,
            );
        }

        // We check three things:
        // 1. Is the upper-bound tracker tracking anything?
        // 2. Is the update a lower-bound or upper-bound update?
        // 3. Can updates still take place?
        if !self.upper_bound.is_empty()
            && (predicate_type.is_lower_bound() || predicate_type.is_upper_bound())
            && !self.upper_bound.is_fixed(trailed_values)
        {
            self.upper_bound.on_update(
                predicate_type.into_predicate(domain, assignments, None),
                trailed_values,
                predicate_id_assignments,
            );
        }

        // The case for disequalities is more involved since we need to find the actual holes in
        // the domain which were created.
        //
        // Note that only the trackers of equalities and disequalities care about removal events
        if predicate_type.is_disequality() {
            // We first check whether anything is being tracked
            if !self.disequality.is_empty() || !self.equality.is_empty() {
                // After that we check whether one of them is unfixed, and we cache the result
                let disequality_is_fixed = self.disequality.is_fixed(trailed_values);
                let equality_is_fixed = self.equality.is_fixed(trailed_values);

                if disequality_is_fixed && equality_is_fixed {
                    return;
                }

                for removed_value in assignments.get_holes_on_current_decision_level(domain) {
                    let predicate =
                        predicate_type.into_predicate(domain, assignments, Some(removed_value));
                    if !self.disequality.is_empty() && !disequality_is_fixed {
                        self.disequality.on_update(
                            predicate,
                            trailed_values,
                            predicate_id_assignments,
                        );
                    }

                    if !self.equality.is_empty() && !equality_is_fixed {
                        self.equality.on_update(
                            predicate,
                            trailed_values,
                            predicate_id_assignments,
                        );
                    }
                }
            }
        } else {
            let predicate = predicate_type.into_predicate(domain, assignments, None);
            // We check two things:
            // 1. Is the disequality tracker tracking anything?
            // 2. Can updates still take place?
            if !self.disequality.is_empty() && !self.disequality.is_fixed(trailed_values) {
                self.disequality
                    .on_update(predicate, trailed_values, predicate_id_assignments);
            }

            // We check two things:
            // 1. Is the equality tracker tracking anything?
            // 2. Can updates still take place?
            if !self.equality.is_empty() && !self.equality.is_fixed(trailed_values) {
                self.equality
                    .on_update(predicate, trailed_values, predicate_id_assignments);
            }
        }
    }

    /// Initialises all of the trackers.
    pub(crate) fn initialise(
        &mut self,
        predicate: Predicate,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    ) {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                self.lower_bound.initialise(
                    predicate.get_domain(),
                    initial_lower_bound,
                    initial_upper_bound,
                    trailed_values,
                );
            }
            PredicateType::UpperBound => {
                self.upper_bound.initialise(
                    predicate.get_domain(),
                    initial_lower_bound,
                    initial_upper_bound,
                    trailed_values,
                );
            }
            PredicateType::NotEqual => {
                self.disequality.initialise(
                    predicate.get_domain(),
                    initial_lower_bound,
                    initial_upper_bound,
                    trailed_values,
                );
            }
            PredicateType::Equal => {
                self.equality.initialise(
                    predicate.get_domain(),
                    initial_lower_bound,
                    initial_upper_bound,
                    trailed_values,
                );
            }
        }
    }
}
