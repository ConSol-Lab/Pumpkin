use super::trackers::DisequalityTracker;
use super::trackers::DomainTracker;
use super::trackers::DomainTrackerInformation;
use super::trackers::EqualityTracker;
use super::trackers::LowerBoundTracker;
use super::trackers::UpperBoundTracker;
use crate::basic_types::PredicateId;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::variables::DomainId;

/// A structure for managing the trackers of the polarity for a specific [`DomainId`].
#[derive(Debug, Clone)]
pub(crate) struct DomainFaithfulnessForDomain {
    /// A tracker for the lower-bound [`Predicate`]s which are tracked.
    lower_bound: LowerBoundTracker,
    /// A tracker for the upper-bound [`Predicate`]s which are tracked.
    upper_bound: UpperBoundTracker,
    /// A tracker for the disequality [`Predicate`]s which are tracked.
    disequality: DisequalityTracker,
    /// A tracker for the equality [`Predicate`]s which are tracked.
    equality: EqualityTracker,
}

impl DomainFaithfulnessForDomain {
    pub(crate) fn new(trailed_values: &mut TrailedValues) -> Self {
        Self {
            lower_bound: LowerBoundTracker::new(trailed_values),
            upper_bound: UpperBoundTracker::new(trailed_values),
            disequality: DisequalityTracker::new(trailed_values),
            equality: EqualityTracker::new(trailed_values),
        }
    }
}

impl DomainFaithfulnessForDomain {
    /// This method will extend the scope of [`DomainFaithfulnessForDomain`] by adding the provided
    /// [`Predicate`] to the scope of the correct tracker.
    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        id: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => self
                .lower_bound
                .track(lower_bound, id, trailed_values, assignments),
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => self
                .upper_bound
                .track(upper_bound, id, trailed_values, assignments),
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => self
                .disequality
                .track(not_equal_constant, id, trailed_values, assignments),
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => self
                .equality
                .track(equality_constant, id, trailed_values, assignments),
        }
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [`Predicate`]).
    ///
    /// This method will pass it along to the correct tracker; if no [`Predicate`] are being
    /// watched by the corresponding tracker then no updates will take place.
    pub(crate) fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut TrailedValues,
        falsified_predicates: &mut Vec<PredicateId>,
        satisfied_predicates: &mut Vec<PredicateId>,
        predicate_id: Option<PredicateId>,
    ) {
        if !self.lower_bound.is_empty() {
            self.lower_bound.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                None,
            );
        }

        if !self.upper_bound.is_empty() {
            self.upper_bound.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                None,
            );
        }

        if !self.disequality.is_empty() {
            self.disequality.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                predicate_id,
            );
        }

        if !self.equality.is_empty() {
            self.equality.has_been_updated(
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
