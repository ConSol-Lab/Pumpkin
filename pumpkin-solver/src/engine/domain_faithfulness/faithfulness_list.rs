use super::watchers::DomainWatcher;
use super::watchers::DomainWatcherInformation;
use super::watchers::EqualityWatcher;
use super::watchers::InequalityWatcher;
use super::watchers::LowerBoundWatcher;
use super::watchers::UpperBoundWatcher;
use crate::basic_types::PredicateId;
use crate::basic_types::Trail;
use crate::engine::Assignments;
use crate::engine::StateChange;
use crate::predicates::Predicate;
use crate::variables::DomainId;

#[derive(Debug, Clone, Default)]
pub(crate) struct Faithfullness {
    lower_bound: LowerBoundWatcher,
    upper_bound: UpperBoundWatcher,
    inequality: InequalityWatcher,
    equality: EqualityWatcher,
}

impl Faithfullness {
    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        id: PredicateId,
        stateful_trail: &mut Trail<StateChange>,
        assignments: &Assignments,
    ) {
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => self
                .lower_bound
                .add(lower_bound, id, stateful_trail, assignments),
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => self
                .upper_bound
                .add(upper_bound, id, stateful_trail, assignments),
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => self
                .inequality
                .add(not_equal_constant, id, stateful_trail, assignments),
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => self
                .equality
                .add(equality_constant, id, stateful_trail, assignments),
        }
    }

    pub(crate) fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_trail: &mut Trail<StateChange>,
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

        if !self.inequality.is_empty() {
            self.inequality.has_been_updated(
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
        self.inequality
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
        self.equality
            .initialise(domain_id, initial_lower_bound, initial_upper_bound);
    }
}
