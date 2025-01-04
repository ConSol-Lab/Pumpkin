use super::watchers::DomainWatcher;
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
        assignments: &Assignments,
        predicate_id: Option<PredicateId>,
    ) {
        if !self.lower_bound.is_empty() {
            self.lower_bound.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
                None,
            );
        }

        if !self.upper_bound.is_empty() {
            self.upper_bound.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
                None,
            );
        }

        if !self.inequality.is_empty() {
            self.inequality.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
                predicate_id,
            );
        }

        if !self.equality.is_empty() {
            self.equality.has_been_updated(
                predicate,
                stateful_trail,
                falsified_predicates,
                satisfied_predicates,
                assignments,
                None,
            );
        }
    }

    pub(crate) fn set_domain_id(&mut self, domain_id: DomainId) {
        self.lower_bound.set_domain_id(domain_id);
        self.upper_bound.set_domain_id(domain_id);
        self.inequality.set_domain_id(domain_id);
        self.equality.set_domain_id(domain_id);
    }
}
