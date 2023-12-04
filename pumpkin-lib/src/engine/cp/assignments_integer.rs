use crate::{
    basic_types::{DomainId, IntegerVariableGeneratorIterator, Predicate},
    predicate, pumpkin_assert_moderate, pumpkin_assert_simple,
};

use super::{event_sink::EventSink, DomainEvent, PropagatorId, PropagatorVarId};

#[derive(Clone, Default)]
pub struct AssignmentsInteger {
    current_decision_level: u32,
    trail_delimiter: Vec<u32>, //[i] is the position where the i-th decision level ends (exclusive) on the trail
    trail: Vec<ConstraintProgrammingTrailEntry>,
    domains: Vec<IntegerDomainExplicit>, //[domain_id.id][j] indicates if value j is in the domain of the integer variable

    events: EventSink,
}

#[derive(Clone, Copy, Debug)]
pub struct EmptyDomain;

impl AssignmentsInteger {
    pub fn increase_decision_level(&mut self) {
        self.current_decision_level += 1;
        self.trail_delimiter.push(self.trail.len() as u32);
    }

    pub fn get_decision_level(&self) -> u32 {
        self.current_decision_level
    }

    pub fn num_domains(&self) -> u32 {
        self.domains.len() as u32
    }

    pub fn get_domains(&self) -> IntegerVariableGeneratorIterator {
        IntegerVariableGeneratorIterator::new(0, self.num_domains())
    }

    pub fn num_trail_entries(&self) -> usize {
        self.trail.len()
    }

    pub fn get_trail_entry(&self, index: usize) -> ConstraintProgrammingTrailEntry {
        self.trail[index]
    }

    pub fn get_last_entry_on_trail(&self) -> ConstraintProgrammingTrailEntry {
        *self.trail.last().unwrap()
    }

    pub fn get_last_predicates_on_trail(&self, num_predicates: usize) -> Vec<Predicate> {
        //perhaps this could be done with an iteration without needing to copy
        self.trail[(self.num_trail_entries() - num_predicates)..self.num_trail_entries()]
            .iter()
            .map(|e| e.predicate)
            .collect::<Vec<Predicate>>()
    }

    pub fn get_last_entries_on_trail(
        &self,
        num_predicates: usize,
    ) -> Vec<ConstraintProgrammingTrailEntry> {
        //perhaps this could be done with an iteration without needing to copy
        self.trail[(self.num_trail_entries() - num_predicates)..self.num_trail_entries()].to_vec()
    }

    //registers the domain of a new integer variable
    //note that this is an internal method that does _not_ allocate additional information necessary for the solver apart from the domain
    //when creating a new integer variable, use create_new_domain_id in the ConstraintSatisfactionSolver
    pub fn grow(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        let id = DomainId {
            id: self.num_domains(),
        };

        self.domains
            .push(IntegerDomainExplicit::new(lower_bound, upper_bound, id));

        self.events.grow();

        id
    }

    //todo explain that it can return None
    pub fn get_propagator_id_on_trail(&self, index_on_trail: usize) -> Option<PropagatorId> {
        self.trail[index_on_trail]
            .propagator_reason
            .map(|entry| entry.propagator)
    }

    pub fn drain_domain_events(&mut self) -> impl Iterator<Item = (DomainEvent, DomainId)> + '_ {
        self.events.drain()
    }
}

//methods for getting info about the domains
impl AssignmentsInteger {
    pub fn get_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].lower_bound
    }

    pub fn get_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].upper_bound
    }

    pub fn get_assigned_value(&self, domain_id: DomainId) -> i32 {
        pumpkin_assert_simple!(self.is_domain_assigned(domain_id));
        self.domains[domain_id].lower_bound
    }

    pub fn get_lower_bound_predicate(&self, domain_id: DomainId) -> Predicate {
        Predicate::LowerBound {
            domain_id,
            lower_bound: self.get_lower_bound(domain_id),
        }
    }

    pub fn get_upper_bound_predicate(&self, domain_id: DomainId) -> Predicate {
        let upper_bound = self.get_upper_bound(domain_id);
        Predicate::UpperBound {
            domain_id,
            upper_bound,
        }
    }

    pub fn get_domain_description(&self, domain_id: DomainId) -> Vec<Predicate> {
        let mut predicates = Vec::new();
        let domain = &self.domains[domain_id];
        // if fixed, this is just one predicate
        if domain.lower_bound == domain.upper_bound {
            predicates.push(predicate![domain_id == domain.lower_bound]);
            return predicates;
        }
        // if not fixed, start with the bounds...
        predicates.push(predicate![domain_id >= domain.lower_bound]);
        predicates.push(predicate![domain_id <= domain.upper_bound]);
        // then the holes...
        for i in (domain.lower_bound + 1)..domain.upper_bound {
            if !domain.is_value_in_domain[domain.get_index(i)] {
                predicates.push(predicate![domain_id != i]);
            }
        }
        predicates
    }

    pub fn get_lower_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domain_ids: I,
    ) -> Vec<Predicate> {
        domain_ids
            .map(|i| self.get_lower_bound_predicate(*i))
            .collect()
    }

    pub fn get_upper_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domain_ids: I,
    ) -> Vec<Predicate> {
        domain_ids
            .map(|i| self.get_upper_bound_predicate(*i))
            .collect()
    }

    pub fn get_bound_predicates<'a, I: Iterator<Item = &'a DomainId>>(
        &self,
        domain_ids: I,
    ) -> Vec<Predicate> {
        domain_ids
            .flat_map(|domain_id| {
                [
                    self.get_lower_bound_predicate(*domain_id),
                    self.get_upper_bound_predicate(*domain_id),
                ]
            })
            .collect()
    }

    pub fn is_value_in_domain(&self, domain_id: DomainId, value: i32) -> bool {
        let domain = &self.domains[domain_id];
        domain.contains(value)
    }

    pub fn is_domain_assigned(&self, domain_id: DomainId) -> bool {
        self.get_lower_bound(domain_id) == self.get_upper_bound(domain_id)
    }

    pub fn is_domain_assigned_to_value(&self, domain_id: DomainId, value: i32) -> bool {
        self.is_domain_assigned(domain_id) && self.get_lower_bound(domain_id) == value
    }
}

//methods to change the domains
impl AssignmentsInteger {
    pub fn tighten_lower_bound(
        &mut self,
        domain_id: DomainId,
        new_lower_bound: i32,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        if new_lower_bound <= self.get_lower_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = Predicate::LowerBound {
            domain_id,
            lower_bound: new_lower_bound,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.set_lower_bound(new_lower_bound, &mut self.events);

        domain.verify_consistency()
    }

    pub fn tighten_upper_bound(
        &mut self,
        domain_id: DomainId,
        new_upper_bound: i32,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        if new_upper_bound >= self.get_upper_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = Predicate::UpperBound {
            domain_id,
            upper_bound: new_upper_bound,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.set_upper_bound(new_upper_bound, &mut self.events);

        domain.verify_consistency()
    }

    pub fn make_assignment(
        &mut self,
        domain_id: DomainId,
        assigned_value: i32,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        pumpkin_assert_moderate!(!self.is_domain_assigned_to_value(domain_id, assigned_value));

        //only tighten the lower bound if needed
        if self.get_lower_bound(domain_id) < assigned_value {
            self.tighten_lower_bound(domain_id, assigned_value, propagator_reason)?;
        }

        //only tighten the uper bound if needed
        if self.get_upper_bound(domain_id) > assigned_value {
            self.tighten_upper_bound(domain_id, assigned_value, propagator_reason)?;
        }

        self.domains[domain_id].verify_consistency()
    }

    pub fn remove_value_from_domain(
        &mut self,
        domain_id: DomainId,
        removed_value_from_domain: i32,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        if !self.domains[domain_id].contains(removed_value_from_domain) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = Predicate::NotEqual {
            domain_id,
            not_equal_constant: removed_value_from_domain,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.remove_value(removed_value_from_domain, &mut self.events);

        domain.verify_consistency()
    }

    //changes the domains according to the predicate
    //  in case the predicate is already true, no changes happen
    //  however in case the predicate would lead to inconsistent domains, e.g., decreasing the upper bound past the lower bound
    //      pumpkin asserts will make the program crash
    pub fn apply_predicate(
        &mut self,
        predicate: &Predicate,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        if self.does_predicate_hold(predicate) {
            return Ok(());
        }

        match *predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.tighten_lower_bound(domain_id, lower_bound, propagator_reason),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.tighten_upper_bound(domain_id, upper_bound, propagator_reason),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.remove_value_from_domain(domain_id, not_equal_constant, propagator_reason),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.make_assignment(domain_id, equality_constant, propagator_reason),
        }
    }

    pub fn does_predicate_hold(&self, predicate: &Predicate) -> bool {
        match *predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.get_lower_bound(domain_id) >= lower_bound,
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.get_upper_bound(domain_id) <= upper_bound,
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => !self.is_value_in_domain(domain_id, not_equal_constant),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.is_domain_assigned_to_value(domain_id, equality_constant),
        }
    }

    pub fn undo_trail(&mut self, num_trail_entries_to_remove: usize) {
        pumpkin_assert_simple!(num_trail_entries_to_remove <= self.trail.len());

        for _i in 0..num_trail_entries_to_remove {
            pumpkin_assert_moderate!(
                !self.trail.last().unwrap().predicate.is_equality_predicate(),
                "For now we do not expect equality predicates on the trail, since currently equality predicates are split into lower and upper bound predicates."
            );

            let popped_entry = self.trail.pop().unwrap();
            let domain_id = popped_entry.predicate.get_domain();

            self.domains[domain_id].undo_trail_entry(popped_entry);
        }
    }

    pub fn synchronise(&mut self, new_decision_level: u32) {
        pumpkin_assert_simple!(new_decision_level < self.current_decision_level);

        let num_trail_entries_to_remove =
            self.trail.len() - self.trail_delimiter[new_decision_level as usize] as usize;

        self.undo_trail(num_trail_entries_to_remove);
        self.current_decision_level = new_decision_level;
        self.trail_delimiter.truncate(new_decision_level as usize);
    }
}

#[derive(Clone, Copy)]
pub struct ConstraintProgrammingTrailEntry {
    pub predicate: Predicate,
    pub old_lower_bound: i32, //explicitly store the bound before the predicate was applied so that it is easier later on to update the bounds when backtracking
    pub old_upper_bound: i32,
    pub propagator_reason: Option<PropagatorVarId>, //stores the id of the propagator that made the assignment, only makes sense if a propagation took place, e.g., does _not_ make sense in the case of a decision or if the update was due to synchronisation from the propositional trail
}

/// This is the CP representation of a domain. It stores the individual values that are in the
/// domain, alongside the current bounds. To support negative values, and to prevent allocating
/// more memory than the size of the domain, an offset is determined which is used to index into
/// the slice that keeps track of whether an individual value is in the domain.
///
/// When the domain is in an empty state, `lower_bound > upper_bound` and the state of the
/// `is_value_in_domain` field is undefined.
#[derive(Clone)]
struct IntegerDomainExplicit {
    id: DomainId,

    lower_bound: i32,
    upper_bound: i32,

    offset: i32,

    is_value_in_domain: Box<[bool]>,
}

impl IntegerDomainExplicit {
    pub fn new(lower_bound: i32, upper_bound: i32, id: DomainId) -> IntegerDomainExplicit {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Cannot create an empty domain.");

        let size = upper_bound - lower_bound + 1;
        let is_value_in_domain = vec![true; size as usize];

        let offset = -lower_bound;

        IntegerDomainExplicit {
            id,
            lower_bound,
            upper_bound,
            offset,
            is_value_in_domain: is_value_in_domain.into(),
        }
    }

    fn contains(&self, value: i32) -> bool {
        let idx = self.get_index(value);

        self.lower_bound <= value && value <= self.upper_bound && self.is_value_in_domain[idx]
    }

    fn remove_value(&mut self, value: i32, events: &mut EventSink) {
        if value < self.lower_bound || value > self.upper_bound {
            return;
        }

        let idx = self.get_index(value);

        if self.is_value_in_domain[idx] {
            events.event_occurred(DomainEvent::Any, self.id);
        }

        self.is_value_in_domain[idx] = false;

        self.update_lower_bound(events);
        self.update_upper_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(DomainEvent::Assign, self.id);
        }
    }

    fn set_upper_bound(&mut self, value: i32, events: &mut EventSink) {
        if value >= self.upper_bound {
            return;
        }

        events.event_occurred(DomainEvent::UpperBound, self.id);

        self.upper_bound = value;
        self.update_upper_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(DomainEvent::Assign, self.id);
        }
    }

    fn set_lower_bound(&mut self, value: i32, events: &mut EventSink) {
        if value <= self.lower_bound {
            return;
        }

        events.event_occurred(DomainEvent::LowerBound, self.id);

        self.lower_bound = value;
        self.update_lower_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(DomainEvent::Assign, self.id);
        }
    }

    fn update_lower_bound(&mut self, events: &mut EventSink) {
        while self.get_index(self.lower_bound) < self.is_value_in_domain.len()
            && !self.is_value_in_domain[self.get_index(self.lower_bound)]
        {
            events.event_occurred(DomainEvent::LowerBound, self.id);
            self.lower_bound += 1;
        }
    }

    fn update_upper_bound(&mut self, events: &mut EventSink) {
        while self.upper_bound + self.offset >= 0
            && !self.is_value_in_domain[self.get_index(self.upper_bound)]
        {
            events.event_occurred(DomainEvent::UpperBound, self.id);
            self.upper_bound -= 1;
        }
    }

    fn get_index(&self, value: i32) -> usize {
        (value + self.offset) as usize
    }

    fn debug_bounds_check(&self) -> bool {
        // If the domain is empty, the lower bound will be greater than the upper bound.
        if self.lower_bound > self.upper_bound {
            true
        } else {
            let lb_idx = self.get_index(self.lower_bound);
            let ub_idx = self.get_index(self.upper_bound);

            lb_idx < self.is_value_in_domain.len()
                && ub_idx < self.is_value_in_domain.len()
                && self.is_value_in_domain[lb_idx]
                && self.is_value_in_domain[ub_idx]
        }
    }

    fn verify_consistency(&self) -> Result<(), EmptyDomain> {
        if self.lower_bound > self.upper_bound {
            Err(EmptyDomain)
        } else {
            Ok(())
        }
    }

    fn undo_trail_entry(&mut self, entry: ConstraintProgrammingTrailEntry) {
        if let Predicate::NotEqual {
            domain_id: _,
            not_equal_constant,
        } = entry.predicate
        {
            let value_idx = self.get_index(not_equal_constant);
            self.is_value_in_domain[value_idx] = true;
        }

        self.lower_bound = entry.old_lower_bound;
        self.upper_bound = entry.old_upper_bound;

        pumpkin_assert_moderate!(self.debug_bounds_check());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lower_bound_change_lower_bound_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .tighten_lower_bound(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 2);

        assert_contains_events(&events, d1, [DomainEvent::LowerBound, DomainEvent::Any]);
    }

    #[test]
    fn upper_bound_change_triggers_upper_bound_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .tighten_upper_bound(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 2);
        assert_contains_events(&events, d1, [DomainEvent::UpperBound, DomainEvent::Any]);
    }

    #[test]
    fn bounds_change_can_also_trigger_assign_event() {
        let mut assignment = AssignmentsInteger::default();

        let d1 = assignment.grow(1, 5);
        let d2 = assignment.grow(1, 5);

        assignment
            .tighten_lower_bound(d1, 5, None)
            .expect("non-empty domain");
        assignment
            .tighten_upper_bound(d2, 1, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 6);

        assert_contains_events(
            &events,
            d1,
            [
                DomainEvent::LowerBound,
                DomainEvent::Any,
                DomainEvent::Assign,
            ],
        );
        assert_contains_events(
            &events,
            d2,
            [
                DomainEvent::UpperBound,
                DomainEvent::Any,
                DomainEvent::Assign,
            ],
        );
    }

    #[test]
    fn making_assignment_triggers_appropriate_events() {
        let mut assignment = AssignmentsInteger::default();

        let d1 = assignment.grow(1, 5);
        let d2 = assignment.grow(1, 5);
        let d3 = assignment.grow(1, 5);

        assignment
            .make_assignment(d1, 1, None)
            .expect("non-empty domain");
        assignment
            .make_assignment(d2, 5, None)
            .expect("non-empty domain");
        assignment
            .make_assignment(d3, 3, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 10);

        assert_contains_events(
            &events,
            d1,
            [
                DomainEvent::Assign,
                DomainEvent::UpperBound,
                DomainEvent::Any,
            ],
        );
        assert_contains_events(
            &events,
            d2,
            [
                DomainEvent::Assign,
                DomainEvent::LowerBound,
                DomainEvent::Any,
            ],
        );
        assert_contains_events(
            &events,
            d3,
            [
                DomainEvent::Assign,
                DomainEvent::LowerBound,
                DomainEvent::UpperBound,
                DomainEvent::Any,
            ],
        );
    }

    #[test]
    fn removal_triggers_any_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert!(events.contains(&(DomainEvent::Any, d1)));
    }

    #[test]
    fn values_can_be_removed_from_domains() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(2, &mut events);

        assert!(!domain.contains(2));
    }

    #[test]
    fn removing_the_lower_bound_updates_that_lower_bound() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(2, &mut events);
        domain.remove_value(1, &mut events);

        assert_eq!(3, domain.lower_bound);
    }

    #[test]
    fn removing_the_upper_bound_updates_the_upper_bound() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, &mut events);
        domain.remove_value(5, &mut events);

        assert_eq!(3, domain.upper_bound);
    }

    #[test]
    fn an_empty_domain_accepts_removal_operations() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, &mut events);
        domain.remove_value(1, &mut events);
        domain.remove_value(1, &mut events);
    }

    #[test]
    fn setting_lower_bound_rounds_up_to_nearest_value_in_domain() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(2, &mut events);
        domain.set_lower_bound(2, &mut events);

        assert_eq!(3, domain.lower_bound);
    }

    #[test]
    fn setting_upper_bound_rounds_down_to_nearest_value_in_domain() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, &mut events);
        domain.set_upper_bound(4, &mut events);

        assert_eq!(3, domain.upper_bound);
    }

    #[test]
    fn undo_removal_at_bounds_indexes_into_values_domain_correctly() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 5, None)
            .expect("non-empty domain");

        assignment.synchronise(0);

        assert_eq!(5, assignment.get_upper_bound(d1));
    }

    fn assert_contains_events(
        slice: &[(DomainEvent, DomainId)],
        domain: DomainId,
        required_events: impl AsRef<[DomainEvent]>,
    ) {
        for event in required_events.as_ref() {
            assert!(slice.contains(&(*event, domain)));
        }
    }
}
