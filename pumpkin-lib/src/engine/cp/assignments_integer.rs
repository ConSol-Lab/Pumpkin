use crate::basic_types::KeyedVec;
use crate::basic_types::Trail;
use crate::engine::cp::event_sink::EventSink;
use crate::engine::cp::reason::ReasonRef;
use crate::engine::cp::IntDomainEvent;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::variables::DomainGeneratorIterator;
use crate::engine::variables::DomainId;
use crate::predicate;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Clone, Default, Debug)]
pub struct AssignmentsInteger {
    trail: Trail<ConstraintProgrammingTrailEntry>,
    /// indicates if value j is in the domain of the integer variable
    domains: KeyedVec<DomainId, IntegerDomainExplicit>,

    /// Keeps track of the [`IntDomainEvent`]s which occur while propagating/making decisions, this
    /// is used to implement [`Propagator::notify`].
    events: EventSink,

    /// Keeps track of the [`IntDomainEvent`]s which are undone while backtracking, this is used to
    /// implement [`Propagator::notify_backtrack`].
    backtrack_events: EventSink,
}

#[derive(Clone, Copy, Debug)]
pub struct EmptyDomain;

impl AssignmentsInteger {
    pub fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub fn get_decision_level(&self) -> usize {
        self.trail.get_decision_level()
    }

    pub fn num_domains(&self) -> u32 {
        self.domains.len() as u32
    }

    pub fn get_domains(&self) -> DomainGeneratorIterator {
        DomainGeneratorIterator::new(0, self.num_domains())
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

    pub fn get_last_predicates_on_trail(
        &self,
        num_predicates: usize,
    ) -> impl Iterator<Item = IntegerPredicate> + '_ {
        self.trail[(self.num_trail_entries() - num_predicates)..self.num_trail_entries()]
            .iter()
            .map(|e| e.predicate)
    }

    pub fn get_last_entries_on_trail(
        &self,
        num_predicates: usize,
    ) -> &[ConstraintProgrammingTrailEntry] {
        &self.trail[(self.num_trail_entries() - num_predicates)..self.num_trail_entries()]
    }

    // registers the domain of a new integer variable
    // note that this is an internal method that does _not_ allocate additional information
    // necessary for the solver apart from the domain when creating a new integer variable, use
    // create_new_domain_id in the ConstraintSatisfactionSolver
    pub fn grow(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        let id = DomainId {
            id: self.num_domains(),
        };

        self.domains
            .push(IntegerDomainExplicit::new(lower_bound, upper_bound, id));

        self.events.grow();
        self.backtrack_events.grow();

        id
    }

    pub fn drain_domain_events(&mut self) -> impl Iterator<Item = (IntDomainEvent, DomainId)> + '_ {
        self.events.drain()
    }

    pub fn drain_backtrack_domain_events(
        &mut self,
    ) -> impl Iterator<Item = (IntDomainEvent, DomainId)> + '_ {
        self.backtrack_events.drain()
    }

    pub fn debug_create_empty_clone(&self) -> Self {
        let mut domains = self.domains.clone();
        let event_sink = EventSink::new(domains.len());
        let backtrack_sink = EventSink::new(domains.len());
        self.trail.iter().rev().for_each(|entry| {
            domains[entry.predicate.get_domain()].undo_trail_entry(entry);
        });
        AssignmentsInteger {
            trail: Default::default(),
            domains,
            events: event_sink,
            backtrack_events: backtrack_sink,
        }
    }
}

// methods for getting info about the domains
impl AssignmentsInteger {
    pub fn get_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].lower_bound
    }

    pub fn get_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].upper_bound
    }

    pub fn get_initial_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_lower_bound
    }

    pub fn get_initial_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_upper_bound
    }

    pub fn get_initial_holes(&self, domain_id: DomainId) -> impl Iterator<Item = i32> + '_ {
        self.domains[domain_id]
            .initial_removed_values
            .iter()
            .copied()
    }

    pub fn get_assigned_value(&self, domain_id: DomainId) -> i32 {
        pumpkin_assert_simple!(self.is_domain_assigned(domain_id));
        self.domains[domain_id].lower_bound
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

// methods to change the domains
impl AssignmentsInteger {
    pub fn tighten_lower_bound(
        &mut self,
        domain_id: DomainId,
        new_lower_bound: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if new_lower_bound <= self.get_lower_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = IntegerPredicate::LowerBound {
            domain_id,
            lower_bound: new_lower_bound,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.set_lower_bound(new_lower_bound, &mut self.events);

        domain.verify_consistency()
    }

    pub fn tighten_upper_bound(
        &mut self,
        domain_id: DomainId,
        new_upper_bound: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if new_upper_bound >= self.get_upper_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = IntegerPredicate::UpperBound {
            domain_id,
            upper_bound: new_upper_bound,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.set_upper_bound(new_upper_bound, &mut self.events);

        domain.verify_consistency()
    }

    pub fn make_assignment(
        &mut self,
        domain_id: DomainId,
        assigned_value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        pumpkin_assert_moderate!(!self.is_domain_assigned_to_value(domain_id, assigned_value));

        // only tighten the lower bound if needed
        if self.get_lower_bound(domain_id) < assigned_value {
            self.tighten_lower_bound(domain_id, assigned_value, reason)?;
        }

        // only tighten the uper bound if needed
        if self.get_upper_bound(domain_id) > assigned_value {
            self.tighten_upper_bound(domain_id, assigned_value, reason)?;
        }

        self.domains[domain_id].verify_consistency()
    }

    pub fn remove_initial_value_from_domain(
        &mut self,
        domain_id: DomainId,
        removed_value_from_domain: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if !self.domains[domain_id].contains(removed_value_from_domain) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = IntegerPredicate::NotEqual {
            domain_id,
            not_equal_constant: removed_value_from_domain,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.remove_initial_value(removed_value_from_domain, &mut self.events);

        domain.verify_consistency()
    }

    pub fn remove_value_from_domain(
        &mut self,
        domain_id: DomainId,
        removed_value_from_domain: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if !self.domains[domain_id].contains(removed_value_from_domain) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = IntegerPredicate::NotEqual {
            domain_id,
            not_equal_constant: removed_value_from_domain,
        };

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let domain = &mut self.domains[domain_id];
        domain.remove_value(removed_value_from_domain, &mut self.events);

        domain.verify_consistency()
    }

    /// Apply the given [`Predicate`] to the integer domains.
    ///
    /// In case where the [`Predicate`] is already true, this does nothing. If instead applying the
    /// [`Predicate`] leads to an [`EmptyDomain`], the error variant is returned.
    pub fn apply_integer_predicate(
        &mut self,
        predicate: IntegerPredicate,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if self.does_integer_predicate_hold(predicate) {
            return Ok(());
        }

        match predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.tighten_lower_bound(domain_id, lower_bound, reason),
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.tighten_upper_bound(domain_id, upper_bound, reason),
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.remove_value_from_domain(domain_id, not_equal_constant, reason),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => self.make_assignment(domain_id, equality_constant, reason),
        }
    }

    /// Determines whether the provided [`Predicate`] holds in the current state of the
    /// [`AssignmentsInteger`].
    pub fn does_integer_predicate_hold(&self, predicate: IntegerPredicate) -> bool {
        match predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.get_lower_bound(domain_id) >= lower_bound,
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.get_upper_bound(domain_id) <= upper_bound,
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => !self.is_value_in_domain(domain_id, not_equal_constant),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => self.is_domain_assigned_to_value(domain_id, equality_constant),
        }
    }

    /// Synchronises the internal structures of [`AssignmentsInteger`] based on the fact that
    /// backtracking to `new_decision_level` is taking place. This method returns the list of
    /// [`DomainId`]s and their values which were fixed (i.e. domain of size one) before
    /// backtracking and are unfixed (i.e. domain of two or more values) after synchronisation.
    pub fn synchronise(
        &mut self,
        new_decision_level: usize,
        is_watching_any_backtrack_events: bool,
    ) -> Vec<(DomainId, i32)> {
        let mut unfixed_variables = Vec::new();
        self.trail.synchronise(new_decision_level).for_each(|entry| {
            pumpkin_assert_moderate!(
                !entry.predicate.is_equality_predicate(),
                "For now we do not expect equality predicates on the trail, since currently equality predicates are split into lower and upper bound predicates."
            );
            let domain_id = entry.predicate.get_domain();

            let lower_bound_before = self.domains[domain_id].lower_bound;
            let upper_bound_before = self.domains[domain_id].upper_bound;
            let fixed_before = upper_bound_before == lower_bound_before;


            self.domains[domain_id].undo_trail_entry(&entry);

            if fixed_before && self.domains[domain_id].lower_bound != self.domains[domain_id].upper_bound {
                if is_watching_any_backtrack_events {
                    // This `domain_id` was unassigned while backtracking
                    self.backtrack_events.event_occurred(IntDomainEvent::Assign, domain_id);
                }

                // Variable used to be fixed but is not after backtracking
                unfixed_variables.push((domain_id, lower_bound_before));
            }

            if is_watching_any_backtrack_events {
                // Now we add the remaining events which can occur while backtracking, note that the case of equality has already been handled!
                if lower_bound_before != self.domains[domain_id].lower_bound {
                    self.backtrack_events.event_occurred(IntDomainEvent::LowerBound, domain_id)
                }
                if upper_bound_before != self.domains[domain_id].upper_bound {
                    self.backtrack_events.event_occurred(IntDomainEvent::UpperBound, domain_id)
                }
                if matches!(entry.predicate, IntegerPredicate::NotEqual { domain_id: _, not_equal_constant: _ }) {
                    self.backtrack_events.event_occurred(IntDomainEvent::Removal, domain_id)
                }
            }

        });
        unfixed_variables
    }
}

#[cfg(test)]
impl AssignmentsInteger {
    pub fn get_reason_for_predicate(&self, predicate: IntegerPredicate) -> ReasonRef {
        self.trail
            .iter()
            .find_map(|entry| {
                if entry.predicate == predicate {
                    entry.reason
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("found no reason with predicate {}", predicate))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstraintProgrammingTrailEntry {
    pub predicate: IntegerPredicate,
    /// Explicitly store the bound before the predicate was applied so that it is easier later on
    ///  to update the bounds when backtracking.
    pub old_lower_bound: i32,
    pub old_upper_bound: i32,
    /// Stores the a reference to the reason in the `ReasonStore`, only makes sense if a
    /// propagation  took place, e.g., does _not_ make sense in the case of a decision or if
    /// the update was due  to synchronisation from the propositional trail.
    pub reason: Option<ReasonRef>,
}

/// This is the CP representation of a domain. It stores the individual values that are in the
/// domain, alongside the current bounds. To support negative values, and to prevent allocating
/// more memory than the size of the domain, an offset is determined which is used to index into
/// the slice that keeps track of whether an individual value is in the domain.
///
/// When the domain is in an empty state, `lower_bound > upper_bound` and the state of the
/// `is_value_in_domain` field is undefined.
#[derive(Clone, Debug)]
struct IntegerDomainExplicit {
    id: DomainId,

    lower_bound: i32,
    upper_bound: i32,
    initial_lower_bound: i32,
    initial_upper_bound: i32,
    initial_removed_values: Vec<i32>,

    offset: i32,

    is_value_in_domain: Box<[bool]>,
}

impl IntegerDomainExplicit {
    fn new(lower_bound: i32, upper_bound: i32, id: DomainId) -> IntegerDomainExplicit {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Cannot create an empty domain.");

        let size = upper_bound - lower_bound + 1;
        let is_value_in_domain = vec![true; size as usize];

        let offset = -lower_bound;

        IntegerDomainExplicit {
            id,
            lower_bound,
            upper_bound,
            initial_removed_values: vec![],
            initial_lower_bound: lower_bound,
            initial_upper_bound: upper_bound,
            offset,
            is_value_in_domain: is_value_in_domain.into(),
        }
    }

    fn contains(&self, value: i32) -> bool {
        let idx = self.get_index(value);

        self.lower_bound <= value && value <= self.upper_bound && self.is_value_in_domain[idx]
    }

    fn remove_initial_value(&mut self, value: i32, events: &mut EventSink) {
        self.initial_removed_values.push(value);
        self.remove_value(value, events)
    }

    fn remove_value(&mut self, value: i32, events: &mut EventSink) {
        if value < self.lower_bound || value > self.upper_bound {
            return;
        }

        let idx = self.get_index(value);

        if self.is_value_in_domain[idx] {
            events.event_occurred(IntDomainEvent::Removal, self.id);
        }

        self.is_value_in_domain[idx] = false;

        self.update_lower_bound(events);
        self.update_upper_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }
    }

    fn set_upper_bound(&mut self, value: i32, events: &mut EventSink) {
        if value >= self.upper_bound {
            return;
        }

        events.event_occurred(IntDomainEvent::UpperBound, self.id);

        self.upper_bound = value;
        self.update_upper_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }
    }

    fn set_lower_bound(&mut self, value: i32, events: &mut EventSink) {
        if value <= self.lower_bound {
            return;
        }

        events.event_occurred(IntDomainEvent::LowerBound, self.id);

        self.lower_bound = value;
        self.update_lower_bound(events);

        if self.lower_bound == self.upper_bound {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }
    }

    fn update_lower_bound(&mut self, events: &mut EventSink) {
        while self.get_index(self.lower_bound) < self.is_value_in_domain.len()
            && !self.is_value_in_domain[self.get_index(self.lower_bound)]
        {
            events.event_occurred(IntDomainEvent::LowerBound, self.id);
            self.lower_bound += 1;
        }
    }

    fn update_upper_bound(&mut self, events: &mut EventSink) {
        while self.upper_bound + self.offset >= 0
            && !self.is_value_in_domain[self.get_index(self.upper_bound)]
        {
            events.event_occurred(IntDomainEvent::UpperBound, self.id);
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

    fn undo_trail_entry(&mut self, entry: &ConstraintProgrammingTrailEntry) {
        if let IntegerPredicate::NotEqual {
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
    fn jump_in_bound_change_lower_and_upper_bound_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 1, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 5, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 3);

        assert_contains_events(&events, d1, [IntDomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::Removal]);
    }

    #[test]
    fn jump_in_bound_change_assign_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 3, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 4, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 5, None)
            .expect("non-empty domain");
        let _ = assignment.remove_value_from_domain(d1, 1, None);

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 4);

        assert_contains_events(&events, d1, [IntDomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::Removal]);
        assert_contains_events(&events, d1, [IntDomainEvent::Assign]);
    }

    #[test]
    fn jump_in_bound_change_upper_bound_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 3, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 4, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 5, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 2);

        assert_contains_events(&events, d1, [IntDomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::Removal]);
    }

    #[test]
    fn jump_in_bound_change_lower_bound_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 3, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");
        assignment
            .remove_value_from_domain(d1, 1, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 2);

        assert_contains_events(&events, d1, [IntDomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [IntDomainEvent::Removal]);
    }

    #[test]
    fn lower_bound_change_lower_bound_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .tighten_lower_bound(d1, 2, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);

        assert_contains_events(&events, d1, [IntDomainEvent::LowerBound]);
    }

    #[test]
    fn upper_bound_change_upper_bound_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .tighten_upper_bound(d1, 2, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);

        assert_contains_events(&events, d1, [IntDomainEvent::UpperBound]);
    }

    #[test]
    fn removal_change_addition_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);

        assert_contains_events(&events, d1, [IntDomainEvent::Removal]);
    }

    #[test]
    fn assign_change_unassign_event_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .make_assignment(d1, 2, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, true);

        let events = assignment
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 3);

        assert_contains_events(&events, d1, [IntDomainEvent::Assign]);
    }

    #[test]
    fn lower_bound_change_lower_bound_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .tighten_lower_bound(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 1);

        assert_contains_events(&events, d1, [IntDomainEvent::LowerBound]);
    }

    #[test]
    fn upper_bound_change_triggers_upper_bound_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .tighten_upper_bound(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert_contains_events(&events, d1, [IntDomainEvent::UpperBound]);
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
        assert_eq!(events.len(), 4);

        assert_contains_events(
            &events,
            d1,
            [IntDomainEvent::LowerBound, IntDomainEvent::Assign],
        );
        assert_contains_events(
            &events,
            d2,
            [IntDomainEvent::UpperBound, IntDomainEvent::Assign],
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
        assert_eq!(events.len(), 7);

        assert_contains_events(
            &events,
            d1,
            [IntDomainEvent::Assign, IntDomainEvent::UpperBound],
        );
        assert_contains_events(
            &events,
            d2,
            [IntDomainEvent::Assign, IntDomainEvent::LowerBound],
        );
        assert_contains_events(
            &events,
            d3,
            [
                IntDomainEvent::Assign,
                IntDomainEvent::LowerBound,
                IntDomainEvent::UpperBound,
            ],
        );
    }

    #[test]
    fn removal_triggers_removal_event() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");

        let events = assignment.drain_domain_events().collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert!(events.contains(&(IntDomainEvent::Removal, d1)));
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

        let _ = assignment.synchronise(0, false);

        assert_eq!(5, assignment.get_upper_bound(d1));
    }

    fn assert_contains_events<DomainEvent: PartialEq + Copy>(
        slice: &[(DomainEvent, DomainId)],
        domain: DomainId,
        required_events: impl AsRef<[DomainEvent]>,
    ) {
        for event in required_events.as_ref() {
            assert!(slice.contains(&(*event, domain)));
        }
    }
}
