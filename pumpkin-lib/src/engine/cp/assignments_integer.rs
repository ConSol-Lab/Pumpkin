use std::cmp;

use crate::basic_types::HashMap;
use crate::basic_types::KeyedVec;
use crate::basic_types::Trail;
use crate::engine::cp::event_sink::EventSink;
use crate::engine::cp::reason::ReasonRef;
use crate::engine::cp::IntDomainEvent;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
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

    events: EventSink,
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

        id
    }

    pub fn drain_domain_events(&mut self) -> impl Iterator<Item = (IntDomainEvent, DomainId)> + '_ {
        self.events.drain()
    }

    pub fn debug_create_empty_clone(&self) -> Self {
        let mut domains = self.domains.clone();
        let event_sink = EventSink::new(domains.len());
        self.trail.iter().rev().for_each(|entry| {
            domains[entry.predicate.get_domain()].undo_trail_entry(entry);
        });
        AssignmentsInteger {
            trail: Default::default(),
            domains,
            events: event_sink,
        }
    }
}

// methods for getting info about the domains
impl AssignmentsInteger {
    pub fn get_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].lower_bound()
    }

    pub fn get_lower_bound_at_trail_position(
        &self,
        domain_id: DomainId,
        trail_position: usize,
    ) -> i32 {
        self.domains[domain_id].lower_bound_at_trail_position(trail_position)
    }

    pub fn get_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].upper_bound()
    }

    pub fn get_upper_bound_at_trail_position(
        &self,
        domain_id: DomainId,
        trail_position: usize,
    ) -> i32 {
        self.domains[domain_id].upper_bound_at_trail_position(trail_position)
    }

    pub fn get_initial_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_lower_bound()
    }

    pub fn get_initial_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_upper_bound()
    }

    pub fn get_assigned_value(&self, domain_id: DomainId) -> i32 {
        pumpkin_assert_simple!(self.is_domain_assigned(domain_id));
        self.domains[domain_id].lower_bound()
    }

    pub fn get_domain_iterator(&self, domain_id: DomainId) {
        self.domains[domain_id].domain_iterator()
    }

    pub fn get_domain_description(&self, domain_id: DomainId) -> Vec<Predicate> {
        let mut predicates = Vec::new();
        let domain = &self.domains[domain_id];
        // if fixed, this is just one predicate
        if domain.lower_bound() == domain.upper_bound() {
            predicates.push(predicate![domain_id == domain.lower_bound()]);
            return predicates;
        }
        // if not fixed, start with the bounds...
        predicates.push(predicate![domain_id >= domain.lower_bound()]);
        predicates.push(predicate![domain_id <= domain.upper_bound()]);
        // then the holes...
        for i in (domain.lower_bound() + 1)..domain.upper_bound() {
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

    pub fn is_value_in_domain_at_trail_position(
        &self,
        domain_id: DomainId,
        value: i32,
        trail_position: usize,
    ) -> bool {
        self.domains[domain_id].contains_at_trail_position(value, trail_position)
    }

    pub fn is_domain_assigned(&self, domain_id: DomainId) -> bool {
        self.get_lower_bound(domain_id) == self.get_upper_bound(domain_id)
    }

    pub fn is_domain_assigned_to_value(&self, domain_id: DomainId, value: i32) -> bool {
        self.is_domain_assigned(domain_id) && self.get_lower_bound(domain_id) == value
    }

    /// Returns the index of the trail entry at which point the given predicate became true.
    /// In case the predicate is not true, then the function returns None.
    /// Note that it is not necessary for the predicate to be explicitly present on the trail,
    /// e.g., if [x >= 10] is explicitly present on the trail but not [x >= 6], then the
    /// trail position for [x >= 10] will be returned for the case [x >= 6].
    pub fn get_trail_position(&self, integer_predicate: IntegerPredicate) -> Option<usize> {
        self.domains[integer_predicate.get_domain()].trail_position(integer_predicate)
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

        // important to record trail position _before_ pushing to the trail
        let trail_position = self.trail.len();

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let decision_level = self.get_decision_level();
        let domain = &mut self.domains[domain_id];

        domain.set_lower_bound(
            new_lower_bound,
            decision_level,
            trail_position,
            &mut self.events,
        );

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

        // important to record trail position _before_ pushing to the trail
        let trail_position = self.trail.len();

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let decision_level = self.get_decision_level();
        let domain = &mut self.domains[domain_id];

        domain.set_upper_bound(
            new_upper_bound,
            decision_level,
            trail_position,
            &mut self.events,
        );

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

        // important to record trail position _before_ pushing to the trail
        let trail_position = self.trail.len();

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            reason,
        });

        let decision_level = self.get_decision_level();
        let domain = &mut self.domains[domain_id];

        domain.remove_value(
            removed_value_from_domain,
            decision_level,
            trail_position,
            &mut self.events,
        );

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
    pub fn synchronise(&mut self, new_decision_level: usize) -> Vec<(DomainId, i32)> {
        let mut unfixed_variables = Vec::new();
        self.trail.synchronise(new_decision_level).for_each(|entry| {
            pumpkin_assert_moderate!(
                !entry.predicate.is_equality_predicate(),
                "For now we do not expect equality predicates on the trail, since currently equality predicates are split into lower and upper bound predicates."
            );
            let domain_id = entry.predicate.get_domain();
            let fixed_before = self.domains[domain_id].lower_bound() == self.domains[domain_id].upper_bound();
                let value_before = self.domains[domain_id].lower_bound();
                self.domains[domain_id].undo_trail_entry(&entry);
                if fixed_before && self.domains[domain_id].lower_bound() != self.domains[domain_id].upper_bound() {
                    // Variable used to be fixed but is not after backtracking
                    unfixed_variables.push((domain_id, value_before));
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
            .unwrap_or_else(|| panic!("found a reason with predicate {}", predicate))
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

#[derive(Clone, Debug)]
struct PairDecisionLevelTrailPosition {
    #[allow(dead_code)]
    decision_level: usize,
    trail_position: usize,
}

#[derive(Clone, Debug)]
struct BoundUpdateInfo {
    bound: i32,
    decision_level: usize,
    trail_position: usize,
}

#[derive(Clone, Debug)]
struct HoleUpdateInfo {
    removed_value: i32,
    #[allow(dead_code)]
    decision_level: usize,
    #[allow(dead_code)]
    trail_position: usize,
    triggered_lower_bound_update: bool,
    triggered_upper_bound_update: bool,
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
    /// 'updates' fields chronologically records the changes to the domain
    lower_bound_updates: Vec<BoundUpdateInfo>,
    upper_bound_updates: Vec<BoundUpdateInfo>,
    hole_updates: Vec<HoleUpdateInfo>,
    /// Auxiliary data structure to make it easy to check if a value is present or not.
    /// This is done to avoid going through 'hole_updates'.
    /// It maps a removed value with its decision level and trail position.
    /// Note: This field subsumes the role of 'is_value_in_domain',
    /// which will eventually be removed.
    holes: HashMap<i32, PairDecisionLevelTrailPosition>,

    // lower_bound: i32,
    // upper_bound: i32,
    // initial_lower_bound: i32,
    // initial_upper_bound: i32,
    offset: i32,

    is_value_in_domain: Box<[bool]>,
}

impl IntegerDomainExplicit {
    fn new(lower_bound: i32, upper_bound: i32, id: DomainId) -> IntegerDomainExplicit {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Cannot create an empty domain.");

        let size = upper_bound - lower_bound + 1;
        let is_value_in_domain = vec![true; size as usize];

        let offset = -lower_bound;

        let lower_bound_updates = vec![BoundUpdateInfo {
            bound: lower_bound,
            decision_level: 0,
            trail_position: 0,
        }];

        let upper_bound_updates = vec![BoundUpdateInfo {
            bound: upper_bound,
            decision_level: 0,
            trail_position: 0,
        }];

        IntegerDomainExplicit {
            id,
            lower_bound_updates,
            upper_bound_updates,
            hole_updates: vec![],
            holes: Default::default(),
            offset,
            is_value_in_domain: is_value_in_domain.into(),
        }
    }

    fn lower_bound(&self) -> i32 {
        // the last entry contains the current lower bound
        self.lower_bound_updates
            .last()
            .expect("Cannot be empty.")
            .bound
    }

    fn initial_lower_bound(&self) -> i32 {
        // the first entry is never removed,
        // and contains the bound that was assigned upon creation
        self.lower_bound_updates[0].bound
    }

    fn lower_bound_at_trail_position(&self, trail_position: usize) -> i32 {
        // for now a simple inefficient linear scan
        // in the future this should be done with binary search
        // possibly caching old queries, and
        // maybe even first checking large/small trail position values
        // (in case those are commonly used)

        // find the update with largest trail position
        // that is smaller than or equal to the input trail position

        // Recall that by the nature of the updates,
        // the updates are stored in increasing order of trail position.
        self.lower_bound_updates
            .iter()
            .filter(|u| u.trail_position <= trail_position)
            .last()
            .expect("Cannot fail")
            .bound
    }

    fn upper_bound(&self) -> i32 {
        // the last entry contains the current upper bound
        self.upper_bound_updates
            .last()
            .expect("Cannot be empty.")
            .bound
    }

    fn initial_upper_bound(&self) -> i32 {
        // the first entry is never removed,
        // and contains the bound that was assigned upon creation
        self.upper_bound_updates[0].bound
    }

    fn upper_bound_at_trail_position(&self, trail_position: usize) -> i32 {
        // for now a simple inefficient linear scan
        // in the future this should be done with binary search
        // possibly caching old queries, and
        // maybe even first checking large/small trail position values
        // (in case those are commonly used)

        // find the update with largest trail position
        // that is smaller than or equal to the input trail position

        // Recall that by the nature of the updates,
        // the updates are stored in increasing order of trail position.
        self.upper_bound_updates
            .iter()
            .filter(|u| u.trail_position <= trail_position)
            .last()
            .expect("Cannot fail")
            .bound
    }

    fn domain_iterator(&self) {
        // to be done at some point later,
        // for now we keep the method as a reminder
        todo!();
    }

    fn contains(&self, value: i32) -> bool {
        let idx = self.get_index(value);
        self.lower_bound() <= value && value <= self.upper_bound() && self.is_value_in_domain[idx]
    }

    fn contains_at_trail_position(&self, value: i32, trail_position: usize) -> bool {
        // if the value is out of bounds, then we can safety say that the value is not in the domain
        if self.lower_bound_at_trail_position(trail_position) > value
            || self.upper_bound_at_trail_position(trail_position) < value
        {
            return false;
        }
        // otherwise we need to check if there is a hole with that specific value

        // in case the hole is made at the given trail position or earlier,
        // the value is not in the domain
        if let Some(p) = self.holes.get(&value) {
            if p.trail_position <= trail_position {
                return false;
            }
        }
        // since none of the previous checks triggered, the value is in the domain
        true
    }

    fn remove_value(
        &mut self,
        removed_value: i32,
        decision_level: usize,
        trail_position: usize,
        events: &mut EventSink,
    ) {
        if removed_value < self.lower_bound() || removed_value > self.upper_bound() {
            return;
        }

        let idx = self.get_index(removed_value);

        if !self.is_value_in_domain[idx] {
            return;
        }

        events.event_occurred(IntDomainEvent::Removal, self.id);

        let mut hole_update_info = HoleUpdateInfo {
            removed_value,
            decision_level,
            trail_position,
            triggered_lower_bound_update: false,
            triggered_upper_bound_update: false,
        };

        self.is_value_in_domain[idx] = false;

        // check if removing a value triggers a lower bound update
        if self.lower_bound() == removed_value {
            self.set_lower_bound(removed_value + 1, decision_level, trail_position, events);
            hole_update_info.triggered_lower_bound_update = true;
        }
        // check if removing the value triggers an upper bound update
        if self.upper_bound() == removed_value {
            self.set_upper_bound(removed_value - 1, decision_level, trail_position, events);
            hole_update_info.triggered_upper_bound_update = true;
        }

        if self.lower_bound() == self.upper_bound() {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }

        self.hole_updates.push(hole_update_info);
        let old_entry = self.holes.insert(
            removed_value,
            PairDecisionLevelTrailPosition {
                decision_level,
                trail_position,
            },
        );
        pumpkin_assert_moderate!(old_entry.is_none());
    }

    fn debug_is_valid_upper_bound_domain_update(
        &self,
        decision_level: usize,
        trail_position: usize,
    ) -> bool {
        trail_position == 0
            || self.upper_bound_updates.last().unwrap().decision_level <= decision_level
                && self.upper_bound_updates.last().unwrap().trail_position < trail_position
    }

    fn set_upper_bound(
        &mut self,
        new_upper_bound: i32,
        decision_level: usize,
        trail_position: usize,
        events: &mut EventSink,
    ) {
        pumpkin_assert_moderate!(
            self.debug_is_valid_upper_bound_domain_update(decision_level, trail_position)
        );

        if new_upper_bound >= self.upper_bound() {
            return;
        }

        events.event_occurred(IntDomainEvent::UpperBound, self.id);

        self.upper_bound_updates.push(BoundUpdateInfo {
            bound: new_upper_bound,
            decision_level,
            trail_position,
        });
        self.update_upper_bound_with_respect_to_holes();

        if self.lower_bound() == self.upper_bound() {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }
    }

    fn update_upper_bound_with_respect_to_holes(&mut self) {
        // the first check ensures that we do not access a vector location with negative index
        while self.upper_bound() + self.offset >= 0
            && !self.is_value_in_domain[self.get_index(self.upper_bound())]
        {
            // events.event_occurred(IntDomainEvent::UpperBound, self.id);
            self.upper_bound_updates.last_mut().unwrap().bound -= 1;
        }
    }

    fn debug_is_valid_lower_bound_domain_update(
        &self,
        decision_level: usize,
        trail_position: usize,
    ) -> bool {
        trail_position == 0
            || self.lower_bound_updates.last().unwrap().decision_level <= decision_level
                && self.lower_bound_updates.last().unwrap().trail_position < trail_position
    }

    fn set_lower_bound(
        &mut self,
        new_lower_bound: i32,
        decision_level: usize,
        trail_position: usize,
        events: &mut EventSink,
    ) {
        pumpkin_assert_moderate!(
            self.debug_is_valid_lower_bound_domain_update(decision_level, trail_position)
        );

        if new_lower_bound <= self.lower_bound() {
            return;
        }

        events.event_occurred(IntDomainEvent::LowerBound, self.id);

        self.lower_bound_updates.push(BoundUpdateInfo {
            bound: new_lower_bound,
            decision_level,
            trail_position,
        });
        self.update_lower_bound_with_respect_to_holes();

        if self.lower_bound() == self.upper_bound() {
            events.event_occurred(IntDomainEvent::Assign, self.id);
        }
    }

    fn update_lower_bound_with_respect_to_holes(&mut self) {
        while self.get_index(self.lower_bound()) < self.is_value_in_domain.len()
            && !self.is_value_in_domain[self.get_index(self.lower_bound())]
        {
            // events.event_occurred(IntDomainEvent::LowerBound, self.id);
            self.lower_bound_updates.last_mut().unwrap().bound += 1;
        }
    }

    fn get_index(&self, value: i32) -> usize {
        (value + self.offset) as usize
    }

    fn debug_bounds_check(&self) -> bool {
        // If the domain is empty, the lower bound will be greater than the upper bound.
        if self.lower_bound() > self.upper_bound() {
            true
        } else {
            let lb_idx = self.get_index(self.lower_bound());
            let ub_idx = self.get_index(self.upper_bound());

            lb_idx < self.is_value_in_domain.len()
                && ub_idx < self.is_value_in_domain.len()
                && self.is_value_in_domain[lb_idx]
                && self.is_value_in_domain[ub_idx]
        }
    }

    fn verify_consistency(&self) -> Result<(), EmptyDomain> {
        if self.lower_bound() > self.upper_bound() {
            Err(EmptyDomain)
        } else {
            Ok(())
        }
    }

    fn undo_trail_entry(&mut self, entry: &ConstraintProgrammingTrailEntry) {
        match entry.predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: _,
            } => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let _ = self.lower_bound_updates.pop();
                pumpkin_assert_moderate!(!self.lower_bound_updates.is_empty());
            }
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound: _,
            } => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let _ = self.upper_bound_updates.pop();
                pumpkin_assert_moderate!(!self.upper_bound_updates.is_empty());
            }
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let hole_update = self
                    .hole_updates
                    .pop()
                    .expect("Must have record of domain removal.");
                pumpkin_assert_moderate!(hole_update.removed_value == not_equal_constant);

                let value_idx = self.get_index(not_equal_constant);
                self.is_value_in_domain[value_idx] = true;

                let _ = self
                    .holes
                    .remove(&not_equal_constant)
                    .expect("Must be present.");

                if hole_update.triggered_lower_bound_update {
                    let _ = self.lower_bound_updates.pop();
                    pumpkin_assert_moderate!(!self.lower_bound_updates.is_empty());
                }

                if hole_update.triggered_upper_bound_update {
                    let _ = self.upper_bound_updates.pop();
                    pumpkin_assert_moderate!(!self.upper_bound_updates.is_empty());
                }
            }
            IntegerPredicate::Equal {
                domain_id: _,
                equality_constant: _,
            } => {
                // I think we never push equality predicates to the trail
                // in the current version. Equality gets substituted
                // by a lower and upper bound predicate.
                unreachable!()
            }
        };

        // these asserts will be removed, for now it is a sanity check
        // later we may remove the old bound from the trail entry since it is not needed
        pumpkin_assert_simple!(self.lower_bound() == entry.old_lower_bound);
        pumpkin_assert_simple!(self.upper_bound() == entry.old_upper_bound);

        pumpkin_assert_moderate!(self.debug_bounds_check());
    }

    fn trail_position(&self, integer_predicate: IntegerPredicate) -> Option<usize> {
        // Perhaps the recursion could be done in a cleaner way,
        // e.g., separate functions dependibng on the type of predicate.
        // For the initial version, the current version is okay.
        match integer_predicate {
            IntegerPredicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => {
                // Recall that by the nature of the updates,
                // the updates are stored in increasing order of the lower bound.

                // for now a simple inefficient linear scan
                // in the future this should be done with binary search

                // find the update with smallest lower bound
                // that is greater than or equal to the input lower bound
                self.lower_bound_updates
                    .iter()
                    .find(|u| u.bound >= lower_bound)
                    .map(|u| u.trail_position)
            }
            IntegerPredicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => {
                // Recall that by the nature of the updates,
                // the updates are stored in decreasing order of the upper bound.

                // for now a simple inefficient linear scan
                // in the future this should be done with binary search

                // find the update with greatest upper bound
                // that is smaller than or equal to the input upper bound
                self.lower_bound_updates
                    .iter()
                    .find(|u| u.bound <= upper_bound)
                    .map(|u| u.trail_position)
            }
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => {
                // Check in the explictly stored holes.
                // If the value has been removed explicitly,
                // then the stored time is the first time the value was removed.
                if let Some(p) = self.holes.get(&not_equal_constant) {
                    Some(p.trail_position)
                } else {
                    // Otherwise, check the case when the lower/upper bound surpassed the value.
                    // If this never happened, then report that the predicate is not true.

                    // Note that it cannot be that both the lower bound and upper bound surpassed
                    // the not equals constant, i.e., at most one of the two may happen.
                    // So we can stop as soon as we find one of the two.

                    // Check the lower bound first.
                    if let Some(trail_position) =
                        self.trail_position(IntegerPredicate::LowerBound {
                            domain_id,
                            lower_bound: not_equal_constant + 1,
                        })
                    {
                        // The lower bound removed the value from the domain,
                        // report the trail position of the lower bound.
                        Some(trail_position)
                    } else {
                        // The lower bound did not surpass the value,
                        // now check the upper bound.
                        self.trail_position(IntegerPredicate::UpperBound {
                            domain_id,
                            upper_bound: not_equal_constant - 1,
                        })
                    }
                }
            }
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => {
                // For equality to hold, both the lower and upper bound predicates must hold.
                // Check lower bound first.
                if let Some(lb_trail_position) = self.trail_position(IntegerPredicate::LowerBound {
                    domain_id,
                    lower_bound: equality_constant,
                }) {
                    // The lower bound found,
                    // now the check depends on the upper bound.

                    // If both the lower and upper bounds are present,
                    // report the trail position of the bound that was set last.
                    // Otherwise, return that the predicate is not on the trail.
                    self.trail_position(IntegerPredicate::UpperBound {
                        domain_id,
                        upper_bound: equality_constant,
                    })
                    .map(|ub_trail_position| cmp::max(lb_trail_position, ub_trail_position))
                }
                // If the lower bound is never reached,
                // then surely the equality predicate cannot be true.
                else {
                    None
                }
            }
        }
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
        domain.remove_value(2, 0, 1, &mut events);

        assert!(!domain.contains(2));
    }

    #[test]
    fn removing_the_lower_bound_updates_that_lower_bound() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(2, 0, 1, &mut events);
        domain.remove_value(1, 0, 2, &mut events);

        assert_eq!(3, domain.lower_bound());
    }

    #[test]
    fn removing_the_upper_bound_updates_the_upper_bound() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, 0, 1, &mut events);
        domain.remove_value(5, 0, 2, &mut events);

        assert_eq!(3, domain.upper_bound());
    }

    #[test]
    fn an_empty_domain_accepts_removal_operations() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, 0, 1, &mut events);
        domain.remove_value(1, 0, 2, &mut events);
        domain.remove_value(1, 0, 3, &mut events);
    }

    #[test]
    fn setting_lower_bound_rounds_up_to_nearest_value_in_domain() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(2, 0, 1, &mut events);
        domain.set_lower_bound(2, 0, 2, &mut events);

        assert_eq!(3, domain.lower_bound());
    }

    #[test]
    fn setting_upper_bound_rounds_down_to_nearest_value_in_domain() {
        let mut events = EventSink::default();
        events.grow();

        let mut domain = IntegerDomainExplicit::new(1, 5, DomainId::new(0));
        domain.remove_value(4, 0, 1, &mut events);
        domain.set_upper_bound(4, 0, 2, &mut events);

        assert_eq!(3, domain.upper_bound());
    }

    #[test]
    fn undo_removal_at_bounds_indexes_into_values_domain_correctly() {
        let mut assignment = AssignmentsInteger::default();
        let d1 = assignment.grow(1, 5);

        assignment.increase_decision_level();

        assignment
            .remove_value_from_domain(d1, 5, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0);

        assert_eq!(5, assignment.get_upper_bound(d1));
    }

    fn assert_contains_events(
        slice: &[(IntDomainEvent, DomainId)],
        domain: DomainId,
        required_events: impl AsRef<[IntDomainEvent]>,
    ) {
        for event in required_events.as_ref() {
            assert!(slice.contains(&(*event, domain)));
        }
    }

    fn get_domain1() -> (DomainId, IntegerDomainExplicit, EventSink) {
        let mut events = EventSink::default();
        events.grow();

        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomainExplicit::new(0, 100, domain_id);
        domain.set_lower_bound(2, 0, 1, &mut events);
        domain.set_lower_bound(5, 1, 2, &mut events);
        domain.set_lower_bound(10, 2, 10, &mut events);
        domain.set_lower_bound(20, 5, 50, &mut events);
        domain.set_lower_bound(50, 10, 70, &mut events);

        (domain_id, domain, events)
    }

    #[test]
    fn lower_bound_trail_position_inbetween_value() {
        let (domain_id, domain, _) = get_domain1();

        assert_eq!(
            domain.trail_position(IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: 12,
            }),
            Some(50)
        );
    }

    #[test]
    fn lower_bound_trail_position_last_bound() {
        let (domain_id, domain, _) = get_domain1();

        assert_eq!(
            domain.trail_position(IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: 50,
            }),
            Some(70)
        );
    }

    #[test]
    fn lower_bound_trail_position_beyond_value() {
        let (domain_id, domain, _) = get_domain1();

        assert_eq!(
            domain.trail_position(IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: 101,
            }),
            None
        );
    }

    #[test]
    fn lower_bound_trail_position_trivial() {
        let (domain_id, domain, _) = get_domain1();

        assert_eq!(
            domain.trail_position(IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: -10,
            }),
            Some(0)
        );
    }

    #[test]
    fn lower_bound_trail_position_with_removals() {
        let (domain_id, mut domain, mut events) = get_domain1();
        domain.remove_value(50, 11, 75, &mut events);
        domain.remove_value(51, 11, 77, &mut events);
        domain.remove_value(52, 11, 80, &mut events);

        assert_eq!(
            domain.trail_position(IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: 52,
            }),
            Some(77)
        );
    }

    #[test]
    fn removal_trail_position() {
        let (domain_id, mut domain, mut events) = get_domain1();
        domain.remove_value(50, 11, 75, &mut events);
        domain.remove_value(51, 11, 77, &mut events);
        domain.remove_value(52, 11, 80, &mut events);

        assert_eq!(
            domain.trail_position(IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant: 50,
            }),
            Some(75)
        );
    }

    #[test]
    fn removal_trail_position_after_lower_bound() {
        let (domain_id, mut domain, mut events) = get_domain1();
        domain.remove_value(50, 11, 75, &mut events);
        domain.remove_value(51, 11, 77, &mut events);
        domain.remove_value(52, 11, 80, &mut events);
        domain.set_lower_bound(60, 11, 150, &mut events);

        assert_eq!(
            domain.trail_position(IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant: 55,
            }),
            Some(150)
        );
    }

    #[test]
    fn lower_bound_change_backtrack() {
        let mut assignment = AssignmentsInteger::default();
        let domain_id1 = assignment.grow(0, 100);
        let domain_id2 = assignment.grow(0, 50);

        // decision level 1
        assignment.increase_decision_level();
        assignment
            .apply_integer_predicate(
                IntegerPredicate::LowerBound {
                    domain_id: domain_id1,
                    lower_bound: 2,
                },
                None,
            )
            .expect("");
        assignment
            .apply_integer_predicate(
                IntegerPredicate::LowerBound {
                    domain_id: domain_id2,
                    lower_bound: 25,
                },
                None,
            )
            .expect("");

        // decision level 2
        assignment.increase_decision_level();
        assignment
            .apply_integer_predicate(
                IntegerPredicate::LowerBound {
                    domain_id: domain_id1,
                    lower_bound: 5,
                },
                None,
            )
            .expect("");

        // decision level 3
        assignment.increase_decision_level();
        assignment
            .apply_integer_predicate(
                IntegerPredicate::LowerBound {
                    domain_id: domain_id1,
                    lower_bound: 7,
                },
                None,
            )
            .expect("");

        assert_eq!(assignment.get_lower_bound(domain_id1), 7);

        let _ = assignment.synchronise(1);

        assert_eq!(assignment.get_lower_bound(domain_id1), 2);
    }

    #[test]
    fn lower_bound_inbetween_updates() {
        let (_, domain, _) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(25), 10);
    }

    #[test]
    fn lower_bound_beyond_trail_position() {
        let (_, domain, _) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(1000), 50);
    }

    #[test]
    fn lower_bound_at_update() {
        let (_, domain, _) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(50), 20);
    }

    #[test]
    fn lower_bound_at_trail_position_after_removals() {
        let (_, mut domain, mut events) = get_domain1();
        domain.remove_value(50, 11, 75, &mut events);
        domain.remove_value(51, 11, 77, &mut events);
        domain.remove_value(52, 11, 80, &mut events);

        assert_eq!(domain.lower_bound_at_trail_position(77), 52);
    }

    #[test]
    fn lower_bound_at_trail_position_after_removals_and_bound_update() {
        let (_, mut domain, mut events) = get_domain1();
        domain.remove_value(50, 11, 75, &mut events);
        domain.remove_value(51, 11, 77, &mut events);
        domain.remove_value(52, 11, 80, &mut events);
        domain.set_lower_bound(60, 11, 150, &mut events);

        assert_eq!(domain.lower_bound_at_trail_position(100), 53);
    }
}
