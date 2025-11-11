use crate::basic_types::Trail;
use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::engine::cp::reason::ReasonRef;
use crate::engine::notifications::NotificationEngine;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate::PredicateType;
use crate::engine::variables::DomainGeneratorIterator;
use crate::engine::variables::DomainId;
use crate::predicate;
use crate::proof::InferenceCode;
use crate::pumpkin_assert_eq_moderate;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct Assignments {
    pub(crate) trail: Trail<ConstraintProgrammingTrailEntry>,
    /// The current bounds of the domain. This is a quick lookup of the data stored more verbosely
    /// in `domains`.
    bounds: KeyedVec<DomainId, (i32, i32)>,
    domains: KeyedVec<DomainId, IntegerDomain>,
    /// The number of values that have been pruned from the domain.
    pruned_values: u64,
}

impl Default for Assignments {
    fn default() -> Self {
        let mut assignments = Self {
            trail: Default::default(),
            bounds: Default::default(),
            domains: Default::default(),
            pruned_values: 0,
        };

        // As a convention, we allocate a dummy domain_id=0, which represents a 0-1 variable that is
        // assigned to one. We use it to represent predicates that are trivially true.
        let dummy_variable = assignments.grow(1, 1);
        assert_eq!(dummy_variable.id(), 0);

        assignments
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EmptyDomain;

impl Assignments {
    #[allow(unused, reason = "Could be used in the future")]
    /// Returns all of the holes in the domain which were created at the provided decision level
    pub(crate) fn get_holes_on_decision_level(
        &self,
        domain_id: DomainId,
        decision_level: usize,
    ) -> impl Iterator<Item = i32> + '_ {
        self.domains[domain_id].get_holes_from_decision_level(decision_level)
    }

    /// Returns all of the holes in the domain which were created at the current decision level
    pub(crate) fn get_holes_on_current_decision_level(
        &self,
        domain_id: DomainId,
    ) -> impl Iterator<Item = i32> + '_ {
        self.domains[domain_id].get_holes_from_current_decision_level(self.get_decision_level())
    }

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    pub(crate) fn get_holes(&self, domain_id: DomainId) -> impl Iterator<Item = i32> + '_ {
        self.domains[domain_id].get_holes()
    }

    pub(crate) fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub(crate) fn find_last_decision(&self) -> Option<Predicate> {
        if self.get_decision_level() == 0 {
            None
        } else {
            let values_on_current_decision_level = self
                .trail
                .values_on_decision_level(self.get_decision_level());
            let entry = values_on_current_decision_level[0];
            pumpkin_assert_eq_simple!(None, entry.reason);

            Some(entry.predicate)
        }
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        self.trail.get_decision_level()
    }

    pub(crate) fn num_domains(&self) -> u32 {
        self.domains.len() as u32
    }

    pub(crate) fn get_domains(&self) -> DomainGeneratorIterator {
        // todo: we use 1 here to prevent the always true literal from ending up in the blocking
        // clause
        DomainGeneratorIterator::new(1, self.num_domains())
    }

    pub(crate) fn num_trail_entries(&self) -> usize {
        self.trail.len()
    }

    pub(crate) fn get_trail_entry(&self, index: usize) -> ConstraintProgrammingTrailEntry {
        self.trail[index]
    }

    // registers the domain of a new integer variable
    // note that this is an internal method that does _not_ allocate additional information
    // necessary for the solver apart from the domain when creating a new integer variable, use
    // create_new_domain_id in the ConstraintSatisfactionSolver
    pub(crate) fn grow(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        // This is necessary for the metric that maintains relative domain size. It is only updated
        // when values are removed at levels beyond the root, and then it becomes a tricky value to
        // update when a fresh domain needs to be considered.
        pumpkin_assert_simple!(
            self.get_decision_level() == 0,
            "can only create variables at the root"
        );

        let id = DomainId::new(self.num_domains());

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate: predicate!(id >= lower_bound),
            old_lower_bound: lower_bound,
            old_upper_bound: upper_bound,
            reason: None,
        });
        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate: predicate!(id <= upper_bound),
            old_lower_bound: lower_bound,
            old_upper_bound: upper_bound,
            reason: None,
        });

        let _ = self.domains.push(IntegerDomain::new(
            lower_bound,
            upper_bound,
            id,
            self.trail.len() - 1,
        ));

        let _ = self.bounds.push((lower_bound, upper_bound));

        id
    }
    pub fn create_new_integer_variable_sparse(&mut self, mut values: Vec<i32>) -> DomainId {
        assert!(
            !values.is_empty(),
            "cannot create a variable with an empty domain"
        );

        values.sort();
        values.dedup();

        let lower_bound = values[0];
        let upper_bound = values[values.len() - 1];

        let domain_id = self.grow(lower_bound, upper_bound);

        let mut next_idx = 0;
        for value in lower_bound..=upper_bound {
            if value == values[next_idx] {
                next_idx += 1;
            } else {
                let _ = self
                    .remove_value_from_domain(domain_id, value, None)
                    .expect("the domain should not be empty");

                self.domains[domain_id].initial_holes.push(value);
            }
        }
        self.domains[domain_id].initial_bounds_below_trail = self.trail.len() - 1;
        pumpkin_assert_simple!(
            next_idx == values.len(),
            "Expected all values to have been processed"
        );

        self.update_bounds_snapshot(domain_id);

        domain_id
    }

    pub(crate) fn debug_create_empty_clone(&self) -> Self {
        let mut domains = self.domains.clone();
        let mut bounds = self.bounds.clone();

        let maximum_trail_entry = domains
            .iter()
            .map(|domain| domain.initial_bounds_below_trail + 1)
            .max()
            .unwrap_or(0);
        self.trail
            .iter()
            .skip(maximum_trail_entry)
            .rev()
            .for_each(|entry| {
                domains[entry.predicate.get_domain()].undo_trail_entry(entry);
            });

        for (id, domain) in self.domains.keys().zip(domains.iter_mut()) {
            domain
                .lower_bound_updates
                .retain(|update| update.trail_position < domain.initial_bounds_below_trail);
            domain
                .upper_bound_updates
                .retain(|update| update.trail_position < domain.initial_bounds_below_trail);
            domain.holes.retain(|value, update| {
                let is_initial_bound = update.trail_position < domain.initial_bounds_below_trail;
                if !is_initial_bound {
                    let to_remove_index = domain
                        .hole_updates
                        .iter()
                        .position(|update| update.removed_value == *value)
                        .unwrap();
                    let _ = domain.hole_updates.remove(to_remove_index);
                }

                is_initial_bound
            });

            bounds[id] = (domain.lower_bound(), domain.upper_bound());
        }

        Assignments {
            trail: Default::default(),
            bounds,
            domains,
            pruned_values: self.pruned_values,
        }
    }

    pub(crate) fn is_initial_bound(&self, predicate: Predicate) -> bool {
        let domain_id = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => value <= self.domains[domain_id].initial_lower_bound(),
            PredicateType::UpperBound => value >= self.domains[domain_id].initial_upper_bound(),
            PredicateType::NotEqual => {
                self.get_trail_position(&predicate).unwrap_or_else(|| {
                    panic!("Expected to be able to get trail entry of {predicate}")
                }) <= self.domains[domain_id].initial_bounds_below_trail
            }
            PredicateType::Equal => {
                value == self.domains[domain_id].initial_lower_bound()
                    && value == self.domains[domain_id].initial_upper_bound()
            }
        }
    }
}

// methods for getting info about the domains
impl Assignments {
    pub(crate) fn get_lower_bound(&self, domain_id: DomainId) -> i32 {
        let (lower_bound, _) = self.bounds[domain_id];

        pumpkin_assert_eq_moderate!(
            lower_bound,
            self.domains[domain_id].lower_bound(),
            "bounds for {domain_id} out of sync"
        );

        lower_bound
    }

    pub(crate) fn get_lower_bound_at_trail_position(
        &self,
        domain_id: DomainId,
        trail_position: usize,
    ) -> i32 {
        self.domains[domain_id].lower_bound_at_trail_position(trail_position)
    }

    pub(crate) fn get_upper_bound(&self, domain_id: DomainId) -> i32 {
        let (_, upper_bound) = self.bounds[domain_id];

        pumpkin_assert_eq_moderate!(
            upper_bound,
            self.domains[domain_id].upper_bound(),
            "bounds for {domain_id} out of sync"
        );

        upper_bound
    }

    pub(crate) fn get_upper_bound_at_trail_position(
        &self,
        domain_id: DomainId,
        trail_position: usize,
    ) -> i32 {
        self.domains[domain_id].upper_bound_at_trail_position(trail_position)
    }

    pub(crate) fn get_initial_lower_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_lower_bound()
    }

    pub(crate) fn get_initial_upper_bound(&self, domain_id: DomainId) -> i32 {
        self.domains[domain_id].initial_upper_bound()
    }

    pub(crate) fn get_initial_holes(&self, domain_id: DomainId) -> Vec<i32> {
        self.domains[domain_id].initial_holes.clone()
    }

    pub(crate) fn get_assigned_value<Var: IntegerVariable>(&self, var: &Var) -> Option<i32> {
        self.is_domain_assigned(var).then(|| var.lower_bound(self))
    }

    pub(crate) fn is_decision_predicate(&self, predicate: &Predicate) -> bool {
        if let Some(trail_position) = self.get_trail_position(predicate) {
            self.trail[trail_position].reason.is_none()
                && self.trail[trail_position].predicate == *predicate
        } else {
            false
        }
    }

    pub(crate) fn get_domain_iterator(&self, domain_id: DomainId) -> IntegerDomainIterator<'_> {
        self.domains[domain_id].domain_iterator()
    }

    /// Returns the conjunction of predicates that define the domain.
    /// Root level predicates are ignored.
    pub(crate) fn get_domain_description(&self, domain_id: DomainId) -> Vec<Predicate> {
        let mut predicates = Vec::new();
        let domain = &self.domains[domain_id];

        // If the domain assigned at a nonroot level, this is just one predicate.
        if domain.lower_bound() == domain.upper_bound()
            && domain.lower_bound_decision_level() > 0
            && domain.upper_bound_decision_level() > 0
        {
            predicates.push(predicate![domain_id == domain.lower_bound()]);
            return predicates;
        }

        // Add bounds but avoid root assignments.
        if domain.lower_bound_decision_level() > 0 {
            predicates.push(predicate![domain_id >= domain.lower_bound()]);
        }

        if domain.upper_bound_decision_level() > 0 {
            predicates.push(predicate![domain_id <= domain.upper_bound()]);
        }

        // Add holes.
        for hole in &self.domains[domain_id].holes {
            // Only record holes that are within the lower and upper bound,
            // that are not root assignments.
            // Since bound values cannot be in the holes,
            // we can use '<' or '>'.
            if hole.1.decision_level > 0
                && domain.lower_bound() < *hole.0
                && *hole.0 < domain.upper_bound()
            {
                predicates.push(predicate![domain_id != *hole.0]);
            }
        }
        predicates
    }

    pub(crate) fn is_value_in_domain(&self, domain_id: DomainId, value: i32) -> bool {
        let (lower_bound, upper_bound) = self.bounds[domain_id];

        if value < lower_bound || value > upper_bound {
            return false;
        }

        let domain = &self.domains[domain_id];
        domain.contains(value)
    }

    pub(crate) fn is_value_in_domain_at_trail_position(
        &self,
        domain_id: DomainId,
        value: i32,
        trail_position: usize,
    ) -> bool {
        self.domains[domain_id].contains_at_trail_position(value, trail_position)
    }

    pub(crate) fn is_domain_assigned<Var: IntegerVariable>(&self, var: &Var) -> bool {
        var.lower_bound(self) == var.upper_bound(self)
    }

    /// Returns the index of the trail entry at which point the given predicate became true.
    /// In case the predicate is not true, then the function returns None.
    /// Note that it is not necessary for the predicate to be explicitly present on the trail,
    /// e.g., if [x >= 10] is explicitly present on the trail but not [x >= 6], then the
    /// trail position for [x >= 10] will be returned for the case [x >= 6].
    pub(crate) fn get_trail_position(&self, predicate: &Predicate) -> Option<usize> {
        self.domains[predicate.get_domain()]
            .get_update_info(predicate)
            .map(|u| u.trail_position)
    }

    /// If the predicate is assigned true, returns the decision level of the predicate.
    /// Otherwise returns None.
    pub(crate) fn get_decision_level_for_predicate(&self, predicate: &Predicate) -> Option<usize> {
        self.domains[predicate.get_domain()]
            .get_update_info(predicate)
            .map(|u| u.decision_level)
    }

    pub fn get_domain_descriptions(&self) -> Vec<Predicate> {
        let mut descriptions: Vec<Predicate> = vec![];
        for domain in self.domains.iter().enumerate() {
            let domain_id = DomainId::new(domain.0 as u32);
            descriptions.append(&mut self.get_domain_description(domain_id));
        }
        descriptions
    }
}

type AssignmentReason = (ReasonRef, InferenceCode);

// methods to change the domains
impl Assignments {
    fn tighten_lower_bound(
        &mut self,
        domain_id: DomainId,
        new_lower_bound: i32,
        reason: Option<AssignmentReason>,
    ) -> Result<bool, EmptyDomain> {
        // No need to do any changes if the new lower bound is weaker.
        if new_lower_bound <= self.get_lower_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = predicate!(domain_id >= new_lower_bound);

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

        let update_took_place =
            domain.set_lower_bound(new_lower_bound, decision_level, trail_position);

        self.bounds[domain_id].0 = domain.lower_bound();

        self.pruned_values += domain.lower_bound().abs_diff(old_lower_bound) as u64;

        let _ = domain.verify_consistency()?;

        Ok(update_took_place)
    }

    fn tighten_upper_bound(
        &mut self,
        domain_id: DomainId,
        new_upper_bound: i32,
        reason: Option<AssignmentReason>,
    ) -> Result<bool, EmptyDomain> {
        // No need to do any changes if the new upper bound is weaker.
        if new_upper_bound >= self.get_upper_bound(domain_id) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = predicate!(domain_id <= new_upper_bound);

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

        let update_took_place =
            domain.set_upper_bound(new_upper_bound, decision_level, trail_position);

        self.bounds[domain_id].1 = domain.upper_bound();

        self.pruned_values += old_upper_bound.abs_diff(domain.upper_bound()) as u64;

        let _ = domain.verify_consistency()?;

        Ok(update_took_place)
    }

    fn make_assignment(
        &mut self,
        domain_id: DomainId,
        assigned_value: i32,
        reason: Option<AssignmentReason>,
    ) -> Result<bool, EmptyDomain> {
        let mut update_took_place = false;

        let predicate = predicate!(domain_id == assigned_value);

        let old_lower_bound = self.get_lower_bound(domain_id);
        let old_upper_bound = self.get_upper_bound(domain_id);

        if old_lower_bound == assigned_value && old_upper_bound == assigned_value {
            return self.domains[domain_id].verify_consistency();
        }

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

        if old_lower_bound < assigned_value {
            update_took_place |=
                domain.set_lower_bound(assigned_value, decision_level, trail_position);
            self.bounds[domain_id].0 = domain.lower_bound();
            self.pruned_values += domain.lower_bound().abs_diff(old_lower_bound) as u64;
        }

        if old_upper_bound > assigned_value {
            update_took_place |=
                domain.set_upper_bound(assigned_value, decision_level, trail_position);
            self.bounds[domain_id].1 = domain.upper_bound();
            self.pruned_values += domain.upper_bound().abs_diff(old_upper_bound) as u64;
        }

        let _ = self.domains[domain_id].verify_consistency()?;

        Ok(update_took_place)
    }

    fn remove_value_from_domain(
        &mut self,
        domain_id: DomainId,
        removed_value_from_domain: i32,
        reason: Option<AssignmentReason>,
    ) -> Result<bool, EmptyDomain> {
        // No need to do any changes if the value is not present anyway.
        if !self.domains[domain_id].contains(removed_value_from_domain) {
            return self.domains[domain_id].verify_consistency();
        }

        let predicate = predicate!(domain_id != removed_value_from_domain);

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

        let _ = domain.remove_value(removed_value_from_domain, decision_level, trail_position);

        let changed_lower_bound = domain.lower_bound().abs_diff(old_lower_bound) as u64;
        let changed_upper_bound = old_upper_bound.abs_diff(domain.upper_bound()) as u64;

        if changed_lower_bound + changed_upper_bound > 0 {
            self.pruned_values += changed_upper_bound + changed_lower_bound;
        } else {
            self.pruned_values += 1;
        }

        self.update_bounds_snapshot(domain_id);
        let _ = self.domains[domain_id].verify_consistency()?;

        Ok(true)
    }

    /// Apply the given [`Predicate`] to the integer domains.
    ///
    /// In case where the [`Predicate`] is already true, this does nothing and will
    /// return `false`. If the predicate was unassigned and became true, then `true`
    /// is returned. If instead applying the [`Predicate`] leads to an
    /// [`EmptyDomain`], the error variant is returned.
    pub(crate) fn post_predicate(
        &mut self,
        predicate: Predicate,
        reason: Option<AssignmentReason>,
        notification_engine: &mut NotificationEngine,
    ) -> Result<bool, EmptyDomain> {
        let (lower_bound_before, upper_bound_before) = self.bounds[predicate.get_domain()];

        let mut removal_took_place = false;

        let domain_id = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        let update_took_place = match predicate.get_predicate_type() {
            PredicateType::LowerBound => self.tighten_lower_bound(domain_id, value, reason)?,
            PredicateType::UpperBound => self.tighten_upper_bound(domain_id, value, reason)?,
            PredicateType::NotEqual => {
                removal_took_place = self.remove_value_from_domain(domain_id, value, reason)?;
                removal_took_place
            }
            PredicateType::Equal => self.make_assignment(domain_id, value, reason)?,
        };

        if update_took_place {
            notification_engine.event_occurred(
                lower_bound_before,
                upper_bound_before,
                self.domains[predicate.get_domain()].lower_bound(),
                self.domains[predicate.get_domain()].upper_bound(),
                removal_took_place,
                predicate.get_domain(),
            );
        }

        Ok(update_took_place)
    }

    /// Determines whether the provided [`Predicate`] holds in the current state of the
    /// [`Assignments`]. In case the predicate is not assigned yet (neither true nor false),
    /// returns None.
    pub(crate) fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool> {
        let domain_id = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if self.get_lower_bound(domain_id) >= value {
                    Some(true)
                } else if self.get_upper_bound(domain_id) < value {
                    Some(false)
                } else {
                    None
                }
            }
            PredicateType::UpperBound => {
                if self.get_upper_bound(domain_id) <= value {
                    Some(true)
                } else if self.get_lower_bound(domain_id) > value {
                    Some(false)
                } else {
                    None
                }
            }
            PredicateType::NotEqual => {
                if !self.is_value_in_domain(domain_id, value) {
                    Some(true)
                } else if let Some(assigned_value) = self.get_assigned_value(&domain_id) {
                    // Previous branch concluded the value is not in the domain, so if the variable
                    // is assigned, then it is assigned to the not equals value.
                    pumpkin_assert_simple!(assigned_value == value);
                    Some(false)
                } else {
                    None
                }
            }
            PredicateType::Equal => {
                if !self.is_value_in_domain(domain_id, value) {
                    Some(false)
                } else if let Some(assigned_value) = self.get_assigned_value(&domain_id) {
                    pumpkin_assert_moderate!(assigned_value == value);
                    Some(true)
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn is_predicate_satisfied(&self, predicate: Predicate) -> bool {
        self.evaluate_predicate(predicate)
            .is_some_and(|truth_value| truth_value)
    }

    #[allow(unused, reason = "makes sense to have in this API")]
    pub(crate) fn is_predicate_falsified(&self, predicate: Predicate) -> bool {
        self.evaluate_predicate(predicate)
            .is_some_and(|truth_value| !truth_value)
    }

    /// Synchronises the internal structures of [`Assignments`] based on the fact that
    /// backtracking to `new_decision_level` is taking place. This method returns the list of
    /// [`DomainId`]s and their values which were fixed (i.e. domain of size one) before
    /// backtracking and are unfixed (i.e. domain of two or more values) after synchronisation.
    pub(crate) fn synchronise(
        &mut self,
        new_decision_level: usize,
        notification_engine: &mut NotificationEngine,
    ) -> Vec<(DomainId, i32)> {
        let mut unfixed_variables = Vec::new();
        let num_trail_entries_before_synchronisation = self.num_trail_entries();

        pumpkin_assert_simple!(
            new_decision_level <= self.trail.get_decision_level(),
            "Expected the new decision level {new_decision_level} to be less than or equal to the current decision level {}",
            self.trail.get_decision_level(),
        );

        self.trail
            .synchronise(new_decision_level)
            .enumerate()
            .for_each(|(index, entry)| {
                // Calculate how many values are re-introduced into the domain.
                let domain_id = entry.predicate.get_domain();
                let lower_bound_before = self.domains[domain_id].lower_bound();
                let upper_bound_before = self.domains[domain_id].upper_bound();

                let trail_index = num_trail_entries_before_synchronisation - index - 1;

                let add_on_upper_bound = entry.old_upper_bound.abs_diff(upper_bound_before) as u64;
                let add_on_lower_bound = entry.old_lower_bound.abs_diff(lower_bound_before) as u64;
                self.pruned_values -= add_on_upper_bound + add_on_lower_bound;

                if entry.predicate.is_not_equal_predicate()
                    && add_on_lower_bound + add_on_upper_bound == 0
                {
                    self.pruned_values -= 1;
                }

                let fixed_before =
                    self.domains[domain_id].lower_bound() == self.domains[domain_id].upper_bound();
                self.domains[domain_id].undo_trail_entry(&entry);

                let new_lower_bound = self.domains[domain_id].lower_bound();
                let new_upper_bound = self.domains[domain_id].upper_bound();
                self.bounds[domain_id] = (new_lower_bound, new_upper_bound);

                notification_engine.undo_trail_entry(
                    fixed_before,
                    lower_bound_before,
                    upper_bound_before,
                    new_lower_bound,
                    new_upper_bound,
                    trail_index,
                    entry.predicate,
                );

                if new_lower_bound != new_upper_bound {
                    // Variable used to be fixed but is not after backtracking
                    unfixed_variables.push((domain_id, lower_bound_before));
                }
            });

        // Drain does not remove the events from the internal data structure. Elements are removed
        // lazily, as the iterator gets executed. For this reason we go through the entire iterator.
        notification_engine.clear_events();

        unfixed_variables
    }

    /// todo: This is a temporary hack, not to be used in general.
    pub(crate) fn remove_last_trail_element(&mut self) -> (Predicate, ReasonRef, InferenceCode) {
        let entry = self.trail.pop().unwrap();
        let domain_id = entry.predicate.get_domain();
        self.domains[domain_id].undo_trail_entry(&entry);
        self.update_bounds_snapshot(domain_id);

        let (reason, inference_code) = entry.reason.unwrap();

        (entry.predicate, reason, inference_code)
    }

    /// Get the number of values pruned from all the domains.
    pub(crate) fn get_pruned_value_count(&self) -> u64 {
        self.pruned_values
    }

    fn update_bounds_snapshot(&mut self, domain_id: DomainId) {
        self.bounds[domain_id] = (
            self.domains[domain_id].lower_bound(),
            self.domains[domain_id].upper_bound(),
        );
    }
}

#[cfg(test)]
impl Assignments {
    pub(crate) fn get_reason_for_predicate_brute_force(&self, predicate: Predicate) -> ReasonRef {
        self.trail
            .iter()
            .find_map(|entry| {
                if entry.predicate == predicate {
                    entry.reason.map(|(reason_ref, _)| reason_ref)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("could not find a reason for predicate {predicate}"))
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ConstraintProgrammingTrailEntry {
    pub(crate) predicate: Predicate,
    /// Explicitly store the bound before the predicate was applied so that it is easier later on
    ///  to update the bounds when backtracking.
    pub(crate) old_lower_bound: i32,
    pub(crate) old_upper_bound: i32,
    /// Stores the a reference to the reason in the `ReasonStore`, only makes sense if a
    /// propagation  took place, e.g., does _not_ make sense in the case of a decision or if
    /// the update was due  to synchronisation from the propositional trail.
    pub(crate) reason: Option<AssignmentReason>,
}

#[derive(Clone, Copy, Debug)]
struct PairDecisionLevelTrailPosition {
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

    decision_level: usize,

    triggered_lower_bound_update: bool,
    triggered_upper_bound_update: bool,
}

/// This is the CP representation of a domain. It stores the bounds alongside holes in the domain.
/// When the domain is in an empty state, `lower_bound > upper_bound`.
/// The domain tracks all domain changes, so it is possible to query the domain at a given
/// cp trail position, i.e., the domain at some previous point in time.
/// This is needed to support lazy explanations.
#[derive(Clone, Debug)]
struct IntegerDomain {
    id: DomainId,
    /// The 'updates' fields chronologically records the changes to the domain.
    lower_bound_updates: Vec<BoundUpdateInfo>,
    upper_bound_updates: Vec<BoundUpdateInfo>,
    hole_updates: Vec<HoleUpdateInfo>,
    /// Auxiliary data structure to make it easy to check if a value is present or not.
    /// This is done to avoid going through 'hole_updates'.
    /// It maps a removed value with its decision level and trail position.
    /// In the future we could consider using direct hashing if the domain is small.
    holes: HashMap<i32, PairDecisionLevelTrailPosition>,
    // Records the trail entry at which all of the root bounds are true
    initial_bounds_below_trail: usize,
    /// The holes that exist in the input problem.
    initial_holes: Vec<i32>,
}

impl IntegerDomain {
    fn new(
        lower_bound: i32,
        upper_bound: i32,
        id: DomainId,
        initial_bounds_below_trail: usize,
    ) -> IntegerDomain {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Cannot create an empty domain.");

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

        IntegerDomain {
            id,
            initial_holes: vec![],
            lower_bound_updates,
            upper_bound_updates,
            hole_updates: vec![],
            holes: Default::default(),
            initial_bounds_below_trail,
        }
    }

    fn lower_bound(&self) -> i32 {
        // the last entry contains the current lower bound
        self.lower_bound_updates
            .last()
            .expect("Cannot be empty.")
            .bound
    }

    fn lower_bound_decision_level(&self) -> usize {
        self.lower_bound_updates
            .last()
            .expect("Cannot be empty.")
            .decision_level
    }

    fn initial_lower_bound(&self) -> i32 {
        // the first entry is never removed,
        // and contains the bound that was assigned upon creation
        self.lower_bound_updates[0].bound
    }

    fn lower_bound_at_trail_position(&self, trail_position: usize) -> i32 {
        // TODO: could possibly cache old queries, and maybe even first checking large/small trail
        // position values (in case those are commonly used)

        // We find the update with the largest trail position such that it is smaller than or equal
        // to the input trail position
        //
        // Recall that by the nature of the updates, the updates are stored in increasing order of
        // trail position.
        //
        // We find the first index such that `u.trail_position > trail_position` and then we
        // subtract 1 from that
        let index = self
            .lower_bound_updates
            .partition_point(|u| u.trail_position <= trail_position);

        self.lower_bound_updates[index.saturating_sub(1)].bound
    }

    fn upper_bound(&self) -> i32 {
        // the last entry contains the current upper bound
        self.upper_bound_updates
            .last()
            .expect("Cannot be empty.")
            .bound
    }

    fn upper_bound_decision_level(&self) -> usize {
        self.upper_bound_updates
            .last()
            .expect("Cannot be empty.")
            .decision_level
    }

    fn initial_upper_bound(&self) -> i32 {
        // the first entry is never removed,
        // and contains the bound that was assigned upon creation
        self.upper_bound_updates[0].bound
    }

    fn upper_bound_at_trail_position(&self, trail_position: usize) -> i32 {
        // TODO: could possibly cache old queries, and maybe even first checking large/small trail
        // position values (in case those are commonly used)

        // We find the update with the largest trail position such that it is smaller than or equal
        // to the input trail position
        //
        // Recall that by the nature of the updates, the updates are stored in increasing order of
        // trail position.
        //
        // We find the first index such that `u.trail_position > trail_position` and then we
        // subtract 1 from that
        let index = self
            .upper_bound_updates
            .partition_point(|u| u.trail_position <= trail_position)
            .saturating_sub(1);

        self.upper_bound_updates[index].bound
    }

    fn domain_iterator(&self) -> IntegerDomainIterator<'_> {
        // Ideally we use into_iter but I did not manage to get it to work,
        // because the iterator takes a lifelines
        // (the iterator takes a reference to the domain).
        // So this will do for now.
        IntegerDomainIterator::new(self)
    }

    fn contains(&self, value: i32) -> bool {
        self.lower_bound() <= value
            && value <= self.upper_bound()
            && !self.holes.contains_key(&value)
    }

    fn contains_at_trail_position(&self, value: i32, trail_position: usize) -> bool {
        // If the value is out of bounds,
        // then we can safety say that the value is not in the domain.
        if self.lower_bound_at_trail_position(trail_position) > value
            || self.upper_bound_at_trail_position(trail_position) < value
        {
            return false;
        }
        // Otherwise we need to check if there is a hole with that specific value.

        // In case the hole is made at the given trail position or earlier,
        // the value is not in the domain.
        if let Some(hole_info) = self.holes.get(&value)
            && hole_info.trail_position <= trail_position
        {
            return false;
        }

        // Since none of the previous checks triggered, the value is in the domain.
        true
    }

    fn remove_value(
        &mut self,
        removed_value: i32,
        decision_level: usize,
        trail_position: usize,
    ) -> bool {
        if removed_value < self.lower_bound()
            || removed_value > self.upper_bound()
            || self.holes.contains_key(&removed_value)
        {
            return false;
        }

        self.hole_updates.push(HoleUpdateInfo {
            removed_value,
            decision_level,
            triggered_lower_bound_update: false,
            triggered_upper_bound_update: false,
        });
        // Note that it is important to remove the hole now,
        // because the later if statements may use the holes.
        let old_none_entry = self.holes.insert(
            removed_value,
            PairDecisionLevelTrailPosition {
                decision_level,
                trail_position,
            },
        );
        pumpkin_assert_moderate!(old_none_entry.is_none());

        // Check if removing a value triggers a lower bound update.
        if self.lower_bound() == removed_value {
            let _ = self.set_lower_bound(removed_value + 1, decision_level, trail_position);
            self.hole_updates
                .last_mut()
                .expect("we just pushed a value, so must be present")
                .triggered_lower_bound_update = true;
        }
        // Check if removing the value triggers an upper bound update.
        if self.upper_bound() == removed_value {
            let _ = self.set_upper_bound(removed_value - 1, decision_level, trail_position);
            self.hole_updates
                .last_mut()
                .expect("we just pushed a value, so must be present")
                .triggered_upper_bound_update = true;
        }

        true
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
    ) -> bool {
        pumpkin_assert_moderate!(
            self.debug_is_valid_upper_bound_domain_update(decision_level, trail_position)
        );

        if new_upper_bound >= self.upper_bound() {
            return false;
        }

        self.upper_bound_updates.push(BoundUpdateInfo {
            bound: new_upper_bound,
            decision_level,
            trail_position,
        });
        self.update_upper_bound_with_respect_to_holes();

        true
    }

    fn update_upper_bound_with_respect_to_holes(&mut self) {
        while self.holes.contains_key(&self.upper_bound())
            && self.lower_bound() <= self.upper_bound()
        {
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
    ) -> bool {
        pumpkin_assert_moderate!(
            self.debug_is_valid_lower_bound_domain_update(decision_level, trail_position)
        );

        if new_lower_bound <= self.lower_bound() {
            return false;
        }

        self.lower_bound_updates.push(BoundUpdateInfo {
            bound: new_lower_bound,
            decision_level,
            trail_position,
        });
        self.update_lower_bound_with_respect_to_holes();

        true
    }

    fn update_lower_bound_with_respect_to_holes(&mut self) {
        while self.holes.contains_key(&self.lower_bound())
            && self.lower_bound() <= self.upper_bound()
        {
            self.lower_bound_updates.last_mut().unwrap().bound += 1;
        }
    }

    fn debug_bounds_check(&self) -> bool {
        // If the domain is empty, the lower bound will be greater than the upper bound.
        if self.lower_bound() > self.upper_bound() {
            true
        } else {
            self.lower_bound() >= self.initial_lower_bound()
                && self.upper_bound() <= self.initial_upper_bound()
                && !self.holes.contains_key(&self.lower_bound())
                && !self.holes.contains_key(&self.upper_bound())
        }
    }

    fn verify_consistency(&self) -> Result<bool, EmptyDomain> {
        if self.lower_bound() > self.upper_bound() {
            Err(EmptyDomain)
        } else {
            Ok(false)
        }
    }

    fn undo_trail_entry(&mut self, entry: &ConstraintProgrammingTrailEntry) {
        let domain_id = entry.predicate.get_domain();
        match entry.predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let _ = self.lower_bound_updates.pop();
                pumpkin_assert_moderate!(!self.lower_bound_updates.is_empty());
            }
            PredicateType::UpperBound => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let _ = self.upper_bound_updates.pop();
                pumpkin_assert_moderate!(!self.upper_bound_updates.is_empty());
            }
            PredicateType::NotEqual => {
                pumpkin_assert_moderate!(domain_id == self.id);

                let not_equal_constant = entry.predicate.get_right_hand_side();

                let hole_update = self
                    .hole_updates
                    .pop()
                    .expect("Must have record of domain removal.");
                pumpkin_assert_moderate!(hole_update.removed_value == not_equal_constant);

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
            PredicateType::Equal => {
                let lower_bound_update = self.lower_bound_updates.last().unwrap();
                let upper_bound_update = self.upper_bound_updates.last().unwrap();

                if lower_bound_update.trail_position > upper_bound_update.trail_position {
                    let _ = self.lower_bound_updates.pop();
                } else if upper_bound_update.trail_position > lower_bound_update.trail_position {
                    let _ = self.upper_bound_updates.pop();
                } else {
                    let _ = self.lower_bound_updates.pop();
                    let _ = self.upper_bound_updates.pop();
                }
            }
        };

        // these asserts will be removed, for now it is a sanity check
        // later we may remove the old bound from the trail entry since it is not needed
        pumpkin_assert_eq_simple!(self.lower_bound(), entry.old_lower_bound);
        pumpkin_assert_eq_simple!(self.upper_bound(), entry.old_upper_bound);

        pumpkin_assert_moderate!(self.debug_bounds_check());
    }

    fn get_update_info(&self, predicate: &Predicate) -> Option<PairDecisionLevelTrailPosition> {
        // Perhaps the recursion could be done in a cleaner way,
        // e.g., separate functions dependibng on the type of predicate.
        // For the initial version, the current version is okay.
        let domain_id = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                // Recall that by the nature of the updates,
                // the updates are stored in increasing order of the lower bound.

                // find the update with smallest lower bound
                // that is greater than or equal to the input lower bound
                let position = self
                    .lower_bound_updates
                    .partition_point(|u| u.bound < value);

                (position < self.lower_bound_updates.len()).then(|| {
                    let u = &self.lower_bound_updates[position];
                    PairDecisionLevelTrailPosition {
                        decision_level: u.decision_level,
                        trail_position: u.trail_position,
                    }
                })
            }
            PredicateType::UpperBound => {
                // Recall that by the nature of the updates,
                // the updates are stored in decreasing order of the upper bound.

                // find the update with greatest upper bound
                // that is smaller than or equal to the input upper bound
                let position = self
                    .upper_bound_updates
                    .partition_point(|u| u.bound > value);

                (position < self.upper_bound_updates.len()).then(|| {
                    let u = &self.upper_bound_updates[position];
                    PairDecisionLevelTrailPosition {
                        decision_level: u.decision_level,
                        trail_position: u.trail_position,
                    }
                })
            }
            PredicateType::NotEqual => {
                // Check the explictly stored holes.
                // If the value has been removed explicitly,
                // then the stored time is the first time the value was removed.
                if let Some(hole_info) = self.holes.get(&value) {
                    Some(*hole_info)
                } else {
                    // Otherwise, check the case when the lower/upper bound surpassed the value.
                    // If this never happened, then report that the predicate is not true.

                    // Note that it cannot be that both the lower bound and upper bound surpassed
                    // the not equals constant, i.e., at most one of the two may happen.
                    // So we can stop as soon as we find one of the two.

                    // Check the lower bound first.
                    if let Some(trail_position) =
                        self.get_update_info(&predicate!(domain_id >= value + 1))
                    {
                        // The lower bound removed the value from the domain,
                        // report the trail position of the lower bound.
                        Some(trail_position)
                    } else {
                        // The lower bound did not surpass the value,
                        // now check the upper bound.
                        self.get_update_info(&predicate!(domain_id <= value - 1))
                    }
                }
            }
            PredicateType::Equal => {
                // For equality to hold, both the lower and upper bound predicates must hold.
                // Check lower bound first.
                if let Some(lb_trail_position) =
                    self.get_update_info(&predicate!(domain_id >= value))
                {
                    // The lower bound found,
                    // now the check depends on the upper bound.

                    // If both the lower and upper bounds are present,
                    // report the trail position of the bound that was set last.
                    // Otherwise, return that the predicate is not on the trail.
                    self.get_update_info(&predicate!(domain_id <= value))
                        .map(|ub_trail_position| {
                            if lb_trail_position.trail_position > ub_trail_position.trail_position {
                                lb_trail_position
                            } else {
                                ub_trail_position
                            }
                        })
                }
                // If the lower bound is never reached,
                // then surely the equality predicate cannot be true.
                else {
                    None
                }
            }
        }
    }

    /// Returns the holes which were created on the provided decision level.
    pub(crate) fn get_holes_from_decision_level(
        &self,
        decision_level: usize,
    ) -> impl Iterator<Item = i32> + '_ {
        self.hole_updates
            .iter()
            .filter(move |entry| entry.decision_level == decision_level)
            .map(|entry| entry.removed_value)
    }

    /// Returns the holes which were created on the current decision level.
    pub(crate) fn get_holes_from_current_decision_level(
        &self,
        current_decision_level: usize,
    ) -> impl Iterator<Item = i32> + '_ {
        self.hole_updates
            .iter()
            .rev()
            .take_while(move |entry| entry.decision_level == current_decision_level)
            .map(|entry| entry.removed_value)
    }

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    pub(crate) fn get_holes(&self) -> impl Iterator<Item = i32> + '_ {
        self.holes.keys().copied()
    }
}

#[derive(Debug)]
pub(crate) struct IntegerDomainIterator<'a> {
    domain: &'a IntegerDomain,
    current_value: i32,
}

impl IntegerDomainIterator<'_> {
    fn new(domain: &IntegerDomain) -> IntegerDomainIterator<'_> {
        IntegerDomainIterator {
            domain,
            current_value: domain.lower_bound(),
        }
    }
}

impl Iterator for IntegerDomainIterator<'_> {
    type Item = i32;
    fn next(&mut self) -> Option<i32> {
        // We would not expect to iterate through inconsistent domains,
        // although we support trying to do so. Not sure if this is good a idea?
        if self.domain.verify_consistency().is_err() {
            return None;
        }

        // Note that the current value is never a hole. This is guaranteed by 1) having
        // a consistent domain, 2) the iterator starts with the lower bound,
        // and 3) the while loop after this if statement updates the current value
        // to a non-hole value (if there are any left within the bounds).
        let result = if self.current_value <= self.domain.upper_bound() {
            Some(self.current_value)
        } else {
            None
        };

        self.current_value += 1;
        // If the current value is within the bounds, but is not in the domain,
        // linearly look for the next non-hole value.
        while self.current_value <= self.domain.upper_bound()
            && !self.domain.contains(self.current_value)
        {
            self.current_value += 1;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::notifications::DomainEvent;

    #[test]
    fn jump_in_bound_change_lower_and_upper_bound_event_backtrack() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        assignment.increase_decision_level();

        let _ = assignment
            .post_predicate(predicate!(d1 != 1), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 5), None, &mut notification_engine)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, &mut notification_engine);

        let events = notification_engine
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 3);

        assert_contains_events(&events, d1, [DomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [DomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [DomainEvent::Removal]);
    }

    #[test]
    fn jump_in_bound_change_assign_event_backtrack() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        assignment.increase_decision_level();

        let _ = assignment
            .post_predicate(predicate!(d1 != 2), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 3), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 4), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 5), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 1), None, &mut notification_engine)
            .expect_err("empty domain");

        let _ = assignment.synchronise(0, &mut notification_engine);

        let events = notification_engine
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 4);

        assert_contains_events(&events, d1, [DomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [DomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [DomainEvent::Removal]);
        assert_contains_events(&events, d1, [DomainEvent::Assign]);
    }

    #[test]
    fn jump_in_bound_change_upper_bound_event_backtrack() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        assignment.increase_decision_level();

        let _ = assignment
            .post_predicate(predicate!(d1 != 3), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 4), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d1 != 5), None, &mut notification_engine)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, &mut notification_engine);

        let events = notification_engine
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 2);

        assert_contains_events(&events, d1, [DomainEvent::UpperBound]);
        assert_contains_events(&events, d1, [DomainEvent::Removal]);
    }

    #[test]
    fn jump_in_bound_change_lower_bound_event_backtrack() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        assignment.increase_decision_level();

        let _ = assignment
            .remove_value_from_domain(d1, 3, None)
            .expect("non-empty domain");
        let _ = assignment
            .remove_value_from_domain(d1, 2, None)
            .expect("non-empty domain");
        let _ = assignment
            .remove_value_from_domain(d1, 1, None)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, &mut notification_engine);

        let events = notification_engine
            .drain_backtrack_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 2);

        assert_contains_events(&events, d1, [DomainEvent::LowerBound]);
        assert_contains_events(&events, d1, [DomainEvent::Removal]);
    }

    #[test]
    fn lower_bound_change_lower_bound_event() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        let _ = assignment
            .post_predicate(predicate!(d1 >= 2), None, &mut notification_engine)
            .expect("non-empty domain");

        let events = notification_engine
            .drain_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);

        assert_contains_events(&events, d1, [DomainEvent::LowerBound]);
    }

    #[test]
    fn upper_bound_change_triggers_upper_bound_event() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        let _ = assignment
            .post_predicate(predicate!(d1 <= 2), None, &mut notification_engine)
            .expect("non-empty domain");

        let events = notification_engine
            .drain_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert_contains_events(&events, d1, [DomainEvent::UpperBound]);
    }

    #[test]
    fn bounds_change_can_also_trigger_assign_event() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();

        let d1 = assignment.grow(1, 5);
        let d2 = assignment.grow(1, 5);
        notification_engine.grow();
        notification_engine.grow();

        let _ = assignment
            .post_predicate(predicate!(d1 >= 5), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d2 <= 1), None, &mut notification_engine)
            .expect("non-empty domain");

        let events = notification_engine
            .drain_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 4, "expected more than 4 events: {events:?}");

        assert_contains_events(&events, d1, [DomainEvent::LowerBound, DomainEvent::Assign]);
        assert_contains_events(&events, d2, [DomainEvent::UpperBound, DomainEvent::Assign]);
    }

    #[test]
    fn making_assignment_triggers_appropriate_events() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();

        let d1 = assignment.grow(1, 5);
        let d2 = assignment.grow(1, 5);
        let d3 = assignment.grow(1, 5);
        notification_engine.grow();
        notification_engine.grow();
        notification_engine.grow();

        let _ = assignment
            .post_predicate(predicate!(d1 == 1), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d2 == 5), None, &mut notification_engine)
            .expect("non-empty domain");
        let _ = assignment
            .post_predicate(predicate!(d3 == 3), None, &mut notification_engine)
            .expect("non-empty domain");

        let events = notification_engine
            .drain_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 7);

        assert_contains_events(&events, d1, [DomainEvent::Assign, DomainEvent::UpperBound]);
        assert_contains_events(&events, d2, [DomainEvent::Assign, DomainEvent::LowerBound]);
        assert_contains_events(
            &events,
            d3,
            [
                DomainEvent::Assign,
                DomainEvent::LowerBound,
                DomainEvent::UpperBound,
            ],
        );
    }

    #[test]
    fn removal_triggers_removal_event() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        let _ = assignment
            .post_predicate(predicate!(d1 != 2), None, &mut notification_engine)
            .expect("non-empty domain");

        let events = notification_engine
            .drain_domain_events()
            .collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert!(events.contains(&(DomainEvent::Removal, d1)));
    }

    #[test]
    fn value_can_be_removed_from_domains() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(1, 1, 2);

        assert!(domain.contains(2));
        assert!(!domain.contains(1));
    }

    #[test]
    fn removing_the_lower_bound_updates_that_lower_bound() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(1, 1, 1);
        let _ = domain.remove_value(2, 1, 2);

        assert_eq!(3, domain.lower_bound());
    }

    #[test]
    fn removing_the_upper_bound_updates_the_upper_bound() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(4, 0, 1);
        let _ = domain.remove_value(5, 0, 2);

        assert_eq!(3, domain.upper_bound());
    }

    #[test]
    fn an_empty_domain_accepts_removal_operations() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(4, 0, 1);
        let _ = domain.remove_value(1, 0, 2);
        let _ = domain.remove_value(1, 0, 3);
    }

    #[test]
    fn setting_lower_bound_rounds_up_to_nearest_value_in_domain() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(2, 1, 2);
        let _ = domain.remove_value(3, 1, 3);
        let _ = domain.set_lower_bound(2, 1, 4);

        assert_eq!(4, domain.lower_bound());
    }

    #[test]
    fn setting_upper_bound_rounds_down_to_nearest_value_in_domain() {
        let mut domain = IntegerDomain::new(1, 5, DomainId::new(0), 0);
        let _ = domain.remove_value(4, 0, 1);
        let _ = domain.set_upper_bound(4, 0, 2);

        assert_eq!(3, domain.upper_bound());
    }

    #[test]
    fn undo_removal_at_bounds_indexes_into_values_domain_correctly() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let d1 = assignment.grow(1, 5);
        notification_engine.grow();

        assignment.increase_decision_level();

        let _ = assignment
            .post_predicate(predicate!(d1 != 5), None, &mut notification_engine)
            .expect("non-empty domain");

        let _ = assignment.synchronise(0, &mut notification_engine);

        assert_eq!(5, assignment.get_upper_bound(d1));
    }

    fn assert_contains_events(
        slice: &[(DomainEvent, DomainId)],
        domain: DomainId,
        required_events: impl IntoIterator<Item = DomainEvent>,
    ) {
        for event in required_events {
            assert!(slice.contains(&(event, domain)));
        }
    }

    fn get_domain1() -> (DomainId, IntegerDomain) {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 100, domain_id, 0);
        let _ = domain.set_lower_bound(1, 0, 1);
        let _ = domain.set_lower_bound(5, 1, 2);
        let _ = domain.set_lower_bound(10, 2, 10);
        let _ = domain.set_lower_bound(20, 5, 50);
        let _ = domain.set_lower_bound(50, 10, 70);

        (domain_id, domain)
    }

    #[test]
    fn lower_bound_trail_position_inbetween_value() {
        let (domain_id, domain) = get_domain1();

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id >= 12))
                .unwrap()
                .trail_position,
            50
        );
    }

    #[test]
    fn lower_bound_trail_position_last_bound() {
        let (domain_id, domain) = get_domain1();

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id >= 50))
                .unwrap()
                .trail_position,
            70
        );
    }

    #[test]
    fn lower_bound_trail_position_beyond_value() {
        let (domain_id, domain) = get_domain1();

        assert!(
            domain
                .get_update_info(&predicate!(domain_id >= 101))
                .is_none()
        );
    }

    #[test]
    fn lower_bound_trail_position_trivial() {
        let (domain_id, domain) = get_domain1();

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id >= -10))
                .unwrap()
                .trail_position,
            0
        );
    }

    #[test]
    fn lower_bound_trail_position_with_removals() {
        let (domain_id, mut domain) = get_domain1();
        let _ = domain.remove_value(50, 11, 75);
        let _ = domain.remove_value(51, 11, 77);
        let _ = domain.remove_value(52, 11, 80);

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id >= 52))
                .unwrap()
                .trail_position,
            77
        );
    }

    #[test]
    fn removal_trail_position() {
        let (domain_id, mut domain) = get_domain1();
        let _ = domain.remove_value(50, 11, 75);
        let _ = domain.remove_value(51, 11, 77);
        let _ = domain.remove_value(52, 11, 80);

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id != 50))
                .unwrap()
                .trail_position,
            75
        );
    }

    #[test]
    fn removal_trail_position_after_lower_bound() {
        let (domain_id, mut domain) = get_domain1();
        let _ = domain.remove_value(50, 11, 75);
        let _ = domain.remove_value(51, 11, 77);
        let _ = domain.remove_value(52, 11, 80);
        let _ = domain.set_lower_bound(60, 11, 150);

        assert_eq!(
            domain
                .get_update_info(&predicate!(domain_id != 55))
                .unwrap()
                .trail_position,
            150
        );
    }

    #[test]
    fn lower_bound_change_backtrack() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignment = Assignments::default();
        let domain_id1 = assignment.grow(0, 100);
        let domain_id2 = assignment.grow(0, 50);
        notification_engine.grow();
        notification_engine.grow();

        // decision level 1
        assignment.increase_decision_level();
        let _ = assignment
            .post_predicate(predicate!(domain_id1 >= 2), None, &mut notification_engine)
            .expect("");
        let _ = assignment
            .post_predicate(predicate!(domain_id2 >= 25), None, &mut notification_engine)
            .expect("");

        // decision level 2
        assignment.increase_decision_level();
        let _ = assignment
            .post_predicate(predicate!(domain_id1 >= 5), None, &mut notification_engine)
            .expect("");

        // decision level 3
        assignment.increase_decision_level();
        let _ = assignment
            .post_predicate(predicate!(domain_id1 >= 7), None, &mut notification_engine)
            .expect("");

        assert_eq!(assignment.get_lower_bound(domain_id1), 7);

        let _ = assignment.synchronise(1, &mut notification_engine);

        assert_eq!(assignment.get_lower_bound(domain_id1), 2);
    }

    #[test]
    fn lower_bound_inbetween_updates() {
        let (_, domain) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(25), 10);
    }

    #[test]
    fn lower_bound_beyond_trail_position() {
        let (_, domain) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(1000), 50);
    }

    #[test]
    fn lower_bound_at_update() {
        let (_, domain) = get_domain1();
        assert_eq!(domain.lower_bound_at_trail_position(50), 20);
    }

    #[test]
    fn lower_bound_at_trail_position_after_removals() {
        let (_, mut domain) = get_domain1();
        let _ = domain.remove_value(50, 11, 75);
        let _ = domain.remove_value(51, 11, 77);
        let _ = domain.remove_value(52, 11, 80);

        assert_eq!(domain.lower_bound_at_trail_position(77), 52);
    }

    #[test]
    fn lower_bound_at_trail_position_after_removals_and_bound_update() {
        let (_, mut domain) = get_domain1();
        let _ = domain.remove_value(50, 11, 75);
        let _ = domain.remove_value(51, 11, 77);
        let _ = domain.remove_value(52, 11, 80);
        let _ = domain.set_lower_bound(60, 11, 150);

        assert_eq!(domain.lower_bound_at_trail_position(100), 53);
    }

    #[test]
    fn inconsistent_bound_updates() {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 2, domain_id, 0);
        let _ = domain.set_lower_bound(2, 1, 1);
        let _ = domain.set_upper_bound(1, 1, 2);
        assert!(domain.verify_consistency().is_err());
    }

    #[test]
    fn inconsistent_domain_removals() {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 2, domain_id, 0);
        let _ = domain.remove_value(1, 1, 1);
        let _ = domain.remove_value(2, 1, 2);
        let _ = domain.remove_value(0, 1, 3);
        assert!(domain.verify_consistency().is_err());
    }

    #[test]
    fn domain_iterator_simple() {
        let domain_id = DomainId::new(0);
        let domain = IntegerDomain::new(0, 5, domain_id, 0);
        let mut iter = domain.domain_iterator();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn domain_iterator_skip_holes() {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 5, domain_id, 0);
        let _ = domain.remove_value(1, 0, 5);
        let _ = domain.remove_value(4, 0, 10);

        let mut iter = domain.domain_iterator();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn domain_iterator_removed_bounds() {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 5, domain_id, 0);
        let _ = domain.remove_value(0, 0, 1);
        let _ = domain.remove_value(5, 0, 10);

        let mut iter = domain.domain_iterator();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn domain_iterator_removed_values_present_beyond_bounds() {
        let domain_id = DomainId::new(0);
        let mut domain = IntegerDomain::new(0, 10, domain_id, 0);
        let _ = domain.remove_value(7, 0, 1);
        let _ = domain.remove_value(9, 0, 5);
        let _ = domain.remove_value(2, 0, 10);
        let _ = domain.set_upper_bound(6, 1, 10);

        let mut iter = domain.domain_iterator();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), Some(6));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn various_tests_evaluate_predicate() {
        let mut notification_engine = NotificationEngine::default();
        let mut assignments = Assignments::default();
        // Create the domain {0, 1, 3, 4, 5, 6}
        let domain_id = assignments.grow(0, 10);
        notification_engine.grow();

        let _ =
            assignments.post_predicate(predicate!(domain_id != 7), None, &mut notification_engine);
        let _ =
            assignments.post_predicate(predicate!(domain_id != 9), None, &mut notification_engine);
        let _ =
            assignments.post_predicate(predicate!(domain_id != 2), None, &mut notification_engine);
        let _ =
            assignments.post_predicate(predicate!(domain_id <= 6), None, &mut notification_engine);

        let lb_predicate = |lower_bound: i32| -> Predicate { predicate!(domain_id >= lower_bound) };
        let ub_predicate = |upper_bound: i32| -> Predicate { predicate!(domain_id <= upper_bound) };
        let eq_predicate =
            |equality_constant: i32| -> Predicate { predicate!(domain_id == equality_constant) };
        let neq_predicate =
            |not_equal_constant: i32| -> Predicate { predicate!(domain_id != not_equal_constant) };

        assert!(
            assignments
                .evaluate_predicate(lb_predicate(0))
                .is_some_and(|x| x)
        );
        assert!(assignments.evaluate_predicate(lb_predicate(1)).is_none());
        assert!(assignments.evaluate_predicate(lb_predicate(2)).is_none());
        assert!(assignments.evaluate_predicate(lb_predicate(3)).is_none());
        assert!(assignments.evaluate_predicate(lb_predicate(4)).is_none());
        assert!(assignments.evaluate_predicate(lb_predicate(5)).is_none());
        assert!(assignments.evaluate_predicate(lb_predicate(6)).is_none());
        assert!(
            assignments
                .evaluate_predicate(lb_predicate(7))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(lb_predicate(8))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(lb_predicate(9))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(lb_predicate(10))
                .is_some_and(|x| !x)
        );

        assert!(assignments.evaluate_predicate(ub_predicate(0)).is_none());
        assert!(assignments.evaluate_predicate(ub_predicate(1)).is_none());
        assert!(assignments.evaluate_predicate(ub_predicate(2)).is_none());
        assert!(assignments.evaluate_predicate(ub_predicate(3)).is_none());
        assert!(assignments.evaluate_predicate(ub_predicate(4)).is_none());
        assert!(assignments.evaluate_predicate(ub_predicate(5)).is_none());
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(6))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(7))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(8))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(9))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(10))
                .is_some_and(|x| x)
        );

        assert!(assignments.evaluate_predicate(neq_predicate(0)).is_none());
        assert!(assignments.evaluate_predicate(neq_predicate(1)).is_none());
        assert!(
            assignments
                .evaluate_predicate(neq_predicate(2))
                .is_some_and(|x| x)
        );
        assert!(assignments.evaluate_predicate(neq_predicate(3)).is_none());
        assert!(assignments.evaluate_predicate(neq_predicate(4)).is_none());
        assert!(assignments.evaluate_predicate(neq_predicate(5)).is_none());
        assert!(assignments.evaluate_predicate(neq_predicate(6)).is_none());
        assert!(
            assignments
                .evaluate_predicate(neq_predicate(7))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(neq_predicate(8))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(neq_predicate(9))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(neq_predicate(10))
                .is_some_and(|x| x)
        );

        assert!(assignments.evaluate_predicate(eq_predicate(0)).is_none());
        assert!(assignments.evaluate_predicate(eq_predicate(1)).is_none());
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(2))
                .is_some_and(|x| !x)
        );
        assert!(assignments.evaluate_predicate(eq_predicate(3)).is_none());
        assert!(assignments.evaluate_predicate(eq_predicate(4)).is_none());
        assert!(assignments.evaluate_predicate(eq_predicate(5)).is_none());
        assert!(assignments.evaluate_predicate(eq_predicate(6)).is_none());
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(7))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(8))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(9))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(10))
                .is_some_and(|x| !x)
        );

        let _ =
            assignments.post_predicate(predicate!(domain_id >= 6), None, &mut notification_engine);

        assert!(
            assignments
                .evaluate_predicate(neq_predicate(6))
                .is_some_and(|x| !x)
        );
        assert!(
            assignments
                .evaluate_predicate(eq_predicate(6))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(lb_predicate(6))
                .is_some_and(|x| x)
        );
        assert!(
            assignments
                .evaluate_predicate(ub_predicate(6))
                .is_some_and(|x| x)
        );
    }
}
