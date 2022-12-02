use crate::{
    basic_types::{
        IntegerVariable, IntegerVariableGeneratorIterator, Predicate, PropagatorIdentifier,
    },
    pumpkin_asserts::*,
};

#[derive(Clone)]
pub struct AssignmentsInteger {
    state: AssignmentsIntegerInternalState,
    current_decision_level: u32,
    trail_delimiter: Vec<u32>, //[i] is the position where the i-th decision level ends (exclusive) on the trail
    trail: Vec<ConstraintProgrammingTrailEntry>,
    domains: Vec<IntegerDomainExplicit>, //[integer_variable.id][j] indicates if value j is in the domain of the integer variable
}

impl AssignmentsInteger {
    pub fn new() -> AssignmentsInteger {
        AssignmentsInteger {
            state: AssignmentsIntegerInternalState::Ok,
            current_decision_level: 0,
            trail: vec![],
            trail_delimiter: Vec::new(),
            domains: vec![],
        }
    }

    pub fn increase_decision_level(&mut self) {
        self.current_decision_level += 1;
        self.trail_delimiter.push(self.trail.len() as u32);
    }

    pub fn get_decision_level(&self) -> u32 {
        self.current_decision_level
    }

    pub fn num_integer_variables(&self) -> u32 {
        self.domains.len() as u32
    }

    pub fn get_integer_variables_variables(&self) -> IntegerVariableGeneratorIterator {
        IntegerVariableGeneratorIterator::new(0, self.num_integer_variables())
    }

    pub fn num_trail_entries(&self) -> usize {
        self.trail.len()
    }

    pub fn get_predicate_on_trail(&self, index: usize) -> Predicate {
        self.trail[index].predicate
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
    //when creating a new integer variable, use create_new_integer_variable in the ConstraintSatisfactionSolver
    pub fn grow(&mut self, lower_bound: i32, upper_bound: i32) -> IntegerVariable {
        self.domains
            .push(IntegerDomainExplicit::new(lower_bound, upper_bound));

        IntegerVariable {
            id: self.num_integer_variables() - 1,
        }
    }

    //todo explain that it can return None
    pub fn get_propagator_identifier_on_trail(
        &self,
        index_on_trail: usize,
    ) -> Option<PropagatorIdentifier> {
        self.trail[index_on_trail].propagator_identifier
    }
}

//methods for getting info about the domains
impl AssignmentsInteger {
    pub fn get_lower_bound(&self, integer_variable: IntegerVariable) -> i32 {
        self.domains[integer_variable].lower_bound
    }

    pub fn get_upper_bound(&self, integer_variable: IntegerVariable) -> i32 {
        self.domains[integer_variable].upper_bound
    }

    pub fn get_assigned_value(&self, integer_variable: IntegerVariable) -> i32 {
        pumpkin_assert_simple!(self.is_integer_variable_assigned(integer_variable));
        self.domains[integer_variable].lower_bound
    }

    pub fn get_lower_bound_predicate(&self, integer_variable: IntegerVariable) -> Predicate {
        Predicate::LowerBound {
            integer_variable,
            lower_bound: self.get_lower_bound(integer_variable),
        }
    }

    pub fn get_upper_bound_predicate(&self, integer_variable: IntegerVariable) -> Predicate {
        let upper_bound = self.get_upper_bound(integer_variable);
        Predicate::UpperBound {
            integer_variable,
            upper_bound,
        }
    }

    pub fn get_lower_bound_predicates(
        &self,
        integer_variables: &[IntegerVariable],
    ) -> Vec<Predicate> {
        integer_variables
            .iter()
            .map(|i| self.get_lower_bound_predicate(*i))
            .collect()
    }

    pub fn get_upper_bound_predicates(
        &self,
        integer_variables: &[IntegerVariable],
    ) -> Vec<Predicate> {
        integer_variables
            .iter()
            .map(|i| self.get_upper_bound_predicate(*i))
            .collect()
    }

    pub fn get_bound_predicates(&self, integer_variables: &[IntegerVariable]) -> Vec<Predicate> {
        self.get_lower_bound_predicates(integer_variables)
            .into_iter()
            .chain(
                self.get_upper_bound_predicates(integer_variables)
                    .into_iter(),
            )
            .collect()
    }

    pub fn is_value_in_domain(&self, integer_variable: IntegerVariable, value: i32) -> bool {
        //recall that the data structure is lazy
        //  so we first need to check whether the value falls within the bounds,
        //  and only then check the is_value_in_domain vector
        self.get_lower_bound(integer_variable) <= value
            && value <= self.get_upper_bound(integer_variable)
            && self.domains[integer_variable].is_value_in_domain[value as usize]
    }

    pub fn is_integer_variable_assigned(&self, integer_variable: IntegerVariable) -> bool {
        self.get_lower_bound(integer_variable) == self.get_upper_bound(integer_variable)
    }

    pub fn is_integer_variable_assigned_to_value(
        &self,
        integer_variable: IntegerVariable,
        value: i32,
    ) -> bool {
        self.is_integer_variable_assigned(integer_variable)
            && self.get_lower_bound(integer_variable) == value
    }
}

//methods to change the domains
impl AssignmentsInteger {
    pub fn tighten_lower_bound_no_notify(
        &mut self,
        integer_variable: IntegerVariable,
        new_lower_bound: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
    ) -> DomainOperationOutcome {
        pumpkin_assert_simple!(
            self.state.is_ok(),
            "Cannot tighten lower bound if state is not ok."
        );
        pumpkin_assert_moderate!(new_lower_bound > self.get_lower_bound(integer_variable));

        let predicate = Predicate::LowerBound {
            integer_variable,
            lower_bound: new_lower_bound,
        };

        if new_lower_bound > self.get_upper_bound(integer_variable) {
            self.state = AssignmentsIntegerInternalState::Conflict {
                conflicting_predicate: predicate,
            };
            return DomainOperationOutcome::Failure;
        }

        let old_lower_bound = self.get_lower_bound(integer_variable);
        let old_upper_bound = self.get_upper_bound(integer_variable);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_identifier,
        });

        self.domains[integer_variable].lower_bound = new_lower_bound;
        DomainOperationOutcome::Success
    }

    pub fn tighten_upper_bound_no_notify(
        &mut self,
        integer_variable: IntegerVariable,
        new_upper_bound: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
    ) -> DomainOperationOutcome {
        pumpkin_assert_simple!(
            self.state.is_ok(),
            "Cannot tighten upper if state is not ok."
        );
        pumpkin_assert_moderate!(new_upper_bound < self.get_upper_bound(integer_variable));

        let predicate = Predicate::UpperBound {
            integer_variable,
            upper_bound: new_upper_bound,
        };

        if new_upper_bound < self.get_lower_bound(integer_variable) {
            self.state = AssignmentsIntegerInternalState::Conflict {
                conflicting_predicate: predicate,
            };
            return DomainOperationOutcome::Failure;
        }

        let old_lower_bound = self.get_lower_bound(integer_variable);
        let old_upper_bound = self.get_upper_bound(integer_variable);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_identifier,
        });

        self.domains[integer_variable].upper_bound = new_upper_bound;
        DomainOperationOutcome::Success
    }

    pub fn make_assignment_no_notify(
        &mut self,
        integer_variable: IntegerVariable,
        assigned_value: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
    ) -> DomainOperationOutcome {
        pumpkin_assert_simple!(
            self.state.is_ok(),
            "Cannot make assignment if state is not ok."
        );
        pumpkin_assert_moderate!(
            !self.is_integer_variable_assigned_to_value(integer_variable, assigned_value)
        );

        if !self.is_value_in_domain(integer_variable, assigned_value) {
            self.state = AssignmentsIntegerInternalState::Conflict {
                conflicting_predicate: Predicate::Equal {
                    integer_variable,
                    equality_constant: assigned_value,
                },
            };
            return DomainOperationOutcome::Failure;
        }

        //only tighten the lower bound if needed
        if self.get_lower_bound(integer_variable) < assigned_value {
            self.tighten_lower_bound_no_notify(
                integer_variable,
                assigned_value,
                propagator_identifier,
            );
        }

        //only tighten the uper bound if needed
        if self.get_upper_bound(integer_variable) > assigned_value {
            self.tighten_upper_bound_no_notify(
                integer_variable,
                assigned_value,
                propagator_identifier,
            );
        }
        DomainOperationOutcome::Success
    }

    pub fn remove_value_from_domain_no_notify(
        &mut self,
        integer_variable: IntegerVariable,
        removed_value_from_domain: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
    ) -> DomainOperationOutcome {
        let predicate = Predicate::NotEqual {
            integer_variable,
            not_equal_constant: removed_value_from_domain,
        };

        if !self.is_value_in_domain(integer_variable, removed_value_from_domain) {
            self.state = AssignmentsIntegerInternalState::Conflict {
                conflicting_predicate: predicate,
            };
            return DomainOperationOutcome::Failure;
        }

        let old_lower_bound = self.get_lower_bound(integer_variable);
        let old_upper_bound = self.get_upper_bound(integer_variable);

        self.trail.push(ConstraintProgrammingTrailEntry {
            predicate,
            old_lower_bound,
            old_upper_bound,
            propagator_identifier,
        });

        let domain = &mut self.domains[integer_variable];

        domain.is_value_in_domain[removed_value_from_domain as usize] = false;

        //adjust the lower bound
        if old_lower_bound == removed_value_from_domain {
            //set the lower bound to the next value
            //  note that the lower bound might increase by more than one, if the values greater than 'not_equal_constant' are also not in the domain
            while domain.is_value_in_domain[domain.lower_bound as usize] {
                domain.lower_bound += 1;

                pumpkin_assert_moderate!(domain.debug_bounds_check());
            }
        }
        //adjust the upper bound
        if old_upper_bound == removed_value_from_domain {
            //set the upper bound to the next value
            //  note that the upper bound might increase by more than one, if the values lower than 'not_equal_constant' are also not in the domain
            while domain.is_value_in_domain[domain.upper_bound as usize] {
                domain.upper_bound -= 1;

                pumpkin_assert_moderate!(domain.debug_bounds_check());
            }
        }
        DomainOperationOutcome::Success
    }

    //changes the domains according to the predicate
    //  in case the predicate is already true, no changes happen
    //  however in case the predicate would lead to inconsistent domains, e.g., decreasing the upper bound past the lower bound
    //      pumpkin asserts will make the program crash
    pub fn apply_predicate_no_notify(
        &mut self,
        predicate: &Predicate,
        propagator_identifier: Option<PropagatorIdentifier>,
    ) -> DomainOperationOutcome {
        pumpkin_assert_simple!(
            self.state.is_ok(),
            "Cannot apply predicate after getting into a bad state."
        );

        if self.does_predicate_hold(predicate) {
            return DomainOperationOutcome::Success;
        }

        match *predicate {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => self.tighten_lower_bound_no_notify(
                integer_variable,
                lower_bound,
                propagator_identifier,
            ),
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => self.tighten_upper_bound_no_notify(
                integer_variable,
                upper_bound,
                propagator_identifier,
            ),
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => self.remove_value_from_domain_no_notify(
                integer_variable,
                not_equal_constant,
                propagator_identifier,
            ),
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => self.make_assignment_no_notify(
                integer_variable,
                equality_constant,
                propagator_identifier,
            ),
        }
    }

    pub fn does_predicate_hold(&self, predicate: &Predicate) -> bool {
        pumpkin_assert_simple!(
            self.state.is_ok(),
            "Cannot evaluate predicate after getting into a bad state."
        );

        match *predicate {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => self.get_lower_bound(integer_variable) >= lower_bound,
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => self.get_upper_bound(integer_variable) <= upper_bound,
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => !self.is_value_in_domain(integer_variable, not_equal_constant),
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => self.is_integer_variable_assigned_to_value(integer_variable, equality_constant),
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
            let integer_variable = popped_entry.predicate.get_integer_variable();

            if let Predicate::NotEqual {
                integer_variable: _,
                not_equal_constant,
            } = popped_entry.predicate
            {
                self.domains[integer_variable].is_value_in_domain[not_equal_constant as usize] =
                    true;
            }

            self.domains[integer_variable].lower_bound = popped_entry.old_lower_bound;
            self.domains[integer_variable].upper_bound = popped_entry.old_upper_bound;

            pumpkin_assert_moderate!(self.domains[integer_variable].debug_bounds_check());
        }
    }

    pub fn synchronise(&mut self, new_decision_level: u32) {
        pumpkin_assert_simple!(new_decision_level < self.current_decision_level);

        let num_trail_entries_to_remove =
            self.trail.len() - self.trail_delimiter[new_decision_level as usize] as usize;

        self.undo_trail(num_trail_entries_to_remove);
        self.current_decision_level = new_decision_level;
        self.trail_delimiter.truncate(new_decision_level as usize);

        if self.is_conflict() {
            self.restore_state_to_ok();
        }
    }

    pub fn is_conflict(&self) -> bool {
        self.state.is_conflict()
    }

    pub fn restore_state_to_ok(&mut self) {
        pumpkin_assert_simple!(self.is_conflict());
        self.state = AssignmentsIntegerInternalState::Ok;
    }
}

#[derive(Clone, Copy)]
pub struct ConstraintProgrammingTrailEntry {
    pub predicate: Predicate,
    pub old_lower_bound: i32, //explicitly store the bound before the predicate was applied so that it is easier later on to update the bounds when backtracking
    pub old_upper_bound: i32,
    pub propagator_identifier: Option<PropagatorIdentifier>, //stores the id of the propagator that made the assignment, only makes sense if a propagation took place, e.g., does _not_ make sense in the case of a decision or if the update was due to synchronisation from the propositional trail
}

#[derive(Clone)]
struct IntegerDomainExplicit {
    lower_bound: i32, //note that even though we only support nonnegative domains, i32 are used over u32 for simplicity
    upper_bound: i32,
    is_value_in_domain: Vec<bool>,
}

impl IntegerDomainExplicit {
    pub fn new(lower_bound: i32, upper_bound: i32) -> IntegerDomainExplicit {
        pumpkin_assert_simple!(
            !lower_bound.is_negative() && !upper_bound.is_negative(),
            "Currently we only support nonnegative domains."
        );
        pumpkin_assert_simple!(
            upper_bound.is_positive(),
            "In principle we could allocate integers with upper bound zero,
            but for now we consider this an error."
        );

        let mut is_value_in_domain = vec![true; (upper_bound + 1) as usize];

        //values outside of the domain need to be marked as false
        for value in is_value_in_domain.iter_mut().take(lower_bound as usize) {
            *value = false;
        }

        IntegerDomainExplicit {
            lower_bound,
            upper_bound,
            is_value_in_domain,
        }
    }

    fn debug_bounds_check(&self) -> bool {
        self.lower_bound <= self.upper_bound
            && (self.lower_bound as usize) < self.is_value_in_domain.len()
            && (self.upper_bound as usize) < self.is_value_in_domain.len()
            && self.is_value_in_domain[self.lower_bound as usize] //the lower and upper bound value should at least be in the is_value_in_domain
            && self.is_value_in_domain[self.upper_bound as usize]
    }

    fn debug_is_domain_empty(&self) -> bool {
        !self.is_value_in_domain.contains(&true)
    }
}

#[derive(Clone)]
enum AssignmentsIntegerInternalState {
    Ok,
    Conflict { conflicting_predicate: Predicate },
}

impl AssignmentsIntegerInternalState {
    pub fn is_ok(&self) -> bool {
        matches!(*self, AssignmentsIntegerInternalState::Ok)
    }

    pub fn is_conflict(&self) -> bool {
        matches!(
            *self,
            AssignmentsIntegerInternalState::Conflict {
                conflicting_predicate: _
            }
        )
    }
}

pub enum DomainOperationOutcome {
    Success,
    Failure,
}
