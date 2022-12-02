use crate::{
    arguments::ArgumentHandler,
    basic_types::{IntegerVariable, Predicate, PropagatorIdentifier},
    propagators::ConstraintProgrammingPropagator,
};

use super::{AssignmentsInteger, DomainOperationOutcome, PropagatorQueue, WatchListCP};

pub struct CPEngineDataStructures {
    pub assignments_integer: AssignmentsInteger,
    pub watch_list_cp: WatchListCP,
    pub propagator_queue: PropagatorQueue,
}

impl CPEngineDataStructures {
    pub fn new(_argument_handler: &ArgumentHandler) -> CPEngineDataStructures {
        CPEngineDataStructures {
            assignments_integer: AssignmentsInteger::new(),
            watch_list_cp: WatchListCP::new(),
            propagator_queue: PropagatorQueue::new(5),
        }
    }

    pub fn backtrack(&mut self, backtrack_level: u32) {
        self.assignments_integer.synchronise(backtrack_level);
        self.propagator_queue.clear();
    }
}

//methods for motifying the domains of variables
//  note that modifying the domain will inform propagators about the changes through the notify functions
impl CPEngineDataStructures {
    pub fn tighten_lower_bound(
        &mut self,
        integer_variable: IntegerVariable,
        new_lower_bound: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> DomainOperationOutcome {
        let old_lower_bound = self.assignments_integer.get_lower_bound(integer_variable);

        let outcome = self.assignments_integer.tighten_lower_bound_no_notify(
            integer_variable,
            new_lower_bound,
            propagator_identifier,
        );

        match outcome {
            DomainOperationOutcome::Success => {
                self.watch_list_cp
                    .notify_lower_bound_subscribed_propagators(
                        integer_variable,
                        old_lower_bound,
                        new_lower_bound,
                        cp_propagators,
                        &mut self.propagator_queue,
                        &mut self.assignments_integer,
                    );
                DomainOperationOutcome::Success
            }
            DomainOperationOutcome::Failure => DomainOperationOutcome::Failure,
        }
    }

    pub fn tighten_upper_bound(
        &mut self,
        integer_variable: IntegerVariable,
        new_upper_bound: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> DomainOperationOutcome {
        let old_upper_bound = self.assignments_integer.get_upper_bound(integer_variable);

        let outcome = self.assignments_integer.tighten_upper_bound_no_notify(
            integer_variable,
            new_upper_bound,
            propagator_identifier,
        );

        match outcome {
            DomainOperationOutcome::Success => {
                self.watch_list_cp
                    .notify_upper_bound_subscribed_propagators(
                        integer_variable,
                        old_upper_bound,
                        new_upper_bound,
                        cp_propagators,
                        &mut self.propagator_queue,
                        &mut self.assignments_integer,
                    );
                DomainOperationOutcome::Success
            }
            DomainOperationOutcome::Failure => DomainOperationOutcome::Failure,
        }
    }

    pub fn make_assignment(
        &mut self,
        integer_variable: IntegerVariable,
        assigned_value: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> DomainOperationOutcome {
        let old_lower_bound = self.assignments_integer.get_lower_bound(integer_variable);
        let old_upper_bound = self.assignments_integer.get_upper_bound(integer_variable);

        let outcome = self.assignments_integer.make_assignment_no_notify(
            integer_variable,
            assigned_value,
            propagator_identifier,
        );

        let new_lower_bound = self.assignments_integer.get_lower_bound(integer_variable);
        let new_upper_bound = self.assignments_integer.get_upper_bound(integer_variable);

        match outcome {
            DomainOperationOutcome::Success => {
                if old_lower_bound < new_lower_bound {
                    self.watch_list_cp
                        .notify_lower_bound_subscribed_propagators(
                            integer_variable,
                            old_lower_bound,
                            new_lower_bound,
                            cp_propagators,
                            &mut self.propagator_queue,
                            &mut self.assignments_integer,
                        );
                }

                if old_upper_bound > new_upper_bound {
                    self.watch_list_cp
                        .notify_upper_bound_subscribed_propagators(
                            integer_variable,
                            old_upper_bound,
                            new_upper_bound,
                            cp_propagators,
                            &mut self.propagator_queue,
                            &mut self.assignments_integer,
                        );
                }
                DomainOperationOutcome::Success
            }
            DomainOperationOutcome::Failure => DomainOperationOutcome::Failure,
        }
    }

    pub fn remove_value_from_domain(
        &mut self,
        integer_variable: IntegerVariable,
        removed_value_from_domain: i32,
        propagator_identifier: Option<PropagatorIdentifier>,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> DomainOperationOutcome {
        let old_lower_bound = self.assignments_integer.get_lower_bound(integer_variable);
        let old_upper_bound = self.assignments_integer.get_upper_bound(integer_variable);

        let outcome = self.assignments_integer.remove_value_from_domain_no_notify(
            integer_variable,
            removed_value_from_domain,
            propagator_identifier,
        );

        let new_lower_bound = self.assignments_integer.get_lower_bound(integer_variable);
        let new_upper_bound = self.assignments_integer.get_upper_bound(integer_variable);

        match outcome {
            DomainOperationOutcome::Success => {
                //...if the inequality operation was a lower bound change
                if old_lower_bound < new_lower_bound {
                    self.watch_list_cp
                        .notify_lower_bound_subscribed_propagators(
                            integer_variable,
                            old_lower_bound,
                            new_lower_bound,
                            cp_propagators,
                            &mut self.propagator_queue,
                            &mut self.assignments_integer,
                        );
                }
                //...if the inequality operation was an upper bound change
                else if old_upper_bound > new_upper_bound {
                    self.watch_list_cp
                        .notify_upper_bound_subscribed_propagators(
                            integer_variable,
                            old_upper_bound,
                            new_upper_bound,
                            cp_propagators,
                            &mut self.propagator_queue,
                            &mut self.assignments_integer,
                        );
                }
                //...otherwise the operation created a hole in the domain
                else {
                    self.watch_list_cp.notify_hole_subscribed_propagators(
                        integer_variable,
                        removed_value_from_domain,
                        cp_propagators,
                        &mut self.propagator_queue,
                        &mut self.assignments_integer,
                    );
                }
                DomainOperationOutcome::Success
            }
            DomainOperationOutcome::Failure => DomainOperationOutcome::Failure,
        }
    }

    //changes the domains according to the predicate
    //  in case the predicate is already true, no changes happen
    //  however in case the predicate would lead to inconsistent domains, e.g., decreasing the upper bound past the lower bound
    //      pumpkin asserts will make the program crash
    pub fn apply_predicate(
        &mut self,
        predicate: &Predicate,
        propagator_identifier: Option<PropagatorIdentifier>,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> DomainOperationOutcome {
        if self.does_predicate_hold(predicate) {
            return DomainOperationOutcome::Success;
        }

        match *predicate {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => self.tighten_lower_bound(
                integer_variable,
                lower_bound,
                propagator_identifier,
                cp_propagators,
            ),
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => self.tighten_upper_bound(
                integer_variable,
                upper_bound,
                propagator_identifier,
                cp_propagators,
            ),
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => self.remove_value_from_domain(
                integer_variable,
                not_equal_constant,
                propagator_identifier,
                cp_propagators,
            ),
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => self.make_assignment(
                integer_variable,
                equality_constant,
                propagator_identifier,
                cp_propagators,
            ),
        }
    }

    pub fn does_predicate_hold(&self, predicate: &Predicate) -> bool {
        self.assignments_integer.does_predicate_hold(predicate)
    }
}
