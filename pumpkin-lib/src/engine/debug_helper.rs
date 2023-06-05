use log::{debug, warn};
use std::iter::once;

use crate::{
    basic_types::{Predicate, PropagationStatusCP, PropagatorIdentifier, PropositionalConjunction},
    engine::cp::DomainManager,
    propagators::ConstraintProgrammingPropagator,
    pumpkin_assert_eq_simple, pumpkin_assert_simple,
};

use super::{
    cp::{AssignmentsInteger, DomainOperationOutcome},
    SATEngineDataStructures,
};

pub struct DebugHelper {}

impl DebugHelper {
    //this method is only to be called after the solver completed propagation until a fixed point and no conflict were detected
    //  the point is to check whether there is a propagation that missed a propagation or failure
    //  additionally checks whether the internal data structures of the clausal propagator are okay and consistent with the assignments_propositional
    pub fn debug_fixed_point_propagation(
        assignments_integer: &AssignmentsInteger,
        sat_data_structures: &SATEngineDataStructures,
        propagators_cp: &[Box<dyn ConstraintProgrammingPropagator>],
    ) -> bool {
        let mut assignments_integer_clone = assignments_integer.clone();
        //check whether constraint programming propagators missed anything
        //  ask each propagator to propagate from scratch, and check whether any new propagations took place
        //  if a new propagation took place, then the main propagation loop missed at least one propagation, indicating buggy behaviour
        //  two notes:
        //      1. it could still be that the main propagation loop propagates more than it should
        //          however this will not be detected with this debug check
        //          instead such behaviour may be detected when debug-checking the reason for propagation
        //      2. we assume fixed-point propagation, it could be in the future that this may change
        //  todo expand the output given by the debug check
        for propagator in propagators_cp.iter().enumerate() {
            let num_entries_on_trail_before_propagation =
                assignments_integer_clone.num_trail_entries();

            let mut domains = DomainManager::new(propagator.0, &mut assignments_integer_clone);
            let propagation_status_cp = propagator.1.debug_propagate_from_scratch(&mut domains);

            if let PropagationStatusCP::ConflictDetected { ref failure_reason } =
                propagation_status_cp
            {
                warn!("Propagator '{}' with id '{}' seems to have missed a conflict in its regular propagation algorithms! Aborting!\nExpected reason: {}", propagator.1.name(), propagator.0, failure_reason);
                panic!();
            }

            let num_missed_propagations = assignments_integer_clone.num_trail_entries()
                - num_entries_on_trail_before_propagation;
            pumpkin_assert_eq_simple!(num_missed_propagations, 0,
                "Propagator '{}' with id '{}' propagated {} predicates after calling debug_propagate_from_scratch, meaning it missed propagations in its regular 'propagate' method. 
                    Aborting!", propagator.1.name(), propagator.0, num_missed_propagations);
        }
        //then check the clausal propagator
        pumpkin_assert_simple!(sat_data_structures.clausal_propagator.debug_check_state(
            &sat_data_structures.assignments_propositional,
            &sat_data_structures.clause_allocator
        ));
        true
    }

    pub fn debug_reported_failure(
        assignments_integer: &AssignmentsInteger,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_id: PropagatorIdentifier,
    ) -> bool {
        DebugHelper::debug_reported_propagations_reproduce_failure(
            assignments_integer,
            failure_reason,
            propagator,
            propagator_id,
        );

        DebugHelper::debug_reported_propagations_negate_failure_and_check(
            assignments_integer,
            failure_reason,
            propagator,
            propagator_id,
        );
        true
    }

    pub fn debug_propagator_reason(
        propagated_predicate: Predicate,
        reason: &PropositionalConjunction,
        assignments_integer: &AssignmentsInteger,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_id: PropagatorIdentifier,
    ) -> bool {
        if reason
            .clone()
            .into_iter()
            .map(|p| p.get_integer_variable())
            .any(|integer_variable| integer_variable == propagated_predicate.get_integer_variable())
        {
            panic!("{}", format!("The reason for propagation should not contain the integer variable that was propagated.
            Propagator: {},    
            id: {},
            The reported propagation reason: {},
            Propagated predicate: {}", propagator.name(), propagator_id.id, reason, propagated_predicate));
        }

        //two checks are done
        //  Check #1. Does setting the predicates from the reason indeed lead to the propagation?
        {
            let mut assignments_integer_clone =
                DebugHelper::debug_create_empty_assignment_integers_clone(assignments_integer);

            let reason_predicates: Vec<Predicate> = reason.clone().into_iter().collect();
            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_integer_clone,
                    &reason_predicates,
                );

            if adding_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut domains =
                    DomainManager::new(propagator_id.id as usize, &mut assignments_integer_clone);
                let debug_propagation_status_cp =
                    propagator.debug_propagate_from_scratch(&mut domains);

                assert!(
                    debug_propagation_status_cp.no_conflict(),
                    "{}",
                    format!("Debug propagation detected a conflict when consider a reason for propagation by the propagator '{}' with id '{}'.\nThe reported reason: {}\nReported propagated predicate: {}", propagator.name(), propagator_id.id, reason, propagated_predicate));

                assert!(
                    assignments_integer_clone.does_predicate_hold(&propagated_predicate),
                    "{}",
                    format!("Debug propagation could not obtain the propagated predicate given the provided reason.\nPropagator: '{}'\nPropagator id: {}\nReported reason: {}\nReported propagation: {}", propagator.name(), propagator_id.id, reason, propagated_predicate)
                );
            } else {
                //if even adding the predicates failed, the method adding the predicates would have printed debug info already
                //  so we just need to add more information to indicate where the failure happened
                panic!("{}", format!("Bug detected for '{}' propagator with id '{}' after a reason was given by the propagator.", propagator.name(), propagator_id.id));
            }
        }

        //  Check #2. Does setting the predicates from reason while having the negated propagated predicate lead to failure?
        //      this idea is by Graeme Gange in the context of debugging lazy explanations
        //          and is closely related to reverse unit propagation
        {
            let mut assignments_integer_clone =
                DebugHelper::debug_create_empty_assignment_integers_clone(assignments_integer);

            let failing_predicates: Vec<Predicate> = once(!propagated_predicate)
                .chain(reason.clone().into_iter())
                .collect();

            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_integer_clone,
                    &failing_predicates,
                );

            if adding_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut domains =
                    DomainManager::new(propagator_id.id as usize, &mut assignments_integer_clone);
                let debug_propagation_status_cp =
                    propagator.debug_propagate_from_scratch(&mut domains);

                assert!(
                    debug_propagation_status_cp.conflict_detected(),
                    "{}",
                    format!("Debug propagation could not obtain a failure by setting the reason and negating the propagated predicate.\nPropagator: '{}'\nPropagator id: '{}'.\nThe reported reason: {}\nReported propagated predicate: {}", propagator.name(), propagator_id.id, reason, propagated_predicate)
                );
            } else {
                //if even adding the predicates failed, the method adding the predicates would have printed debug info already
                //  so we just need to add more information to indicate where the failure happened
                panic!("{}", format!("Bug detected for '{}' propagator with id '{}' after trying to negate the reason for propagator.", propagator.name(), propagator_id.id));
            }
        }
        true
    }

    fn debug_reported_propagations_reproduce_failure(
        assignments_integer: &AssignmentsInteger,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_id: PropagatorIdentifier,
    ) {
        let mut assignments_integer_clone =
            DebugHelper::debug_create_empty_assignment_integers_clone(assignments_integer);

        let reason_predicates: Vec<Predicate> = failure_reason.clone().into_iter().collect();
        let adding_predicates_was_successful =
            DebugHelper::debug_add_predicates_to_assignment_integers(
                &mut assignments_integer_clone,
                &reason_predicates,
            );

        if adding_predicates_was_successful {
            //  now propagate using the debug propagation method
            let mut domains =
                DomainManager::new(propagator_id.id as usize, &mut assignments_integer_clone);
            let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(&mut domains);

            assert!(debug_propagation_status_cp.conflict_detected(), "{}", format!("Debug propagation could not reproduce the conflict reported by the propagator '{}' with id '{}'.\nThe reported failure: {}", propagator.name(), propagator_id.id, failure_reason));
        } else {
            //if even adding the predicates failed, the method adding the predicates would have printed debug info already
            //  so we just need to add more information to indicate where the failure happened
            panic!("{}", format!("Bug detected for '{}' propagator with id '{}' after a failure reason was given by the propagator.", propagator.name(), propagator_id.id));
        }
    }

    fn debug_reported_propagations_negate_failure_and_check(
        assignments_integer: &AssignmentsInteger,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_id: PropagatorIdentifier,
    ) {
        //let the failure be: (p1 && p2 && p3) -> failure
        //  then (!p1 || !p2 || !p3) should not lead to immediate failure

        //empty reasons are by definition satisifed after negation
        if failure_reason.num_predicates() == 0 {
            return;
        }

        let reason_predicates: Vec<Predicate> = failure_reason.clone().into_iter().collect();
        let mut found_nonconflicting_state_at_root = false;
        for predicate in &reason_predicates {
            let mut assignments_integer_clone =
                DebugHelper::debug_create_empty_assignment_integers_clone(assignments_integer);

            let negated_predicate = !*predicate;
            let outcome =
                assignments_integer_clone.apply_predicate_no_notify(&negated_predicate, None);

            match outcome {
                DomainOperationOutcome::Success => {
                    let mut domains = DomainManager::new(
                        propagator_id.id as usize,
                        &mut assignments_integer_clone,
                    );
                    let debug_propagation_status_cp =
                        propagator.debug_propagate_from_scratch(&mut domains);

                    if debug_propagation_status_cp.no_conflict() {
                        found_nonconflicting_state_at_root = true;
                        break;
                    }
                }
                DomainOperationOutcome::Failure => {
                    //do nothing
                }
            }
        }
        if !found_nonconflicting_state_at_root {
            panic!(
                "Negating the reason for failure was still leading to failure for propagator '{}' with id '{}'.\n
                        The reported failure: {}\n",
                        propagator.name(),
                        propagator_id.id,
                        failure_reason
            );
        }
    }
}

//methods that serve as small utility functions
impl DebugHelper {
    fn debug_create_empty_assignment_integers_clone(
        assignments_integer: &AssignmentsInteger,
    ) -> AssignmentsInteger {
        let mut assignments_integer_clone = assignments_integer.clone();
        let num_trail_entries = assignments_integer_clone.num_trail_entries();
        assignments_integer_clone.undo_trail(num_trail_entries);
        assignments_integer_clone
    }

    fn debug_add_predicates_to_assignment_integers(
        assignments_integer: &mut AssignmentsInteger,
        predicates: &[Predicate],
    ) -> bool {
        for predicate in predicates {
            let outcome = assignments_integer.apply_predicate_no_notify(predicate, None);
            match outcome {
                DomainOperationOutcome::Success => {
                    //do nothing, everything is okay
                }
                DomainOperationOutcome::Failure => {
                    //trivial failure, this is unexpected
                    //  e.g., this can happen if the propagator reported [x >= a] and [x <= a-1]
                    debug!(
                        "Trivial failure detected in the given reason.\n
                                The reported failure: {}\n
                                Failure detected after trying to apply '{}'.",
                        predicate, predicate
                    );
                    return false;
                }
            }
        }
        true
    }
}
