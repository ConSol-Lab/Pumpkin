use std::fmt::Debug;
use std::fmt::Formatter;
use std::iter::once;
use std::ops::Not;

use log::debug;
use log::warn;

use super::predicates::integer_predicate::IntegerPredicate;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::Assignments;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;

#[derive(Copy, Clone)]
pub struct DebugDyn<'a> {
    trait_name: &'a str,
}

impl<'a> DebugDyn<'a> {
    pub fn from(trait_name: &'a str) -> Self {
        DebugDyn { trait_name }
    }
}

impl<'a> Debug for DebugDyn<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<dyn {}>", self.trait_name)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DebugHelper {}

impl DebugHelper {
    // this method is only to be called after the solver completed propagation until a fixed point
    // and no conflict were detected  the point is to check whether there is a propagation that
    // missed a propagation or failure  additionally checks whether the internal data structures
    // of the clausal propagator are okay and consistent with the assignments_propositional
    pub fn debug_fixed_point_propagation(
        assignments: &Assignments,
        propagators: &[Box<dyn Propagator>],
    ) -> bool {
        let mut assignments_clone = assignments.clone();
        // check whether constraint programming propagators missed anything
        //  ask each propagator to propagate from scratch, and check whether any new propagations
        // took place  if a new propagation took place, then the main propagation loop
        // missed at least one propagation, indicating buggy behaviour  two notes:
        //      1. it could still be that the main propagation loop propagates more than it should
        //         however this will not be detected with this debug check instead such behaviour
        //         may be detected when debug-checking the reason for propagation
        //      2. we assume fixed-point propagation, it could be in the future that this may change
        //  todo expand the output given by the debug check
        for (propagator_id, propagator) in propagators.iter().enumerate() {
            let num_entries_on_trail_before_propagation = assignments_clone.num_trail_entries();

            let mut reason_store = Default::default();
            let mut context = PropagationContextMut::new(
                &mut assignments_clone,
                &mut reason_store,
                PropagatorId(propagator_id as u32),
            );
            let propagation_status_cp = propagator.debug_propagate_from_scratch(&mut context);

            if let Err(ref failure_reason) = propagation_status_cp {
                warn!(
                    "Propagator '{}' with id '{propagator_id}' seems to have missed a conflict in its regular propagation algorithms!
                     Aborting!\n
                     Expected reason: {failure_reason:?}", propagator.name()
                );
                panic!();
            }

            let num_missed_propagations =
                assignments_clone.num_trail_entries() - num_entries_on_trail_before_propagation;

            if num_missed_propagations > 0 {
                eprintln!(
                    "Propagator '{}' with id '{propagator_id}' missed predicates:",
                    propagator.name(),
                );

                for idx in
                    num_entries_on_trail_before_propagation..assignments_clone.num_trail_entries()
                {
                    let trail_entry = assignments_clone.get_trail_entry(idx);
                    let pred = trail_entry.predicate;
                    eprintln!("  - {pred:?}");
                }

                panic!("missed propagations");
            }
        }
        true
    }

    pub fn debug_reported_failure(
        assignments: &Assignments,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) -> bool {
        DebugHelper::debug_reported_propagations_reproduce_failure(
            assignments,
            failure_reason,
            propagator,
            propagator_id,
        );

        DebugHelper::debug_reported_propagations_negate_failure_and_check(
            assignments,
            failure_reason,
            propagator,
            propagator_id,
        );
        true
    }

    pub fn debug_propagator_reason(
        propagated_predicate: IntegerPredicate,
        reason: &PropositionalConjunction,
        assignments: &Assignments,
        propagator: &dyn Propagator,
        propagator_id: u32,
    ) -> bool {
        // todo: commented out the code below, see if it worth in the new version
        // todo: this function is not used anywhere? Why?

        // let reason: PropositionalConjunction = reason
        // .iter()
        // .cloned()
        // .filter(|&predicate| predicate != Predicate::True)
        // .collect();
        //
        // if reason
        // .iter()
        // .any(|&predicate| predicate == Predicate::False)
        // {
        // panic!(
        // "The reason for propagation should not contain the trivially false predicate.
        // Propagator: {},
        // id: {propagator_id},
        // The reported propagation reason: {reason},
        // Propagated predicate: {propagated_predicate}",
        // propagator.name(),
        // );
        // }

        if reason
            .iter()
            .map(|p| p.get_domain())
            .any(|domain| domain == propagated_predicate.get_domain())
        {
            panic!("The reason for propagation should not contain the integer variable that was propagated.
             Propagator: {propagator_id},
             id: {reason},
             The reported propagation reason: {propagated_predicate},
             Propagated predicate: {}",
             propagator.name()
            );
        }

        // Two checks are done.
        // Check #1: Does setting the predicates from the reason indeed lead to the propagation?
        {
            let mut assignments_clone = assignments.debug_create_empty_clone();

            let reason_predicates: Vec<IntegerPredicate> = reason.iter().copied().collect();
            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_clone,
                    &reason_predicates,
                );

            if adding_predicates_was_successful {
                // Now propagate using the debug propagation method.
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_clone,
                    &mut reason_store,
                    PropagatorId(propagator_id),
                );
                let debug_propagation_status_cp =
                    propagator.debug_propagate_from_scratch(&mut context);

                assert!(
                    debug_propagation_status_cp.is_ok(),
                    "Debug propagation detected a conflict when consider a reason for propagation
                     by the propagator '{}' with id '{propagator_id}'.\n
                     The reported reason: {reason}\n
                     Reported propagated predicate: {propagated_predicate}",
                    propagator.name()
                );

                // The predicate was either a propagation for the assignments or
                // assignments_propositional
                assert!(
                    assignments_clone.evaluate_predicate(propagated_predicate).is_some_and(|x| x),
                    "Debug propagation could not obtain the propagated predicate given the provided reason.\n
                     Propagator: '{}'\n
                     Propagator id: {propagator_id}\n
                     Reported reason: {reason}\n
                     Reported propagation: {propagated_predicate}",
                    propagator.name()
                );
            } else {
                // if even adding the predicates failed, the method adding the predicates would have
                // printed debug info already  so we just need to add more
                // information to indicate where the failure happened
                panic!(
                    "Bug detected for '{}' propagator with id '{propagator_id}'
                     after a reason was given by the propagator.",
                    propagator.name()
                );
            }
        }

        // Check #2. Does setting the predicates from reason while having the negated propagated
        // predicate lead to failure?
        // This idea is by Graeme Gange in the context of debugging lazy explanations, and is
        // closely related to reverse constraint propagation.
        {
            let mut assignments_clone = assignments.debug_create_empty_clone();

            let failing_predicates: Vec<IntegerPredicate> = once(!propagated_predicate)
                .chain(reason.iter().copied())
                .collect();

            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_clone,
                    &failing_predicates,
                );

            if adding_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_clone,
                    &mut reason_store,
                    PropagatorId(propagator_id),
                );
                let debug_propagation_status_cp =
                    propagator.debug_propagate_from_scratch(&mut context);

                assert!(
                    debug_propagation_status_cp.is_err(),
                    "Debug propagation could not obtain a failure by setting the reason and negating the propagated predicate.\n
                     Propagator: '{}'\n
                     Propagator id: '{propagator_id}'.\n
                     The reported reason: {reason}\n
                     Reported propagated predicate: {propagated_predicate}",
                    propagator.name()
                );
            } else {
                // if even adding the predicates failed, the method adding the predicates would have
                // printed debug info already  so we just need to add more
                // information to indicate where the failure happened
                panic!(
                    "Bug detected for '{}' propagator with id '{propagator_id}'
                     after trying to negate the reason for propagator.",
                    propagator.name(),
                );
            }
        }
        true
    }

    fn debug_reported_propagations_reproduce_failure(
        assignments: &Assignments,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        let mut assignments_clone = assignments.debug_create_empty_clone();

        let reason_predicates: Vec<IntegerPredicate> = failure_reason.iter().copied().collect();
        let adding_predicates_was_successful =
            DebugHelper::debug_add_predicates_to_assignment_integers(
                &mut assignments_clone,
                &reason_predicates,
            );

        if adding_predicates_was_successful {
            //  now propagate using the debug propagation method
            let mut reason_store = Default::default();
            let mut context = PropagationContextMut::new(
                &mut assignments_clone,
                &mut reason_store,
                propagator_id,
            );
            let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(&mut context);
            assert!(
                debug_propagation_status_cp.is_err(),
                "Debug propagation could not reproduce the conflict reported
                 by the propagator '{}' with id '{propagator_id}'.\n
                 The reported failure: {failure_reason}",
                propagator.name()
            );
        } else {
            // if even adding the predicates failed, the method adding the predicates would have
            // printed debug info already  so we just need to add more information to
            // indicate where the failure happened
            panic!(
                "Bug detected for '{}' propagator with id '{propagator_id}' after a failure reason
                 was given by the propagator.",
                propagator.name()
            );
        }
    }

    fn debug_reported_propagations_negate_failure_and_check(
        assignments: &Assignments,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        // The nogood propagator is special, so the code below does not necessarily hold.
        // This is because the propagator gets updated during solving.
        // For example, x != y with x,y\in{0,1}, and we ask to enumerate all solutions,
        // after adding nogoods to forbid the two solutions, the negation of failure
        // will still be a failure!
        if propagator.name() == "NogoodPropagator" {
            return;
        }

        // let the failure be: (p1 && p2 && p3) -> failure
        //  then (!p1 || !p2 || !p3) should not lead to immediate failure

        // empty reasons are by definition satisifed after negation
        if failure_reason.num_predicates() == 0 {
            return;
        }

        let reason_predicates: Vec<IntegerPredicate> = failure_reason.iter().copied().collect();
        let mut found_nonconflicting_state_at_root = false;
        for predicate in &reason_predicates {
            let mut assignments_clone = assignments.debug_create_empty_clone();

            let negated_predicate = predicate.not();
            let outcome = assignments_clone.post_integer_predicate(negated_predicate, None);

            if outcome.is_ok() {
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_clone,
                    &mut reason_store,
                    propagator_id,
                );
                let debug_propagation_status_cp =
                    propagator.debug_propagate_from_scratch(&mut context);

                if debug_propagation_status_cp.is_ok() {
                    found_nonconflicting_state_at_root = true;
                    break;
                }
            }
        }

        if !found_nonconflicting_state_at_root {
            panic!(
                "Negating the reason for failure was still leading to failure
                 for propagator '{}' with id '{propagator_id}'.\n
                 The reported failure: {failure_reason}\n",
                propagator.name(),
            );
        }
    }
}

// methods that serve as small utility functions
impl DebugHelper {
    fn debug_add_predicates_to_assignment_integers(
        assignments: &mut Assignments,
        predicates: &[IntegerPredicate],
    ) -> bool {
        for integer_predicate in predicates {
            let outcome = assignments.post_integer_predicate(*integer_predicate, None);
            match outcome {
                Ok(()) => {
                    // do nothing, everything is okay
                }
                Err(_) => {
                    // Trivial failure, this is unexpected.
                    // E.g., this can happen if the propagator reported [x >= a] and [x <= a-1].
                    debug!(
                        "Trivial failure detected in the given reason.\n
                         The reported failure: {:?}\n
                         Failure detected after trying to apply '{integer_predicate}'.",
                        predicates
                    );
                    return false;
                }
            }
        }
        true
    }
}
