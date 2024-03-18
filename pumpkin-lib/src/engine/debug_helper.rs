use std::fmt::Debug;
use std::fmt::Formatter;
use std::iter::once;

use log::debug;
use log::warn;

use crate::basic_types::Predicate;
use crate::basic_types::PropositionalConjunction;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::cp::AssignmentsInteger;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsPropositional;
use crate::engine::SATCPMediator;
use crate::engine::SATEngineDataStructures;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_simple;

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
        clausal_propagator: &ClausalPropagatorType,
        assignments_integer: &AssignmentsInteger,
        sat_data_structures: &SATEngineDataStructures,
        propagators_cp: &[Box<dyn Propagator>],
    ) -> bool {
        let mut assignments_integer_clone = assignments_integer.clone();
        let mut assignments_propostional_clone =
            sat_data_structures.assignments_propositional.clone();
        // check whether constraint programming propagators missed anything
        //  ask each propagator to propagate from scratch, and check whether any new propagations
        // took place  if a new propagation took place, then the main propagation loop
        // missed at least one propagation, indicating buggy behaviour  two notes:
        //      1. it could still be that the main propagation loop propagates more than it should
        //         however this will not be detected with this debug check instead such behaviour
        //         may be detected when debug-checking the reason for propagation
        //      2. we assume fixed-point propagation, it could be in the future that this may change
        //  todo expand the output given by the debug check
        for (propagator_id, propagator) in propagators_cp.iter().enumerate() {
            let num_entries_on_trail_before_propagation =
                assignments_integer_clone.num_trail_entries();
            let num_entries_on_propositional_trail_before_propagation =
                assignments_propostional_clone.num_trail_entries();

            let mut reason_store = Default::default();
            let mut context = PropagationContextMut::new(
                &mut assignments_integer_clone,
                &mut reason_store,
                &mut assignments_propostional_clone,
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

            let num_missed_propagations = assignments_integer_clone.num_trail_entries()
                - num_entries_on_trail_before_propagation;

            let num_missed_propositional_propagations = assignments_propostional_clone
                .num_trail_entries()
                - num_entries_on_propositional_trail_before_propagation;

            if num_missed_propagations > 0 {
                eprintln!(
                    "Propagator '{}' with id '{propagator_id}' missed predicates:",
                    propagator.name(),
                );

                for idx in num_entries_on_trail_before_propagation
                    ..assignments_integer_clone.num_trail_entries()
                {
                    let trail_entry = assignments_integer_clone.get_trail_entry(idx);
                    let pred = trail_entry.predicate;
                    eprintln!("  - {pred:?}");
                }

                panic!("missed propagations");
            }
            if num_missed_propositional_propagations > 0 {
                panic!("Missed propositional propagations");
            }
        }
        // then check the clausal propagator
        pumpkin_assert_simple!(clausal_propagator.debug_check_state(
            &sat_data_structures.assignments_propositional,
            &sat_data_structures.clause_allocator
        ));
        true
    }

    pub fn debug_reported_failure(
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        sat_cp_mediator: &SATCPMediator,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) -> bool {
        DebugHelper::debug_reported_propagations_reproduce_failure(
            assignments_integer,
            assignments_propositional,
            sat_cp_mediator,
            failure_reason,
            propagator,
            propagator_id,
        );

        DebugHelper::debug_reported_propagations_negate_failure_and_check(
            assignments_integer,
            assignments_propositional,
            sat_cp_mediator,
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
        assignments_propositional: &AssignmentsPropositional,
        sat_cp_mediator: &SATCPMediator,
        propagator: &dyn Propagator,
        propagator_id: u32,
    ) -> bool {
        let reason: PropositionalConjunction = reason
            .iter()
            .cloned()
            .filter(|&predicate| predicate != Predicate::True)
            .collect();

        if reason
            .iter()
            .any(|&predicate| predicate == Predicate::False)
        {
            panic!(
                "The reason for propagation should not contain the trivially false predicate.
                 Propagator: {},
                 id: {propagator_id},
                 The reported propagation reason: {reason},
                 Propagated predicate: {propagated_predicate}",
                propagator.name(),
            );
        }

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

        // two checks are done
        //  Check #1. Does setting the predicates from the reason indeed lead to the propagation?
        {
            let mut assignments_integer_clone = assignments_integer.debug_create_empty_clone();
            let mut assignments_propositional_clone =
                assignments_propositional.debug_create_empty_clone();

            let reason_predicates: Vec<Predicate> = reason.iter().copied().collect();
            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_integer_clone,
                    &reason_predicates,
                );

            let adding_propositional_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_propositional(
                    &assignments_integer_clone,
                    &mut assignments_propositional_clone,
                    sat_cp_mediator,
                    &reason_predicates,
                    &reason.iter_literals().cloned().collect::<Vec<_>>(),
                );

            if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_integer_clone,
                    &mut reason_store,
                    &mut assignments_propositional_clone,
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

                // The predicate was either a propagation for the assignments_integer or
                // assignments_propositional
                assert!(
                    assignments_integer_clone.does_predicate_hold(propagated_predicate) | assignments_propositional_clone.is_literal_assigned_true(sat_cp_mediator.get_predicate_literal(propagated_predicate, &assignments_integer_clone,)),
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

        //  Check #2. Does setting the predicates from reason while having the negated propagated
        // predicate lead to failure?      this idea is by Graeme Gange in the context of
        // debugging lazy explanations          and is closely related to reverse unit
        // propagation
        {
            let mut assignments_integer_clone = assignments_integer.debug_create_empty_clone();

            let mut assignments_propositional_clone =
                assignments_propositional.debug_create_empty_clone();

            let failing_predicates: Vec<Predicate> = once(!propagated_predicate)
                .chain(reason.iter().copied())
                .collect();

            let adding_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_integers(
                    &mut assignments_integer_clone,
                    &failing_predicates,
                );

            let adding_propositional_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_propositional(
                    &assignments_integer_clone,
                    &mut assignments_propositional_clone,
                    sat_cp_mediator,
                    &failing_predicates,
                    &reason.iter_literals().cloned().collect::<Vec<_>>(),
                );

            if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_integer_clone,
                    &mut reason_store,
                    &mut assignments_propositional_clone,
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
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        sat_cp_mediator: &SATCPMediator,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        let mut assignments_integer_clone = assignments_integer.debug_create_empty_clone();
        let mut assignments_propositional_clone =
            assignments_propositional.debug_create_empty_clone();

        let reason_predicates: Vec<Predicate> = failure_reason.iter().copied().collect();
        let adding_predicates_was_successful =
            DebugHelper::debug_add_predicates_to_assignment_integers(
                &mut assignments_integer_clone,
                &reason_predicates,
            );
        let adding_propositional_predicates_was_successful =
            DebugHelper::debug_add_predicates_to_assignment_propositional(
                &assignments_integer_clone,
                &mut assignments_propositional_clone,
                sat_cp_mediator,
                &reason_predicates,
                &failure_reason.iter_literals().cloned().collect::<Vec<_>>(),
            );

        if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
            //  now propagate using the debug propagation method
            let mut reason_store = Default::default();
            let mut context = PropagationContextMut::new(
                &mut assignments_integer_clone,
                &mut reason_store,
                &mut assignments_propositional_clone,
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
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        sat_cp_mediator: &SATCPMediator,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        // let the failure be: (p1 && p2 && p3) -> failure
        //  then (!p1 || !p2 || !p3) should not lead to immediate failure

        // empty reasons are by definition satisifed after negation
        if failure_reason.num_predicates() == 0 {
            return;
        }

        let reason_predicates: Vec<Predicate> = failure_reason.iter().copied().collect();
        let mut found_nonconflicting_state_at_root = false;
        for predicate in &reason_predicates {
            let mut assignments_integer_clone = assignments_integer.debug_create_empty_clone();
            let mut assignments_propositional_clone =
                assignments_propositional.debug_create_empty_clone();

            let negated_predicate = !*predicate;
            let outcome = assignments_integer_clone.apply_predicate(negated_predicate, None);

            let adding_propositional_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_propositional(
                    &assignments_integer_clone,
                    &mut assignments_propositional_clone,
                    sat_cp_mediator,
                    &reason_predicates,
                    &failure_reason.iter_literals().cloned().collect::<Vec<_>>(),
                );

            if outcome.is_ok() && adding_propositional_predicates_was_successful {
                let mut reason_store = Default::default();
                let mut context = PropagationContextMut::new(
                    &mut assignments_integer_clone,
                    &mut reason_store,
                    &mut assignments_propositional_clone,
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
        assignments_integer: &mut AssignmentsInteger,
        predicates: &[Predicate],
    ) -> bool {
        for predicate in predicates {
            let outcome = assignments_integer.apply_predicate(*predicate, None);
            match outcome {
                Ok(()) => {
                    // do nothing, everything is okay
                }
                Err(_) => {
                    // trivial failure, this is unexpected
                    //  e.g., this can happen if the propagator reported [x >= a] and [x <= a-1]
                    debug!(
                        "Trivial failure detected in the given reason.\n
                         The reported failure: {predicate}\n
                         Failure detected after trying to apply '{predicate}'.",
                    );
                    return false;
                }
            }
        }
        true
    }

    fn debug_add_predicates_to_assignment_propositional(
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &mut AssignmentsPropositional,
        sat_cp_mediator: &SATCPMediator,
        predicates: &[Predicate],
        literals: &[Literal],
    ) -> bool {
        for predicate in predicates {
            let literal = sat_cp_mediator.get_predicate_literal(*predicate, assignments_integer);
            if assignments_propositional.is_literal_assigned_false(literal) {
                debug!(
                    "Trivial failure detected in the given reason.\n
                     The reported failure: {predicate}\n
                     Failure detected after trying to apply '{predicate}'.",
                );
                return false;
            }
            if !assignments_propositional.is_literal_assigned(literal) {
                // It could be the case that the explanation of a failure/propagation contains a
                // predicate which is always true For example, if we have a variable
                // x \in [0..10] and the explanation contains [x >= -1] then this will always
                // evaluate to the true literal However, the true literal is always
                // assigned leading to checks related to this enqueuing failing
                assignments_propositional.enqueue_decision_literal(literal);
            }
        }
        for literal in literals {
            assignments_propositional.enqueue_decision_literal(*literal);
        }
        true
    }
}
