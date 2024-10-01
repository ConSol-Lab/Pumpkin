use std::fmt::Debug;
use std::fmt::Formatter;
use std::iter::once;

use log::debug;
use log::warn;

use super::predicates::integer_predicate::IntegerPredicate;
use super::propagation::store::PropagatorStore;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::cp::AssignmentsInteger;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::AssignmentsPropositional;
use crate::engine::VariableLiteralMappings;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_simple;

#[derive(Copy, Clone)]
pub(crate) struct DebugDyn<'a> {
    trait_name: &'a str,
}

impl<'a> DebugDyn<'a> {
    pub(crate) fn from(trait_name: &'a str) -> Self {
        DebugDyn { trait_name }
    }
}

impl<'a> Debug for DebugDyn<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<dyn {}>", self.trait_name)
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct DebugHelper {}

impl DebugHelper {
    /// Method which checks whether the reported fixed point is correct (i.e. whether any
    /// propagations/conflicts were missed)
    ///
    /// This method is only to be called after the solver completed propagation until a fixed point
    /// and no conflict was detected
    ///
    /// Additionally checks whether the internal data structures of the clausal propagator are okay
    /// and consistent with the assignments_propositional
    pub(crate) fn debug_fixed_point_propagation(
        clausal_propagator: &ClausalPropagatorType,
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        clause_allocator: &ClauseAllocator,
        propagators_cp: &PropagatorStore,
    ) -> bool {
        let mut assignments_integer_clone = assignments_integer.clone();
        let mut assignments_propostional_clone = assignments_propositional.clone();
        // Check whether constraint programming propagators missed anything
        //
        //  It works by asking each propagator to propagate from scratch, and checking whether any
        // new propagations  took place
        //
        //  If a new propagation took place, then the main propagation loop
        //  missed at least one propagation, indicating buggy behaviour
        //
        //  Two notes:
        //      1. It could still be that the main propagation loop propagates more than it should.
        //         However this will not be detected with this debug check instead such behaviour
        //         may be detected when debug-checking the reason for propagation
        //      2. we assume fixed-point propagation, it could be in the future that this may change
        //  todo expand the output given by the debug check
        for (propagator_id, propagator) in propagators_cp.iter_propagators().enumerate() {
            let num_entries_on_trail_before_propagation =
                assignments_integer_clone.num_trail_entries();
            let num_entries_on_propositional_trail_before_propagation =
                assignments_propostional_clone.num_trail_entries();

            let mut reason_store = Default::default();
            let context = PropagationContextMut::new(
                &mut assignments_integer_clone,
                &mut reason_store,
                &mut assignments_propostional_clone,
                PropagatorId(propagator_id.try_into().unwrap()),
            );
            let propagation_status_cp = propagator.debug_propagate_from_scratch(context);

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
        // Then we check the state of the clausal propagator
        pumpkin_assert_simple!(
            clausal_propagator.debug_check_state(assignments_propositional, clause_allocator)
        );
        true
    }

    pub(crate) fn debug_reported_failure(
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        variable_literal_mappings: &VariableLiteralMappings,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) -> bool {
        DebugHelper::debug_reported_propagations_reproduce_failure(
            assignments_integer,
            assignments_propositional,
            variable_literal_mappings,
            failure_reason,
            propagator,
            propagator_id,
        );

        DebugHelper::debug_reported_propagations_negate_failure_and_check(
            assignments_integer,
            assignments_propositional,
            variable_literal_mappings,
            failure_reason,
            propagator,
            propagator_id,
        );
        true
    }

    pub(crate) fn debug_propagator_reason(
        propagated_predicate: Predicate,
        reason: &PropositionalConjunction,
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        variable_literal_mappings: &VariableLiteralMappings,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
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

        // Note that it could be the case that the reason contains the trivially false predicate in
        // case of lifting!
        //
        // Also note that the reason could contain the integer variable whose domain is propagated
        // itself

        // Two checks are done
        //
        // Check #1
        // Does setting the predicates from the reason indeed lead to the propagation?
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
                    variable_literal_mappings,
                    &reason_predicates,
                );

            if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
                //  Now propagate using the debug propagation method
                let mut reason_store = Default::default();
                let context = PropagationContextMut::new(
                    &mut assignments_integer_clone,
                    &mut reason_store,
                    &mut assignments_propositional_clone,
                    propagator_id,
                );
                let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(context);

                // Note that it could be the case that the propagation leads to conflict, in this
                // case it should be the result of a propagation (i.e. an EmptyDomain)
                if let Err(conflict) = debug_propagation_status_cp {
                    assert!(
                        matches!(conflict, Inconsistency::EmptyDomain),
                        "Debug propagation detected a conflict other than a propagation"
                    );
                }

                // The predicate was either a propagation for the assignments_integer or
                // assignments_propositional
                assert!(
                    (propagated_predicate.is_integer_predicate() && assignments_integer_clone.does_integer_predicate_hold(propagated_predicate.try_into().unwrap()))
                    || (!propagated_predicate.is_integer_predicate() && assignments_propositional_clone.is_literal_assigned_true(propagated_predicate.get_literal_of_bool_predicate(assignments_propositional_clone.true_literal).unwrap())),
                    "Debug propagation could not obtain the propagated predicate given the provided reason.\n
                     Propagator: '{}'\n
                     Propagator id: {propagator_id}\n
                     Reported reason: {reason}\n
                     Reported propagation: {propagated_predicate}",
                    propagator.name()
                );
            } else {
                // Adding the predicates of the reason to the assignments led to failure
                panic!(
                    "Bug detected for '{}' propagator with id '{propagator_id}'
                     after a reason was given by the propagator. This could indicate that the reason contained conflicting predicates.",
                    propagator.name()
                );
            }
        }

        // Check #2
        // Does setting the predicates from reason while having the negated propagated predicate
        // lead to failure?
        //
        // This idea is by Graeme Gange in the context of debugging lazy explanations and is closely
        // related to reverse unit propagation
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
                    variable_literal_mappings,
                    &failing_predicates,
                );

            if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut reason_store = Default::default();

                // Note that it might take multiple iterations before the conflict is reached due
                // to the assumption that some propagators make on that they are not idempotent!
                //
                // This happened in the cumulative where setting the reason led to a new mandatory
                // part being created which meant that the same propagation was not performed (i.e.
                // it did not immediately lead to a conflict) but this new mandatory part would
                // have led to a new mandatory part in the next call to the propagator
                loop {
                    let num_predicates_before = assignments_integer_clone.num_trail_entries();

                    let context = PropagationContextMut::new(
                        &mut assignments_integer_clone,
                        &mut reason_store,
                        &mut assignments_propositional_clone,
                        propagator_id,
                    );
                    let debug_propagation_status_cp =
                        propagator.debug_propagate_from_scratch(context);

                    // We break if an error was found or if there were no more propagations (i.e.
                    // fixpoint was reached)
                    if debug_propagation_status_cp.is_err()
                        || num_predicates_before != assignments_integer_clone.num_trail_entries()
                    {
                        assert!(
                            debug_propagation_status_cp.is_err(),
                            "Debug propagation could not obtain a failure by setting the reason and negating the propagated predicate.\n
                             Propagator: '{}'\n
                             Propagator id: '{propagator_id}'.\n
                             The reported reason: {reason}\n
                             Reported propagated predicate: {propagated_predicate}",
                            propagator.name()
                        );

                        break;
                    }
                }
            } else {
                // Adding the predicates of the reason to the assignments led to failure
                panic!(
                    "Bug detected for '{}' propagator with id '{propagator_id}'
                     after a reason was given by the propagator. This could indicate that the reason contained conflicting predicates.",
                    propagator.name(),
                );
            }
        }
        true
    }

    fn debug_reported_propagations_reproduce_failure(
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        variable_literal_mappings: &VariableLiteralMappings,
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
                variable_literal_mappings,
                &reason_predicates,
            );

        if adding_predicates_was_successful && adding_propositional_predicates_was_successful {
            //  now propagate using the debug propagation method
            let mut reason_store = Default::default();
            let context = PropagationContextMut::new(
                &mut assignments_integer_clone,
                &mut reason_store,
                &mut assignments_propositional_clone,
                propagator_id,
            );
            let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(context);
            assert!(
                debug_propagation_status_cp.is_err(),
                "Debug propagation could not reproduce the conflict reported
                 by the propagator '{}' with id '{propagator_id}'.\n
                 The reported failure: {failure_reason}",
                propagator.name()
            );
        } else {
            // Adding the predicates of the reason to the assignments led to failure
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
        variable_literal_mappings: &VariableLiteralMappings,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        // Let the failure be: (p1 /\ p2 /\ p3) -> failure
        // Then (!p1 || !p2 || !p3) should not lead to immediate failure

        // Empty reasons are by definition satisifed after negation
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
            let outcome = if let Ok(integer_predicate) =
                <Predicate as TryInto<IntegerPredicate>>::try_into(negated_predicate)
            {
                assignments_integer_clone.apply_integer_predicate(integer_predicate, None)
            } else {
                // Will be handled when adding the predicates to the assignments propositional
                Ok(())
            };

            let adding_propositional_predicates_was_successful =
                DebugHelper::debug_add_predicates_to_assignment_propositional(
                    &assignments_integer_clone,
                    &mut assignments_propositional_clone,
                    variable_literal_mappings,
                    &reason_predicates,
                );

            if outcome.is_ok() && adding_propositional_predicates_was_successful {
                let mut reason_store = Default::default();
                let context = PropagationContextMut::new(
                    &mut assignments_integer_clone,
                    &mut reason_store,
                    &mut assignments_propositional_clone,
                    propagator_id,
                );
                let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(context);

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

/// Methods that serve as small utility functions
impl DebugHelper {
    fn debug_add_predicates_to_assignment_integers(
        assignments_integer: &mut AssignmentsInteger,
        predicates: &[Predicate],
    ) -> bool {
        for predicate in predicates {
            if let Ok(integer_predicate) =
                <Predicate as TryInto<IntegerPredicate>>::try_into(*predicate)
            {
                let outcome = assignments_integer.apply_integer_predicate(integer_predicate, None);
                match outcome {
                    Ok(()) => {
                        // Do nothing, everything is okay
                    }
                    Err(_) => {
                        // Trivial failure, this is unexpected
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
        }
        true
    }

    fn debug_add_predicates_to_assignment_propositional(
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &mut AssignmentsPropositional,
        variable_literal_mappings: &VariableLiteralMappings,
        predicates: &[Predicate],
    ) -> bool {
        for predicate in predicates {
            let literal = if let Ok(integer_predicate) =
                <Predicate as TryInto<IntegerPredicate>>::try_into(*predicate)
            {
                variable_literal_mappings.get_literal(
                    integer_predicate,
                    assignments_propositional,
                    assignments_integer,
                )
            } else {
                predicate
                    .get_literal_of_bool_predicate(assignments_propositional.true_literal)
                    .unwrap()
            };
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
        true
    }
}
