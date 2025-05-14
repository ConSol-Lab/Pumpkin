use std::fmt::Debug;
use std::fmt::Formatter;
use std::iter::once;
use std::ops::Not;

use log::debug;
use log::warn;

use super::conflict_analysis::SemanticMinimiser;
use super::predicates::predicate::Predicate;
use super::propagation::store::PropagatorStore;
use super::propagation::ExplanationContext;
use super::reason::ReasonStore;
use super::ConstraintSatisfactionSolver;
use super::TrailedValues;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::Assignments;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;

#[derive(Copy, Clone)]
pub(crate) struct DebugDyn<'a> {
    trait_name: &'a str,
}

impl<'a> DebugDyn<'a> {
    pub(crate) fn from(trait_name: &'a str) -> Self {
        DebugDyn { trait_name }
    }
}

impl Debug for DebugDyn<'_> {
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
        trailed_values: &TrailedValues,
        assignments: &Assignments,
        propagators: &PropagatorStore,
    ) -> bool {
        let mut assignments_clone = assignments.clone();
        let mut trailed_values_clone = trailed_values.clone();
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
        for (propagator_id, propagator) in propagators.iter_propagators().enumerate() {
            let num_entries_on_trail_before_propagation = assignments_clone.num_trail_entries();

            let mut reason_store = Default::default();
            let mut semantic_minimiser = SemanticMinimiser::default();
            let context = PropagationContextMut::new(
                &mut trailed_values_clone,
                &mut assignments_clone,
                &mut reason_store,
                &mut semantic_minimiser,
                PropagatorId(propagator_id as u32),
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

    pub(crate) fn debug_reported_failure(
        trailed_values: &TrailedValues,
        assignments: &Assignments,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) -> bool {
        DebugHelper::debug_reported_propagations_reproduce_failure(
            trailed_values,
            assignments,
            failure_reason,
            propagator,
            propagator_id,
        );

        DebugHelper::debug_reported_propagations_negate_failure_and_check(
            trailed_values,
            assignments,
            failure_reason,
            propagator,
            propagator_id,
        );
        true
    }

    /// Checks whether the propagations of the propagator since `num_trail_entries_before` are
    /// reproducible by performing 2 checks:
    /// 1. Setting the reason for a propagation should lead to the same propagation when debug
    ///    propagating from scratch
    /// 2. Setting the reason for a propagation and the negation of that propagation should lead to
    ///    failure
    pub(crate) fn debug_check_propagations(
        num_trail_entries_before: usize,
        propagator_id: PropagatorId,
        trailed_values: &TrailedValues,
        assignments: &Assignments,
        reason_store: &mut ReasonStore,
        propagators: &mut PropagatorStore,
    ) -> bool {
        if propagator_id == ConstraintSatisfactionSolver::get_nogood_propagator_id() {
            return true;
        }
        let mut result = true;
        for trail_index in num_trail_entries_before..assignments.num_trail_entries() {
            let trail_entry = assignments.get_trail_entry(trail_index);

            let mut reason = vec![];
            let _ = reason_store.get_or_compute(
                trail_entry
                    .reason
                    .expect("Expected checked propagation to have a reason"),
                ExplanationContext::without_working_nogood(assignments, trail_index),
                propagators,
                &mut reason,
            );

            result &= Self::debug_propagator_reason(
                trail_entry.predicate,
                &reason,
                trailed_values,
                assignments,
                &propagators[propagator_id],
                propagator_id,
            );
        }
        result
    }

    fn debug_propagator_reason(
        propagated_predicate: Predicate,
        reason: &[Predicate],
        trailed_values: &TrailedValues,
        assignments: &Assignments,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) -> bool {
        if propagator.name() == "NogoodPropagator" {
            return true;
        }

        assert!(
            reason
                .iter()
                .all(|predicate| assignments.is_predicate_satisfied(*predicate)),
            "Found propagation with predicates which do not hold - Propagator: {}",
            propagator.name()
        );

        let trail_position = assignments
            .get_trail_position(&propagated_predicate)
            .unwrap();
        reason.iter().for_each(|predicate| assert!(assignments.get_trail_position(predicate).unwrap() < trail_position,
            "Predicate {predicate:?} has a higher trail entry ({}) than {propagated_predicate} ({trail_position}); this means that the reason generated by {} has an element which is later on the trail than the propagated predicate",
            assignments.get_trail_position(predicate).unwrap(), propagator.name()
        ));
        // todo: commented out the code below, see if it worth in the new version
        // todo: this function is not used anywhere? Why?

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
            let mut assignments_clone = assignments.debug_create_empty_clone();
            let mut trailed_values_clone = trailed_values.debug_create_empty_clone();

            let reason_predicates: Vec<Predicate> = reason.to_vec();
            let adding_predicates_was_successful = DebugHelper::debug_add_predicates_to_assignments(
                &mut assignments_clone,
                &reason_predicates,
            );

            if adding_predicates_was_successful {
                // Now propagate using the debug propagation method.
                let mut reason_store = Default::default();
                let mut semantic_minimiser = SemanticMinimiser::default();
                let context = PropagationContextMut::new(
                    &mut trailed_values_clone,
                    &mut assignments_clone,
                    &mut reason_store,
                    &mut semantic_minimiser,
                    propagator_id,
                );
                let debug_propagation_status_cp = propagator.debug_propagate_from_scratch(context);

                // Note that it could be the case that the propagation leads to conflict, in this
                // case it should be the result of a propagation (i.e. an EmptyDomain)
                if let Err(conflict) = debug_propagation_status_cp {
                    // If we have found an error then it should either be derived by an empty
                    // domain due to the same propagation holding
                    //
                    // or
                    //
                    // The conflict explanation should be a subset of the reason literals for the
                    // propagation or all of the reason literals should be in the conflict
                    // explanation

                    assert!(
                        {
                            let is_empty_domain = matches!(conflict, Inconsistency::EmptyDomain);
                            let has_propagated_predicate =
                                assignments.is_predicate_satisfied(propagated_predicate);
                            if is_empty_domain && has_propagated_predicate {
                                // We check whether an empty domain was derived, if this is indeed
                                // the case then we check whether the propagated predicate was
                                // reproduced
                                return true;
                            }

                            // If this is not the case then we check whether the explanation is a
                            // subset of the premises
                            if let Inconsistency::Conflict(ref found_inconsistency) = conflict {
                                found_inconsistency
                                    .iter()
                                    .all(|predicate| reason.contains(predicate))
                                    || reason
                                        .iter()
                                        .all(|predicate| found_inconsistency.contains(*predicate))
                            } else {
                                false
                            }
                        },
                        "Debug propagation detected a conflict other than a propagation\n
                         Propagator: '{}'\n
                         Propagator id: {propagator_id}\n
                         Reported reason: {reason:?}\n
                         Reported propagation: {propagated_predicate}\n
                         Reported Conflict: {conflict:?}",
                        propagator.name()
                    );
                } else {
                    // The predicate was either a propagation for the assignments_integer or
                    // assignments_propositional
                    assert!(
                    assignments.is_predicate_satisfied(propagated_predicate),
                    "Debug propagation could not obtain the propagated predicate given the provided reason.\n
                     Propagator: '{}'\n
                     Propagator id: {propagator_id}\n
                     Reported reason: {reason:?}\n
                     Reported propagation: {propagated_predicate}",
                    propagator.name()
                );
                }
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
            let mut assignments_clone = assignments.debug_create_empty_clone();
            let mut trailed_values_clone = trailed_values.debug_create_empty_clone();

            let failing_predicates: Vec<Predicate> = once(!propagated_predicate)
                .chain(reason.iter().copied())
                .collect();

            let adding_predicates_was_successful = DebugHelper::debug_add_predicates_to_assignments(
                &mut assignments_clone,
                &failing_predicates,
            );

            if adding_predicates_was_successful {
                //  now propagate using the debug propagation method
                let mut reason_store = Default::default();
                let mut semantic_minimiser = SemanticMinimiser::default();

                // Note that it might take multiple iterations before the conflict is reached due
                // to the assumption that some propagators make on that they are not idempotent!
                //
                // This happened in the cumulative where setting the reason led to a new mandatory
                // part being created which meant that the same propagation was not performed (i.e.
                // it did not immediately lead to a conflict) but this new mandatory part would
                // have led to a new mandatory part in the next call to the propagator
                loop {
                    let num_predicates_before = assignments_clone.num_trail_entries();

                    let context = PropagationContextMut::new(
                        &mut trailed_values_clone,
                        &mut assignments_clone,
                        &mut reason_store,
                        &mut semantic_minimiser,
                        propagator_id,
                    );
                    let debug_propagation_status_cp =
                        propagator.debug_propagate_from_scratch(context);

                    // We break if an error was found or if there were no more propagations (i.e.
                    // fixpoint was reached)
                    if debug_propagation_status_cp.is_err()
                        || num_predicates_before != assignments.num_trail_entries()
                    {
                        assert!(
                            debug_propagation_status_cp.is_err(),
                            "Debug propagation could not obtain a failure by setting the reason and negating the propagated predicate.\n
                             Propagator: '{}'\n
                             Propagator id: '{propagator_id}'.\n
                             The reported reason: {reason:?}\n
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
        trailed_values: &TrailedValues,
        assignments: &Assignments,
        failure_reason: &PropositionalConjunction,
        propagator: &dyn Propagator,
        propagator_id: PropagatorId,
    ) {
        if propagator.name() == "NogoodPropagator" {
            return;
        }
        let mut assignments_clone = assignments.debug_create_empty_clone();
        let mut trailed_values_clone = trailed_values.debug_create_empty_clone();

        let reason_predicates: Vec<Predicate> = failure_reason.iter().copied().collect();
        let adding_predicates_was_successful = DebugHelper::debug_add_predicates_to_assignments(
            &mut assignments_clone,
            &reason_predicates,
        );

        if adding_predicates_was_successful {
            //  now propagate using the debug propagation method
            let mut reason_store = Default::default();
            let mut semantic_minimiser = SemanticMinimiser::default();
            let context = PropagationContextMut::new(
                &mut trailed_values_clone,
                &mut assignments_clone,
                &mut reason_store,
                &mut semantic_minimiser,
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
        trailed_values: &TrailedValues,
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
        if failure_reason.is_empty() {
            return;
        }

        let reason_predicates: Vec<Predicate> = failure_reason.iter().copied().collect();
        let mut found_nonconflicting_state_at_root = false;
        for predicate in &reason_predicates {
            let mut assignments_clone = assignments.debug_create_empty_clone();
            let mut trailed_values_clone = trailed_values.debug_create_empty_clone();

            let negated_predicate = predicate.not();
            let outcome = assignments_clone.post_predicate(negated_predicate, None);

            if outcome.is_ok() {
                let mut reason_store = Default::default();
                let mut semantic_minimiser = SemanticMinimiser::default();
                let context = PropagationContextMut::new(
                    &mut trailed_values_clone,
                    &mut assignments_clone,
                    &mut reason_store,
                    &mut semantic_minimiser,
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
    fn debug_add_predicates_to_assignments(
        assignments: &mut Assignments,
        predicates: &[Predicate],
    ) -> bool {
        for predicate in predicates {
            let outcome = assignments.post_predicate(*predicate, None);
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
                         Failure detected after trying to apply '{predicate}'.",
                        predicates
                    );
                    return false;
                }
            }
        }
        true
    }
}
