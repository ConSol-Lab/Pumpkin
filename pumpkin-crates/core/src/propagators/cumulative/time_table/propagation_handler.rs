use std::cell::OnceCell;
use std::cmp::max;
use std::cmp::min;
use std::rc::Rc;

use super::CumulativeExplanationType;
use super::explanations::add_propagating_task_predicate_lower_bound;
use super::explanations::add_propagating_task_predicate_upper_bound;
use super::explanations::big_step::create_big_step_conflict_explanation;
use super::explanations::big_step::create_big_step_propagation_explanation;
use super::explanations::create_predicate_propagating_task_lower_bound_propagation;
use super::explanations::create_predicate_propagating_task_upper_bound_propagation;
use super::explanations::naive::create_naive_conflict_explanation;
use super::explanations::naive::create_naive_propagation_explanation;
use super::explanations::pointwise::create_pointwise_conflict_explanation;
use super::explanations::pointwise::create_pointwise_propagation_explanation;
use crate::basic_types::PropagatorConflict;
use crate::engine::EmptyDomainConflict;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::ReadDomains;
use crate::engine::propagation::contexts::HasAssignments;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::InferenceCode;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::propagators::cumulative::time_table::explanations::pointwise;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Structure for handling the creation of propagations and their explanations.
pub(crate) struct CumulativePropagationHandler {
    /// The type of explanation which is used
    explanation_type: CumulativeExplanationType,
    /// If the same profile propagates multiple tasks then it is beneficial to cache that
    /// explanation and re-use it. Note that this will only be used for
    /// [`CumulativeExplanationType::Naive`] and [`CumulativeExplanationType::BigStep`].
    stored_profile_explanation: OnceCell<Rc<PropositionalConjunction>>,
    inference_code: InferenceCode,
}

fn check_explanation(
    explained_predicate: Predicate,
    explanation: &PropositionalConjunction,
    context: PropagationContext,
) -> bool {
    let all_predicates_hold = explanation
        .iter()
        .all(|&predicate| context.assignments().is_predicate_satisfied(predicate));
    if !all_predicates_hold {
        eprintln!("Not all predicates hold in the explanation for {explained_predicate:?}")
    }
    let at_least_one_element_from_current_level = explanation.iter().any(|&predicate| {
        context
            .assignments()
            .get_checkpoint_for_predicate(&predicate)
            .unwrap()
            == context.assignments().get_checkpoint()
    });
    if !at_least_one_element_from_current_level {
        eprintln!(
            "At least one predicate in the explanation for {explained_predicate:?} should be from the current decision level"
        )
    }

    all_predicates_hold && at_least_one_element_from_current_level
}

impl CumulativePropagationHandler {
    pub(crate) fn new(
        explanation_type: CumulativeExplanationType,
        inference_code: InferenceCode,
    ) -> Self {
        Self {
            explanation_type,
            stored_profile_explanation: OnceCell::new(),
            inference_code,
        }
    }

    /// Propagates the lower-bound of the `propagating_task` to not conflict with all of the
    /// `profiles` anymore.
    pub(crate) fn propagate_chain_of_lower_bounds_with_explanations<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profiles: &[&ResourceProfile<Var>],
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomainConflict>
    where
        Var: IntegerVariable + 'static,
    {
        pumpkin_assert_simple!(!profiles.is_empty());
        match self.explanation_type {
            CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                let mut full_explanation = PropositionalConjunction::default();

                for profile in profiles {
                    let explanation = match self.explanation_type {
                        CumulativeExplanationType::Naive => {
                            create_naive_propagation_explanation(profile, context.as_readonly())
                        }
                        CumulativeExplanationType::BigStep => {
                            create_big_step_propagation_explanation(profile)
                        }
                        CumulativeExplanationType::Pointwise => {
                            unreachable!(
                                "At the moment, we do not store the profile explanation for the pointwise explanation since it consists of multiple explanations"
                            )
                        }
                    };

                    full_explanation =
                        full_explanation.extend_and_remove_duplicates(explanation.into_iter());
                }

                let full_explanation = add_propagating_task_predicate_lower_bound(
                    full_explanation,
                    self.explanation_type,
                    context.as_readonly(),
                    propagating_task,
                    profiles[0],
                    None,
                );

                let predicate = predicate![
                    propagating_task.start_variable >= profiles[profiles.len() - 1].end + 1
                ];
                pumpkin_assert_extreme!(check_explanation(
                    predicate,
                    &full_explanation,
                    context.as_readonly()
                ));
                context.post(predicate, full_explanation, self.inference_code)
            }
            CumulativeExplanationType::Pointwise => {
                pointwise::propagate_lower_bounds_with_pointwise_explanations(
                    context,
                    profiles,
                    propagating_task,
                    self.inference_code,
                )
            }
        }
    }

    /// Propagates the upper-bound of the `propagating_task` to not conflict with all of the
    /// `profiles` anymore.
    pub(crate) fn propagate_chain_of_upper_bounds_with_explanations<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profiles: &[&ResourceProfile<Var>],
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomainConflict>
    where
        Var: IntegerVariable + 'static,
    {
        pumpkin_assert_simple!(!profiles.is_empty());

        match self.explanation_type {
            CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                let mut full_explanation = PropositionalConjunction::default();

                for profile in profiles {
                    let explanation = match self.explanation_type {
                        CumulativeExplanationType::Naive => {
                            create_naive_propagation_explanation(profile, context.as_readonly())
                        }
                        CumulativeExplanationType::BigStep => {
                            create_big_step_propagation_explanation(profile)
                        }
                        CumulativeExplanationType::Pointwise => {
                            unreachable!(
                                "At the moment, we do not store the profile explanation for the pointwise explanation since it consists of multiple explanations"
                            )
                        }
                    };

                    full_explanation =
                        full_explanation.extend_and_remove_duplicates(explanation.into_iter());
                }

                let full_explanation = add_propagating_task_predicate_upper_bound(
                    full_explanation,
                    self.explanation_type,
                    context.as_readonly(),
                    propagating_task,
                    profiles[profiles.len() - 1],
                    None,
                );
                let predicate = predicate![
                    propagating_task.start_variable
                        <= profiles[0].start - propagating_task.processing_time
                ];
                pumpkin_assert_extreme!(check_explanation(
                    predicate,
                    &full_explanation,
                    context.as_readonly()
                ));
                context.post(predicate, full_explanation, self.inference_code)
            }
            CumulativeExplanationType::Pointwise => {
                pointwise::propagate_upper_bounds_with_pointwise_explanations(
                    context,
                    profiles,
                    propagating_task,
                    self.inference_code,
                )
            }
        }
    }

    /// Propagates the lower-bound of the `propagating_task` to not conflict with `profile` anymore.
    pub(crate) fn propagate_lower_bound_with_explanations<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomainConflict>
    where
        Var: IntegerVariable + 'static,
    {
        pumpkin_assert_advanced!(
            context.lower_bound(&propagating_task.start_variable) < profile.end + 1
        );

        match self.explanation_type {
            CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                // We use the same procedure for the explanation using naive and bigstep, note that
                // `get_stored_profile_explanation_or_init` and
                // `create_predicate_propagating_task_lower_bound_propagation` both use the
                // explanation type to create the explanations.
                let explanation = self.get_stored_profile_explanation_or_init(context, profile);
                let lower_bound_predicate_propagating_task =
                    create_predicate_propagating_task_lower_bound_propagation(
                        self.explanation_type,
                        context.as_readonly(),
                        propagating_task,
                        profile,
                        None,
                    );
                let predicate = predicate![propagating_task.start_variable >= profile.end + 1];
                pumpkin_assert_extreme!(check_explanation(
                    predicate,
                    &explanation,
                    context.as_readonly()
                ));

                let mut reason = (*explanation).clone();
                reason.add(lower_bound_predicate_propagating_task);
                context.post(predicate, reason, self.inference_code)
            }
            CumulativeExplanationType::Pointwise => {
                pointwise::propagate_lower_bounds_with_pointwise_explanations(
                    context,
                    &[profile],
                    propagating_task,
                    self.inference_code,
                )
            }
        }
    }

    /// Propagates the upper-bound of the `propagating_task` to not conflict with `profile` anymore.
    pub(crate) fn propagate_upper_bound_with_explanations<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomainConflict>
    where
        Var: IntegerVariable + 'static,
    {
        pumpkin_assert_advanced!(
            context.upper_bound(&propagating_task.start_variable)
                > profile.start - propagating_task.processing_time
        );

        match self.explanation_type {
            CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                // We use the same procedure for the explanation using naive and bigstep, note that
                // `get_stored_profile_explanation_or_init` and
                // `create_predicate_propagating_task_upper_bound_propagation` both use the
                // explanation type to create the explanations.
                let explanation = self.get_stored_profile_explanation_or_init(context, profile);
                let upper_bound_predicate_propagating_task =
                    create_predicate_propagating_task_upper_bound_propagation(
                        self.explanation_type,
                        context.as_readonly(),
                        propagating_task,
                        profile,
                        None,
                    );
                let predicate = predicate![
                    propagating_task.start_variable
                        <= profile.start - propagating_task.processing_time
                ];
                pumpkin_assert_extreme!(check_explanation(
                    predicate,
                    &explanation,
                    context.as_readonly()
                ));

                let mut reason = (*explanation).clone();
                reason.add(upper_bound_predicate_propagating_task);
                context.post(predicate, reason, self.inference_code)
            }
            CumulativeExplanationType::Pointwise => {
                pointwise::propagate_upper_bounds_with_pointwise_explanations(
                    context,
                    &[profile],
                    propagating_task,
                    self.inference_code,
                )
            }
        }
    }

    /// Propagates a hole in the domain; note that this explanation does not contain any of the
    /// bounds of `propagating_task`.
    pub(crate) fn propagate_holes_in_domain<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomainConflict>
    where
        Var: IntegerVariable + 'static,
    {
        // We go through all of the time-points which cause `task` to overlap
        // with the resource profile

        // There are two options for determining the lowest value to remove from
        // the domain:
        // - We remove all time-points from `profile.start - duration + 1` (i.e. the earliest
        //   time-point such that `task` necessarily overlaps with the profile).
        // - It could be the case that the lower-bound is larger than the previous earliest
        //   time-point in which case we simply start from the lower-bound of the task.
        let lower_bound_removed_time_points = max(
            context.lower_bound(&propagating_task.start_variable),
            profile.start - propagating_task.processing_time + 1,
        );

        // There are also two options for determine the highest value to remove
        // from the domain:
        // - We remove all time-points up-and-until the end of the profile (i.e. the latest
        //   time-point which would result in the `task` overlapping with the profile)
        // - It could be the case that the upper-bound is smaller than the previous later time-point
        //   in case we remove all time-points up-and-until the upper-bound of the task.
        let upper_bound_removed_time_points = min(
            context.upper_bound(&propagating_task.start_variable),
            profile.end,
        );

        for time_point in lower_bound_removed_time_points..=upper_bound_removed_time_points {
            if !context.contains(&propagating_task.start_variable, time_point) {
                continue;
            }

            match self.explanation_type {
                CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                    // We use the same procedure for the explanation using naive and bigstep, note
                    // that `get_stored_profile_explanation_or_init` uses the
                    // explanation type to create the explanations.
                    let explanation = self.get_stored_profile_explanation_or_init(context, profile);
                    let predicate = predicate![propagating_task.start_variable != time_point];
                    pumpkin_assert_extreme!(check_explanation(
                        predicate,
                        &explanation,
                        context.as_readonly()
                    ));
                    context.post(predicate, (*explanation).clone(), self.inference_code)?;
                }
                CumulativeExplanationType::Pointwise => {
                    // We split into two cases when determining the explanation of the profile
                    // - Either the time-point is before the start of the profile; in which case the
                    //   explanation for the removal of this time-point is that there is a profile
                    //   point at the point `profile.start..time_point +
                    //   propagating_task.processing_time - 1`. In this case, we pick the time point
                    //   plus the processing time value until we reach the middle point of the
                    //   profile in which case we always pick the middle point (this is an untested
                    //   heuristic in terms of performance).
                    // - Or the time-point is after the start of the profile in which case the
                    //   explanation is simply that there is a profile at this time-point (which
                    //   together with the propagating task would overflow the capacity)
                    let corresponding_profile_explanation_point = if time_point < profile.start {
                        min(
                            time_point + propagating_task.processing_time - 1,
                            (profile.end - profile.start) / 2 + profile.start,
                        )
                    } else {
                        time_point
                    };

                    let explanation = create_pointwise_propagation_explanation(
                        corresponding_profile_explanation_point,
                        profile,
                    );
                    let predicate = predicate![propagating_task.start_variable != time_point];
                    pumpkin_assert_extreme!(check_explanation(
                        predicate,
                        &explanation,
                        context.as_readonly()
                    ));
                    context.post(predicate, explanation, self.inference_code)?;
                }
            }
        }

        Ok(())
    }

    /// Signifies that we are moving to another profile and we cannot re-use the cached explanation
    /// of [`CumulativePropagationHandler::stored_profile_explanation`].
    pub(crate) fn next_profile(&mut self) {
        self.stored_profile_explanation = OnceCell::new();
    }

    /// Either we get the stored stored profile explanation or we initialize it.
    fn get_stored_profile_explanation_or_init<Var>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
    ) -> Rc<PropositionalConjunction>
    where
        Var: IntegerVariable + 'static,
    {
        Rc::clone(self.stored_profile_explanation.get_or_init(|| {
            Rc::new(
                match self.explanation_type {
                    CumulativeExplanationType::Naive => {
                        create_naive_propagation_explanation(profile, context.as_readonly())
                    },
                    CumulativeExplanationType::BigStep => {
                        create_big_step_propagation_explanation(profile)
                    },
                    CumulativeExplanationType::Pointwise => {
                        unreachable!("At the moment, we do not store the profile explanation for the pointwise explanation since it consists of multiple explanations")
                    },
                }
            )
        }))
    }
}

/// Creates an explanation of the conflict caused by `conflict_profile` based on the provided
/// `explanation_type`.
pub(crate) fn create_conflict_explanation<Var, Context: ReadDomains + Copy>(
    context: Context,
    inference_code: InferenceCode,
    conflict_profile: &ResourceProfile<Var>,
    explanation_type: CumulativeExplanationType,
) -> PropagatorConflict
where
    Var: IntegerVariable + 'static,
{
    let conjunction = match explanation_type {
        CumulativeExplanationType::Naive => {
            create_naive_conflict_explanation(conflict_profile, context)
        }
        CumulativeExplanationType::BigStep => {
            create_big_step_conflict_explanation(conflict_profile)
        }
        CumulativeExplanationType::Pointwise => {
            create_pointwise_conflict_explanation(conflict_profile)
        }
    };

    PropagatorConflict {
        conjunction,
        inference_code,
    }
}

#[cfg(test)]
pub(crate) mod test_propagation_handler {
    use std::rc::Rc;

    use super::CumulativeExplanationType;
    use super::CumulativePropagationHandler;
    use super::create_conflict_explanation;
    use crate::containers::StorageKey;
    use crate::engine::Assignments;
    use crate::engine::TrailedValues;
    use crate::engine::notifications::NotificationEngine;
    use crate::engine::propagation::ExplanationContext;
    use crate::engine::propagation::LocalId;
    use crate::engine::propagation::PropagationContext;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::propagation::store::PropagatorStore;
    use crate::engine::reason::ReasonStore;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::proof::InferenceCode;
    use crate::propagators::ResourceProfile;
    use crate::propagators::Task;
    use crate::variables::DomainId;

    pub(crate) struct TestPropagationHandler {
        propagation_handler: CumulativePropagationHandler,
        notification_engine: NotificationEngine,
        reason_store: ReasonStore,
        assignments: Assignments,
        trailed_values: TrailedValues,
    }

    impl TestPropagationHandler {
        pub(crate) fn new(explanation_type: CumulativeExplanationType) -> Self {
            let propagation_handler = CumulativePropagationHandler::new(
                explanation_type,
                InferenceCode::create_from_index(0),
            );

            let reason_store = ReasonStore::default();
            let assignments = Assignments::default();
            let trailed_values = TrailedValues::default();
            let notification_engine = NotificationEngine::default();
            Self {
                propagation_handler,
                notification_engine,
                reason_store,
                assignments,
                trailed_values,
            }
        }

        pub(crate) fn set_up_conflict_example(&mut self) -> (PropositionalConjunction, DomainId) {
            let y = self.assignments.grow(15, 16);
            self.notification_engine.grow();

            let profile_task = Task {
                start_variable: y,
                processing_time: 4,
                resource_usage: 1,
                id: LocalId::from(1),
            };

            let profile = ResourceProfile {
                start: 15,
                end: 17,
                profile_tasks: vec![Rc::new(profile_task)],
                height: 1,
            };

            let reason = create_conflict_explanation(
                PropagationContext::new(&self.assignments),
                self.propagation_handler.inference_code,
                &profile,
                self.propagation_handler.explanation_type,
            );

            (reason.conjunction, y)
        }

        pub(crate) fn set_up_example_lower_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId) {
            let x = self.assignments.grow(11, 20);
            let y = self.assignments.grow(15, 16);
            self.notification_engine.grow();
            self.notification_engine.grow();

            let propagating_task = Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            };

            let profile_task = Task {
                start_variable: y,
                processing_time: 4,
                resource_usage: 1,
                id: LocalId::from(1),
            };

            let profile = ResourceProfile {
                start: 16,
                end: 18,
                profile_tasks: vec![Rc::new(profile_task)],
                height: 1,
            };

            let result = self
                .propagation_handler
                .propagate_lower_bound_with_explanations(
                    &mut PropagationContextMut::new(
                        &mut self.trailed_values,
                        &mut self.assignments,
                        &mut self.reason_store,
                        &mut self.notification_engine,
                        PropagatorId(0),
                    ),
                    &profile,
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.assignments.get_lower_bound(x), 19);

            let reason = self.get_reason_for(predicate!(x >= 19));

            (reason, x, y)
        }

        pub(crate) fn set_up_example_sequence_lower_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId, DomainId) {
            let x = self.assignments.grow(11, 25);
            let y = self.assignments.grow(15, 16);
            let z = self.assignments.grow(15, 19);
            self.notification_engine.grow();
            self.notification_engine.grow();
            self.notification_engine.grow();

            let propagating_task = Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            };

            let profile_task_y = Task {
                start_variable: y,
                processing_time: 4,
                resource_usage: 1,
                id: LocalId::from(1),
            };
            let profile_y = ResourceProfile {
                start: 16,
                end: 18,
                profile_tasks: vec![Rc::new(profile_task_y)],
                height: 1,
            };

            let profile_task_z = Task {
                start_variable: z,
                processing_time: 7,
                resource_usage: 1,
                id: LocalId::from(2),
            };
            let profile_z = ResourceProfile {
                start: 19,
                end: 21,
                profile_tasks: vec![Rc::new(profile_task_z)],
                height: 1,
            };

            let result = self
                .propagation_handler
                .propagate_chain_of_lower_bounds_with_explanations(
                    &mut PropagationContextMut::new(
                        &mut self.trailed_values,
                        &mut self.assignments,
                        &mut self.reason_store,
                        &mut self.notification_engine,
                        PropagatorId(0),
                    ),
                    &[&profile_y, &profile_z],
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.assignments.get_lower_bound(x), 22);

            let reason = self.get_reason_for(predicate!(x >= 22));

            (reason, x, y, z)
        }

        pub(crate) fn set_up_example_upper_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId) {
            let x = self.assignments.grow(5, 16);
            let y = self.assignments.grow(15, 16);
            self.notification_engine.grow();
            self.notification_engine.grow();

            let propagating_task = Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            };

            let profile_task = Task {
                start_variable: y,
                processing_time: 4,
                resource_usage: 1,
                id: LocalId::from(1),
            };

            let profile = ResourceProfile {
                start: 16,
                end: 18,
                profile_tasks: vec![Rc::new(profile_task)],
                height: 1,
            };

            let result = self
                .propagation_handler
                .propagate_upper_bound_with_explanations(
                    &mut PropagationContextMut::new(
                        &mut self.trailed_values,
                        &mut self.assignments,
                        &mut self.reason_store,
                        &mut self.notification_engine,
                        PropagatorId(0),
                    ),
                    &profile,
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.assignments.get_upper_bound(x), 10);

            let reason = self.get_reason_for(predicate!(x <= 10));

            (reason, x, y)
        }

        pub(crate) fn set_up_example_sequence_upper_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId, DomainId) {
            let x = self.assignments.grow(0, 16);
            let y = self.assignments.grow(15, 16);
            let z = self.assignments.grow(7, 9);
            self.notification_engine.grow();
            self.notification_engine.grow();
            self.notification_engine.grow();

            let propagating_task = Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            };

            let profile_task_y = Task {
                start_variable: y,
                processing_time: 4,
                resource_usage: 1,
                id: LocalId::from(1),
            };
            let profile_y = ResourceProfile {
                start: 16,
                end: 18,
                profile_tasks: vec![Rc::new(profile_task_y)],
                height: 1,
            };

            let profile_task_z = Task {
                start_variable: z,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(2),
            };
            let profile_z = ResourceProfile {
                start: 9,
                end: 12,
                profile_tasks: vec![Rc::new(profile_task_z)],
                height: 1,
            };

            let result = self
                .propagation_handler
                .propagate_chain_of_upper_bounds_with_explanations(
                    &mut PropagationContextMut::new(
                        &mut self.trailed_values,
                        &mut self.assignments,
                        &mut self.reason_store,
                        &mut self.notification_engine,
                        PropagatorId(0),
                    ),
                    &[&profile_z, &profile_y],
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.assignments.get_upper_bound(x), 3);

            let reason = self.get_reason_for(predicate!(x <= 3));

            (reason, x, y, z)
        }

        pub(crate) fn get_reason_for(&mut self, predicate: Predicate) -> PropositionalConjunction {
            let reason_ref = self
                .assignments
                .get_reason_for_predicate_brute_force(predicate);
            let mut propagator_store = PropagatorStore::default();
            let mut reason = vec![];
            let _ = self.reason_store.get_or_compute(
                reason_ref,
                ExplanationContext::without_working_nogood(
                    &self.assignments,
                    self.assignments.get_trail_position(&predicate).unwrap(),
                    &mut self.notification_engine,
                ),
                &mut propagator_store,
                &mut reason,
            );

            reason.into()
        }
    }
}
