use std::cell::OnceCell;
use std::cmp::max;
use std::cmp::min;
use std::rc::Rc;

use pumpkin_core::Random;
use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::asserts::pumpkin_assert_extreme;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::containers::HashMap;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::rand::Rng;
use pumpkin_core::rand::SeedableRng;
use pumpkin_core::rand::rngs::SmallRng;
use pumpkin_core::state::EmptyDomainConflict;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::IntegerVariable;

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
use crate::cumulative::CumulativeParameters;
use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::propagators::cumulative::time_table::explanations::pointwise;

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
    context: Domains,
) -> bool {
    let all_predicates_hold = explanation.iter().all(|&predicate| {
        context
            .evaluate_predicate(predicate)
            .is_some_and(|value| value)
    });
    if !all_predicates_hold {
        eprintln!("Not all predicates hold in the explanation for {explained_predicate:?}")
    }
    let at_least_one_element_from_current_level = explanation.iter().any(|&predicate| {
        context.get_checkpoint_for_predicate(predicate).unwrap() == context.get_checkpoint()
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
        context: &mut PropagationContext,
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
                            create_naive_propagation_explanation(profile, context.domains())
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
                    context.domains(),
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
                    context.domains()
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
        context: &mut PropagationContext,
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
                            create_naive_propagation_explanation(profile, context.domains())
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
                    context.domains(),
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
                    context.domains()
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
        context: &mut PropagationContext,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
        parameters: &mut CumulativeParameters<Var>,
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
                        context.domains(),
                        propagating_task,
                        profile,
                        None,
                    );
                let predicate = predicate![propagating_task.start_variable >= profile.end + 1];
                pumpkin_assert_extreme!(check_explanation(
                    predicate,
                    &explanation,
                    context.domains()
                ));

                let mut reason = (*explanation).clone();

                // if parameters.options.log_test_cases
                //     && context.evaluate_predicate(predicate).is_none()
                //     && SmallRng::from_entropy().gen_range(0.0..=1.0) <= 0.025
                // {
                //     let mut bounds: HashMap<DomainId, Vec<i32>> = HashMap::default();
                //     for element in reason.iter() {
                //         let entry = bounds.entry(element.get_domain()).or_default();
                //         entry.push(element.get_right_hand_side());
                //         entry.sort();
                //     }
                //     println!(
                //         "
                // #[test]
                // fn cumulative_time_table_lower_bound_{}(){{
                //     // Test case with {} variables
                //     let (solver, result, variables) = set_up_cumulative_state(&{:?}, {}, false);
                //
                //     assert!(result.is_ok());
                //     assert_ge!(solver.lower_bound(*variables.last().unwrap()), {})
                // }}
                // ",
                //         SmallRng::from_entropy().generate_i32_in_range(0, 1_000_000),
                //         bounds.len(),
                //         profile
                //             .profile_tasks
                //             .iter()
                //             .map(|task| {
                //                 let domain =
                //                     task.start_variable.lower_bound_predicate(0).get_domain();
                //                 let current_bounds = &bounds[&domain];
                //                 assert_eq!(current_bounds.len(), 2);
                //                 (
                //                     (current_bounds[0], current_bounds[1]),
                //                     task.processing_time,
                //                     task.resource_usage,
                //                 )
                //             })
                //             .chain(std::iter::once((
                //                 (
                //                     lower_bound_predicate_propagating_task.get_right_hand_side(),
                //                     context.upper_bound(&propagating_task.start_variable)
                //                 ),
                //                 propagating_task.processing_time,
                //                 propagating_task.resource_usage
                //             )))
                //             .collect::<Vec<_>>(),
                //         parameters.capacity,
                //         profile.end + 1
                //     );
                //     parameters.options.num_test_cases_generated += 1;
                // }
                reason.push(lower_bound_predicate_propagating_task);
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
        context: &mut PropagationContext,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
        parameters: &mut CumulativeParameters<Var>,
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
                        context.domains(),
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
                    context.domains()
                ));

                let mut reason = (*explanation).clone();
                // if parameters.options.log_test_cases
                //     && context.evaluate_predicate(predicate).is_none()
                //     && SmallRng::from_entropy().gen_range(0.0..=1.0) <= 0.025
                // {
                //     let mut bounds: HashMap<DomainId, Vec<i32>> = HashMap::default();
                //     for element in reason.iter() {
                //         let entry = bounds.entry(element.get_domain()).or_default();
                //         entry.push(element.get_right_hand_side());
                //         entry.sort();
                //     }
                //     println!(
                //         "
                // #[test]
                // fn cumulative_time_table_upper_bound_{}(){{
                //     // Test case with {} variables
                //     let (solver, result, variables) = set_up_cumulative_state(&{:?}, {}, false);
                //
                //     assert!(result.is_ok());
                //     assert_le!(solver.upper_bound(*variables.last().unwrap()), {});
                // }}
                // ",
                //         SmallRng::from_entropy().generate_i32_in_range(0, 1_000_000),
                //         bounds.len(),
                //         profile
                //             .profile_tasks
                //             .iter()
                //             .map(|task| {
                //                 let domain =
                //                     task.start_variable.lower_bound_predicate(0).get_domain();
                //                 let current_bounds = &bounds[&domain];
                //                 assert_eq!(current_bounds.len(), 2);
                //                 (
                //                     (current_bounds[0], current_bounds[1]),
                //                     task.processing_time,
                //                     task.resource_usage,
                //                 )
                //             })
                //             .chain(std::iter::once((
                //                 (
                //                     context.lower_bound(&propagating_task.start_variable),
                //                     upper_bound_predicate_propagating_task.get_right_hand_side()
                //                 ),
                //                 propagating_task.processing_time,
                //                 propagating_task.resource_usage
                //             )))
                //             .collect::<Vec<_>>(),
                //         parameters.capacity,
                //         profile.start - propagating_task.processing_time
                //     );
                //     parameters.options.num_test_cases_generated += 1;
                // }
                reason.push(upper_bound_predicate_propagating_task);
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
        context: &mut PropagationContext,
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
                        context.domains()
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
                        context.domains()
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
        context: &mut PropagationContext,
        profile: &ResourceProfile<Var>,
    ) -> Rc<PropositionalConjunction>
    where
        Var: IntegerVariable + 'static,
    {
        Rc::clone(self.stored_profile_explanation.get_or_init(|| {
            Rc::new(
                match self.explanation_type {
                    CumulativeExplanationType::Naive => {
                        create_naive_propagation_explanation(profile, context.domains())
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
pub(crate) fn create_conflict_explanation<Var, Context: ReadDomains>(
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

    use pumpkin_core::containers::StorageKey;
    use pumpkin_core::predicate;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::predicates::PropositionalConjunction;
    use pumpkin_core::proof::InferenceCode;
    use pumpkin_core::propagation::LocalId;
    use pumpkin_core::state::CurrentNogood;
    use pumpkin_core::state::State;
    use pumpkin_core::variables::DomainId;

    use super::CumulativeExplanationType;
    use super::CumulativePropagationHandler;
    use super::create_conflict_explanation;
    use crate::cumulative::CumulativeParameters;
    use crate::cumulative::ResourceProfile;
    use crate::cumulative::Task;
    use crate::cumulative::options::CumulativePropagatorOptions;

    pub(crate) struct TestPropagationHandler {
        propagation_handler: CumulativePropagationHandler,
        state: State,
        reason_buffer: Vec<Predicate>,
    }

    impl TestPropagationHandler {
        pub(crate) fn new(explanation_type: CumulativeExplanationType) -> Self {
            let propagation_handler = CumulativePropagationHandler::new(
                explanation_type,
                InferenceCode::create_from_index(0),
            );

            let state = State::default();
            Self {
                propagation_handler,
                state,
                reason_buffer: Default::default(),
            }
        }

        pub(crate) fn set_up_conflict_example(&mut self) -> (PropositionalConjunction, DomainId) {
            let y = self.state.new_interval_variable(15, 16, None);

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
                self.state.get_domains(),
                self.propagation_handler.inference_code,
                &profile,
                self.propagation_handler.explanation_type,
            );

            (reason.conjunction, y)
        }

        pub(crate) fn set_up_example_lower_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId) {
            let x = self.state.new_interval_variable(11, 20, None);
            let y = self.state.new_interval_variable(15, 16, None);

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
                    &mut self.state.get_propagation_context(),
                    &profile,
                    &Rc::new(propagating_task),
                    &mut CumulativeParameters::new(
                        vec![],
                        0,
                        CumulativePropagatorOptions::default(),
                    ),
                );
            assert!(result.is_ok());
            assert_eq!(self.state.lower_bound(x), 19);

            let reason = self.get_reason_for(predicate!(x >= 19));

            (reason, x, y)
        }

        pub(crate) fn set_up_example_sequence_lower_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId, DomainId) {
            let x = self.state.new_interval_variable(11, 25, None);
            let y = self.state.new_interval_variable(15, 16, None);
            let z = self.state.new_interval_variable(15, 19, None);

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
                    &mut self.state.get_propagation_context(),
                    &[&profile_y, &profile_z],
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.state.lower_bound(x), 22);

            let reason = self.get_reason_for(predicate!(x >= 22));

            (reason, x, y, z)
        }

        pub(crate) fn set_up_example_upper_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId) {
            let x = self.state.new_interval_variable(5, 16, None);
            let y = self.state.new_interval_variable(15, 16, None);

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
                    &mut self.state.get_propagation_context(),
                    &profile,
                    &Rc::new(propagating_task),
                    &mut CumulativeParameters::new(
                        vec![],
                        0,
                        CumulativePropagatorOptions::default(),
                    ),
                );
            assert!(result.is_ok());
            assert_eq!(self.state.upper_bound(x), 10);

            let reason = self.get_reason_for(predicate!(x <= 10));

            (reason, x, y)
        }

        pub(crate) fn set_up_example_sequence_upper_bound(
            &mut self,
        ) -> (PropositionalConjunction, DomainId, DomainId, DomainId) {
            let x = self.state.new_interval_variable(0, 16, None);
            let y = self.state.new_interval_variable(15, 16, None);
            let z = self.state.new_interval_variable(7, 9, None);

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
                    &mut self.state.get_propagation_context(),
                    &[&profile_z, &profile_y],
                    &Rc::new(propagating_task),
                );
            assert!(result.is_ok());
            assert_eq!(self.state.upper_bound(x), 3);

            let reason = self.get_reason_for(predicate!(x <= 3));

            (reason, x, y, z)
        }

        pub(crate) fn get_reason_for(&mut self, predicate: Predicate) -> PropositionalConjunction {
            let _ = self.state.get_propagation_reason(
                predicate,
                &mut self.reason_buffer,
                CurrentNogood::empty(),
            );

            let result = self.reason_buffer.clone().into();
            self.reason_buffer.clear();
            result
        }
    }
}
