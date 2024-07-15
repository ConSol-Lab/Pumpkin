use std::cell::OnceCell;
use std::cmp::max;
use std::fmt::Display;
use std::rc::Rc;

use super::time_table_util::ResourceProfile;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::EmptyDomain;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::Task;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Determines what type of explanation is used for the cumulative constraint based on the
/// explanations described in Section 4.5.1 and 4.5.2 of \[1\].
///
/// For propagations, we different between 2 types of explanations;
///
/// # Bibliography
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug, Clone, Copy, Default)]
pub enum CumulativeExplanationType {
    Naive,
    BigStep,
    #[default]
    PointWise,
}

impl Display for CumulativeExplanationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativeExplanationType::Naive => write!(f, "naive"),
            CumulativeExplanationType::BigStep => write!(f, "big-step"),
            CumulativeExplanationType::PointWise => write!(f, "pointwise"),
        }
    }
}

pub(crate) struct CumulativeExplanationHandler {
    explanation_type: CumulativeExplanationType,
    stored_profile_explanation: OnceCell<Rc<PropositionalConjunction>>,
}

impl CumulativeExplanationHandler {
    pub(crate) fn new(explanation_type: CumulativeExplanationType) -> Self {
        Self {
            explanation_type,
            stored_profile_explanation: OnceCell::new(),
        }
    }

    pub(crate) fn next_profile(&mut self) {
        self.stored_profile_explanation = OnceCell::new();
    }

    fn get_stored_profile_explanation_or_init<Var: IntegerVariable + 'static>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
    ) -> Rc<PropositionalConjunction> {
        Rc::clone(self.stored_profile_explanation.get_or_init(|| {
            Rc::new(
                match self.explanation_type {
                    CumulativeExplanationType::Naive => {
                        create_naive_propagation_explanation(profile, &context.as_readonly())
                    },
                    CumulativeExplanationType::BigStep => {
                        create_big_step_propagation_explanation(profile)
                    },
                    CumulativeExplanationType::PointWise => {
                        unreachable!("At the moment, we do not store the profile explanation for the pointwise explanation since it consists of multiple explanations")
                    },
                }
            )
        }))
    }

    pub(crate) fn propagate_lower_bound_with_explanations<Var: IntegerVariable + 'static>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomain> {
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
                        &context.as_readonly(),
                        propagating_task,
                        profile,
                        None,
                    );
                context.set_lower_bound(
                    &propagating_task.start_variable,
                    profile.end + 1,
                    move |_context: &PropagationContext| {
                        let mut reason = (*explanation).clone();
                        reason.add(lower_bound_predicate_propagating_task);
                        reason
                    },
                )
            }
            CumulativeExplanationType::PointWise => {
                let mut time_point = profile.start;
                loop {
                    if time_point >= profile.end {
                        // We ensure that the last time-point is always the end of the profile
                        let explanation = add_propagating_task_predicate_lower_bound(
                            create_pointwise_propagation_explanation(profile.end, profile),
                            CumulativeExplanationType::PointWise,
                            &context.as_readonly(),
                            propagating_task,
                            profile,
                            Some(profile.end),
                        );

                        context.set_lower_bound(
                            &propagating_task.start_variable,
                            profile.end + 1,
                            explanation,
                        )?;
                        break;
                    }

                    let explanation = add_propagating_task_predicate_lower_bound(
                        create_pointwise_propagation_explanation(time_point, profile),
                        CumulativeExplanationType::PointWise,
                        &context.as_readonly(),
                        propagating_task,
                        profile,
                        Some(time_point),
                    );
                    context.set_lower_bound(
                        &propagating_task.start_variable,
                        time_point + 1,
                        explanation,
                    )?;

                    time_point += propagating_task.processing_time
                }
                Ok(())
            }
        }
    }

    pub(crate) fn propagate_upper_bound_with_explanations<Var: IntegerVariable + 'static>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
    ) -> Result<(), EmptyDomain> {
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
                        &context.as_readonly(),
                        propagating_task,
                        profile,
                        None,
                    );
                context.set_upper_bound(
                    &propagating_task.start_variable,
                    profile.start - propagating_task.processing_time,
                    move |_context: &PropagationContext| {
                        let mut reason = (*explanation).clone();
                        reason.add(upper_bound_predicate_propagating_task);
                        reason
                    },
                )
            }
            CumulativeExplanationType::PointWise => {
                let mut time_point = profile.end;
                loop {
                    if time_point <= profile.start {
                        let explanation = add_propagating_task_predicate_upper_bound(
                            create_pointwise_propagation_explanation(profile.start, profile),
                            CumulativeExplanationType::PointWise,
                            &context.as_readonly(),
                            propagating_task,
                            profile,
                            Some(profile.start),
                        );
                        // We ensure that the last time-point is always the end of the profile
                        context.set_upper_bound(
                            &propagating_task.start_variable,
                            profile.start - propagating_task.processing_time,
                            explanation,
                        )?;
                        break;
                    }
                    let explanation = add_propagating_task_predicate_upper_bound(
                        create_pointwise_propagation_explanation(time_point, profile),
                        CumulativeExplanationType::PointWise,
                        &context.as_readonly(),
                        propagating_task,
                        profile,
                        Some(time_point),
                    );
                    context.set_upper_bound(
                        &propagating_task.start_variable,
                        time_point - propagating_task.processing_time,
                        explanation,
                    )?;

                    time_point -= propagating_task.processing_time
                }
                Ok(())
            }
        }
    }

    pub(crate) fn propagate_hole_in_domain<Var: IntegerVariable + 'static>(
        &mut self,
        context: &mut PropagationContextMut,
        profile: &ResourceProfile<Var>,
        propagating_task: &Rc<Task<Var>>,
        time_point: i32,
    ) -> Result<(), EmptyDomain> {
        match self.explanation_type {
            CumulativeExplanationType::Naive | CumulativeExplanationType::BigStep => {
                // We use the same procedure for the explanation using naive and bigstep, note that
                // `get_stored_profile_explanation_or_init` uses the explanation type to create the
                // explanations.
                let explanation = self.get_stored_profile_explanation_or_init(context, profile);
                context.remove(
                    &propagating_task.start_variable,
                    time_point,
                    move |_context: &PropagationContext| (*explanation).clone(),
                )
            }
            CumulativeExplanationType::PointWise => {
                let explanation = create_pointwise_propagation_explanation(time_point, profile);
                context.remove(&propagating_task.start_variable, time_point, explanation)
            }
        }
    }
}

pub(crate) fn create_conflict_explanation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    conflict_profile: &ResourceProfile<Var>,
    explanation_type: CumulativeExplanationType,
) -> PropositionalConjunction {
    match explanation_type {
        CumulativeExplanationType::Naive => conflict_profile
            .profile_tasks
            .iter()
            .flat_map(|profile_task| {
                [
                    predicate!(
                        profile_task.start_variable
                            >= context.lower_bound(&profile_task.start_variable)
                    ),
                    predicate!(
                        profile_task.start_variable
                            <= context.upper_bound(&profile_task.start_variable)
                    ),
                ]
            })
            .collect(),
        CumulativeExplanationType::BigStep => conflict_profile
            .profile_tasks
            .iter()
            .flat_map(|profile_task| {
                [
                    predicate!(
                        profile_task.start_variable
                            >= conflict_profile.end - profile_task.processing_time + 1
                    ),
                    predicate!(profile_task.start_variable <= conflict_profile.start),
                ]
            })
            .collect(),
        CumulativeExplanationType::PointWise => {
            // As stated in improving scheduling by learning, we choose the middle point; this
            // could potentially be improved
            let middle_point =
                (conflict_profile.end - conflict_profile.start) / 2 + conflict_profile.start;
            pumpkin_assert_simple!(
                middle_point >= conflict_profile.start && middle_point <= conflict_profile.end
            );

            conflict_profile
                .profile_tasks
                .iter()
                .flat_map(|profile_task| {
                    [
                        predicate!(
                            profile_task.start_variable
                                >= middle_point - profile_task.processing_time + 1
                        ),
                        predicate!(profile_task.start_variable <= middle_point),
                    ]
                })
                .collect()
        }
    }
}

fn add_propagating_task_predicate_lower_bound<Var: IntegerVariable + 'static>(
    mut explanation: PropositionalConjunction,
    explanation_type: CumulativeExplanationType,
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> PropositionalConjunction {
    explanation.add(create_predicate_propagating_task_lower_bound_propagation(
        explanation_type,
        context,
        task,
        profile,
        time_point,
    ));
    explanation
}

fn add_propagating_task_predicate_upper_bound<Var: IntegerVariable + 'static>(
    mut explanation: PropositionalConjunction,
    explanation_type: CumulativeExplanationType,
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> PropositionalConjunction {
    explanation.add(create_predicate_propagating_task_upper_bound_propagation(
        explanation_type,
        context,
        task,
        profile,
        time_point,
    ));
    explanation
}

fn create_big_step_propagation_explanation<Var: IntegerVariable + 'static>(
    profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= profile.end - profile_task.processing_time
                ),
                predicate!(profile_task.start_variable <= profile.start),
            ]
        })
        .collect()
}

fn create_pointwise_propagation_explanation<Var: IntegerVariable + 'static>(
    time_point: i32,
    profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(move |profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= time_point + 1 - profile_task.processing_time
                ),
                predicate!(profile_task.start_variable <= time_point),
            ]
        })
        .collect()
}

fn create_naive_propagation_explanation<'a, Var: IntegerVariable + 'static>(
    profile: &'a ResourceProfile<Var>,
    context: &'a PropagationContext,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable
                        >= context.lower_bound(&profile_task.start_variable)
                ),
                predicate!(
                    profile_task.start_variable
                        <= context.upper_bound(&profile_task.start_variable)
                ),
            ]
        })
        .collect()
}

pub(crate) fn create_predicate_propagating_task_lower_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    explanation_type: CumulativeExplanationType,
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> Predicate {
    match explanation_type {
        CumulativeExplanationType::Naive => {
            predicate!(task.start_variable >= context.lower_bound(&task.start_variable))
        }
        CumulativeExplanationType::BigStep => {
            predicate!(task.start_variable >= profile.start + 1 - task.processing_time)
        }
        CumulativeExplanationType::PointWise => {
            predicate!(
                task.start_variable
                    >= time_point.expect(
                        "Expected time-point to be provided to pointwise explanation creation"
                    ) + 1
                        - task.processing_time
            )
        }
    }
}

pub(crate) fn create_predicate_propagating_task_upper_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    explanation_type: CumulativeExplanationType,
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> Predicate {
    match explanation_type {
        CumulativeExplanationType::Naive => {
            predicate!(task.start_variable <= context.upper_bound(&task.start_variable))
        }
        CumulativeExplanationType::BigStep => {
            predicate!(
                task.start_variable
                    <= max(context.upper_bound(&task.start_variable), profile.start)
            )
        }
        CumulativeExplanationType::PointWise => predicate!(
            task.start_variable
                <= time_point
                    .expect("Expected time-point to be provided to pointwise explanation creation")
        ),
    }
}
