use std::cell::OnceCell;
use std::cmp::max;
use std::fmt::Display;
use std::rc::Rc;

use super::time_table_util::ResourceProfile;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::Task;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

#[derive(Debug, Clone, Copy, Default)]
pub enum ExplanationType {
    Naive,
    BigStep,
    #[default]
    PointWise,
}

impl Display for ExplanationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExplanationType::Naive => write!(f, "naive"),
            ExplanationType::BigStep => write!(f, "big-step"),
            ExplanationType::PointWise => write!(f, "pointwise"),
        }
    }
}

pub(crate) fn create_right_hand_side_lower_bound_propagation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    explanation_type: ExplanationType,
) -> Predicate {
    match explanation_type {
        ExplanationType::Naive => {
            predicate!(task.start_variable >= context.lower_bound(&task.start_variable))
        }
        ExplanationType::BigStep | ExplanationType::PointWise => {
            predicate!(task.start_variable >= profile.start + 1 - task.processing_time)
        }
    }
}

pub(crate) fn create_right_hand_side_upper_bound_propagation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    explanation_type: ExplanationType,
) -> Predicate {
    match explanation_type {
        ExplanationType::Naive => {
            predicate!(task.start_variable <= context.upper_bound(&task.start_variable))
        }
        ExplanationType::BigStep | ExplanationType::PointWise => {
            predicate!(
                task.start_variable
                    <= max(context.upper_bound(&task.start_variable), profile.start)
            )
        }
    }
}

/// Initialises the `profile_explanation` structure with the [`Predicate::lower_bound_predicate`]
/// and [`Predicate::upper_bound_predicate`] of all of the [`ResourceProfile::profile_tasks`] of the
/// provided `profile`.
///
/// Creates an explanation consisting of all bounds of the variables causing a propagation (See [Section 4.5 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)).
/// Note that this is not necessarily a minimal explanation, it could be the case that some of the
/// tasks can be removed from the explanation depending on which task is propagated.
///
/// Note that this method stores an [`Rc`] in `profile_explanations` and then clones it, this means
/// that the [`Rc`] is created only once for each profile.
///
/// # Bibliography
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
pub(crate) fn create_profile_explanation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    profile: &ResourceProfile<Var>,
    profile_explanation: &mut OnceCell<Rc<Vec<Predicate>>>,
    explanation_type: ExplanationType,
) -> Rc<Vec<Predicate>> {
    Rc::clone(profile_explanation.get_or_init(|| {
        Rc::new(match explanation_type {
            // This explanation reflects the current bounds of the activities
            ExplanationType::Naive => profile
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
                .collect::<Vec<Predicate>>(),
            ExplanationType::BigStep | ExplanationType::PointWise => {
                // At the moment, we only propagate using a single profile rather than a chain of
                // profiles, this means that the stepwise explanation is the same as the pointwise
                // explanation, this could change in the future!
                profile
                    .profile_tasks
                    .iter()
                    .flat_map(|profile_task| {
                        [
                            predicate!(
                                profile_task.start_variable
                                    >= profile.end - profile_task.processing_time
                            ),
                            predicate!(profile_task.start_variable <= profile.start),
                        ]
                    })
                    .collect::<Vec<Predicate>>()
            }
        })
    }))
}

pub(crate) fn create_conflict_explanation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    conflict_profile: &ResourceProfile<Var>,
    explanation_type: ExplanationType,
) -> PropositionalConjunction {
    match explanation_type {
        ExplanationType::Naive => conflict_profile
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
        ExplanationType::BigStep => conflict_profile
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
        ExplanationType::PointWise => {
            // As stated in improving scheduling by learning, we choose the middle point; this could
            // potentially be improved
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
