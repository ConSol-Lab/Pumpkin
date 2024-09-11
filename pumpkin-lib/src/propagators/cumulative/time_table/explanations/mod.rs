pub(crate) mod big_step;
pub(crate) mod naive;
pub(crate) mod pointwise;
use std::fmt::Display;
use std::rc::Rc;

use big_step::create_big_step_predicate_propagating_task_lower_bound_propagation;
use big_step::create_big_step_predicate_propagating_task_upper_bound_propagation;
use naive::create_naive_predicate_propagating_task_lower_bound_propagation;
use naive::create_naive_predicate_propagating_task_upper_bound_propagation;
use pointwise::create_pointwise_predicate_propagating_task_lower_bound_propagation;
use pointwise::create_pointwise_predicate_propagating_task_upper_bound_propagation;

use super::time_table_util::ResourceProfile;
use crate::engine::propagation::PropagationContext;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Determines what type of explanation is used for the cumulative constraint based on the
/// explanations described in Section 4.5.1 and 4.5.2 of \[1\].
///
/// For the explanations of conflicts and conflicts, we different between 3 types of explanations:
/// - The naive explanation (see [`CumulativeExplanationType::Naive`])
/// - The bigstep explanation (see [CumulativeExplanationType::BigStep])
/// - The pointwise explanation (see [CumulativeExplanationType::PointWise])
///
/// # Bibliography
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug, Clone, Copy, Default)]
pub enum CumulativeExplanationType {
    /// The naive explanation approach simply uses the current bounds of the profile and the
    /// propagated task in the explanation.
    Naive,
    /// The default; lifts the explanation to create an explanation which uses the bounds which
    /// would cause the tasks in the profile to have mandatory parts in the range of the
    /// propagating profile.
    #[default]
    BigStep,
    /// Creates an explanation over a set of time-points;
    ///
    /// ## Propagations
    /// Note that we currently do not generate chains of profiles which cause a propagation. This
    /// means that the explanation only concerns a single profile; the selected time-points for
    /// a propagation of task i are constructed as follows in the case of a lower-bound
    /// propagation: `[profile.start, profile.start + i.process_time, profile.start + (2 *
    /// i.processing_time), ..., profile.end]`. Thus, if the profile is shorter than
    /// `i.processing_time`, two explanations are generated for the time-points `profile.start`
    /// and `profile.end`.
    ///
    /// ## Conflicts
    /// For conflicts we follow the work by Schutt (see the documentation for
    /// [`CumulativeExplanationType`]) and select the middle point in the profile as the point used
    /// for the explanation.
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

/// Creates the lower-bound [`Predicate`] of the `propagating_task` based on the `explanation_type`.
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
            create_naive_predicate_propagating_task_lower_bound_propagation(context, task)
        }
        CumulativeExplanationType::BigStep => {
            create_big_step_predicate_propagating_task_lower_bound_propagation(task, profile)
        }
        CumulativeExplanationType::PointWise => {
            create_pointwise_predicate_propagating_task_lower_bound_propagation(task, time_point)
        }
    }
}

/// Adds the lower-bound predicate of the propagating task to the provided `explanation`.
pub(crate) fn add_propagating_task_predicate_lower_bound<Var: IntegerVariable + 'static>(
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

/// Creates the upper-bound [`Predicate`] of the `propagating_task` based on the `explanation_type`.
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
            create_naive_predicate_propagating_task_upper_bound_propagation(context, task)
        }
        CumulativeExplanationType::BigStep => {
            create_big_step_predicate_propagating_task_upper_bound_propagation(
                task, profile, context,
            )
        }
        CumulativeExplanationType::PointWise => {
            create_pointwise_predicate_propagating_task_upper_bound_propagation(task, time_point)
        }
    }
}

/// Adds the upper-bound predicate of the propagating task to the provided `explanation`.
pub(crate) fn add_propagating_task_predicate_upper_bound<Var: IntegerVariable + 'static>(
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
