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
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::Domains;
use pumpkin_core::variables::IntegerVariable;

use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;

/// Determines what type of explanation is used for the cumulative constraint based on the
/// explanations described in Section 4.5.1 and 4.5.2 of \[1\].
///
/// For the explanations of conflicts and conflicts, we different between 3 types of explanations:
/// - The naive explanation (see [`CumulativeExplanationType::Naive`])
/// - The bigstep explanation (see [CumulativeExplanationType::BigStep])
/// - The pointwise explanation (see [CumulativeExplanationType::Pointwise])
///
/// # Bibliography
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum CumulativeExplanationType {
    /// The naive explanation approach simply uses the current bounds of the profile and the
    /// propagated task in the explanation.
    Naive,
    /// Lifts the explanation to create an explanation which uses the bounds which
    /// would cause the tasks in the profile to have mandatory parts in the range of the
    /// propagating profile.
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
    #[default]
    Pointwise,
}

impl Display for CumulativeExplanationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativeExplanationType::Naive => write!(f, "naive"),
            CumulativeExplanationType::BigStep => write!(f, "big-step"),
            CumulativeExplanationType::Pointwise => write!(f, "pointwise"),
        }
    }
}

/// Calculates a minimal set of tasks which overflows the capacity (*not* the minimum set of
/// tasks) and applies the provided `convert_to_predicate` function.
pub(crate) fn get_minimal_profile<Var: IntegerVariable + 'static, ConversionFunction>(
    profile: &ResourceProfile<Var>,
    convert_to_predicate: ConversionFunction,
    capacity: i32,
    propagating_task_usage: Option<i32>,
) -> impl Iterator<Item = Predicate>
where
    ConversionFunction: Fn(&Task<Var>) -> [Predicate; 2],
{
    // Note that the minimal height does not contain the final minimal height; the closure gets a
    // copy of minimal height, but it passes a mutable reference in each iteration
    let mut minimal_height = profile.height + propagating_task_usage.unwrap_or_default();
    profile
        .profile_tasks
        .iter()
        .filter_map(move |task| {
            if minimal_height - task.resource_usage > capacity {
                minimal_height -= task.resource_usage;
                None
            } else {
                Some(convert_to_predicate(task))
            }
        })
        .flatten()
}

/// Creates the lower-bound [`Predicate`] of the `propagating_task` based on the `explanation_type`.
pub(crate) fn create_predicate_propagating_task_lower_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    explanation_type: CumulativeExplanationType,
    context: Domains,
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
        CumulativeExplanationType::Pointwise => {
            create_pointwise_predicate_propagating_task_lower_bound_propagation(task, time_point)
        }
    }
}

/// Adds the lower-bound predicate of the propagating task to the provided `explanation`.
pub(crate) fn add_propagating_task_predicate_lower_bound<Var: IntegerVariable + 'static>(
    explanation: impl Iterator<Item = Predicate>,
    explanation_type: CumulativeExplanationType,
    context: Domains,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> impl Iterator<Item = Predicate> {
    explanation.chain(std::iter::once(
        create_predicate_propagating_task_lower_bound_propagation(
            explanation_type,
            context,
            task,
            profile,
            time_point,
        ),
    ))
}

/// Creates the upper-bound [`Predicate`] of the `propagating_task` based on the `explanation_type`.
pub(crate) fn create_predicate_propagating_task_upper_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    explanation_type: CumulativeExplanationType,
    context: Domains,
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
        CumulativeExplanationType::Pointwise => {
            create_pointwise_predicate_propagating_task_upper_bound_propagation(task, time_point)
        }
    }
}

/// Adds the upper-bound predicate of the propagating task to the provided `explanation`.
pub(crate) fn add_propagating_task_predicate_upper_bound<Var: IntegerVariable + 'static>(
    explanation: impl Iterator<Item = Predicate>,
    explanation_type: CumulativeExplanationType,
    context: Domains,
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    time_point: Option<i32>,
) -> impl Iterator<Item = Predicate> {
    explanation.chain(std::iter::once(
        create_predicate_propagating_task_upper_bound_propagation(
            explanation_type,
            context,
            task,
            profile,
            time_point,
        ),
    ))
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::propagation::LocalId;
    use pumpkin_core::state::State;

    use crate::cumulative::ResourceProfile;
    use crate::cumulative::Task;
    use crate::cumulative::time_table::explanations::get_minimal_profile;

    #[test]
    fn test_minimal_conflict_returned() {
        let mut state = State::default();

        let profile = ResourceProfile {
            start: 5,
            end: 10,
            profile_tasks: vec![
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 5,
                    id: LocalId::from(0),
                }),
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 1,
                    id: LocalId::from(1),
                }),
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 2,
                    id: LocalId::from(2),
                }),
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 4,
                    id: LocalId::from(3),
                }),
            ],
            height: 12,
        };

        let minimal_profile = get_minimal_profile(
            &profile,
            |_| [Predicate::trivially_true(), Predicate::trivially_true()],
            9,
            Some(1),
        );

        assert_eq!(minimal_profile.count() / 2, 2);
    }

    #[test]
    fn test_does_not_remove_both() {
        let mut state = State::default();

        let profile = ResourceProfile {
            start: 5,
            end: 10,
            profile_tasks: vec![
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 1,
                    id: LocalId::from(1),
                }),
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 1,
                    id: LocalId::from(2),
                }),
                Rc::new(Task {
                    start_variable: state.new_interval_variable(5, 5, None),
                    processing_time: 6,
                    resource_usage: 4,
                    id: LocalId::from(3),
                }),
            ],
            height: 6,
        };

        let minimal_profile = get_minimal_profile(
            &profile,
            |_| [Predicate::trivially_true(), Predicate::trivially_true()],
            4,
            None,
        );

        assert_eq!(minimal_profile.count() / 2, 2);
    }
}
