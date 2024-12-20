use std::rc::Rc;

use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::PropagationContext;
use crate::propagators::create_time_table_per_point_from_scratch;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::CumulativeParameters;
use crate::propagators::PerPointTimeTableType;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// Returns whether the synchronised conflict explanation created by
/// [`TimeTablePerPointIncrementalPropgator`] is the same as that created by
/// [`TimeTablePerPointPropagator`].
pub(crate) fn check_synchronisation_conflict_explanation_per_point<
    Var: IntegerVariable + 'static,
>(
    synchronised_conflict_explanation: &PropagationStatusCP,
    context: PropagationContext,
    parameters: &CumulativeParameters<Var>,
) -> bool {
    let error_from_scratch = create_time_table_per_point_from_scratch(context, parameters);
    if let Err(explanation_scratch) = error_from_scratch {
        if let Err(Inconsistency::Conflict(explanation)) = &synchronised_conflict_explanation {
            // We check whether both inconsistencies are of the same type and then we check their
            // corresponding explanations
            *explanation == explanation_scratch
        } else {
            false
        }
    } else {
        false
    }
}

/// Finds the conflicting profile which would have been found by the
/// [`TimeTablePerPointPropagator`]; this is the conflicting profile which has the minimum maximum
/// ID in set of the first `n` profile tasks (when sorted on ID) which overflow the capacity
pub(crate) fn find_synchronised_conflict<Var: IntegerVariable + 'static>(
    time_table: &mut PerPointTimeTableType<Var>,
    parameters: &CumulativeParameters<Var>,
) -> Option<u32> {
    let mut profile_time_point = None;
    let mut minimum_maximum_id = u32::MAX;

    // We go over every profile
    for (time_point, profile) in time_table.iter_mut() {
        if profile.height <= parameters.capacity {
            // If the profile cannot overflow the resource capacity then we move onto the next
            // profile
            continue;
        }

        // Then we find the minimum set of the first `n` consecutive tasks such that the capacity
        // is overflown and we get the last element in this set (which has the one with the maximum
        // ID since the profile is sorted in the method based on ID)
        let mut new_height = 0;
        let conflicting_tasks =
            get_minimum_set_of_tasks_which_overflow_capacity(profile, parameters, &mut new_height);
        if let Some(task_with_maximum_id) = conflicting_tasks.last() {
            pumpkin_assert_moderate!(new_height > parameters.capacity);
            if task_with_maximum_id.id.unpack() < minimum_maximum_id {
                minimum_maximum_id = task_with_maximum_id.id.unpack();
                profile_time_point = Some(*time_point);
            }
        }
    }
    profile_time_point
}

/// Sorts the profile based on ID and then finds the minimum set of consecutive tasks starting from
/// the first profile which would have overflown the resource capacity.
///
/// The sum of the heights of the tasks is stored in the provided `output_height`; note that this
/// means that the iterator should be consumed before reading the `output_height`
fn get_minimum_set_of_tasks_which_overflow_capacity<'a, Var: IntegerVariable + 'static>(
    profile: &'a mut ResourceProfile<Var>,
    parameters: &'a CumulativeParameters<Var>,
    output_height: &'a mut i32,
) -> impl Iterator<Item = Rc<Task<Var>>> + 'a {
    // First we sort the profile based on the ID
    sort_profile_based_on_id(profile);

    // We go over each task and take while the resource usage is less than or equal to the
    // capacity
    let calculate_tasks = |resource_usage: &'a mut i32| {
        profile
            .profile_tasks
            .iter()
            .take_while(move |task| {
                if *resource_usage > parameters.capacity {
                    return false;
                }
                *resource_usage += task.resource_usage;
                true
            })
            .cloned()
    };
    (calculate_tasks)(output_height)
}

/// Given the `conflicting_profile` (which is the same conflict profile which would have been found
/// by [`TimeTablePerPointPropagator`]), this function calculates the error which would have been
/// reported by [`TimeTablePerPointPropagator`] by finding the tasks which should be included in the
/// profile and sorting them in the same order.
pub(crate) fn create_synchronised_conflict_explanation<Var: IntegerVariable + 'static>(
    context: PropagationContext,
    conflicting_profile: &mut ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // Store because we are mutably borrowing the conflicting profile
    let new_profile_start = conflicting_profile.start;
    let new_profile_end = conflicting_profile.end;
    let mut new_height = 0;

    // If we need to synchronise then we need to find the conflict profile which
    // would have been found by the non-incremental propagator;
    // we thus sort on the IDs and take the first `n` tasks which lead to an overflow
    let new_profile = get_minimum_set_of_tasks_which_overflow_capacity(
        conflicting_profile,
        parameters,
        &mut new_height,
    )
    .collect();

    Err(create_conflict_explanation(
        context,
        &ResourceProfile {
            start: new_profile_start,
            end: new_profile_end,
            profile_tasks: new_profile,
            height: new_height,
        },
        parameters.options.explanation_type,
    )
    .into())
}

/// Synchronises the time-table; one action is performed:
/// 1. Each profile is sorted such that it corresponds to the order in which
///    [`TimeTableOverIntervalPropagator`] would have found them
pub(crate) fn synchronise_time_table<'a, Var: IntegerVariable + 'static>(
    time_table: impl Iterator<Item = &'a mut ResourceProfile<Var>>,
) {
    time_table.for_each(|profile| sort_profile_based_on_id(profile))
}

/// Sorts the provided `profile` on non-decreasing order of ID
fn sort_profile_based_on_id<Var: IntegerVariable + 'static>(profile: &mut ResourceProfile<Var>) {
    profile
        .profile_tasks
        .sort_by(|a, b| a.id.unpack().cmp(&b.id.unpack()));
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::find_synchronised_conflict;
    use crate::containers::KeyedVec;
    use crate::engine::propagation::LocalId;
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::CumulativeParameters;
    use crate::propagators::CumulativePropagatorOptions;
    use crate::propagators::PerPointTimeTableType;
    use crate::propagators::ResourceProfile;
    use crate::propagators::Task;

    #[test]
    fn test_correct_conflict_returned() {
        let mut solver = TestSolver::default();

        let x0 = solver.new_variable(0, 10);
        let x1 = solver.new_variable(0, 10);
        let x2 = solver.new_variable(0, 10);

        let tasks = vec![
            Task {
                start_variable: x0,
                processing_time: 2,
                resource_usage: 2,
                id: LocalId::from(0),
            },
            Task {
                start_variable: x1,
                processing_time: 2,
                resource_usage: 2,
                id: LocalId::from(1),
            },
            Task {
                start_variable: x2,
                processing_time: 2,
                resource_usage: 1,
                id: LocalId::from(2),
            },
        ];

        let parameters = CumulativeParameters::new(
            tasks,
            1,
            CumulativePropagatorOptions::default(),
            KeyedVec::default(),
        );

        let mut time_table = PerPointTimeTableType::default();
        let _ = time_table.insert(
            3,
            ResourceProfile {
                start: 3,
                end: 3,
                profile_tasks: vec![Rc::clone(&parameters.tasks[1])],
                height: 2,
            },
        );
        let _ = time_table.insert(
            4,
            ResourceProfile {
                start: 4,
                end: 4,
                profile_tasks: vec![
                    Rc::clone(&parameters.tasks[0]),
                    Rc::clone(&parameters.tasks[2]),
                ],
                height: 3,
            },
        );

        let result = find_synchronised_conflict(&mut time_table, &parameters);
        assert!(matches!(result, Some(4)));
    }
}
