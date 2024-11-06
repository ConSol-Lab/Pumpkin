use std::cmp::max;
use std::rc::Rc;

use super::find_possible_updates;
use super::lower_bound_can_be_propagated_by_profile;
use super::overflows_capacity_and_is_not_part_of_profile;
use super::upper_bound_can_be_propagated_by_profile;
use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::propagators::cumulative::time_table::propagation_handler::CumulativePropagationHandler;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::Task;
use crate::propagators::UpdatableStructures;
use crate::variables::IntegerVariable;

/// For each task this method goes through the profiles in chronological order to find one which can
/// update the task's bounds.
///
/// If it can find such a profile then it proceeds to generate a sequence of profiles
/// which can propagate the bound of the task and uses these to explain the propagation rather than
/// the individual profiles (for propagating individual profiles see [`propagate_single_profiles`]).
///
/// Especially in the case of [`CumulativeExplanationType::Pointwise`] this is likely to be
/// beneficial.
pub(crate) fn propagate_sequence_of_profiles<
    'a,
    Var: IntegerVariable + 'static,
    const SHOULD_RESET_UPDATED: bool,
>(
    context: &mut PropagationContextMut,
    time_table: impl Iterator<Item = &'a mut (impl ResourceProfileInterface<Var> + 'a)>,
    updatable_structures: &mut UpdatableStructures<Var>,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // We create the structure responsible for propagations and explanations
    let mut propagation_handler =
        CumulativePropagationHandler::new(parameters.options.explanation_type);

    // We collect the time-table since we will need to index into it
    let mut time_table = time_table.collect::<Vec<_>>();

    // Then we go over all the possible tasks
    let mut task_index = 0;
    while task_index < updatable_structures.number_of_unfixed_tasks() {
        let task = updatable_structures.get_unfixed_task_at_index(task_index);
        if context.is_fixed(&task.start_variable) {
            // If the task is fixed then we are not able to propagate it further
            task_index += 1;
            continue;
        }

        let task_has_been_updated = updatable_structures.has_task_been_updated(&task);

        // Then we go over all the different profiles
        let mut profile_index = 0;
        'profile_loop: while profile_index < time_table.len() {
            let profile = &time_table[profile_index];

            if profile.get_start()
                > context.upper_bound(&task.start_variable) + task.processing_time
            {
                // The profiles are sorted, if we cannot update using this one then we cannot update
                // using the subsequent profiles, we can break from the loop
                break 'profile_loop;
            }

            if !task_has_been_updated && !profile.is_updated() {
                // Both the task and the profile have not been updated which means that we can
                // continue to the next profile (which might have been updated)
                profile_index += 1;
                continue;
            }

            let possible_upates = find_possible_updates(context, &task, *profile, parameters);

            if possible_upates.is_empty() {
                // The task cannot be propagate by the profile so we move to the next one
                profile_index += 1;
                continue;
            }

            propagation_handler.next_profile();

            // Keep track of the next profile index to use after we generate the sequence of
            // profiles
            let mut new_profile_index = profile_index;

            // Then we check what propagations can be performed
            if lower_bound_can_be_propagated_by_profile(
                context.as_readonly(),
                &task,
                *profile,
                parameters.capacity,
            ) {
                // We find the index (non-inclusive) of the last profile in the chain of lower-bound
                // propagations
                let last_index = find_index_last_profile_which_propagates_lower_bound(
                    profile_index,
                    time_table.iter().map(|element| &**element),
                    context.as_readonly(),
                    &task,
                    parameters.capacity,
                );

                // Then we provide the propagation handler with the chain of profiles and propagate
                // all of them
                propagation_handler.propagate_chain_of_lower_bounds_with_explanations(
                    context,
                    time_table[profile_index..last_index]
                        .iter()
                        .map(|element| &**element),
                    &task,
                )?;

                // The bounds of the tasks have now been updated and this should be accounted for
                // when propagating further
                updatable_structures.task_has_been_updated(Rc::clone(&task));

                // Then we set the new profile index to the last index, note that this index (since
                // it is non-inclusive) will always be larger than the current profile index
                new_profile_index = last_index;
            }

            if upper_bound_can_be_propagated_by_profile(
                context.as_readonly(),
                &task,
                *profile,
                parameters.capacity,
            ) {
                // We find the index (inclusive) of the last profile in the chain of upper-bound
                // propagations (note that the index of this last profile in the chain is `<=
                // profile_index`)
                let first_index = find_index_last_profile_which_propagates_upper_bound(
                    profile_index,
                    time_table.iter().map(|element| &**element),
                    context.as_readonly(),
                    &task,
                    parameters.capacity,
                );
                // Then we provide the propagation handler with the chain of profiles and propagate
                // all of them
                propagation_handler.propagate_chain_of_upper_bounds_with_explanations(
                    context,
                    time_table[first_index..=profile_index]
                        .iter()
                        .map(|element| &**element),
                    &task,
                )?;

                // The bounds of the tasks have now been updated and this should be accounted for
                // when propagating further
                updatable_structures.task_has_been_updated(Rc::clone(&task));

                // Then we set the new profile index to maximum of the previous value of the new
                // profile index and the next profile index
                new_profile_index = max(new_profile_index, profile_index + 1);
            }

            if parameters.options.allow_holes_in_domain {
                // If we allow the propagation of holes in the domain then we simply let the
                // propagation handler handle it
                propagation_handler.propagate_holes_in_domain(context, *profile, &task)?;

                // Then we set the new profile index to maximum of the previous value of the new
                // profile index and the next profile index
                new_profile_index = max(new_profile_index, profile_index + 1);
            }

            // Finally, we simply set the profile index to the index of the new profile
            profile_index = max(new_profile_index, profile_index + 1);
        }
        if SHOULD_RESET_UPDATED {
            updatable_structures.remove_task_from_updated(&task)
        }
        task_index += 1;
    }

    if SHOULD_RESET_UPDATED {
        time_table
            .iter_mut()
            .for_each(|profile| profile.mark_processed());
    }
    Ok(())
}

/// Returns the index of the profile which cannot propagate the lower-bound of the provided task any
/// further based on the propagation of the upper-bound due to `time_table[profile_index]`.
fn find_index_last_profile_which_propagates_lower_bound<
    'a,
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var> + 'a,
>(
    profile_index: usize,
    time_table: impl Iterator<Item = &'a ResourceProfileType>,
    context: PropagationContext,
    task: &Rc<Task<Var>>,
    capacity: i32,
) -> usize {
    let mut time_table = time_table.enumerate().peekable().skip(profile_index);

    let (mut index, mut current_profile) = time_table
        .next()
        .expect("Expected the number of profiles to exist to at least be equal to the next");
    for (next_index, next_profile) in time_table {
        if next_profile.get_start() - current_profile.get_end() >= task.processing_time
            || !overflows_capacity_and_is_not_part_of_profile(context, task, next_profile, capacity)
        {
            break;
        }
        index = next_index;
        current_profile = next_profile;
    }
    // Note that the index is non-inclusive
    index + 1
}

/// Returns the index of the last profile which could propagate the upper-bound of the task based on
/// the propagation of the upper-bound due to `time_table[profile_index]`.
fn find_index_last_profile_which_propagates_upper_bound<
    'a,
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var> + 'a,
>(
    profile_index: usize,
    time_table: impl DoubleEndedIterator<Item = &'a ResourceProfileType> + ExactSizeIterator,
    context: PropagationContext,
    task: &Rc<Task<Var>>,
    capacity: i32,
) -> usize {
    if profile_index == 0 {
        return 0;
    }
    let num_profiles = time_table.len();
    let mut time_table = time_table
        .enumerate()
        .rev()
        .skip(num_profiles - profile_index - 1);

    let (mut index, mut current_profile) = time_table
        .next()
        .expect("Expected element to exists at position {profile_index}");
    for (previous_index, previous_profile) in time_table {
        if current_profile.get_start() - previous_profile.get_end() >= task.processing_time
            || !overflows_capacity_and_is_not_part_of_profile(
                context,
                task,
                previous_profile,
                capacity,
            )
        {
            // The index here is already correctly set
            break;
        }

        current_profile = previous_profile;
        index = previous_index;
    }
    // Note that the index is inclusive
    index
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::find_index_last_profile_which_propagates_lower_bound;
    use crate::engine::propagation::LocalId;
    use crate::engine::propagation::PropagationContext;
    use crate::engine::AssignmentsInteger;
    use crate::engine::AssignmentsPropositional;
    use crate::propagators::cumulative::time_table::propagation::sequence::find_index_last_profile_which_propagates_upper_bound;
    use crate::propagators::ResourceProfile;
    use crate::propagators::Task;

    #[test]
    fn test_finding_last_index_lower_bound() {
        let mut assignments_integer = AssignmentsInteger::default();
        let assignments_propositional = AssignmentsPropositional::default();

        let x = assignments_integer.grow(0, 10);
        let y = assignments_integer.grow(5, 5);
        let z = assignments_integer.grow(8, 8);

        let time_table = [
            ResourceProfile::new(
                5,
                6,
                vec![Rc::new(Task {
                    start_variable: y,
                    processing_time: 2,
                    resource_usage: 1,
                    id: LocalId::from(1),
                })],
                1,
            ),
            ResourceProfile::new(
                8,
                8,
                vec![Rc::new(Task {
                    start_variable: z,
                    processing_time: 1,
                    resource_usage: 1,
                    id: LocalId::from(2),
                })],
                1,
            ),
        ];

        let last_index = find_index_last_profile_which_propagates_lower_bound(
            0,
            time_table.iter(),
            PropagationContext::new(&assignments_integer, &assignments_propositional),
            &Rc::new(Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            }),
            1,
        );
        assert_eq!(last_index, 2);
    }

    #[test]
    fn test_finding_last_index_upper_bound() {
        let mut assignments_integer = AssignmentsInteger::default();
        let assignments_propositional = AssignmentsPropositional::default();

        let x = assignments_integer.grow(7, 7);
        let y = assignments_integer.grow(5, 5);
        let z = assignments_integer.grow(8, 8);

        let time_table = [
            ResourceProfile::new(
                5,
                6,
                vec![Rc::new(Task {
                    start_variable: y,
                    processing_time: 2,
                    resource_usage: 1,
                    id: LocalId::from(1),
                })],
                1,
            ),
            ResourceProfile::new(
                8,
                8,
                vec![Rc::new(Task {
                    start_variable: z,
                    processing_time: 1,
                    resource_usage: 1,
                    id: LocalId::from(2),
                })],
                1,
            ),
        ];

        let last_index = find_index_last_profile_which_propagates_upper_bound(
            1,
            time_table.iter(),
            PropagationContext::new(&assignments_integer, &assignments_propositional),
            &Rc::new(Task {
                start_variable: x,
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(0),
            }),
            1,
        );
        assert_eq!(last_index, 0);
    }
}
