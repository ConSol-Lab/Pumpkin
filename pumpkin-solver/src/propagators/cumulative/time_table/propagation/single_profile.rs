use super::CanUpdate;
use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContextMut;
use crate::propagators::cumulative::time_table::propagation::find_possible_updates;
use crate::propagators::cumulative::time_table::propagation_handler::CumulativePropagationHandler;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::UpdatableStructures;
use crate::variables::IntegerVariable;

/// For each profile in chronological order, this method goes through the tasks and checks whether
/// the profile can propagate the domain of the task.
///
/// If it can then it will immediately propagate it even if this propagation would cause subsequent
/// propagations by the next profile. For a method which propagates based on a sequence of profiles
/// see [`propagate_sequence_of_profiles`].
///
/// This type of propagation is likely to be less beneficial for the explanation
/// [`CumulativeExplanationType::Pointwise`].
pub(crate) fn propagate_single_profiles<
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

    // Then we go over all of the profiles in the time-table
    'profile_loop: for profile in time_table {
        // We indicate to the propagation handler that we cannot re-use an existing profile
        // explanation
        propagation_handler.next_profile();

        // Then we go over all the different tasks
        let mut task_index = 0;
        while task_index
            < if profile.is_updated() {
                updatable_structures.number_of_unfixed_tasks()
            } else {
                updatable_structures.number_of_updated_tasks()
            }
        {
            let task = if profile.is_updated() {
                updatable_structures.get_unfixed_task_at_index(task_index)
            } else {
                updatable_structures.get_updated_task_at_index(task_index)
            };
            if context.is_fixed(&task.start_variable)
                || profile.get_start()
                    > context.upper_bound(&task.start_variable) + task.processing_time
            {
                // The task is currently fixed after propagating
                //
                // Note that we fix this task temporarily and then wait for the notification to
                // come in before properly fixing it - this is to avoid fixing a task without ever
                // receiving the notification for it (which would result in a task never becoming
                // unfixed since no backtrack notification would occur)
                updatable_structures.temporarily_remove_task_from_unfixed(&task);
                if SHOULD_RESET_UPDATED {
                    updatable_structures.remove_task_from_updated(&task);
                }
                if updatable_structures.has_no_unfixed_tasks() {
                    // There are no tasks left to consider, we can exit the loop
                    break 'profile_loop;
                }
                continue;
            }

            task_index += 1;

            // We get the updates which are possible (i.e. a lower-bound update, an upper-bound
            // update or a hole in the domain)
            let possible_updates = find_possible_updates(context, &task, profile, parameters);
            for possible_update in possible_updates.iter() {
                // For every possible update we let the propagation handler propagate
                let result = match possible_update {
                    CanUpdate::LowerBound => propagation_handler
                        .propagate_lower_bound_with_explanations(context, profile, &task),
                    CanUpdate::UpperBound => propagation_handler
                        .propagate_upper_bound_with_explanations(context, profile, &task),
                    CanUpdate::Holes => {
                        propagation_handler.propagate_holes_in_domain(context, profile, &task)
                    }
                };
                if result.is_err() {
                    updatable_structures.restore_temporarily_removed();
                    // Note that we do not reset the updated here since we cannot know whether the
                    // backtracking will undo the update; if this were not to be te case and we
                    // were to remove the task from the updated set then it would lead to potential
                    // missed propagations
                    result?;
                }
            }
            if !possible_updates.is_empty() {
                updatable_structures.task_has_been_updated(task);
            }
        }

        if SHOULD_RESET_UPDATED {
            profile.mark_processed();
        }
    }

    if SHOULD_RESET_UPDATED {
        // If we should reset the updated then we remove all of the updated from consideration
        while updatable_structures.number_of_updated_tasks() > 0 {
            let updated_task = updatable_structures.get_updated_task_at_index(0);
            updatable_structures.remove_task_from_updated(&updated_task);
        }
    }
    updatable_structures.restore_temporarily_removed();
    Ok(())
}
