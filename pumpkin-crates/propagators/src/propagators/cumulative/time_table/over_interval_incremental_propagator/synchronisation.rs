use std::rc::Rc;

use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::Conflict;
use pumpkin_core::variables::IntegerVariable;

use super::debug::are_mergeable;
use super::debug::merge_profiles;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::ResourceProfile;
use crate::cumulative::time_table::OverIntervalTimeTableType;
use crate::cumulative::time_table::create_time_table_over_interval_from_scratch;
use crate::cumulative::time_table::propagation_handler::create_conflict_explanation;

/// Finds the conflicting profile which would have been found by the
/// [`TimeTableOverIntervalPropagator`]; this is the first conflicting profile in terms of start
/// time, however, the returned profile should be merged with adjacent profiles to create the
/// returned conflict profile.
pub(crate) fn find_synchronised_conflict<Var: IntegerVariable + 'static>(
    time_table: &mut OverIntervalTimeTableType<Var>,
    parameters: &CumulativeParameters<Var>,
) -> Option<ResourceProfile<Var>> {
    if time_table.is_empty() {
        return None;
    }

    let first_conflict_profile_index = time_table
        .iter()
        .position(|profile| profile.height > parameters.capacity);
    if let Some(mut first_conflict_profile_index) = first_conflict_profile_index {
        let mut new_profile = time_table[first_conflict_profile_index].clone();

        while first_conflict_profile_index < time_table.len() - 1 {
            if are_mergeable(
                &time_table[first_conflict_profile_index],
                &time_table[first_conflict_profile_index + 1],
            ) {
                new_profile.end = time_table[first_conflict_profile_index + 1].end;
                first_conflict_profile_index += 1;
            } else {
                break;
            }
        }
        return Some(new_profile);
    }
    None
}

/// Returns whether the synchronised conflict explanation created by
/// [`TimeTableOverIntervalIncrementalPropgator`] is the same as that created by
/// [`TimeTableOverIntervalPropagator`].
pub(crate) fn check_synchronisation_conflict_explanation_over_interval<
    Var: IntegerVariable + 'static,
>(
    synchronised_conflict_explanation: &PropagationStatusCP,
    context: Domains,
    parameters: &mut CumulativeParameters<Var>,
    inference_code: InferenceCode,
) -> bool {
    let error_from_scratch =
        create_time_table_over_interval_from_scratch(context, parameters, inference_code);
    if let Err(explanation_scratch) = error_from_scratch {
        if let Err(Conflict::Propagator(explanation)) = &synchronised_conflict_explanation {
            // We check whether both inconsistencies are of the same type and then we check their
            // corresponding explanations
            explanation.conjunction == explanation_scratch.conjunction
        } else {
            false
        }
    } else {
        false
    }
}

/// Given the `conflicting_profile` (which is the same conflict profile which would have been found
/// by [`TimeTableOverIntervalPropagator`]), this function calculates the error which would have
/// been reported by [`TimeTableOverIntervalPropagator`] by finding the tasks which should be
/// included in the profile and sorting them in the same order.
pub(crate) fn create_synchronised_conflict_explanation<Var: IntegerVariable + 'static>(
    mut context: Domains,
    inference_code: InferenceCode,
    conflicting_profile: &mut ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // If we need to synchronise then we need to find the conflict profile which
    // would have been found by the non-incremental propagator; we thus first sort based on
    // upper-bounds of the tasks (i.e. the starts of the mandatory parts) and then tie-break on the
    // ids and take the first `n` tasks which lead to an overflow

    // First we sort based on the upper-bound of the task and then we sort based on
    // the ID if there is a tie
    sort_profile_based_on_upper_bound_and_id(conflicting_profile, context.reborrow());

    let mut resource_usage = 0;
    let mut index = 0;
    let mut new_profile = Vec::new();

    // Now we find the tasks in the profile which together overflow the resource
    while resource_usage <= parameters.capacity {
        let task = &conflicting_profile.profile_tasks[index];
        resource_usage += task.resource_usage;
        new_profile.push(Rc::clone(task));
        index += 1;
    }

    Err(create_conflict_explanation(
        context,
        inference_code,
        &ResourceProfile {
            start: conflicting_profile.start,
            end: conflicting_profile.end,
            profile_tasks: new_profile,
            height: resource_usage,
        },
        parameters.options.explanation_type,
    )
    .into())
}

/// Synchronises the time-table; two actions are performed:
/// 1. Adjacent profiles are merged which have been split due to the incremental updates
/// 2. Each profile is sorted such that it corresponds to the order in which
///    [`TimeTableOverIntervalPropagator`] would have found them
pub(crate) fn synchronise_time_table<Var: IntegerVariable + 'static>(
    time_table: &mut OverIntervalTimeTableType<Var>,
    mut context: Domains,
) {
    if !time_table.is_empty() {
        // If the time-table is not empty then we merge all the profiles in the range
        let time_table_len = time_table.len();
        merge_profiles(time_table, 0, time_table_len - 1);
    }

    // And then we sort each profile according to upper-bound and then ID
    time_table
        .iter_mut()
        .for_each(|profile| sort_profile_based_on_upper_bound_and_id(profile, context.reborrow()))
}

/// Sorts the provided `profile` on non-decreasing order of upper-bound while tie-breaking in
/// non-decreasing order of ID
fn sort_profile_based_on_upper_bound_and_id<Var: IntegerVariable + 'static>(
    profile: &mut ResourceProfile<Var>,
    context: Domains,
) {
    profile.profile_tasks.sort_by(|a, b| {
        // First match on the upper-bound of the variable
        match context
            .upper_bound(&a.start_variable)
            .cmp(&context.upper_bound(&b.start_variable))
        {
            std::cmp::Ordering::Equal => {
                // Then sort on ID if the upper-bounds are equal
                a.id.unpack().cmp(&b.id.unpack())
            }
            other => other,
        }
    });
}
