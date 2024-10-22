use std::rc::Rc;

use crate::basic_types::ConflictInfo;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::PropagationContext;
use crate::propagators::create_time_table_per_point_from_scratch;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfile;
use crate::variables::IntegerVariable;

pub(crate) fn check_synchronisation_conflict_explanation_per_point<
    Var: IntegerVariable + 'static,
>(
    synchronised_conflict_explanation: &PropagationStatusCP,
    context: PropagationContext,
    parameters: &CumulativeParameters<Var>,
) -> bool {
    let error_from_scratch = create_time_table_per_point_from_scratch(context, parameters);
    if let Err(explanation_scratch) = error_from_scratch {
        if let Err(Inconsistency::Other(ConflictInfo::Explanation(explanation))) =
            &synchronised_conflict_explanation
        {
            *explanation == explanation_scratch
        } else {
            false
        }
    } else {
        false
    }
}

pub(crate) fn find_synchronised_conflict<'a, Var: IntegerVariable + 'static>(
    time_table: impl Iterator<Item = (&'a u32, &'a ResourceProfile<Var>)>,
    parameters: &CumulativeParameters<Var>,
) -> u32 {
    let (profile_time_point, _) = time_table
        .filter(|(_, profile)| profile.height > parameters.capacity)
        .fold(
            (u32::MAX, u32::MAX),
            |(time_point, max_id), (profile_time_point, profile)| {
                let max = profile
                    .profile_tasks
                    .iter()
                    .map(|profile| profile.id.unpack())
                    .max()
                    .unwrap_or(u32::MAX);
                if max < max_id {
                    (*profile_time_point, max)
                } else {
                    (time_point, max_id)
                }
            },
        );
    profile_time_point
}

pub(crate) fn create_synchronised_conflict_explanation<Var: IntegerVariable + 'static>(
    context: PropagationContext,
    conflicting_profile: &mut ResourceProfile<Var>,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // If we need to synchronise then we need to find the conflict profile which
    // would have been found by the non-incremental propagator; this conflict
    // profile satisfies the following conditions:
    //
    // 1. It is the conflict profile with the earliest start
    // 2. It contains only the tasks necessary to overflow the capacity, sorted first in order of
    //    upper-bound and then tie-breaking is performed on the ID

    // First we sort on the ID
    sort_profile_based_on_id(conflicting_profile);

    let mut resource_usage = 0;
    let mut index = 0;
    let mut new_profile = Vec::new();

    while resource_usage <= parameters.capacity {
        let task = &conflicting_profile.profile_tasks[index];
        resource_usage += task.resource_usage;
        new_profile.push(Rc::clone(task));
        index += 1;
    }

    Err(create_conflict_explanation(
        context,
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

pub(crate) fn synchronise_time_table<'a, Var: IntegerVariable + 'static>(
    time_table: impl Iterator<Item = &'a mut ResourceProfile<Var>>,
) {
    time_table.for_each(|profile| sort_profile_based_on_id(profile))
}

fn sort_profile_based_on_id<Var: IntegerVariable + 'static>(profile: &mut ResourceProfile<Var>) {
    profile
        .profile_tasks
        .sort_by(|a, b| a.id.unpack().cmp(&b.id.unpack()));
}
