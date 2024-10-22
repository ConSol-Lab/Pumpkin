use std::rc::Rc;

use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::CumulativeParameters;
use crate::propagators::ResourceProfile;
use crate::variables::IntegerVariable;

fn sort_profile_based_on_upper_bound_and_id<Var: IntegerVariable + 'static>(
    profile: &mut ResourceProfile<Var>,
    context: PropagationContext,
) {
    profile.profile_tasks.sort_by(|a, b| {
        match context
            .upper_bound(&a.start_variable)
            .cmp(&context.upper_bound(&b.start_variable))
        {
            std::cmp::Ordering::Equal => a.id.unpack().cmp(&b.id.unpack()),
            other => other,
        }
    });
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

    // First we sort based on the upper-bound of the task and then we sort based on
    // the ID if there is a tie
    sort_profile_based_on_upper_bound_and_id(conflicting_profile, context);

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
    context: PropagationContext,
) {
    time_table.for_each(|profile| sort_profile_based_on_upper_bound_and_id(profile, context))
}
