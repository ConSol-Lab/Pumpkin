use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::cumulative::time_table::time_table_util::ResourceProfile;
use crate::variables::IntegerVariable;

/// Creates the propagation explanation using the big-step approach (see
/// [`CumulativeExplanationType::BigStep`])
pub(crate) fn create_big_step_propagation_explanation<Var: IntegerVariable + 'static>(
    profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= profile.end - profile_task.processing_time + 1
                ),
                predicate!(profile_task.start_variable <= profile.start),
            ]
        })
        .collect()
}

/// Creates the conflict explanation using the big-step approach (see
/// [`CumulativeExplanationType::BigStep`])
pub(crate) fn create_big_step_conflict_explanation<Var: IntegerVariable + 'static>(
    conflict_profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    conflict_profile
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
        .collect()
}
