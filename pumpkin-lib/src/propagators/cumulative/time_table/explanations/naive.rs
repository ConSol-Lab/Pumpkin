use std::rc::Rc;

use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::cumulative::time_table::time_table_util::ResourceProfile;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Creates the propagation explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_propagation_explanation<'a, Var: IntegerVariable + 'static>(
    profile: &'a ResourceProfile<Var>,
    context: &'a PropagationContext,
) -> PropositionalConjunction {
    profile
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
        .collect()
}

/// Creates the conflict explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_conflict_explanation<Var: IntegerVariable + 'static>(
    conflict_profile: &ResourceProfile<Var>,
    context: &PropagationContext,
) -> PropositionalConjunction {
    conflict_profile
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
        .collect()
}

pub(crate) fn create_naive_predicate_propagating_task_lower_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
) -> Predicate {
    predicate!(task.start_variable >= context.lower_bound(&task.start_variable))
}

pub(crate) fn create_naive_predicate_propagating_task_upper_bound_propagation<
    Var: IntegerVariable + 'static,
>(
    context: &PropagationContext,
    task: &Rc<Task<Var>>,
) -> Predicate {
    predicate!(task.start_variable <= context.upper_bound(&task.start_variable))
}
