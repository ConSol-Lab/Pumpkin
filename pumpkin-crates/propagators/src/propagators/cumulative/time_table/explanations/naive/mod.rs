use std::rc::Rc;

use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::cumulative::time_table::explanations::get_minimal_profile;

/// Creates the propagation explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_propagation_explanation<Var: IntegerVariable + 'static>(
    profile: &ResourceProfile<Var>,
    context: Domains,
    capacity: i32,
    propagating_task_usage: i32,
) -> impl Iterator<Item = Predicate> {
    get_minimal_profile(
        profile,
        move |task| {
            [
                predicate!(task.start_variable >= context.lower_bound(&task.start_variable)),
                predicate!(task.start_variable <= context.upper_bound(&task.start_variable)),
            ]
        },
        capacity,
        Some(propagating_task_usage),
    )
}

/// Creates the conflict explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_conflict_explanation<Var, Context: ReadDomains>(
    conflict_profile: &ResourceProfile<Var>,
    context: Context,
    capacity: i32,
) -> impl Iterator<Item = Predicate>
where
    Var: IntegerVariable + 'static,
{
    get_minimal_profile(
        conflict_profile,
        move |task| {
            [
                predicate!(task.start_variable >= context.lower_bound(&task.start_variable)),
                predicate!(task.start_variable <= context.upper_bound(&task.start_variable)),
            ]
        },
        capacity,
        None,
    )
}

pub(crate) fn create_naive_predicate_propagating_task_lower_bound_propagation<Var>(
    context: Domains,
    task: &Rc<Task<Var>>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable >= context.lower_bound(&task.start_variable))
}

pub(crate) fn create_naive_predicate_propagating_task_upper_bound_propagation<Var>(
    context: Domains,
    task: &Rc<Task<Var>>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable <= context.upper_bound(&task.start_variable))
}

#[cfg(test)]
mod tests;
