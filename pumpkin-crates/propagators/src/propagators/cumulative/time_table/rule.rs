use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::CheckerTask;
use pumpkin_checking::checkers::TimeTableChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::MissingRetentionChecker;
use pumpkin_core::variables::IntegerVariable;

use crate::cumulative::ArgTask;
use crate::cumulative::CumulativeParameters;

/// The description of the cumulative constraint: at no time do the tasks that run use more than
/// the capacity of the resource.
#[derive(Clone, Debug)]
pub struct CumulativeDescription<Var> {
    pub tasks: Box<[ArgTask<Var>]>,
    pub capacity: i32,
}

impl<Var: IntegerVariable + 'static> CumulativeDescription<Var> {
    /// The description of the constraint that the propagator with `parameters` propagates.
    ///
    /// It holds the tasks that the propagator considers, which leaves out the tasks that use no
    /// resource or take no time.
    pub(crate) fn from_parameters(parameters: &CumulativeParameters<Var>) -> Self {
        CumulativeDescription {
            tasks: parameters
                .tasks
                .iter()
                .map(|task| ArgTask {
                    start_time: task.start_variable.clone(),
                    processing_time: task.processing_time,
                    resource_usage: task.resource_usage,
                })
                .collect(),
            capacity: parameters.capacity,
        }
    }
}

impl<Var: IntegerVariable> ConstraintDescription for CumulativeDescription<Var> {
    fn scope(&self) -> Scope {
        Scope::from_variables(self.tasks.iter().map(|task| &task.start_time))
    }
}

/// The time-table rule of the cumulative constraint, which all the time-table propagators
/// implement.
#[derive(Clone, Copy, Debug)]
pub struct TimeTableRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for TimeTableRule<Var> {
    type Description = CumulativeDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("time_table")
    }

    fn create_inference_checker(
        description: &CumulativeDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        TimeTableChecker {
            tasks: description
                .tasks
                .iter()
                .map(|task| CheckerTask {
                    start_time: task.start_time.clone(),
                    processing_time: task.processing_time,
                    resource_usage: task.resource_usage,
                })
                .collect(),
            capacity: description.capacity,
        }
    }

    fn create_retention_checker(
        _: &CumulativeDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        MissingRetentionChecker::todo("the time-table rule")
    }
}
