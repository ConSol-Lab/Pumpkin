use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
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

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let Some(start_times) = self
            .tasks
            .iter()
            .map(|task| task.start_time.induced_fixed_value(domains).map(i64::from))
            .collect::<Option<Vec<_>>>()
        else {
            return SolutionCheck::Unknown;
        };

        // The usage only increases when a task starts, so an overload anywhere is an overload at
        // the start of one of the tasks.
        let is_overloaded = start_times.iter().any(|&time| {
            let usage = self
                .tasks
                .iter()
                .zip(&start_times)
                .filter(|&(task, &start_time)| {
                    start_time <= time && time < start_time + i64::from(task.processing_time)
                })
                .map(|(task, _)| i64::from(task.resource_usage))
                .sum::<i64>();

            usage > i64::from(self.capacity)
        });

        if is_overloaded {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}
