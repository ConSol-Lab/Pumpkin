use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

use super::disjunctive_task::ArgDisjunctiveTask;

/// The description of the disjunctive constraint: no two of the tasks overlap.
#[derive(Clone, Debug)]
pub struct DisjunctiveDescription<Var> {
    pub tasks: Vec<ArgDisjunctiveTask<Var>>,
}

impl<Var: IntegerVariable> ConstraintDescription for DisjunctiveDescription<Var> {
    fn scope(&self) -> Scope {
        Scope::from_variables(self.tasks.iter().map(|task| &task.start_time))
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let Some(intervals) = self
            .tasks
            .iter()
            .map(|task| {
                let start_time = i64::from(task.start_time.induced_fixed_value(domains)?);
                Some((start_time, start_time + i64::from(task.processing_time)))
            })
            .collect::<Option<Vec<_>>>()
        else {
            return SolutionCheck::UnfixedVariable;
        };

        let is_overlapping = intervals.iter().enumerate().any(|(index, &(start, end))| {
            intervals[index + 1..]
                .iter()
                .any(|&(other_start, other_end)| start < other_end && other_start < end)
        });

        if is_overlapping {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}
