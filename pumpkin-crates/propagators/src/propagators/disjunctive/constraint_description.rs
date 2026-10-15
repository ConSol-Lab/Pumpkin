use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

use super::disjunctive_task::ArgDisjunctiveTask;

/// The description of the disjunctive constraint: no two of the tasks overlap.
#[derive(Clone, Debug, ScopeItem)]
pub struct DisjunctiveDescription<Var> {
    pub tasks: Vec<ArgDisjunctiveTask<Var>>,
}

impl<Var: IntegerVariable> ConstraintDescription for DisjunctiveDescription<Var> {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let intervals = self
            .tasks
            .iter()
            .map(|task| {
                let start_time = i64::from(solution.integer_value(&task.start_time));
                (start_time, start_time + i64::from(task.processing_time))
            })
            .collect::<Vec<_>>();

        let is_overlapping = intervals.iter().enumerate().any(|(index, &(start, end))| {
            intervals[index + 1..]
                .iter()
                .any(|&(other_start, other_end)| start < other_end && other_start < end)
        });

        if !is_overlapping {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
