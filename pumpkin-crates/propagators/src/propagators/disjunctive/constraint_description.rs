use pumpkin_core::checkers::Scope;
use pumpkin_core::propagation::ConstraintDescription;
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
}
