#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum CSPSolverExecutionFlag {
    Feasible,
    Infeasible,
    Timeout,
}

impl CSPSolverExecutionFlag {
    pub fn is_feasible(&self) -> bool {
        matches!(self, CSPSolverExecutionFlag::Feasible)
    }

    pub fn is_infeasible(&self) -> bool {
        matches!(self, CSPSolverExecutionFlag::Infeasible)
    }
}
