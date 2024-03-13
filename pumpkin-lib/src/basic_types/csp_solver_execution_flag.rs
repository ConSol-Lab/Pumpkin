#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum CSPSolverExecutionFlag {
    Feasible,
    Infeasible,
    Timeout,
}
