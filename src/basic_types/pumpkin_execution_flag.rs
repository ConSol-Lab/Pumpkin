use super::Solution;

pub enum PumpkinExecutionFlag {
    Optimal {
        optimal_solution: Solution,
        objective_value: u64,
    },
    Feasible {
        feasible_solution: Solution,
        objective_value: u64,
    },
    Infeasible,
    Timeout,
}
