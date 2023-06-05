use super::Solution;

pub struct SolutionValuePair {
    pub solution: Solution,
    pub objective_value: u64,
}

impl SolutionValuePair {
    pub fn new(solution: Solution, objective_value: u64) -> Self {
        Self {
            solution,
            objective_value,
        }
    }
}
