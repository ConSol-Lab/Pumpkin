use super::{Function, Solution};

pub struct SolutionTracker {
    objective_function: Function,
}

impl SolutionTracker {
    pub fn new(objective_function: &Function) -> SolutionTracker {
        SolutionTracker {
            objective_function: objective_function.clone(),
        }
    }

    pub fn get_best_objective_value(&self) -> u64 {
        todo!();
    }

    pub fn get_best_solution(&self) -> u64 {
        todo!();
    }

    pub fn has_optimal_solution(&self) -> bool {
        todo!()
    }

    pub fn update_lower_bound(&mut self, _new_lower_bound: u64) {
        todo!()
    }

    pub fn update_solution(&mut self, _new_solution: &Solution) {
        todo!();
    }

    pub fn declare_optimal(&mut self) {
        todo!();
    }
}
