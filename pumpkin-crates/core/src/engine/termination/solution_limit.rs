use super::TerminationCondition;

/// A [`TerminationCondition`] which triggers once the solver has found the specified number of
/// solutions.
#[derive(Clone, Copy, Debug)]
pub struct SolutionLimit {
    /// The number of solutions after which [`SolutionLimit::should_stop()`] becomes true.
    limit: u64,
    /// The number of solutions found so far.
    num_solutions_found: u64,
}

impl SolutionLimit {
    /// Stop the solver once it has found `limit` solutions.
    pub fn new(limit: u64) -> SolutionLimit {
        SolutionLimit {
            limit,
            num_solutions_found: 0,
        }
    }
}

impl TerminationCondition for SolutionLimit {
    fn should_stop(&mut self) -> bool {
        self.num_solutions_found >= self.limit
    }

    fn on_solution_found(&mut self) {
        self.num_solutions_found += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stops_once_limit_is_reached() {
        let mut termination = SolutionLimit::new(2);

        assert!(!termination.should_stop());
        termination.on_solution_found();
        assert!(!termination.should_stop());
        termination.on_solution_found();
        assert!(termination.should_stop());
    }
}
