use std::time::Duration;
use std::time::Instant;

use super::TerminationCondition;

/// Give the solver a specific time budget to complete the search.
#[derive(Clone, Copy, Debug)]
pub struct TimeBudget {
    /// The point in time from which to measure the budget.
    started_at: Instant,
    /// The amount of time before [`TimeBudget::should_stop()`] becomes true.
    budget: Duration,
}

impl TimeBudget {
    /// Give the solver a time budget, starting now.
    pub fn starting_now(budget: Duration) -> TimeBudget {
        let started_at = Instant::now();

        TimeBudget { started_at, budget }
    }
}

impl TerminationCondition for TimeBudget {
    fn should_stop(&mut self) -> bool {
        self.started_at.elapsed() >= self.budget
    }
}
