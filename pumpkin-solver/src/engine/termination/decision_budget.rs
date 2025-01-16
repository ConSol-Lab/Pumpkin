use super::TerminationCondition;

#[derive(Debug, Copy, Clone)]
pub struct DecisionBudget {
    budget: u64,
    num_decisions: u64,
}

impl DecisionBudget {
    pub fn new(budget: u64) -> Self {
        Self {
            budget,
            num_decisions: 0,
        }
    }
}

impl TerminationCondition for DecisionBudget {
    fn should_stop(&mut self) -> bool {
        self.num_decisions >= self.budget
    }

    fn decision_has_been_made(&mut self) {
        self.num_decisions += 1;
    }
}
