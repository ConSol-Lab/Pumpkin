use super::BrancherToUse;
use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
#[cfg(doc)]
use crate::branching::branchers::alternating::AlternatingBrancher;
use crate::branching::branchers::alternating::AlternatingStrategy;
use crate::results::SolutionReference;

/// Specifies that the [`AlternatingBrancher`] should use the provided [`AlternatingStrategy`] until
/// a solution is found *and* a minimum number of conflicts is reached, after which it will use
/// [`BrancherToUse::Default`].
#[derive(Debug, Clone, Copy)]
pub struct UntilXConflictsAndSolution<Strategy> {
    /// The number of conflict encountered.
    num_conflicts_encountered: u32,
    /// The minimum number of conflicts reached before returning [`BrancherToUse::default`].
    conflict_limit: u32,

    /// Whether the strategy has found a solution.
    has_found_solution: bool,

    /// The alternative strategy to use before permanently switching to [`BrancherToUse::default`].
    strategy_before_condition: Strategy,
}

impl<Strategy> UntilXConflictsAndSolution<Strategy> {
    pub fn new(strategy_before_condition: Strategy, conflict_limit: u32) -> Self {
        Self {
            num_conflicts_encountered: 0,
            conflict_limit,
            has_found_solution: false,
            strategy_before_condition,
        }
    }

    /// Checks two conditions:
    /// 1. Whether a solution has been found.
    /// 2. Whether a minimum number of conflicts has been reached.
    fn check_conditions(&self) -> bool {
        self.has_found_solution && self.num_conflicts_encountered >= self.conflict_limit
    }
}

impl<Strategy: AlternatingStrategy> AlternatingStrategy for UntilXConflictsAndSolution<Strategy> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> BrancherToUse {
        if self.check_conditions() {
            BrancherToUse::Default
        } else {
            self.strategy_before_condition.next_decision(context)
        }
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        self.has_found_solution = true;
        if !self.check_conditions() {
            self.strategy_before_condition.on_solution(solution);
        }
    }

    fn on_restart(&mut self) {
        if !self.check_conditions() {
            self.strategy_before_condition.on_restart();
        }
    }

    fn on_conflict(&mut self) {
        if self.check_conditions() {
            self.num_conflicts_encountered += 1;
            self.strategy_before_condition.on_conflict();
        }
    }

    fn is_restart_pointless(
        &mut self,
        default_brancher: &mut impl Brancher,
        other_brancher: &mut impl Brancher,
    ) -> bool {
        if self.check_conditions() {
            // If we have found a solution then we let the default brancher indicate
            default_brancher.is_restart_pointless()
        } else {
            // Otherwise we defer to the other strategy
            self.strategy_before_condition
                .is_restart_pointless(default_brancher, other_brancher)
        }
    }

    fn is_using_default_brancher(&self) -> bool {
        self.check_conditions() || self.strategy_before_condition.is_using_default_brancher()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Conflict, BrancherEvent::Solution]
            .into_iter()
            .chain(self.strategy_before_condition.subscribe_to_events())
            .collect()
    }

    fn will_always_use_default(&self) -> bool {
        self.check_conditions() || self.strategy_before_condition.will_always_use_default()
    }
}
