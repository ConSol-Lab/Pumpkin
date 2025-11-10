//! A [`Brancher`] which alternates between the [`DefaultBrancher`] and another [`Brancher`] based
//! on the strategy specified in [`AlternatingStrategy`].

use super::BrancherToUse;
use crate::basic_types::SolutionReference;
use crate::branching::brancher::BrancherEvent;
use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::statistics::StatisticLogger;
use crate::DefaultBrancher;
use crate::Solver;

/// A [`Brancher`] which switches between its provided brancher and [`DefaultBrancher`] based on the
/// provided [`AlternatingStrategy`].
///
/// Note that the [`DefaultBrancher`] is informed of every
/// conflict and unassignment even if it is not currently utilised as [`Brancher`].
///
/// Note that this brancher starts out by using the provided [`Brancher`] and then switches based on
/// the [`AlternatingStrategy`].
#[derive(Debug)]
pub struct AlternatingBrancher<OtherBrancher, Strategy> {
    other_brancher: OtherBrancher,
    /// The instance of [`DefaultBrancher`] which is used when
    /// [`AlternatingBrancher::is_using_default_brancher`] is true.
    default_brancher: DefaultBrancher,
    /// The strategy used to determine when to switch between the two branchers.
    strategy: Strategy,
}

impl<Strategy: AlternatingStrategy, OtherBrancher: Brancher>
    AlternatingBrancher<OtherBrancher, Strategy>
{
    pub fn new(solver: &Solver, other_brancher: OtherBrancher, strategy: Strategy) -> Self {
        Self {
            other_brancher,
            default_brancher: solver.default_brancher(),
            strategy,
        }
    }

    #[cfg(test)]
    pub(crate) fn is_using_default_brancher(&self) -> bool {
        self.strategy.is_using_default_brancher()
    }
}

impl<Strategy: AlternatingStrategy, OtherBrancher: Brancher> Brancher
    for AlternatingBrancher<OtherBrancher, Strategy>
{
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        match self.strategy.next_decision(context) {
            BrancherToUse::Default => self.default_brancher.next_decision(context),
            BrancherToUse::Other => self.other_brancher.next_decision(context),
        }
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.default_brancher
            .log_statistics(statistic_logger.clone());
        self.other_brancher.log_statistics(statistic_logger);
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.default_brancher
            .on_appearance_in_conflict_predicate(predicate);
        if !self.strategy.will_always_use_default() {
            self.other_brancher
                .on_appearance_in_conflict_predicate(predicate)
        }
    }

    fn on_conflict(&mut self) {
        self.default_brancher.on_conflict();
        if !self.strategy.will_always_use_default() {
            self.other_brancher.on_conflict();
        }
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        self.strategy.on_solution(solution);

        self.default_brancher.on_solution(solution);
        if !self.strategy.will_always_use_default() {
            self.other_brancher.on_solution(solution);
        }
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.default_brancher.on_unassign_integer(variable, value);
        if !self.strategy.will_always_use_default() {
            self.other_brancher.on_unassign_integer(variable, value)
        }
    }

    fn on_restart(&mut self) {
        self.strategy.on_restart();
    }

    fn is_restart_pointless(&mut self) -> bool {
        self.strategy
            .is_restart_pointless(&mut self.default_brancher, &mut self.other_brancher)
    }

    fn on_backtrack(&mut self) {
        self.default_brancher.on_backtrack();
        if !self.strategy.will_always_use_default() {
            self.other_brancher.on_backtrack();
        }
    }

    fn synchronise(&mut self, context: &mut SelectionContext) {
        self.default_brancher.synchronise(context);
        if !self.strategy.will_always_use_default() {
            self.other_brancher.synchronise(context);
        }
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        // First, we determine which events to subscribe to based on the strategy used
        let events = self.strategy.subscribe_to_events();

        // Then we also need to take into account the events in which the sub-branchers are intered
        // in
        events
            .into_iter()
            .chain(self.default_brancher.subscribe_to_events())
            .chain(self.other_brancher.subscribe_to_events())
            .collect()
    }
}
