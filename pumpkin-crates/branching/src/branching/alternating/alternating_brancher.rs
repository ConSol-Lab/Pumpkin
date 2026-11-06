//! A [`Brancher`] which alternates between the [`DefaultBrancher`] and another [`Brancher`] based
//! on the strategy specified in [`AlternatingStrategy`].

use itertools::Itertools;
use pumpkin_core::Solver;
use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::results::SolutionReference;
#[cfg(doc)]
use pumpkin_core::state::State;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::variables::DomainId;

use super::BrancherToUse;
use crate::DefaultBrancher;
use crate::branching::alternating::strategies::AlternatingStrategy;

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
        Self::new_from_domains(solver.get_domains(), other_brancher, strategy)
    }

    /// Creates a new instance considering all of the provided `domains`.
    pub(crate) fn new_from_domains(
        domains: impl IntoIterator<Item = DomainId>,
        other_brancher: OtherBrancher,
        strategy: Strategy,
    ) -> Self {
        let default_brancher = DefaultBrancher::new_from_domains(domains);

        let strategy_events = strategy.subscribe_to_events();
        pumpkin_assert_advanced!(
            strategy_events.iter().all_unique(),
            "The events subscribed to by the strategy should be unique, but found {strategy_events:?} for {strategy:?}"
        );
        let default_events = default_brancher.subscribe_to_events();
        pumpkin_assert_advanced!(
            default_events.iter().all_unique(),
            "The events subscribed to by the default brancher should be unique, but found {default_events:?}"
        );
        let other_events = other_brancher.subscribe_to_events();
        pumpkin_assert_advanced!(
            other_events.iter().all_unique(),
            "The events subscribed to by the other brancher should be unique, but found {other_events:?} for {other_brancher:?}"
        );

        Self {
            other_brancher,
            default_brancher,
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

        self.default_brancher.on_restart();
        if !self.strategy.will_always_use_default() {
            self.other_brancher.on_restart();
        }
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
            // The strategy and sub-branchers could subscribe to the same events
            .unique()
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::rc::Rc;

    use pumpkin_core::branching::Brancher;
    use pumpkin_core::branching::BrancherEvent;
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::containers::HashSet;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::results::Solution;
    use pumpkin_core::state::State;

    use crate::DefaultBrancher;
    use crate::branching::alternating::AlternatingBrancher;
    use crate::branching::alternating::every_x_restarts::EveryXRestarts;
    use crate::branching::alternating::until_solution::UntilSolution;

    #[test]
    fn subscribed_events_are_unique() {
        let state = State::default();
        let brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
            UntilSolution::new(EveryXRestarts::new(1)),
        );

        let events = brancher.subscribe_to_events();
        let unique_events = events.iter().copied().collect::<HashSet<_>>();
        assert_eq!(events.len(), unique_events.len());
    }

    /// A [`Brancher`] which counts how often it is notified of a restart.
    #[derive(Debug, Default)]
    struct RestartCountingBrancher {
        num_restarts: Rc<Cell<u32>>,
    }

    impl Brancher for RestartCountingBrancher {
        fn next_decision(&mut self, _context: &mut SelectionContext) -> Option<Predicate> {
            None
        }

        fn on_restart(&mut self) {
            self.num_restarts.set(self.num_restarts.get() + 1);
        }

        fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
            vec![BrancherEvent::Restart]
        }
    }

    #[test]
    fn restart_is_forwarded_to_other_brancher() {
        let state = State::default();
        let other_brancher = RestartCountingBrancher::default();
        let num_restarts = Rc::clone(&other_brancher.num_restarts);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            other_brancher,
            EveryXRestarts::new(1),
        );

        brancher.on_restart();
        assert_eq!(num_restarts.get(), 1);

        brancher.on_restart();
        assert_eq!(num_restarts.get(), 2);
    }

    #[test]
    fn restart_is_not_forwarded_to_other_brancher_if_always_using_default() {
        let state = State::default();
        let other_brancher = RestartCountingBrancher::default();
        let num_restarts = Rc::clone(&other_brancher.num_restarts);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            other_brancher,
            UntilSolution::new(EveryXRestarts::new(1)),
        );

        brancher.on_restart();
        assert_eq!(num_restarts.get(), 1);

        // After the first solution, only the default brancher is used
        brancher.on_solution(Solution::default().as_reference());

        brancher.on_restart();
        assert_eq!(num_restarts.get(), 1);
    }
}
