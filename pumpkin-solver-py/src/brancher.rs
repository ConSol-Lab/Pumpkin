use pumpkin_solver::core::DefaultBrancher;
use pumpkin_solver::core::branching::Brancher;
use pumpkin_solver::core::branching::BrancherEvent;
use pumpkin_solver::core::branching::SelectionContext;
use pumpkin_solver::core::branching::branchers::warm_start::WarmStart;
use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::predicates::Predicate;
use pumpkin_solver::core::results::SolutionReference;
use pumpkin_solver::core::statistics::StatisticLogger;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;

use crate::variables::IntExpression;

pub struct PythonBrancher {
    warm_start: WarmStart<AffineView<DomainId>>,
    default_brancher: DefaultBrancher,
}

impl PythonBrancher {
    pub(crate) fn new(default_brancher: DefaultBrancher) -> PythonBrancher {
        PythonBrancher {
            warm_start: WarmStart::new(&[], &[]),
            default_brancher,
        }
    }

    pub(crate) fn add_domain(&mut self, domain_id: DomainId) {
        self.default_brancher.add_domain(domain_id);
    }

    pub(crate) fn with_warm_start(&mut self, warm_start: HashMap<IntExpression, i32>) {
        // First create the slice of variables to give to the WarmStart brancher.
        let warm_start_variables: Vec<_> = warm_start.keys().map(|variable| variable.0).collect();

        // For every variable collect the value into another slice.
        let warm_start_values: Vec<_> = warm_start_variables
            .iter()
            .map(|variable| {
                warm_start
                    .get(&IntExpression(*variable))
                    .copied()
                    .expect("all elements are keys")
            })
            .collect();

        self.warm_start = WarmStart::new(&warm_start_variables, &warm_start_values);
    }
}

impl Brancher for PythonBrancher {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        self.warm_start
            .next_decision(context)
            .or_else(|| self.default_brancher.next_decision(context))
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        self.default_brancher.subscribe_to_events()
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.default_brancher.log_statistics(logger);
    }

    fn on_backtrack(&mut self) {
        self.default_brancher.on_backtrack()
    }

    /// Restores dormant predicates after backtracking.
    fn synchronise(&mut self, context: &mut SelectionContext) {
        self.default_brancher.synchronise(context);
    }

    fn on_conflict(&mut self) {
        self.default_brancher.on_conflict();
    }

    fn on_solution(&mut self, solution: SolutionReference<'_>) {
        self.default_brancher.on_solution(solution);
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.default_brancher
            .on_appearance_in_conflict_predicate(predicate);
    }

    fn on_restart(&mut self) {
        self.default_brancher.on_restart();
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.default_brancher.on_unassign_integer(variable, value)
    }

    fn is_restart_pointless(&mut self) -> bool {
        self.default_brancher.is_restart_pointless()
    }
}
