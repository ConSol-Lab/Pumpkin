//! A [`Brancher`] which checks that another [`Brancher`] never makes a decision over a forbidden
//! variable.
use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::containers::HashSet;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::results::SolutionReference;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::variables::DomainId;

/// A [`Brancher`] which forwards every call to `other_brancher` and panics if a decision is over
/// one of the `forbidden_variables`, or if one of them is not fixed when no decision remains.
///
/// The forbidden variables are expected to be fixed by propagation once the other variables are
/// fixed. The checks are not restricted to debug builds, since a violation leads to an incorrect
/// solution or to learned nogoods over forbidden variables.
#[derive(Debug)]
pub struct ForbiddenVariablesBrancher<OtherBrancher> {
    other_brancher: OtherBrancher,
    forbidden_variables: HashSet<DomainId>,
}

impl<OtherBrancher: Brancher> ForbiddenVariablesBrancher<OtherBrancher> {
    pub fn new(other_brancher: OtherBrancher, forbidden_variables: HashSet<DomainId>) -> Self {
        Self {
            other_brancher,
            forbidden_variables,
        }
    }
}

impl<OtherBrancher: Brancher> Brancher for ForbiddenVariablesBrancher<OtherBrancher> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        let decision = self.other_brancher.next_decision(context);

        match decision {
            Some(predicate) => assert!(
                !self.forbidden_variables.contains(&predicate.get_domain()),
                "the brancher made the decision {predicate} over a forbidden variable"
            ),
            None => {
                if let Some(unfixed_variable) = self
                    .forbidden_variables
                    .iter()
                    .find(|&&domain_id| !context.is_integer_fixed(domain_id))
                {
                    panic!(
                        "the forbidden variable {unfixed_variable} is not fixed by propagation when no decision remains"
                    );
                }
            }
        }

        decision
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.other_brancher.log_statistics(statistic_logger)
    }

    fn on_conflict(&mut self) {
        self.other_brancher.on_conflict()
    }

    fn on_backtrack(&mut self) {
        self.other_brancher.on_backtrack()
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        self.other_brancher.on_solution(solution)
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.other_brancher.on_unassign_integer(variable, value)
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.other_brancher
            .on_appearance_in_conflict_predicate(predicate)
    }

    fn on_restart(&mut self) {
        self.other_brancher.on_restart()
    }

    fn synchronise(&mut self, context: &mut SelectionContext) {
        self.other_brancher.synchronise(context)
    }

    fn is_restart_pointless(&mut self) -> bool {
        self.other_brancher.is_restart_pointless()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        self.other_brancher.subscribe_to_events()
    }
}
