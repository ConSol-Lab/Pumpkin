#[cfg(doc)]
use crate::branching::alternating::AlternatingBrancher;
use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::results::SolutionReference;

/// Defines methods for selecting which of two branching strategies to use; the default or the
/// other brancher.
pub trait AlternatingStrategy {
    /// Called when the next decision is made by the [`AlternatingBrancher`]. Returns true if the
    /// default brancher should be used and false otherwise.
    ///
    /// See [`Brancher::next_decision`].
    fn next_decision(&mut self, context: &mut SelectionContext) -> BrancherToUse;

    /// Called when a solution is found.
    ///
    /// See [`Brancher::on_solution`].
    fn on_solution(&mut self, _solution: SolutionReference) {}

    /// Called when a restart is performed.
    ///
    /// See [`Brancher::on_restart`].
    fn on_restart(&mut self) {}
    /// Called when a restart is considered but not necessarily performed.
    ///
    /// See [`Brancher::is_restart_pointless`].
    fn is_restart_pointless(
        &mut self,
        default_brancher: &mut impl Brancher,
        other_brancher: &mut impl Brancher,
    ) -> bool {
        if self.is_using_default_brancher() {
            default_brancher.is_restart_pointless()
        } else {
            other_brancher.is_restart_pointless()
        }
    }

    /// Returns true if only the default strategy is used from now on and false otherwise.
    ///
    /// This is important if [`AlternatingStrategy::SwitchToDefaultAfterFirstSolution`] is used as
    /// the strategy.
    fn will_always_use_default(&self) -> bool {
        false
    }

    /// Returns true if the default strategy is currently being used and false otherwise.
    fn is_using_default_brancher(&self) -> bool;

    /// Indicates which [`BrancherEvent`] are relevant for this particular [`AlternatingStrategy`].
    fn subscribe_to_events(&self) -> Vec<BrancherEvent>;
}

/// Indicates which [`Brancher`] to use in the [`AlternatingBrancher`].
#[derive(Debug, Clone, Copy, Hash)]
pub enum BrancherToUse {
    Default,
    Other,
}

pub mod every_x_restarts;
pub mod every_x_solutions;
pub mod other_only;
pub mod until_solution;
