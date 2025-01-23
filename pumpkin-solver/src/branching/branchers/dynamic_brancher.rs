//! A [`Brancher`] which sequentially applies a list of [`Brancher`]s until all of them can not find
//! another decision.
//!
//! Note that this structure should be used if you want to use dynamic [`Brancher`]s but
//! require a [`Sized`] object (e.g. when a function takes as input `impl Brancher`).
use std::cmp::min;
use std::fmt::Debug;

use crate::basic_types::SolutionReference;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::statistics::StatisticLogger;

/// An implementation of a [`Brancher`] which takes a [`Vec`] of `Box<dyn Brancher>` and
/// sequentially applies [`Brancher::next_decision`] until all of them return [`None`].
///
/// For any other method in [`Brancher`] it will simply pass it along to all of the provided
/// `Box<dyn Brancher>`s. This structure should be used if you want to use dynamic [`Brancher`]s but
/// require a [`Sized`] object (e.g. when a function takes as input `impl Brancher`).
///
/// # Note
/// It is important that the methods [`DynamicBrancher::on_conflict`] and
/// [`DynamicBrancher::on_solution`] are called at the appropriate times as these methods ensure
/// that the index to the current brancher to try is reset. If these methods are not called at the
/// appropriate time then it will (likely) lead to incomplete solutions being returned!
pub struct DynamicBrancher {
    branchers: Vec<Box<dyn Brancher>>,
    brancher_index: usize,
}

impl Debug for DynamicBrancher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynamicBrancher").finish()
    }
}

impl DynamicBrancher {
    /// Creates a new [`DynamicBrancher`] with the provided `branchers`. It will attempt to use the
    /// `branchers` in the order in which they were provided.
    pub fn new(branchers: Vec<Box<dyn Brancher>>) -> Self {
        Self {
            branchers,
            brancher_index: 0,
        }
    }

    pub fn add_brancher(&mut self, brancher: Box<dyn Brancher>) {
        self.branchers.push(brancher)
    }
}

impl Brancher for DynamicBrancher {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        loop {
            if self.brancher_index >= self.branchers.len() {
                return None;
            }
            if let Some(decision) = self.branchers[self.brancher_index].next_decision(context) {
                return Some(decision);
            } else {
                // Check whether the next brancher can make a decision
                self.brancher_index += 1;
            }
        }
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.branchers
            .iter()
            .enumerate()
            .for_each(move |(index, brancher)| {
                brancher.log_statistics(statistic_logger.attach_to_prefix(index))
            })
    }

    fn on_conflict(&mut self) {
        // A conflict has occurred, we do not know which brancher now can select a variable, reset
        // to the first one
        self.brancher_index = 0;
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_conflict());
    }

    fn on_backtrack(&mut self) {
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_backtrack());
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_unassign_integer(variable, value));
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_appearance_in_conflict_predicate(predicate));
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        self.brancher_index = 0;
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_solution(solution));
    }

    fn on_restart(&mut self) {
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.on_restart());
    }

    fn synchronise(&mut self, assignments: &Assignments) {
        self.branchers
            .iter_mut()
            .for_each(|brancher| brancher.synchronise(assignments));
    }

    fn is_restart_pointless(&mut self) -> bool {
        // We return whether all of the branchers up and until this one are static; if this is not
        // the case then restarting could be useful!
        let current_brancher_index = min(self.brancher_index, self.branchers.len() - 1);
        self.branchers[..=current_brancher_index]
            .iter_mut()
            .all(|brancher| brancher.is_restart_pointless())
    }
}
