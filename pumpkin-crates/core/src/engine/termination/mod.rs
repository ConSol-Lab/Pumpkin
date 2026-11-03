//! A [`TerminationCondition`] is a condition which is polled by the solver during the search
//! process. It indicates when the solver should stop, even if no definitive conclusions have been
//! made. The most common example would be [`time_budget::TimeBudget`], which gives the solver a
//! certain time budget to complete its search.

use std::ops::DerefMut;

pub(crate) mod combinator;
pub(crate) mod indefinite;
pub(crate) mod solution_limit;
pub(crate) mod time_budget;

/// The central trait that defines a termination condition. A termination condition determines when
/// the solver should give up searching for solutions.
///
/// # Notes
/// - Any `Box<dyn TerminationCondition>` is a valid implementation of `TerminationCondition`.
pub trait TerminationCondition {
    /// Returns `true` when the solver should stop, `false` otherwise.
    fn should_stop(&mut self) -> bool;

    /// Called by the solver whenever it finds a solution.
    fn on_solution_found(&mut self) {}
}

impl<T: TerminationCondition> TerminationCondition for Option<T> {
    fn should_stop(&mut self) -> bool {
        match self {
            Some(t) => t.should_stop(),
            None => false,
        }
    }

    fn on_solution_found(&mut self) {
        if let Some(t) = self {
            t.on_solution_found();
        }
    }
}

impl TerminationCondition for Box<dyn TerminationCondition> {
    fn should_stop(&mut self) -> bool {
        self.deref_mut().should_stop()
    }

    fn on_solution_found(&mut self) {
        self.deref_mut().on_solution_found();
    }
}
