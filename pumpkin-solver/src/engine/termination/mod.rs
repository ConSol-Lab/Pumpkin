//! A [`TerminationCondition`] is a condition which is polled by the solver during the search
//! process. It indicates when the solver should stop, even if no definitive conclusions have been
//! made. The most common example would be [`time_budget::TimeBudget`], which gives the solver a
//! certain time budget to complete its search.

pub(crate) mod combinator;
pub(crate) mod decision_budget;
pub(crate) mod indefinite;
pub(crate) mod os_signal;
pub(crate) mod time_budget;

/// The central trait that defines a termination condition. A termination condition determines when
/// the solver should give up searching for solutions.
pub trait TerminationCondition {
    /// Returns `true` when the solver should stop, `false` otherwise.
    fn should_stop(&mut self) -> bool;
    fn decision_has_been_made(&mut self) {}
}

impl<T: TerminationCondition> TerminationCondition for Option<T> {
    fn should_stop(&mut self) -> bool {
        match self {
            Some(t) => t.should_stop(),
            None => false,
        }
    }

    fn decision_has_been_made(&mut self) {
        if let Some(t) = self {
            t.decision_has_been_made()
        }
    }
}
