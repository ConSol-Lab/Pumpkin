//! Testing utilities for constructing [`SelectionContext`]s without requiring direct access to
//! the internal state of the [`Solver`].
//!
//! These are intended to be used when testing [`Brancher`] implementations (and their components,
//! e.g. [`VariableSelector`](crate::branching)/[`ValueSelector`](crate::branching)) which live
//! outside of this crate.
#[cfg(doc)]
use crate::Solver;
use crate::basic_types::Random;
use crate::basic_types::Solution;
#[cfg(doc)]
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::Assignments;
use crate::engine::notifications::NotificationEngine;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainGeneratorIterator;

/// Owns the state (i.e. the domains of the variables) backing a [`SelectionContext`] which is
/// created for testing purposes; see [`SelectionContext::create_for_testing`].
#[derive(Debug, Default, Clone)]
pub struct SelectionTestContext {
    assignments: Assignments,
    notification_engine: NotificationEngine,
}

impl SelectionTestContext {
    pub(super) fn new(domains: impl IntoIterator<Item = (i32, i32)>) -> Self {
        let mut assignments = Assignments::default();
        let mut notification_engine = NotificationEngine::default();

        for (lower_bound, upper_bound) in domains {
            let _ = assignments.grow(lower_bound, upper_bound);
            notification_engine.grow();
        }

        Self {
            assignments,
            notification_engine,
        }
    }

    /// Creates a [`SelectionContext`] over the current state of this fixture.
    pub fn context<'a>(&'a self, random_generator: &'a mut dyn Random) -> SelectionContext<'a> {
        SelectionContext::new(&self.assignments, random_generator)
    }

    /// Returns an iterator over the [`DomainId`](crate::variables::DomainId)s known to this
    /// fixture.
    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.assignments.get_domains()
    }

    /// Applies `predicate` as if it were a decision.
    ///
    /// Returns whether this predicate was not already implied by the current assignments. Panics
    /// if applying `predicate` would result in an empty domain.
    pub fn post_predicate(&mut self, predicate: Predicate) -> bool {
        self.assignments
            .post_predicate(predicate, None, &mut self.notification_engine)
            .expect("Expected posting the predicate to not result in an empty domain")
    }

    /// Starts a new checkpoint, as would occur before making a decision.
    pub fn new_checkpoint(&mut self) {
        self.assignments.new_checkpoint();
    }

    /// Backtracks the state of this fixture to `new_checkpoint`.
    pub fn synchronise(&mut self, new_checkpoint: usize) {
        let _ = self
            .assignments
            .synchronise(new_checkpoint, &mut self.notification_engine);
    }

    /// Returns a [`Solution`] containing the values which are currently assigned in this fixture.
    pub fn solution(&self) -> Solution {
        Solution::from(self.assignments.clone())
    }
}
