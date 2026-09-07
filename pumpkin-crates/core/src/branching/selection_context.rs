use std::borrow::Cow;
use std::fmt::Debug;

use crate::basic_types::Random;
use crate::basic_types::Solution;
#[cfg(doc)]
use crate::branching::Brancher;
use crate::engine::State;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainGeneratorIterator;
#[cfg(doc)]
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
#[cfg(doc)]
use crate::propagation::Domains;

/// The context provided to the [`Brancher`],
/// it allows the retrieval of domain values of variables and access to methods from a [`Random`]
/// generator.
#[derive(Debug)]
pub struct SelectionContext<'a> {
    state: Cow<'a, State>,
    random_generator: &'a mut dyn Random,
}

impl<'a> SelectionContext<'a> {
    pub fn new(state: &'a State, rng: &'a mut dyn Random) -> Self {
        SelectionContext {
            state: Cow::Borrowed(state),
            random_generator: rng,
        }
    }

    fn state(&self) -> &State {
        &self.state
    }

    pub fn are_all_variables_assigned(&self) -> bool {
        self.state()
            .assignments
            .get_domains()
            .all(|domain_id| self.state().assignments.is_domain_assigned(&domain_id))
    }

    /// Returns a random generator which can be used to generate random values (see [`Random`] for
    /// more information).
    pub fn random(&mut self) -> &mut dyn Random {
        self.random_generator
    }

    /// Returns the difference between the upper-bound and the lower-bound of the provided
    /// [`IntegerVariable`]. Note that this is different from the number of values which are in the
    /// domain of `var` since this calculation does not take into account holes in the domain.
    pub fn get_size_of_domain<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.state().upper_bound(var.clone()) - self.state().lower_bound(var)
    }

    /// Returns the lower bound of the provided [`IntegerVariable`]
    pub fn lower_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.state().lower_bound(var)
    }

    /// Returns the upper bound of the provided [`IntegerVariable`]
    pub fn upper_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.state().upper_bound(var)
    }

    /// Determines whether the provided value is in the domain of the provided [`IntegerVariable`]
    pub fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        self.state().contains(var, value)
    }

    /// Determines whether the provided [`IntegerVariable`] has a unit domain (i.e. a domain of size
    /// 1)
    pub fn is_integer_fixed<Var: IntegerVariable>(&self, var: Var) -> bool {
        self.lower_bound(var.clone()) == self.upper_bound(var)
    }

    pub fn is_predicate_assigned(&self, predicate: Predicate) -> bool {
        self.state().truth_value(predicate).is_some()
    }

    /// Returns all currently defined [`DomainId`]s.
    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.state().assignments.get_domains()
    }

    /// Creates a [`SelectionContext`] with the variables having the input bounds; intended for
    /// testing [`Brancher`] implementations (and their components) without requiring direct access
    /// to the internal state of the solver.
    pub fn create_for_testing(
        domains: impl IntoIterator<Item = (i32, i32)>,
        rng: &'a mut dyn Random,
    ) -> Self {
        let mut state = State::default();

        for (lower_bound, upper_bound) in domains {
            let _ = state.new_interval_variable(lower_bound, upper_bound, None);
        }

        SelectionContext {
            state: Cow::Owned(state),
            random_generator: rng,
        }
    }

    /// Applies `predicate` as if it were a decision.
    ///
    /// Returns whether this predicate was not already implied by the current assignments. Panics
    /// if applying `predicate` would result in an empty domain.
    pub fn post_predicate(&mut self, predicate: Predicate) -> bool {
        self.state
            .to_mut()
            .post(predicate)
            .expect("Expected posting the predicate to not result in an empty domain")
    }

    /// Starts a new checkpoint, as would occur before making a decision.
    /// [`SelectionContext::create_for_testing`].
    pub fn new_checkpoint(&mut self) {
        self.state.to_mut().new_checkpoint();
    }

    /// Backtracks to `new_checkpoint`.
    pub fn synchronise(&mut self, new_checkpoint: usize) {
        let _ = self.state.to_mut().restore_to(new_checkpoint);
    }

    /// Returns a [`Solution`] containing the values which are currently assigned.
    pub fn solution(&self) -> Solution {
        Solution::from(self.state().assignments.clone())
    }
}
