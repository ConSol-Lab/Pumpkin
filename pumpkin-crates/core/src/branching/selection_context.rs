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
    state: &'a State,
    random_generator: &'a mut dyn Random,
}

impl<'a> SelectionContext<'a> {
    pub fn new(state: &'a State, rng: &'a mut dyn Random) -> Self {
        SelectionContext {
            state,
            random_generator: rng,
        }
    }

    pub fn are_all_variables_assigned(&self) -> bool {
        self.state
            .assignments
            .get_domains()
            .all(|domain_id| self.state.assignments.is_domain_assigned(&domain_id))
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
        self.state.upper_bound(var.clone()) - self.state.lower_bound(var)
    }

    /// Returns the lower bound of the provided [`IntegerVariable`]
    pub fn lower_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.state.lower_bound(var)
    }

    /// Returns the upper bound of the provided [`IntegerVariable`]
    pub fn upper_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.state.upper_bound(var)
    }

    /// Determines whether the provided value is in the domain of the provided [`IntegerVariable`]
    pub fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        self.state.contains(var, value)
    }

    /// Determines whether the provided [`IntegerVariable`] has a unit domain (i.e. a domain of size
    /// 1)
    pub fn is_integer_fixed<Var: IntegerVariable>(&self, var: Var) -> bool {
        self.lower_bound(var.clone()) == self.upper_bound(var)
    }

    pub fn is_predicate_assigned(&self, predicate: Predicate) -> bool {
        self.state.truth_value(predicate).is_some()
    }

    /// Returns all currently defined [`DomainId`]s.
    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.state.assignments.get_domains()
    }

    /// Returns a [`Solution`] containing the values which are currently assigned.
    pub fn solution(&self) -> Solution {
        Solution::from(self.state.assignments.clone())
    }
}
