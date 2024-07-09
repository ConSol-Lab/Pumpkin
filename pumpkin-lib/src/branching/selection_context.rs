use std::fmt::Debug;

use crate::basic_types::Random;
#[cfg(doc)]
use crate::branching::Brancher;
#[cfg(doc)]
use crate::engine::propagation::PropagationContext;
use crate::engine::variables::DomainGeneratorIterator;
#[cfg(doc)]
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::AssignmentsInteger;

/// The context provided to the [`Brancher`],
/// the behaviour is similar to that of the [`PropagationContext`] with a few additional methods
/// which might be extended in the future.
#[derive(Debug)]
pub struct SelectionContext<'a> {
    assignments_integer: &'a AssignmentsInteger,
    random_generator: &'a mut dyn Random,
}

impl<'a> SelectionContext<'a> {
    pub fn new(assignments_integer: &'a AssignmentsInteger, rng: &'a mut dyn Random) -> Self {
        SelectionContext {
            assignments_integer,
            random_generator: rng,
        }
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
        var.upper_bound(self.assignments_integer) - var.lower_bound(self.assignments_integer)
    }

    /// Returns the lower bound of the provided [`IntegerVariable`]
    pub fn lower_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        var.lower_bound(self.assignments_integer)
    }

    /// Returns the upper bound of the provided [`IntegerVariable`]
    pub fn upper_bound<Var: IntegerVariable>(&self, var: Var) -> i32 {
        var.upper_bound(self.assignments_integer)
    }

    /// Determines whether the provided value is in the domain of the provided [`IntegerVariable`]
    pub fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        var.contains(self.assignments_integer, value)
    }

    /// Determines whether the provided [`IntegerVariable`] has a unit domain (i.e. a domain of size
    /// 1)
    pub fn is_integer_fixed<Var: IntegerVariable>(&self, var: Var) -> bool {
        self.lower_bound(var.clone()) == self.upper_bound(var)
    }

    /// Returns all currently defined [`DomainId`] in the provided [`AssignmentsInteger`].
    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.assignments_integer.get_domains()
    }

    #[cfg(test)]
    /// Create an ['AssignmentsInteger'] with the variables having the input bounds.
    pub fn create_for_testing(domains: Vec<(i32, i32)>) -> AssignmentsInteger {
        let mut assignments_integer = AssignmentsInteger::default();

        for (lower_bound, upper_bound) in domains {
            _ = assignments_integer.grow(lower_bound, upper_bound);
        }

        assignments_integer
    }
}
