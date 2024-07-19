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
use crate::engine::variables::PropositionalVariable;
use crate::engine::variables::PropositionalVariableGeneratorIterator;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;

/// The context provided to the [`Brancher`],
/// it allows the retrieval of domain values of variables and access to methods from a [`Random`]
/// generator.
#[derive(Debug)]
pub struct SelectionContext<'a> {
    assignments_integer: &'a AssignmentsInteger,
    assignments_propositional: &'a AssignmentsPropositional,
    random_generator: &'a mut dyn Random,
}

impl<'a> SelectionContext<'a> {
    pub fn new(
        assignments_integer: &'a AssignmentsInteger,
        assignments_propositional: &'a AssignmentsPropositional,
        rng: &'a mut dyn Random,
    ) -> Self {
        SelectionContext {
            assignments_integer,
            assignments_propositional,
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

    /// Determines whether the provided [`PropositionalVariable`] is assigned.
    pub fn is_propositional_variable_fixed(&self, var: PropositionalVariable) -> bool {
        self.assignments_propositional.is_variable_assigned(var)
    }

    /// Returns whether the provided [`PropositionalVariable`] is assigned to true.
    pub fn is_propositional_variable_true(&self, var: PropositionalVariable) -> bool {
        self.assignments_propositional
            .is_variable_assigned_true(var)
    }

    /// Returns all currently defined [`DomainId`]s.
    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.assignments_integer.get_domains()
    }

    /// Returns all currently defined [`PropositionalVariable`]s.
    pub fn get_propositional_variables(&self) -> PropositionalVariableGeneratorIterator {
        self.assignments_propositional.get_propositional_variables()
    }

    #[cfg(test)]
    /// A method for creating and returning `num_integer_variables` [`DomainId`]s and
    /// `num_prop_variables` [`PropositionalVariable`]s in addition to initialising (and
    /// returning) the corresponding [`AssignmentsInteger`] and [`AssignmentsPropositional`].
    pub fn create_for_testing(
        num_integer_variables: usize,
        num_propositional_variables: usize,
        domains: Option<Vec<(i32, i32)>>,
    ) -> (AssignmentsInteger, AssignmentsPropositional) {
        use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
        use crate::engine::variables::Literal;
        use crate::engine::VariableLiteralMappings;
        use crate::engine::WatchListCP;
        use crate::engine::WatchListPropositional;
        use crate::propagators::clausal::BasicClausalPropagator;
        use crate::pumpkin_assert_simple;

        pumpkin_assert_simple!({
            if let Some(domains) = domains.as_ref() {
                num_integer_variables == domains.len()
            } else {
                true
            }
        });

        let mut mediator = VariableLiteralMappings::default();
        let mut clausal_propagator = BasicClausalPropagator::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut clause_allocator = ClauseAllocator::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut watch_list_cp = WatchListCP::default();

        let root_variable = mediator.create_new_propositional_variable(
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
        );
        let true_literal = Literal::new(root_variable, true);

        assignments_propositional.true_literal = true_literal;

        assignments_propositional.false_literal = !true_literal;

        assignments_propositional.enqueue_decision_literal(true_literal);

        if let Some(domains) = domains.as_ref() {
            for (_, (lower_bound, upper_bound)) in (0..num_integer_variables).zip(domains) {
                let _ = mediator.create_new_domain(
                    *lower_bound,
                    *upper_bound,
                    &mut assignments_integer,
                    &mut watch_list_cp,
                    &mut watch_list_propositional,
                    &mut clausal_propagator,
                    &mut assignments_propositional,
                    &mut clause_allocator,
                );
            }
        } else {
            for _ in 0..num_integer_variables {
                let _ = mediator.create_new_domain(
                    0,
                    10,
                    &mut assignments_integer,
                    &mut watch_list_cp,
                    &mut watch_list_propositional,
                    &mut clausal_propagator,
                    &mut assignments_propositional,
                    &mut clause_allocator,
                );
            }
        }

        for _ in 0..num_propositional_variables {
            // We create an additional variable to ensure that the generator returns the correct
            // variables
            let _ = mediator.create_new_propositional_variable(
                &mut watch_list_propositional,
                &mut clausal_propagator,
                &mut assignments_propositional,
            );
        }

        (assignments_integer, assignments_propositional)
    }
}
