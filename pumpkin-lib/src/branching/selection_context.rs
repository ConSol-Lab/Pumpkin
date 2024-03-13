use std::fmt::Debug;

use crate::basic_types::variables::IntVar;
#[cfg(doc)]
use crate::basic_types::DomainId;
use crate::basic_types::IntegerVariableGeneratorIterator;
use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::basic_types::PropositionalVariable;
use crate::basic_types::PropositionalVariableGeneratorIterator;
use crate::basic_types::Random;
#[cfg(doc)]
use crate::branching::Brancher;
#[cfg(doc)]
use crate::engine::propagation::PropagationContext;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::SATCPMediator;
use crate::pumpkin_assert_advanced;

/// The context provided to the [`Brancher`],
/// the behaviour is similar to that of the [`PropagationContext`] with a few additional methods
/// which might be extended in the future.
///
/// Besides retrieving bounds of [`Literal`]s and [`IntVar`]s, it provides methods for retrieving
/// the [`Literal`] which represents the [`IntVar`]s (using
/// [`SelectionContext::get_literal_for_predicate`]).
#[derive(Debug)]
pub struct SelectionContext<'a> {
    sat_cp_mediator: &'a SATCPMediator,
    assignments_integer: &'a AssignmentsInteger,
    assignments_propositional: &'a AssignmentsPropositional,
    random_generator: &'a mut dyn Random,
}

impl<'a> SelectionContext<'a> {
    pub fn new(
        assignments_integer: &'a AssignmentsInteger,
        assignments_propositional: &'a AssignmentsPropositional,
        sat_cp_mediator: &'a SATCPMediator,
        rng: &'a mut dyn Random,
    ) -> Self {
        SelectionContext {
            sat_cp_mediator,
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
    /// [`IntVar`]. Note that this is different from the number of values which are in the domain of
    /// `var` since this calculation does not take into account holes in the domain.
    pub fn get_size_of_domain<Var: IntVar>(&self, var: Var) -> i32 {
        var.upper_bound(self.assignments_integer) - var.lower_bound(self.assignments_integer)
    }

    /// Returns the lower bound of the provided [`IntVar`]
    pub fn lower_bound<Var: IntVar>(&self, var: Var) -> i32 {
        var.lower_bound(self.assignments_integer)
    }

    /// Returns the upper bound of the provided [`IntVar`]
    pub fn upper_bound<Var: IntVar>(&self, var: Var) -> i32 {
        var.upper_bound(self.assignments_integer)
    }

    /// Determines whether the provided value is in the domain of the provided [`IntVar`]
    pub fn contains<Var: IntVar>(&self, var: Var, value: i32) -> bool {
        var.contains(self.assignments_integer, value)
    }

    /// Returns the [`Literal`] representing the provided [`Predicate`]. This method should be used
    /// when making a decision; due to this fact, it should be the case that the predicate
    /// currently does not hold nor is the returned literal assigned.
    pub fn get_literal_for_predicate(&self, pred: Predicate) -> Literal {
        pumpkin_assert_advanced!(!self.assignments_integer.does_predicate_hold(pred), "The provided predicate holds before the decision is made, this indicates a wrongly implemented variable/value selector");
        let literal = self
            .sat_cp_mediator
            .get_predicate_literal(pred, self.assignments_integer);
        pumpkin_assert_advanced!(!self.assignments_propositional.is_literal_assigned(literal), "The returned literal holds before the decision is made, this indicates a wrongly implemented variable/value selector");
        literal
    }

    /// Determines whether the provided [`IntVar`] has a unit domain (i.e. a domain of size 1)
    pub fn is_integer_fixed<Var: IntVar>(&self, var: Var) -> bool {
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

    /// Returns all currently defined [`DomainId`] in the provided [`AssignmentsInteger`].
    pub fn get_domains(&self) -> IntegerVariableGeneratorIterator {
        self.assignments_integer.get_domains()
    }

    /// Returns all currently defined [`PropositionalVariable`]s in the provided
    /// [`AssignmentsPropositional`].
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
    ) -> (AssignmentsInteger, AssignmentsPropositional, SATCPMediator) {
        use crate::engine::CPEngineDataStructures;
        use crate::engine::SATEngineDataStructures;
        use crate::propagators::clausal::BasicClausalPropagator;
        use crate::pumpkin_assert_simple;

        pumpkin_assert_simple!({
            if let Some(domains) = domains.as_ref() {
                num_integer_variables == domains.len()
            } else {
                true
            }
        });

        let mut mediator = SATCPMediator::default();
        let mut clausal_propagator = BasicClausalPropagator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();

        let root_variable = mediator.create_new_propositional_variable(
            &mut cp_data_structures.watch_list_propositional,
            &mut clausal_propagator,
            &mut sat_data_structures,
        );
        let true_literal = Literal::new(root_variable, true);

        sat_data_structures.assignments_propositional.true_literal = true_literal;

        sat_data_structures.assignments_propositional.false_literal = !true_literal;

        mediator.true_literal = true_literal;
        mediator.false_literal = !true_literal;

        sat_data_structures
            .assignments_propositional
            .enqueue_decision_literal(true_literal);

        if let Some(domains) = domains.as_ref() {
            for (_, (lower_bound, upper_bound)) in (0..num_integer_variables).zip(domains) {
                let _ = mediator.create_new_domain(
                    *lower_bound,
                    *upper_bound,
                    &mut clausal_propagator,
                    &mut sat_data_structures,
                    &mut cp_data_structures,
                );
            }
        } else {
            for _ in 0..num_integer_variables {
                let _ = mediator.create_new_domain(
                    0,
                    10,
                    &mut clausal_propagator,
                    &mut sat_data_structures,
                    &mut cp_data_structures,
                );
            }
        }

        for _ in 0..(num_propositional_variables + 1) {
            // We create an additional variable to ensure that the generator returns the correct
            // variables
            let _ = mediator.create_new_propositional_variable(
                &mut cp_data_structures.watch_list_propositional,
                &mut clausal_propagator,
                &mut sat_data_structures,
            );
        }

        (
            cp_data_structures.assignments_integer,
            sat_data_structures.assignments_propositional,
            mediator,
        )
    }
}
