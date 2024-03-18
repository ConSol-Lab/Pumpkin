use log::warn;

use super::ValueSelector;
use crate::basic_types::KeyedVec;
use crate::basic_types::StorageKey;
use crate::branching::SelectionContext;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::pumpkin_assert_moderate;

/// A [`ValueSelector`] which implements [phase saving \[1\]](https://www.researchgate.net/profile/Thammanit-Pipatsrisawat/publication/220944633_A_Lightweight_Component_Caching_Scheme_for_Satisfiability_Solvers/links/0f31753c48ffead666000000/A-Lightweight-Component-Caching-Scheme-for-Satisfiability-Solvers.pdf).
/// During the search process, values of variables are saved whenever they are assigned and the
/// search process will attempt to assign to these values whenever possible. After a variable has
/// been fixed, its value will be saved as the previous value and the search will continue.
/// Values can be frozen meaning that they will not be updated with the previously assigned value
/// during the search process, provided initial values will always be frozen.
///
/// # Bibliography
/// \[1\] K. Pipatsrisawat and A. Darwiche, ‘A lightweight component caching scheme for
/// satisfiability solvers’, in Theory and Applications of Satisfiability Testing--SAT 2007: 10th
/// International Conference, Lisbon, Portugal, May 28-31, 2007. Proceedings 10, 2007, pp. 294–299.
#[derive(Debug)]
pub struct PhaseSaving<Var, Value> {
    /// The saved values used by [`PhaseSaving`]
    saved_values: KeyedVec<Var, StoredValue<Value>>,
}

#[derive(Debug, Clone)]
enum StoredValue<Value> {
    Frozen(Value),
    Regular(Value),
}

impl<Value: Copy> StoredValue<Value> {
    fn get_value(&self) -> Value {
        match self {
            StoredValue::Frozen(value) => *value,
            StoredValue::Regular(value) => *value,
        }
    }
}

impl PhaseSaving<PropositionalVariable, bool> {
    /// Creates a new instance of [`PhaseSaving`] over [`PropositionalVariable`]s with `false` as
    /// its default value.
    pub fn new(variables: &[PropositionalVariable]) -> Self {
        if variables.is_empty() {
            warn!("Empty set of variables provided to phase saving value selector, this could indicate an error")
        }
        PhaseSaving::with_default_value(variables, false)
    }
}

impl<Var: StorageKey + Copy + PartialEq, Value: Copy> PhaseSaving<Var, Value> {
    /// Constructor for creating the [`PhaseSaving`] [`ValueSelector`] with a default value;
    /// the default value will be the selected value if no value is saved for the provided variable
    pub fn with_default_value(variables: &[Var], default_value: Value) -> Self {
        PhaseSaving::with_initial_values(variables, vec![], default_value)
    }

    /// Constructor for creating the [`PhaseSaving`] [`ValueSelector`] with initial values (and a
    /// default value); if no value is saved then the provided initial value is selected and
    /// otherwise the default value is selected.
    ///
    /// It is possible to provide fewer values than number of variables but it is required that
    /// every variable present in `variables_with_initial_value` is also present in `variables`.
    pub fn with_initial_values(
        variables: &[Var],
        variables_with_initial_value: Vec<(Var, Value)>,
        default_value: Value,
    ) -> Self {
        if variables.is_empty() {
            warn!("Empty set of variables provided to phase saving value selector, this could indicate an error");
            return PhaseSaving {
                saved_values: KeyedVec::default(),
            };
        }
        pumpkin_assert_moderate!(
            variables_with_initial_value
                .iter()
                .all(|(variable, _)| variables.contains(variable)),
            "Not every variable in the provided values was in variables"
        );
        let max_index = variables
            .iter()
            .map(|variable| variable.index())
            .max()
            .unwrap();
        let saved_values = KeyedVec::new(vec![StoredValue::Regular(default_value); max_index + 1]);
        if max_index > 0 {
            let mut phase_saving = PhaseSaving { saved_values };
            for (var, value) in variables_with_initial_value {
                phase_saving.freeze(var, value)
            }
            return phase_saving;
        }
        PhaseSaving { saved_values }
    }

    /// Update the value of the variable to the provided value if it is not frozen
    fn update(&mut self, variable: Var, new_value: Value) {
        match self.saved_values[variable] {
            StoredValue::Frozen(_) => {}
            StoredValue::Regular(_) => {
                self.saved_values[variable] = StoredValue::Regular(new_value);
            }
        }
    }

    /// Freeze the value of the provided variable
    pub fn freeze(&mut self, variable: Var, new_value: Value) {
        self.saved_values[variable] = StoredValue::Frozen(new_value)
    }
}

impl ValueSelector<PropositionalVariable> for PhaseSaving<PropositionalVariable, bool> {
    fn select_value(
        &mut self,
        _: &mut SelectionContext,
        decision_variable: PropositionalVariable,
    ) -> Literal {
        Literal::new(
            decision_variable,
            self.saved_values[decision_variable].get_value(),
        )
    }

    fn on_unassign_literal(&mut self, lit: Literal) {
        self.update(lit.get_propositional_variable(), lit.is_positive())
    }

    fn on_encoding_objective_function(&mut self, all_variables: &[PropositionalVariable]) {
        if all_variables.is_empty() {
            warn!("Empty set of variables provided to phase saving value selector, this could indicate an error");
            return;
        }
        while self.saved_values.len()
            <= all_variables
                .iter()
                .map(|variable| variable.index())
                .max()
                .unwrap()
        {
            self.saved_values.push(StoredValue::Regular(false));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PhaseSaving;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;

    #[test]
    fn saved_value_is_returned_prop() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(0, 1, None);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );
        let propositional_variables = context.get_propositional_variables().collect::<Vec<_>>();

        let mut phase_saving = PhaseSaving::new(&propositional_variables);

        phase_saving.update(propositional_variables[0], true);

        let chosen = phase_saving.select_value(&mut context, propositional_variables[0]);

        assert!(chosen.is_positive())
    }
}
