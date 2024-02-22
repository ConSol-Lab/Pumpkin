use log::warn;

use super::ValueSelector;
use crate::basic_types::KeyedVec;
use crate::basic_types::Literal;
use crate::basic_types::PropositionalVariable;
use crate::basic_types::StorageKey;
use crate::branching::SelectionContext;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// The [solution-guided \[1\]](https://people.eng.unimelb.edu.au/pstuckey/papers/lns-restarts.pdf) [`ValueSelector`], in general this method is provided with a solution and it will then search around this existing solution.
/// It does this by saving the values found in the solution (in
/// [`SolutionGuidedValueSelector::saved_values`]) and assigning to those values
/// whenever possible, if it is not possible then it will fall back on the provided
/// [`SolutionGuidedValueSelector::backup_selector`].
///
/// \[1\] E. Demirović, G. Chu, and P. J. Stuckey, ‘Solution-based phase saving for CP: A
/// value-selection heuristic to simulate local search behavior in complete solvers’, in Principles
/// and Practice of Constraint Programming: 24th International Conference, CP 2018, Lille, France,
/// August 27-31, 2018, Proceedings 24, 2018, pp. 99–108.
#[derive(Debug)]
pub struct SolutionGuidedValueSelector<Var, Value, BackUpSelector> {
    /// The value of the variable in the provided solution.
    saved_values: KeyedVec<Var, Option<Value>>,
    /// The [`ValueSelector`] to use in case that the saved value is already assigned
    backup_selector: BackUpSelector,
}

impl<BackupSelector> SolutionGuidedValueSelector<PropositionalVariable, bool, BackupSelector>
where
    BackupSelector: ValueSelector<PropositionalVariable>,
{
    pub fn new(
        variables: &[PropositionalVariable],
        variables_with_initial_value: Vec<(PropositionalVariable, bool)>,
        backup_selector: BackupSelector,
    ) -> Self {
        pumpkin_assert_simple!(
            variables.len() >= variables_with_initial_value.len(),
            "More values were provided than SolutionGuidedValueSelector variables"
        );
        pumpkin_assert_moderate!(
            variables_with_initial_value
                .iter()
                .all(|(variable, _)| variables.contains(variable)),
            "Not every variable in the provided values was in variables"
        );
        if variables.is_empty() {
            warn!("Empty set of variables provided to solution guided value selector, this could indicate an error");
            return SolutionGuidedValueSelector {
                saved_values: KeyedVec::default(),
                backup_selector,
            };
        }
        let max_index = variables
            .iter()
            .map(|variable| variable.index())
            .max()
            .unwrap();
        let saved_values = KeyedVec::new(vec![None; max_index + 1]);
        let mut solution_guided = SolutionGuidedValueSelector {
            saved_values,
            backup_selector,
        };
        for (var, value) in variables_with_initial_value {
            solution_guided.update(var, value)
        }
        solution_guided
    }
}

impl<Var, Value, BackupSelector> SolutionGuidedValueSelector<Var, Value, BackupSelector>
where
    Var: StorageKey,
    Value: Copy,
    BackupSelector: ValueSelector<Var>,
{
    /// Update the value of the current variable
    fn update(&mut self, var: Var, new_value: Value) {
        self.saved_values[var] = Some(new_value);
    }
}

impl<BackupSelector> ValueSelector<PropositionalVariable>
    for SolutionGuidedValueSelector<PropositionalVariable, bool, BackupSelector>
where
    BackupSelector: ValueSelector<PropositionalVariable>,
{
    fn select_value(
        &mut self,
        context: &SelectionContext,
        decision_variable: PropositionalVariable,
    ) -> Literal {
        match self.saved_values[decision_variable] {
            Some(value) => {
                pumpkin_assert_moderate!(
                    !context.is_propositional_variable_fixed(decision_variable)
                );
                Literal::new(decision_variable, value)
            }
            None => self
                .backup_selector
                .select_value(context, decision_variable),
        }
    }

    fn on_encoding_objective_function(&mut self, all_variables: &[PropositionalVariable]) {
        if all_variables.is_empty() {
            warn!("Empty set of variables provided to solution guided value selector, this could indicate an error");
        }
        while self.saved_values.len()
            <= all_variables
                .iter()
                .map(|variable| variable.index())
                .max()
                .unwrap()
        {
            self.saved_values.push(None);
        }
    }

    fn on_solution(&mut self, context: &SelectionContext) {
        pumpkin_assert_advanced!(
            context
                .get_propositional_variables()
                .all(|propositional_variable| context
                    .is_propositional_variable_fixed(propositional_variable)),
            "The solution passed to SolutionGuidedValueSelector is not completely assigned"
        );

        for propositional_variable in context.get_propositional_variables() {
            self.update(
                propositional_variable,
                context.is_propositional_variable_true(propositional_variable),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SolutionGuidedValueSelector;
    use crate::branching::value_selection::PhaseSaving;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;

    #[test]
    fn saved_value_is_returned_prop() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(0, 1);
        let context =
            SelectionContext::new(&assignments_integer, &assignments_propositional, &mediator);
        let propositional_variables = context.get_propositional_variables().collect::<Vec<_>>();

        let mut solution_guided = SolutionGuidedValueSelector::new(
            &propositional_variables,
            Vec::new(),
            PhaseSaving::new(&propositional_variables),
        );

        solution_guided.update(propositional_variables[0], true);

        let chosen = solution_guided.select_value(&context, propositional_variables[0]);

        assert!(chosen.is_positive())
    }

    #[test]
    fn initial_value_is_returned_prop() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(0, 1);
        let context =
            SelectionContext::new(&assignments_integer, &assignments_propositional, &mediator);
        let propositional_variables = context.get_propositional_variables().collect::<Vec<_>>();

        let mut solution_guided = SolutionGuidedValueSelector::new(
            &propositional_variables,
            vec![(propositional_variables[0], true)],
            PhaseSaving::new(&propositional_variables),
        );

        let chosen = solution_guided.select_value(&context, propositional_variables[0]);

        assert!(chosen.is_positive())
    }

    #[test]
    fn backup_is_used_when_value_is_not_saved() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(0, 1);
        let context =
            SelectionContext::new(&assignments_integer, &assignments_propositional, &mediator);
        let propositional_variables = context.get_propositional_variables().collect::<Vec<_>>();

        let backup = PhaseSaving::with_initial_values(
            &propositional_variables,
            vec![(propositional_variables[0], true)],
            false,
        );

        let mut solution_guided =
            SolutionGuidedValueSelector::new(&propositional_variables, Vec::new(), backup);

        let chosen = solution_guided.select_value(&context, propositional_variables[0]);

        assert!(chosen.is_positive())
    }
}
