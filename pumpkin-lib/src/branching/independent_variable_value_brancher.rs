use std::marker::PhantomData;

use super::value_selection::PhaseSaving;
use super::value_selection::ValueSelector;
use super::VariableSelector;
use super::Vsids;
use crate::basic_types::DomainId;
use crate::basic_types::Literal;
use crate::basic_types::PropositionalVariable;
use crate::basic_types::Solution;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::ConstraintSatisfactionSolver;

/// An implementation of a [`Brancher`] which simply uses a single
/// [`VariableSelector`] and a single [`ValueSelector`] independently of one another.
#[derive(Debug)]
pub struct IndependentVariableValueBrancher<Var, VariableSelect, ValueSelect>
where
    VariableSelect: VariableSelector<Var>,
    ValueSelect: ValueSelector<Var>,
{
    /// The [`VariableSelector`] of the [`Brancher`], determines which (unfixed) variable to branch
    /// next on.
    variable_selector: VariableSelect,
    /// The [`ValueSelector`] of the [`Brancher`] determines which value in the domain to branch
    /// next on given a variable.
    value_selector: ValueSelect,
    /// [`PhantomData`] to ensure that the variable type is bound to the
    /// [`IndependentVariableValueBrancher`]
    variable_type: PhantomData<Var>,
}

impl<Var, VariableSelect, ValueSelect>
    IndependentVariableValueBrancher<Var, VariableSelect, ValueSelect>
where
    VariableSelect: VariableSelector<Var>,
    ValueSelect: ValueSelector<Var>,
{
    pub fn new(var_selector: VariableSelect, val_selector: ValueSelect) -> Self {
        IndependentVariableValueBrancher {
            variable_selector: var_selector,
            value_selector: val_selector,
            variable_type: PhantomData,
        }
    }
}

impl
    IndependentVariableValueBrancher<
        PropositionalVariable,
        Vsids<PropositionalVariable>,
        PhaseSaving<PropositionalVariable, bool>,
    >
{
    /// Creates a default [`IndependentVariableValueBrancher`] which uses [`Vsids`] as
    /// [`VariableSelector`] and [`PhaseSaving`] as its [`ValueSelector`]; it searches over all
    /// [`PropositionalVariable`]s defined in the provided `solver`.
    pub fn default_over_all_propositional_variables(
        solver: &ConstraintSatisfactionSolver,
    ) -> IndependentVariableValueBrancher<
        PropositionalVariable,
        Vsids<PropositionalVariable>,
        PhaseSaving<PropositionalVariable, bool>,
    > {
        let variables = solver
            .get_propositional_assignments()
            .get_propositional_variables()
            .collect::<Vec<_>>();
        IndependentVariableValueBrancher {
            variable_selector: Vsids::new(&variables),
            value_selector: PhaseSaving::new(&variables),
            variable_type: PhantomData,
        }
    }
}

impl<Var, VariableSelect, ValueSelect> Brancher
    for IndependentVariableValueBrancher<Var, VariableSelect, ValueSelect>
where
    VariableSelect: VariableSelector<Var>,
    ValueSelect: ValueSelector<Var>,
{
    /// First we select a variable
    ///  - If all variables under consideration are fixed (i.e. `select_variable` return None) then
    ///    we simply return None
    ///  - Otherwise we select a value and return the corresponding literal
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Literal> {
        self.variable_selector
            .select_variable(context)
            .map(|selected_variable| {
                // We have selected a variable, select a value for the PropositionalVariable
                self.value_selector.select_value(context, selected_variable)
            })
    }

    fn on_conflict(&mut self) {
        self.variable_selector.on_conflict()
    }

    fn on_unassign_literal(&mut self, lit: Literal) {
        self.variable_selector.on_unassign_literal(lit);
        self.value_selector.on_unassign_literal(lit);
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.variable_selector.on_unassign_integer(variable, value);
        self.value_selector.on_unassign_integer(variable, value)
    }

    fn on_appearance_in_conflict_literal(&mut self, lit: Literal) {
        self.variable_selector
            .on_appearance_in_conflict_literal(lit)
    }

    fn on_appearance_in_conflict_integer(&mut self, variable: DomainId) {
        self.variable_selector
            .on_appearance_in_conflict_integer(variable)
    }

    fn on_encoding_objective_function(&mut self, all_variables: &[PropositionalVariable]) {
        self.variable_selector
            .on_encoding_objective_function(all_variables);
        self.value_selector
            .on_encoding_objective_function(all_variables);
    }

    fn on_solution(&mut self, solution: &Solution) {
        self.value_selector.on_solution(solution);
    }
}
