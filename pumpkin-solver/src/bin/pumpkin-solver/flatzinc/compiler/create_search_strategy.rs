use pumpkin_solver::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_solver::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_solver::branching::value_selection::InDomainMax;
use pumpkin_solver::branching::value_selection::InDomainMin;
use pumpkin_solver::branching::variable_selection::InputOrder;
use pumpkin_solver::branching::Brancher;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::Literal;

use super::context::CompilationContext;
use crate::flatzinc::ast::BoolSearchArgs;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::ast::IntSearchArgs;
use crate::flatzinc::ast::SearchAnnotation;
use crate::flatzinc::ast::ValueSelectionStrategy;
use crate::flatzinc::ast::VariableSelectionStrategy;
use crate::flatzinc::error::FlatZincError;
use crate::flatzinc::instance::FlatzincObjective;

pub(crate) fn run(
    typed_ast: &Instance,
    context: &mut CompilationContext,
    objective: Option<FlatzincObjective>,
) -> Result<DynamicBrancher, FlatZincError> {
    let search = typed_ast
        .solve
        .annotations
        .iter()
        .map(|node| &node.node)
        .next();

    create_from_search_strategy(search, context, true, objective)
}

fn create_from_search_strategy(
    strategy: Option<&SearchAnnotation>,
    context: &mut CompilationContext,
    append_default_search: bool,
    objective: Option<FlatzincObjective>,
) -> Result<DynamicBrancher, FlatZincError> {
    let mut brancher = match strategy {
        Some(SearchAnnotation::BoolSearch(BoolSearchArgs {
            variables,
            variable_selection_strategy,
            value_selection_strategy,
            ..
        })) => {
            let search_variables = context.resolve_bool_variable_array_vec(variables)?;

            create_search_over_propositional_variables(
                &search_variables,
                variable_selection_strategy,
                value_selection_strategy,
            )
        }
        Some(SearchAnnotation::IntSearch(IntSearchArgs {
            variables,
            variable_selection_strategy,
            value_selection_strategy,
            ..
        })) => {
            let search_variables = context.resolve_integer_variable_array_vec(variables)?;

            create_search_over_domains(
                &search_variables,
                variable_selection_strategy,
                value_selection_strategy,
            )
        }
        Some(SearchAnnotation::Seq(search_strategies)) => DynamicBrancher::new(
            search_strategies
                .iter()
                .map(|strategy| {
                    let downcast: Box<dyn Brancher> = Box::new(
                        create_from_search_strategy(Some(strategy), context, false, objective)
                            .expect("Expected nested sequential strategy to be able to be created"),
                    );
                    downcast
                })
                .collect::<Vec<_>>(),
        ),

        None => {
            assert!(
                append_default_search,
                "when no search is specified, we must add a default search"
            );

            // The default search will be added below, so we give an empty brancher here.
            DynamicBrancher::new(vec![])
        }
    };

    if append_default_search {
        // MiniZinc specification specifies that we need to ensure that all variables are
        // fixed; we ensure this by adding a brancher after the
        // user-provided search which searches over the remainder of the
        // variables

        // First we ensure that the objective is fixed to its extremum.
        match objective {
            Some(FlatzincObjective::Maximize(domain_id)) => brancher.add_brancher(Box::new(
                IndependentVariableValueBrancher::new(InputOrder::new(&[domain_id]), InDomainMax),
            )),
            Some(FlatzincObjective::Minimize(domain_id)) => brancher.add_brancher(Box::new(
                IndependentVariableValueBrancher::new(InputOrder::new(&[domain_id]), InDomainMin),
            )),
            None => {}
        }
        brancher.add_brancher(Box::new(context.solver.default_brancher()));
    }

    Ok(brancher)
}

fn create_search_over_domains(
    search_variables: &[DomainId],
    variable_selection_strategy: &VariableSelectionStrategy,
    value_selection_strategy: &ValueSelectionStrategy,
) -> DynamicBrancher {
    DynamicBrancher::new(vec![Box::new(IndependentVariableValueBrancher::new(
        variable_selection_strategy.create_from_domains(search_variables),
        value_selection_strategy.create_for_domains(),
    ))])
}

fn create_search_over_propositional_variables(
    search_variables: &[Literal],
    variable_selection_strategy: &VariableSelectionStrategy,
    value_selection_strategy: &ValueSelectionStrategy,
) -> DynamicBrancher {
    DynamicBrancher::new(vec![Box::new(IndependentVariableValueBrancher::new(
        variable_selection_strategy.create_from_literals(search_variables),
        value_selection_strategy.create_for_literals(),
    ))])
}
