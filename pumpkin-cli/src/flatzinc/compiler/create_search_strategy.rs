use std::rc::Rc;

use pumpkin_lib::branching::Brancher;
use pumpkin_lib::branching::DynamicBrancher;
use pumpkin_lib::branching::InDomainMax;
use pumpkin_lib::branching::InDomainMin;
use pumpkin_lib::branching::IndependentVariableValueBrancher;
use pumpkin_lib::branching::InputOrder;
use pumpkin_lib::branching::PhaseSaving;
use pumpkin_lib::branching::Vsids;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::variables::PropositionalVariable;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::Search;
use crate::flatzinc::ast::SearchStrategy;
use crate::flatzinc::ast::ValueSelectionStrategy;
use crate::flatzinc::ast::VariableSelectionStrategy;
use crate::flatzinc::error::FlatZincError;
use crate::flatzinc::instance::FlatzincObjective;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    objective: Option<FlatzincObjective>,
) -> Result<DynamicBrancher, FlatZincError> {
    create_from_search_strategy(&ast.search, context, objective)
}

fn create_from_search_strategy(
    strategy: &Search,
    context: &mut CompilationContext,
    objective: Option<FlatzincObjective>,
) -> Result<DynamicBrancher, FlatZincError> {
    match strategy {
        Search::Bool(SearchStrategy {
            variables,
            variable_selection_strategy,
            value_selection_strategy,
        }) => {
            let search_variables = match variables {
                flatzinc::AnnExpr::String(identifier) => {
                    // TODO: unnecessary to create Rc here, for now it's just for the return type
                    vec![context
                        .resolve_bool_variable_from_identifier(identifier)?
                        .get_propositional_variable()]
                }
                flatzinc::AnnExpr::Expr(expr) => context
                    .resolve_bool_variable_array(expr)?
                    .iter()
                    .map(|literal| literal.get_propositional_variable())
                    .collect::<Vec<_>>(),
                other => panic!("Expected string or expression but got {other:?}"),
            };
            let mut brancher = create_search_over_propositional_variables(
                &search_variables,
                variable_selection_strategy,
                value_selection_strategy,
            );
            if let Some(objective) = objective {
                brancher.add_brancher(create_brancher_over_objective(objective));
            }

            Ok(brancher)
        }
        Search::Int(SearchStrategy {
            variables,
            variable_selection_strategy,
            value_selection_strategy,
        }) => {
            let search_variables = match variables {
                flatzinc::AnnExpr::String(identifier) => {
                    // TODO: unnecessary to create Rc here, for now it's just for the return type
                    Rc::new([context.resolve_integer_variable_from_identifier(identifier)?])
                }
                flatzinc::AnnExpr::Expr(expr) => context.resolve_integer_variable_array(expr)?,
                other => panic!("Expected string or expression but got {other:?}"),
            };
            let mut brancher = create_search_over_domains(
                &search_variables,
                variable_selection_strategy,
                value_selection_strategy,
            );

            if let Some(objective) = objective {
                brancher.add_brancher(create_brancher_over_objective(objective));
            }
            Ok(brancher)
        }
        Search::Seq(search_strategies) => {
            let brancher = DynamicBrancher::new(
                search_strategies
                    .iter()
                    .map(|strategy| {
                        let downcast: Box<dyn Brancher> = Box::new(
                            create_from_search_strategy(strategy, context, objective).expect(
                                "Expected nested sequential strategy to be able to be created",
                            ),
                        );
                        downcast
                    })
                    .chain(objective.map(create_brancher_over_objective))
                    .collect::<Vec<_>>(),
            );
            Ok(brancher)
        }
        Search::Unspecified => {
            let variables = context
                .solver
                .get_propositional_assignments()
                .get_propositional_variables()
                .collect::<Vec<_>>();
            Ok(DynamicBrancher::new(vec![Box::new(
                IndependentVariableValueBrancher::new(
                    Vsids::new(&variables),
                    PhaseSaving::new(&variables),
                ),
            )]))
        }
    }
}

fn create_search_over_propositional_variables(
    search_variables: &[PropositionalVariable],
    variable_selection_strategy: &VariableSelectionStrategy,
    value_selection_strategy: &ValueSelectionStrategy,
) -> DynamicBrancher {
    DynamicBrancher::new(vec![Box::new(IndependentVariableValueBrancher::new(
        variable_selection_strategy.create_from_propositional_variables(search_variables),
        value_selection_strategy.create_for_propositional_variables(),
    ))])
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

fn create_brancher_over_objective(objective: FlatzincObjective) -> Box<dyn Brancher> {
    let domain = *objective.get_domain();
    if objective.is_maximizing() {
        return Box::new(IndependentVariableValueBrancher::new(
            InputOrder::new(&[domain]),
            InDomainMax,
        ));
    }
    Box::new(IndependentVariableValueBrancher::new(
        InputOrder::new(&[domain]),
        InDomainMin,
    ))
}
