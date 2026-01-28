use std::rc::Rc;

use flatzinc::AnnExpr;
use pumpkin_solver::core::branching::Brancher;
use pumpkin_solver::core::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_solver::core::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_solver::core::branching::branchers::warm_start::WarmStart;
use pumpkin_solver::core::branching::value_selection::InDomainMax;
use pumpkin_solver::core::branching::value_selection::InDomainMin;
use pumpkin_solver::core::branching::variable_selection::InputOrder;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::Literal;

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
    create_from_search_strategy(&ast.search, context, true, objective)
}

fn get_bool_variables(
    context: &mut CompilationContext,
    variables: &AnnExpr,
) -> Result<Vec<Literal>, FlatZincError> {
    Ok(match variables {
        AnnExpr::String(identifier) => {
            vec![context.resolve_bool_variable_from_identifier(identifier)?]
        }
        AnnExpr::Expr(expr) => context.resolve_bool_variable_array(expr)?.as_ref().to_vec(),
        other => panic!("Expected string or expression but got {other:?}"),
    })
}

fn get_int_variables(
    context: &mut CompilationContext,
    variables: &AnnExpr,
) -> Result<Rc<[DomainId]>, FlatZincError> {
    Ok(match variables {
        AnnExpr::String(identifier) => {
            // TODO: unnecessary to create Rc here, for now it's just for the return type
            Rc::new([context.resolve_integer_variable_from_identifier(identifier)?])
        }
        AnnExpr::Expr(expr) => context.resolve_integer_variable_array(expr)?,
        other => panic!("Expected string or expression but got {other:?}"),
    })
}

fn create_from_search_strategy(
    strategy: &Search,
    context: &mut CompilationContext,
    append_default_search: bool,
    objective: Option<FlatzincObjective>,
) -> Result<DynamicBrancher, FlatZincError> {
    let mut brancher = match strategy {
        Search::Bool(SearchStrategy {
                variables,
                variable_selection_strategy,
                value_selection_strategy,
            }) => {
                let search_variables = get_bool_variables(context, variables)?;

                create_search_over_propositional_variables(
                    &search_variables,
                    variable_selection_strategy,
                    value_selection_strategy,
                )
            }
        Search::Int(SearchStrategy {
                variables,
                variable_selection_strategy,
                value_selection_strategy,
            }) => {
                if let Ok(search_variables) = get_int_variables(context, variables) {
                create_search_over_domains(
                    &search_variables,
                    variable_selection_strategy,
                    value_selection_strategy,
                )
                } else {
                    let search_variables = get_bool_variables(context, variables)?;

                    create_search_over_propositional_variables(
                        &search_variables,
                        variable_selection_strategy,
                        value_selection_strategy,
                    )

                }
            }
        Search::Seq(search_strategies) => DynamicBrancher::new(
                search_strategies
                    .iter()
                    .map(|strategy| {
                        let downcast: Box<dyn Brancher> = Box::new(
                            create_from_search_strategy(strategy, context, false, objective)
                                .expect("Expected nested sequential strategy to be able to be created"),
                        );
                        downcast
                    })
                    .collect::<Vec<_>>(),
            ),
        Search::Unspecified => {
                assert!(
                    append_default_search,
                    "when no search is specified, we must add a default search"
                );

                // The default search will be added below, so we give an empty brancher here.
                DynamicBrancher::new(vec![])
            }
        Search::WarmStartInt { variables, values } => {
                match variables {
                    AnnExpr::String(identifier) => {
                        panic!("Expected either an array of integers or an array of booleans; not an identifier {identifier}")
                    }
                    AnnExpr::Expr(expr) => {
                    let int_variable_array = context.resolve_integer_variable_array(expr)?;
                            match values {
                                AnnExpr::Expr(expr) => {
                                    let int_values_array = context.resolve_array_integer_constants(expr)?;
                                    DynamicBrancher::new(vec![Box::new(WarmStart::new(
                                        &int_variable_array,
                                        &int_values_array,
                                    ))])
                                }
                                x => panic!("Expected an array of integers or an array of booleans; but got {x:?}"),
                            }
                    }
                    other => panic!("Expected expression but got {other:?}"),
                }
            },
        Search::WarmStartBool{ variables, values } => {
                match variables {
                    AnnExpr::String(identifier) => {
                        panic!("Expected either an array of integers or an array of booleans; not an identifier {identifier}")
                    }
                    AnnExpr::Expr(expr) => {
                            let bool_variable_array = context
                                .resolve_bool_variable_array(expr)?
                                .iter()
                                .map(|literal| literal.get_integer_variable())
                                .collect::<Vec<_>>();

                            match values {
                                    AnnExpr::Expr(expr) => {
                                        let bool_values_array = context
                                            .resolve_bool_constants(expr)?
                                            .iter()
                                            .map(|&bool_value| if bool_value { 1 } else { 0 })
                                            .collect::<Vec<_>>();
                                        DynamicBrancher::new(vec![Box::new(WarmStart::new(
                                            &bool_variable_array,
                                            &bool_values_array,
                                        ))])
                                    }
                                x => panic!("Expected an array of integers or an array of booleans; but got {x:?}"),
                                }
                    }
                    other => panic!("Expected expression but got {other:?}"),
                }
            }
        Search::WarmStartArray(search_strategies) => DynamicBrancher::new(
                search_strategies
                    .iter()
                    .map(|strategy| {
                        assert!(
                            matches!(strategy, Search::WarmStartBool { variables: _, values: _ }) ||
                            matches!(
                                strategy,
                                Search::WarmStartInt {
                                    variables: _,
                                    values: _
                                }
                            ) || matches!(strategy, Search::WarmStartArray(_))
                        , "Expected warm start strategy to consist of either `warm_start` or other `warm_start_array` annotations"
                        );
                        let downcast: Box<dyn Brancher> = Box::new(
                            create_from_search_strategy(strategy, context, false, objective)
                                .expect("Expected nested sequential strategy to be able to be created"),
                        );
                        downcast
                    })
                    .collect::<Vec<_>>(),
            ),
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
        brancher.add_brancher(Box::new({
            if matches!(strategy, Search::Unspecified) {
                context.solver.default_brancher()
            } else {
                context.solver.default_brancher().is_default()
            }
        }));
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
