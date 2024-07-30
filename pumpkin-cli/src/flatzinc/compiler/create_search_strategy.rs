use std::rc::Rc;

use pumpkin_lib::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::variables::DomainId;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::Search;
use crate::flatzinc::ast::SearchStrategy;
use crate::flatzinc::ast::ValueSelectionStrategy;
use crate::flatzinc::ast::VariableSelectionStrategy;
use crate::flatzinc::error::FlatZincError;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<DynamicBrancher, FlatZincError> {
    create_from_search_strategy(&ast.search, context, true)
}

fn create_from_search_strategy(
    strategy: &Search,
    context: &mut CompilationContext,
    append_default_search: bool,
) -> Result<DynamicBrancher, FlatZincError> {
    let mut brancher = match strategy {
        Search::Bool(SearchStrategy {
            variables,
            variable_selection_strategy,
            value_selection_strategy,
        }) => {
            let search_variables = match variables {
                flatzinc::AnnExpr::String(identifier) => {
                    vec![context.resolve_bool_variable_from_identifier(identifier)?]
                }
                flatzinc::AnnExpr::Expr(expr) => {
                    context.resolve_bool_variable_array(expr)?.as_ref().to_vec()
                }
                other => panic!("Expected string or expression but got {other:?}"),
            };

            DynamicBrancher::new(vec![Box::new(IndependentVariableValueBrancher::new(
                variable_selection_strategy.create_from_literals(&search_variables),
                value_selection_strategy.create_for_literals(),
            ))])
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
            create_search_over_domains(
                &search_variables,
                variable_selection_strategy,
                value_selection_strategy,
            )
        }
        Search::Seq(search_strategies) => DynamicBrancher::new(
            search_strategies
                .iter()
                .map(|strategy| {
                    let downcast: Box<dyn Brancher> = Box::new(
                        create_from_search_strategy(strategy, context, false)
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
    };
    if append_default_search {
        // MiniZinc specification specifies that we need to ensure that all variables are
        // fixed; we ensure this by adding a brancher after the
        // user-provided search which searches over the remainder of the
        // variables
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
