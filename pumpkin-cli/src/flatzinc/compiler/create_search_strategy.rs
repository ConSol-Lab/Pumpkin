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
    create_from_search_strategy(&ast.search, context)
}

fn create_from_search_strategy(
    strategy: &Search,
    context: &mut CompilationContext,
) -> Result<DynamicBrancher, FlatZincError> {
    match strategy {
        Search::Bool(SearchStrategy {
            variables: _,
            variable_selection_strategy: _,
            value_selection_strategy: _,
        }) => {
            todo!();
            // let search_variables = match variables {
            //    flatzinc::AnnExpr::String(identifier) => {
            //        // TODO: unnecessary to create Rc here, for now it's just for the return type
            //        vec![context
            //            .resolve_bool_variable_from_identifier(identifier)?
            //            .get_propositional_variable()]
            //    }
            //    flatzinc::AnnExpr::Expr(expr) => context
            //        .resolve_bool_variable_array(expr)?
            //        .iter()
            //        .map(|literal| literal.get_propositional_variable())
            //       .collect::<Vec<_>>(),
            //   other => panic!("Expected string or expression but got {other:?}"),
            //};
            // let mut brancher = create_search_over_propositional_variables(
            //    &search_variables,
            //    variable_selection_strategy,
            //    value_selection_strategy,
            //);
            // MiniZinc specification specifies that we need to ensure that all variables are fixed;
            // we ensure this by adding a brancher after the user-provided search which searches
            // over the remainder of the variables
            // brancher.add_brancher(Box::new(
            //    IndependentVariableValueBrancher::default_over_all_variables(context.solver),
            // ));

            // Ok(brancher)
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

            // MiniZinc specification specifies that we need to ensure that all variables are fixed;
            // we ensure this by adding a brancher after the user-provided search which searches
            // over the remainder of the variables
            brancher.add_brancher(Box::new(context.solver.default_brancher()));
            Ok(brancher)
        }
        Search::Seq(search_strategies) => {
            // MiniZinc specification specifies that we need to ensure that all variables are fixed;
            // we ensure this by adding a brancher after the user-provided search which searches
            // over the remainder of the variables
            let brancher_over_all_variables: Box<dyn Brancher> =
                Box::new(context.solver.default_brancher());
            let brancher = DynamicBrancher::new(
                search_strategies
                    .iter()
                    .map(|strategy| {
                        let downcast: Box<dyn Brancher> =
                            Box::new(create_from_search_strategy(strategy, context).expect(
                                "Expected nested sequential strategy to be able to be created",
                            ));
                        downcast
                    })
                    .chain(std::iter::once(brancher_over_all_variables))
                    .collect::<Vec<_>>(),
            );
            Ok(brancher)
        }
        Search::Unspecified => Ok(DynamicBrancher::new(vec![Box::new(
            context.solver.default_brancher(),
        )])),
    }
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
