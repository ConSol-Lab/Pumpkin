mod ast;
mod compiler;
pub(crate) mod error;
mod instance;
mod minizinc_optimiser;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::time::Duration;

use log::warn;

use pumpkin_lib::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::predicate;
use pumpkin_lib::predicates::Predicate;
use pumpkin_lib::results::satisfiable;
use pumpkin_lib::results::satisfiable::IteratedSolution;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::results::SolutionReference;
use pumpkin_lib::termination::TimeBudget;
use pumpkin_lib::variables::Literal;
use pumpkin_lib::Solver;

use self::instance::FlatZincInstance;
use self::instance::FlatzincObjective;
use self::instance::Output;
use self::minizinc_optimiser::MinizincOptimisationResult;
use self::minizinc_optimiser::MinizincOptimiser;
use crate::flatzinc::error::FlatZincError;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

#[derive(Debug, Clone, Copy)]
pub(crate) struct FlatZincOptions {
    /// If `true`, the solver will not strictly keep to the search annotations in the flatzinc.
    pub(crate) free_search: bool,

    /// For satisfaction problems, print all solutions. For optimisation problems, this instructs
    /// the solver to print intermediate solutions.
    pub(crate) all_solutions: bool,
}

pub(crate) fn solve(
    mut solver: Solver,
    instance: impl AsRef<Path>,
    time_limit: Option<Duration>,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let mut termination = time_limit.map(TimeBudget::starting_now);

    let instance = parse_and_compile(&mut solver, instance)?;
    let outputs = instance.outputs.clone();

    let value = if let Some(objective_function) = &instance.objective_function {
        let brancher = if options.free_search {
            // The free search flag is active, we just use the default brancher
            DynamicBrancher::new(vec![Box::new(
                solver.default_brancher_over_all_propositional_variables(),
            )])
        } else {
            instance.search.expect("Expected a search to be defined")
        };

        let mut optimisation_solver = MinizincOptimiser::new(&mut solver, *objective_function);
        match optimisation_solver.solve(&mut termination, brancher, &instance.outputs) {
            MinizincOptimisationResult::Optimal {
                optimal_objective_value,
            } => {
                let objective_bound_literal = solver.get_literal_for_predicate(
                    get_bound_predicate(*objective_function, optimal_objective_value as i32),
                );

                if solver
                    .conclude_proof_optimal(objective_bound_literal)
                    .is_err()
                {
                    warn!("Failed to log solver conclusion");
                };

                println!("==========");
                Some(optimal_objective_value)
            }
            MinizincOptimisationResult::Satisfiable {
                best_found_objective_value,
            } => Some(best_found_objective_value),
            MinizincOptimisationResult::Infeasible => {
                if solver.conclude_proof_unsat().is_err() {
                    warn!("Failed to log solver conclusion");
                };

                println!("{MSG_UNSATISFIABLE}");
                None
            }
            MinizincOptimisationResult::Unknown => {
                println!("{MSG_UNKNOWN}");
                None
            }
        }
    } else {
        let mut brancher = instance.search.expect("Expected a search to be defined");

        match solver.satisfy(&mut brancher, termination) {
            SatisfactionResult::Satisfiable(satisfiable) => {
                if options.all_solutions {
                    let mut solution_iterator = satisfiable.iterate_solutions();
                    loop {
                        match solution_iterator.next_solution() {
                            IteratedSolution::Solution(solution) => {
                                print_solution_from_solver(solution, &outputs);
                                brancher.on_solution(solution);
                            }
                            IteratedSolution::Finished => {
                                println!("==========");
                                break;
                            }
                            IteratedSolution::Unknown => {
                                break;
                            }
                        }
                    }
                }
            }
            SatisfactionResult::Unsatisfiable => {
                if solver.conclude_proof_unsat().is_err() {
                    warn!("Failed to log solver conclusion");
                };

                println!("{MSG_UNSATISFIABLE}");
            }
            SatisfactionResult::Unknown => {
                println!("{MSG_UNKNOWN}");
            }
        }

        None
    };

    if let Some(value) = value {
        solver.log_statistics_with_objective(value)
    } else {
        solver.log_statistics()
    }

    Ok(())
}

fn get_bound_predicate(
    objective_function: FlatzincObjective,
    optimal_objective_value: i32,
) -> Predicate {
    match objective_function {
        FlatzincObjective::Maximize(domain) => predicate![domain <= optimal_objective_value],
        FlatzincObjective::Minimize(domain) => predicate![domain >= optimal_objective_value],
    }
}

fn parse_and_compile(
    solver: &mut Solver,
    instance: impl Read,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver)
}

/// Prints the current solution.
fn print_solution_from_solver(solution: SolutionReference<'_>, outputs: &[Output]) {
    for output_specification in outputs {
        match output_specification {
            Output::Bool(output) => {
                output.print_value(|literal| solution.get_literal_value(*literal))
            }

            Output::Int(output) => {
                output.print_value(|domain_id| solution.get_integer_value(*domain_id))
            }

            Output::ArrayOfBool(output) => {
                output.print_value(|literal| solution.get_literal_value(*literal))
            }

            Output::ArrayOfInt(output) => {
                output.print_value(|domain_id| solution.get_integer_value(*domain_id))
            }
        }
    }

    println!("----------");
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: The following tests rely on observing the interal state of the solver. This is not good
    // design, and these tests should be re-done.
    //
    // #[test]
    // fn single_bool_gets_compiled_to_literal() {
    //     let model = r#"
    //         var bool: SomeVar;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(1, final_variables - starting_variables);
    // }

    // #[test]
    // fn output_annotation_is_interpreted_on_bools() {
    //     let model = r#"
    //         var bool: SomeVar ::output_var;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let instance =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let literal = Literal::new(
    //         PropositionalVariable::new(
    //             solver
    //                 .get_propositional_assignments()
    //                 .num_propositional_variables()
    //                 - 1,
    //         ),
    //         true,
    //     );

    //     let outputs = instance.outputs().collect::<Vec<_>>();
    //     assert_eq!(1, outputs.len());

    //     let output = outputs[0].clone();
    //     assert_eq!(output, Output::bool("SomeVar".into(), literal));
    // }

    // #[test]
    // fn equivalent_bools_refer_to_the_same_literal() {
    //     let model = r#"
    //         var bool: SomeVar;
    //         var bool: OtherVar = SomeVar;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(1, final_variables - starting_variables);
    // }

    // #[test]
    // fn bool_equivalent_to_true_uses_builtin_true_literal() {
    //     let model = r#"
    //         var bool: SomeVar = true;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(0, final_variables - starting_variables);
    // }

    // #[test]
    // fn single_variable_gets_compiled_to_domain_id() {
    //     let instance = "var 1..5: SomeVar;\nsolve satisfy;";
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    // #[test]
    // fn equal_integer_variables_use_one_domain_id() {
    //     let instance = r#"
    //          var 1..10: SomeVar;
    //          var 0..11: OtherVar = SomeVar;
    //          solve satisfy;
    //      "#;
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(10, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    // #[test]
    // fn var_equal_to_constant_reuse_domain_id() {
    //     let instance = r#"
    //          var 1..10: SomeVar = 5;
    //          var 0..11: OtherVar = 5;
    //          solve satisfy;
    //      "#;
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(5, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    #[test]
    fn array_1d_of_boolean_variables() {
        let instance = r#"
            var bool: x1;
            var bool: x2;
            array [1..2] of var bool: xs :: output_array([1..2]) = [x1,x2];
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());

        assert!(matches!(outputs[0], Output::ArrayOfBool(_)));
    }

    #[test]
    fn array_2d_of_boolean_variables() {
        let instance = r#"
            var bool: x1;
            var bool: x2;
            var bool: x3;
            var bool: x4;
            array [1..4] of var bool: xs :: output_array([1..2, 1..2]) = [x1,x2,x3,x4];
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }

    #[test]
    fn array_1d_of_integer_variables() {
        let instance = r#"
            var 1..10: x1;
            var 1..10: x2;
            array [1..2] of var int: xs :: output_array([1..2]) = [x1,x2];
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());

        assert!(matches!(outputs[0], Output::ArrayOfInt(_)));
    }

    #[test]
    fn array_2d_of_integer_variables() {
        let instance = r#"
            var 1..10: x1;
            var 1..10: x2;
            var 1..10: x3;
            var 1..10: x4;
            array [1..4] of var 1..10: xs :: output_array([1..2, 1..2]) = [x1,x2,x3,x4];
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
