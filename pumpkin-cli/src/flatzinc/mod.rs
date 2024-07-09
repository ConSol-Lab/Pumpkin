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

use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::basic_types::ProblemSolution;
use pumpkin_lib::basic_types::SolutionReference;
use pumpkin_lib::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::engine::predicates::integer_predicate::IntegerPredicate;
use pumpkin_lib::engine::termination::time_budget::TimeBudget;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;
use pumpkin_lib::optimisation::log_statistics;
use pumpkin_lib::optimisation::log_statistics_with_objective;
use pumpkin_lib::predicate;

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

    /// Determines whether to allow the cumulative propagator(s) to create holes in the domain
    pub(crate) cumulative_allow_holes: bool,
}

#[cfg(test)]
#[allow(clippy::derivable_impls)]
impl Default for FlatZincOptions {
    fn default() -> Self {
        Self {
            free_search: false,
            all_solutions: false,
            cumulative_allow_holes: false,
        }
    }
}

pub(crate) fn solve(
    mut solver: ConstraintSatisfactionSolver,
    instance: impl AsRef<Path>,
    time_limit: Option<Duration>,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let mut termination = time_limit.map(TimeBudget::starting_now);

    let instance = parse_and_compile(&mut solver, instance, options)?;
    let outputs = instance.outputs.clone();

    let value = if let Some(objective_function) = &instance.objective_function {
        let brancher = if options.free_search {
            // The free search flag is active, we just use the default brancher
            DynamicBrancher::new(vec![Box::new(
                IndependentVariableValueBrancher::default_over_all_variables(&solver),
            )])
        } else {
            instance.search.expect("Expected a search to be defined")
        };

        let mut optimisation_solver = MinizincOptimiser::new(&mut solver, *objective_function);
        match optimisation_solver.solve(&mut termination, brancher, &instance.outputs) {
            MinizincOptimisationResult::Optimal {
                optimal_objective_value,
            } => {
                // todo: add proof logging
                // let objective_bound_literal = solver.get_literal(get_bound_predicate(
                // objective_function,
                // optimal_objective_value as i32,
                // ));
                //
                // if solver
                // .conclude_proof_optimal(objective_bound_literal)
                // .is_err()
                // {
                // warn!("Failed to log solver conclusion");
                // };

                println!("==========");
                Some(optimal_objective_value)
            }
            MinizincOptimisationResult::Satisfiable {
                best_found_objective_value,
            } => Some(best_found_objective_value),
            MinizincOptimisationResult::Infeasible => {
                // todo: readd proof logging
                // if solver.conclude_proof_unsat().is_err() {
                // warn!("Failed to log solver conclusion");
                // };

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

        let mut found_solution = false;

        loop {
            match solver.solve(&mut termination, &mut brancher) {
                CSPSolverExecutionFlag::Feasible => {
                    found_solution = true;

                    #[allow(deprecated)]
                    print_solution_from_solver(solver.get_solution_reference(), &outputs);

                    #[allow(deprecated)]
                    brancher.on_solution(solver.get_solution_reference());

                    let could_find_another_solution =
                        add_blocking_nogood(&mut solver, &outputs, &mut brancher);

                    if !could_find_another_solution {
                        println!("==========");
                        break;
                    }
                }
                CSPSolverExecutionFlag::Infeasible if !found_solution => {
                    // todo: readd proof logging
                    // if solver.conclude_proof_unsat().is_err() {
                    // warn!("Failed to log solver conclusion");
                    // };

                    println!("{MSG_UNSATISFIABLE}");
                    break;
                }
                CSPSolverExecutionFlag::Infeasible => {
                    println!("==========");
                    break;
                }

                // Only when no solutions are found should the UNKNOWN marker be printed.
                CSPSolverExecutionFlag::Timeout if !found_solution => {
                    println!("{MSG_UNKNOWN}");
                    break;
                }
                CSPSolverExecutionFlag::Timeout => break,
            }

            if !options.all_solutions {
                break;
            }
        }

        None
    };

    if let Some(value) = value {
        log_statistics_with_objective(&solver, value)
    } else {
        log_statistics(&solver)
    }

    Ok(())
}

#[allow(dead_code)]
fn get_bound_predicate(
    objective_function: FlatzincObjective,
    optimal_objective_value: i32,
) -> IntegerPredicate {
    match objective_function {
        FlatzincObjective::Maximize(domain) => predicate![domain <= optimal_objective_value],
        FlatzincObjective::Minimize(domain) => predicate![domain >= optimal_objective_value],
    }
}

/// Creates a clause which prevents the current solution from occurring again by going over the
/// defined output variables and creating a clause which prevents those values from being assigned.
///
/// This method is used when attempting to find multiple solutions. It restores the state of the
/// passed [`ConstraintSatisfactionSolver`] to the root (using
/// [`ConstraintSatisfactionSolver::restore_state_at_root`]) and returns true if adding the clause
/// was successful (i.e. it is possible that there could be another solution) and returns false
/// otherwise (i.e. if adding a clause led to a conflict which indicates that there are no more
/// solutions).
fn add_blocking_nogood(
    solver: &mut ConstraintSatisfactionSolver,
    outputs: &[Output],
    brancher: &mut impl Brancher,
) -> bool {
    #[allow(deprecated)]
    let solution = solver.get_solution_reference();

    let nogood = outputs
        .iter()
        .flat_map(|output| match output {
            Output::Bool(_bool) => {
                todo!();
                // let literal = *bool.get_variable();
                //
                // let literal = if solution.get_literal_value(literal) {
                // literal
                // } else {
                // !literal
                // };
                //
                // Box::new(std::iter::once(literal))
            }

            Output::Int(int) => {
                let domain = *int.get_variable();
                let value = solution.get_integer_value(domain);
                Box::new(std::iter::once(predicate![domain == value]))
            }

            #[allow(trivial_casts)]
            Output::ArrayOfBool(_array_of_bool) => {
                todo!();
                // Box::new(array_of_bool.get_contents().map(|&literal| {
                // if solution.get_literal_value(literal) {
                // literal
                // } else {
                // !literal
                // }
                // })) as Box<dyn Iterator<Item = BooleanDomainId>>
            }

            #[allow(trivial_casts)]
            Output::ArrayOfInt(array_of_ints) => {
                Box::new(array_of_ints.get_contents().map(|&domain| {
                    let value = solution.get_integer_value(domain);
                    predicate![domain == value]
                })) as Box<dyn Iterator<Item = IntegerPredicate>>
            }
        })
        .collect::<Vec<_>>();

    solver.restore_state_at_root(brancher);
    if nogood.is_empty() {
        return false;
    }

    solver.add_nogood(nogood).is_ok()
}

fn parse_and_compile(
    solver: &mut ConstraintSatisfactionSolver,
    instance: impl Read,
    options: FlatZincOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver, options)
}

/// Prints the current solution.
fn print_solution_from_solver(solution: SolutionReference<'_>, outputs: &[Output]) {
    for output_specification in outputs {
        match output_specification {
            Output::Bool(output) => output
                .print_value(|boolean| solution.get_integer_value(DomainId::from(*boolean)) == 1),

            Output::Int(output) => {
                output.print_value(|domain_id| solution.get_integer_value(*domain_id))
            }

            Output::ArrayOfBool(output) => output
                .print_value(|boolean| solution.get_integer_value(DomainId::from(*boolean)) == 1),

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
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
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
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
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
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
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
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
