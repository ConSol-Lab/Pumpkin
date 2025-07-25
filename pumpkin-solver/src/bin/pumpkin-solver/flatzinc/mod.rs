mod ast;
mod compiler;
pub(crate) mod error;
mod instance;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::time::Duration;

use pumpkin_core::branching::branchers::alternating::every_x_restarts::EveryXRestarts;
use pumpkin_core::branching::branchers::alternating::until_solution::UntilSolution;
use pumpkin_core::branching::branchers::alternating::AlternatingBrancher;
use pumpkin_solver::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_solver::branching::Brancher;
#[cfg(doc)]
use pumpkin_solver::constraints::cumulative;
use pumpkin_solver::optimisation::linear_sat_unsat::LinearSatUnsat;
use pumpkin_solver::optimisation::linear_unsat_sat::LinearUnsatSat;
use pumpkin_solver::optimisation::OptimisationDirection;
use pumpkin_solver::optimisation::OptimisationStrategy;
use pumpkin_solver::options::CumulativeOptions;
use pumpkin_solver::results::solution_iterator::IteratedSolution;
use pumpkin_solver::results::OptimisationResult;
use pumpkin_solver::results::ProblemSolution;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::results::SolutionReference;
use pumpkin_solver::statistics::StatisticLogger;
use pumpkin_solver::termination::Combinator;
use pumpkin_solver::termination::TerminationCondition;
use pumpkin_solver::termination::TimeBudget;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::Solver;

use self::instance::FlatZincInstance;
use self::instance::Output;
use crate::flatzinc::error::FlatZincError;
use crate::os_signal_termination::OsSignal;
use crate::ProofType;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct FlatZincOptions {
    /// If `true`, the solver will not strictly keep to the search annotations in the flatzinc.
    pub(crate) free_search: bool,

    /// For satisfaction problems, print all solutions. For optimisation problems, this instructs
    /// the solver to print intermediate solutions.
    pub(crate) all_solutions: bool,

    /// Options used for the cumulative constraint (see [`cumulative`]).
    pub(crate) cumulative_options: CumulativeOptions,

    /// Determines which type of search is performed by the solver
    pub(crate) optimisation_strategy: OptimisationStrategy,

    /// The type of proof that is logged. This influences which preprocessing steps we can do.
    pub(crate) proof_type: Option<ProofType>,
}

fn log_statistics(solver: &Solver, brancher: &impl Brancher) {
    brancher.log_statistics(StatisticLogger::default());
    solver.log_statistics();
}

fn solution_callback(
    brancher: &impl Brancher,
    instance_objective_function: Option<DomainId>,
    options_all_solutions: bool,
    outputs: &[Output],
    solver: &Solver,
    solution: SolutionReference,
) {
    if options_all_solutions || instance_objective_function.is_none() {
        brancher.log_statistics(StatisticLogger::default());
        if let Some(objective) = instance_objective_function {
            solver.log_statistics_with_objective(solution.get_integer_value(objective) as i64);
        } else {
            solver.log_statistics()
        }
        print_solution_from_solver(solution, outputs);
    }
}

pub(crate) fn solve(
    mut solver: Solver,
    instance: impl AsRef<Path>,
    time_limit: Option<Duration>,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let mut termination = Combinator::new(
        OsSignal::install(),
        time_limit.map(TimeBudget::starting_now),
    );

    let instance = parse_and_compile(&mut solver, instance, options)?;
    let outputs = instance.outputs.clone();

    let mut brancher = if options.free_search {
        // The free search flag is active
        if instance.objective_function.is_some() {
            // If there is an objective, then we use the provided search until the first solution,
            // and then we switch to default search
            DynamicBrancher::new(vec![Box::new(AlternatingBrancher::new(
                &solver,
                instance.search.expect("Expected a search to be defined"),
                UntilSolution::new(EveryXRestarts::new(1)),
            ))])
        } else {
            // If there is no objective, then we alternate between the provided strategy and the
            // default search every restart
            DynamicBrancher::new(vec![Box::new(AlternatingBrancher::new(
                &solver,
                instance.search.expect("Expected a search to be defined"),
                EveryXRestarts::new(1),
            ))])
        }
    } else {
        instance.search.expect("Expected a search to be defined")
    };

    let (direction, objective): (OptimisationDirection, DomainId) =
        match instance.objective_function {
            Some(objective) => objective.into(),
            None => {
                satisfy(options, &mut solver, brancher, termination, outputs);
                return Ok(());
            }
        };

    let callback =
        |solver: &Solver, solution: SolutionReference<'_>, brancher: &DynamicBrancher| {
            solution_callback(
                brancher,
                Some(objective),
                options.all_solutions,
                &outputs,
                solver,
                solution,
            );
        };

    let result = match options.optimisation_strategy {
        OptimisationStrategy::LinearSatUnsat => solver.optimise(
            &mut brancher,
            &mut termination,
            LinearSatUnsat::new(direction, objective, callback),
        ),
        OptimisationStrategy::LinearUnsatSat => solver.optimise(
            &mut brancher,
            &mut termination,
            LinearUnsatSat::new(direction, objective, callback),
        ),
    };

    match result {
        OptimisationResult::Optimal(optimal_solution) => {
            if !options.all_solutions {
                brancher.log_statistics(StatisticLogger::default());
                solver.log_statistics();
                print_solution_from_solver(optimal_solution.as_reference(), &instance.outputs)
            }
            println!("==========");
            log_statistics(&solver, &brancher);
        }
        OptimisationResult::Satisfiable(_) => {
            // Solutions are printed in the callback.
            log_statistics(&solver, &brancher);
        }
        OptimisationResult::Unsatisfiable => {
            println!("{MSG_UNSATISFIABLE}");
            log_statistics(&solver, &brancher);
        }
        OptimisationResult::Unknown => {
            println!("{MSG_UNKNOWN}");
            log_statistics(&solver, &brancher);
        }
    };

    Ok(())
}

fn satisfy(
    options: FlatZincOptions,
    solver: &mut Solver,
    mut brancher: impl Brancher,
    mut termination: impl TerminationCondition,
    outputs: Vec<Output>,
) {
    if options.all_solutions {
        let mut solution_iterator = solver.get_solution_iterator(&mut brancher, &mut termination);
        let mut has_found_solution = false;
        loop {
            match solution_iterator.next_solution() {
                IteratedSolution::Solution(solution, solver, brancher) => {
                    has_found_solution = true;
                    solution_callback(
                        brancher,
                        None,
                        options.all_solutions,
                        &outputs,
                        solver,
                        solution.as_reference(),
                    );
                }
                IteratedSolution::Finished => {
                    assert!(has_found_solution);
                    println!("==========");
                    log_statistics(solver, &brancher);
                    break;
                }
                IteratedSolution::Unknown => {
                    if !has_found_solution {
                        println!("{MSG_UNKNOWN}");
                    }
                    log_statistics(solver, &brancher);
                    break;
                }
                IteratedSolution::Unsatisfiable => {
                    assert!(!has_found_solution);
                    println!("{MSG_UNSATISFIABLE}");
                    log_statistics(solver, &brancher);
                    break;
                }
            }
        }
    } else {
        match solver.satisfy(&mut brancher, &mut termination) {
            SatisfactionResult::Satisfiable(satisfiable) => solution_callback(
                satisfiable.brancher(),
                None,
                options.all_solutions,
                &outputs,
                satisfiable.solver(),
                satisfiable.solution(),
            ),
            SatisfactionResult::Unsatisfiable(solver, brancher) => {
                println!("{MSG_UNSATISFIABLE}");
                log_statistics(solver, brancher);
            }
            SatisfactionResult::Unknown(solver, brancher) => {
                println!("{MSG_UNKNOWN}");
                log_statistics(solver, brancher);
            }
        }
    }
}

fn parse_and_compile(
    solver: &mut Solver,
    instance: impl Read,
    options: FlatZincOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver, options)
}

/// Prints the current solution.
fn print_solution_from_solver(solution: SolutionReference, outputs: &[Output]) {
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
        let mut solver = Solver::default();

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
        let mut solver = Solver::default();

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
        let mut solver = Solver::default();

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
