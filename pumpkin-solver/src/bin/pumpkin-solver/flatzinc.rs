//! Solving a FlatZinc model with the solver, after the library compiled it.

use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::ops::ControlFlow;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use pumpkin_branching::branching::alternating::AlternatingBrancher;
use pumpkin_branching::branching::alternating::every_x_restarts::EveryXRestarts;
use pumpkin_branching::branching::alternating::until_solution::UntilSolution;
use pumpkin_branching::branching::dynamic_brancher::DynamicBrancher;
use pumpkin_core::conflict_resolving::ConflictResolver;
use pumpkin_core::statistics::log_statistic;
use pumpkin_solver::Solver;
use pumpkin_solver::core::branching::Brancher;
use pumpkin_solver::core::optimisation::OptimisationDirection;
use pumpkin_solver::core::optimisation::OptimisationStrategy;
use pumpkin_solver::core::optimisation::linear_sat_unsat::LinearSatUnsat;
use pumpkin_solver::core::optimisation::linear_unsat_sat::LinearUnsatSat;
use pumpkin_solver::core::results::OptimisationResult;
use pumpkin_solver::core::results::ProblemSolution;
use pumpkin_solver::core::results::SatisfactionResult;
use pumpkin_solver::core::results::SolutionReference;
use pumpkin_solver::core::results::solution_iterator::IteratedSolution;
use pumpkin_solver::core::termination::Combinator;
use pumpkin_solver::core::termination::TerminationCondition;
use pumpkin_solver::core::termination::TimeBudget;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::flatzinc::CompilationOptions;
use pumpkin_solver::flatzinc::FlatZincError;
use pumpkin_solver::flatzinc::FullSolution;
use pumpkin_solver::flatzinc::Output;
use pumpkin_solver::flatzinc::parse_and_compile;

use crate::os_signal_termination::OsSignal;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

#[derive(Debug, Clone, Default)]
pub(crate) struct FlatZincOptions {
    /// If `true`, the solver will not strictly keep to the search annotations in the flatzinc.
    pub(crate) free_search: bool,

    /// Determines which type of search is performed by the solver
    pub(crate) optimisation_strategy: OptimisationStrategy,

    /// Indicates that the solver should perform verbose logging
    pub(crate) verbose: bool,

    /// The options of the FlatZinc compiler, among which whether all solutions are printed: for
    /// satisfaction problems, print all solutions; for optimisation problems, print intermediate
    /// solutions.
    pub(crate) compilation: CompilationOptions,

    /// The file the final solution is written to with the value of every FlatZinc variable.
    pub(crate) full_solution_path: Option<PathBuf>,
}

fn log_statistics(
    solver: &Solver,
    brancher: &impl Brancher,
    resolver: &impl ConflictResolver,
    verbose: bool,
    init_time: Duration,
    objective_value: Option<i64>,
) {
    log_statistic("initTime", init_time.as_secs_f64());
    if let Some(objective) = objective_value {
        solver.log_statistics_with_objective(brancher, resolver, objective, verbose);
    } else {
        solver.log_statistics(brancher, resolver, verbose);
    }
}

#[allow(clippy::too_many_arguments, reason = "Should be refactored")]
fn solution_callback(
    brancher: &impl Brancher,
    resolver: &impl ConflictResolver,
    instance_objective_function: Option<AffineView<DomainId>>,
    options_all_solutions: bool,
    outputs: &[Output],
    solver: &Solver,
    solution: SolutionReference,
    verbose: bool,
    init_time: Duration,
) {
    if options_all_solutions || instance_objective_function.is_none() {
        if let Some(objective) = instance_objective_function {
            log_statistic("initTime", init_time.as_secs_f64());
            solver.log_statistics_with_objective(
                brancher,
                resolver,
                solution.get_integer_value(objective) as i64,
                verbose,
            );
        } else {
            solver.log_statistics(brancher, resolver, verbose)
        }
        print_solution_from_solver(solution, outputs);
    }
}

pub(crate) fn solve<R: ConflictResolver>(
    mut solver: Solver,
    instance: impl AsRef<Path>,
    time_limit: Option<Duration>,
    options: FlatZincOptions,
    mut resolver: R,
) -> Result<(), FlatZincError> {
    let init_start_time = Instant::now();
    // The proof stops at the time limit, so that the run stops there as it does without one.
    if let Some(deadline) = time_limit.and_then(|limit| init_start_time.checked_add(limit)) {
        solver.stop_proof_at(deadline);
    }

    let instance = File::open(instance)?;

    let mut termination = Combinator::new(
        OsSignal::install(),
        time_limit.map(TimeBudget::starting_now),
    );

    let instance = parse_and_compile(&mut solver, instance, options.compilation.clone())?;
    let view_statistics = instance.view_statistics;
    let outputs = instance.outputs.clone();
    let full_solution = options
        .full_solution_path
        .as_deref()
        .zip(instance.full_solution.as_ref());

    let init_time = init_start_time.elapsed();

    if options.compilation.preprocess_only {
        // Written whether or not statistics were asked for, since reporting them is the point.
        let statistics = [
            ("initTime", format!("{}", init_time.as_secs_f64())),
            ("viewPassTime", format!("{}", view_statistics.seconds)),
            ("viewAliases", view_statistics.aliases.to_string()),
            ("viewNegations", view_statistics.negations.to_string()),
            ("viewOtherViews", view_statistics.other_views.to_string()),
            ("viewParameters", view_statistics.parameters.to_string()),
            ("viewDefinitions", view_statistics.definitions.to_string()),
            (
                "viewSkippedConstraints",
                view_statistics.skipped.to_string(),
            ),
            ("viewRewriteSteps", view_statistics.rewrites.to_string()),
            ("viewDeclarations", view_statistics.declarations.to_string()),
            (
                "viewLiteralPredicates",
                view_statistics.literal_predicates.to_string(),
            ),
            ("viewConstants", view_statistics.constants.to_string()),
            (
                "viewsOnRereading",
                view_statistics.views_on_rereading.to_string(),
            ),
            (
                "redundantEqualities",
                view_statistics.redundant_equalities.to_string(),
            ),
            ("unitBounds", view_statistics.unit_bounds.to_string()),
            (
                "halfReifiedClauses",
                view_statistics.half_reified_clauses.to_string(),
            ),
            (
                "postedTwoTermEqualitiesInOneClass",
                view_statistics.two_term_equalities_in_one_class.to_string(),
            ),
            (
                "postedTwoTermEqualitiesDeclined",
                view_statistics.two_term_equalities_declined.to_string(),
            ),
            (
                "postedEqualitiesReducedToTwoTerms",
                view_statistics.equalities_reduced_to_two_terms.to_string(),
            ),
            (
                "postedReifiedTwoTermEqualities",
                view_statistics.reified_two_term_equalities.to_string(),
            ),
            (
                "postedAffineRelationsNotRead",
                view_statistics.affine_relations_not_read.to_string(),
            ),
        ];
        for (name, value) in statistics {
            println!("%%%mzn-stat: {name}={value}");
        }
        println!("%%%mzn-stat-end");
        return Ok(());
    }
    log_statistic("viewPassTime", view_statistics.seconds);

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

    let (direction, objective): (OptimisationDirection, AffineView<DomainId>) =
        match instance.objective_function {
            Some(objective) => objective.into(),
            None => {
                satisfy(
                    options.clone(),
                    &mut solver,
                    brancher,
                    termination,
                    outputs,
                    init_time,
                    resolver,
                    full_solution,
                );
                return Ok(());
            }
        };

    let callback = |solver: &Solver,
                    solution: SolutionReference<'_>,
                    brancher: &DynamicBrancher,
                    resolver: &R|
     -> ControlFlow<()> {
        solution_callback(
            brancher,
            resolver,
            Some(objective),
            options.compilation.all_solutions,
            &outputs,
            solver,
            solution,
            options.verbose,
            init_time,
        );

        ControlFlow::Continue(())
    };

    let result = match options.optimisation_strategy {
        OptimisationStrategy::LinearSatUnsat => solver.optimise(
            &mut brancher,
            &mut termination,
            &mut resolver,
            LinearSatUnsat::new(direction, objective, callback),
        ),
        OptimisationStrategy::LinearUnsatSat => solver.optimise(
            &mut brancher,
            &mut termination,
            &mut resolver,
            LinearUnsatSat::new(direction, objective, callback),
        ),
    };

    match result {
        OptimisationResult::Stopped(_, _) => {
            unreachable!("the callback will never return ControlFlow::Break")
        }
        OptimisationResult::Optimal(optimal_solution) => {
            let objective_value = optimal_solution.get_integer_value(objective) as i64;
            write_full_solution(full_solution, optimal_solution.as_reference());
            if !options.compilation.all_solutions {
                log_statistics(
                    &solver,
                    &brancher,
                    &resolver,
                    options.verbose,
                    init_time,
                    Some(objective_value),
                );
                print_solution_from_solver(optimal_solution.as_reference(), &instance.outputs)
            }
            println!("==========");
            log_statistics(
                &solver,
                &brancher,
                &resolver,
                options.verbose,
                init_time,
                Some(objective_value),
            );
        }
        OptimisationResult::Satisfiable(solution) => {
            // Solutions are printed in the callback.
            let objective_value = solution.get_integer_value(objective) as i64;
            write_full_solution(full_solution, solution.as_reference());
            log_statistics(
                &solver,
                &brancher,
                &resolver,
                options.verbose,
                init_time,
                Some(objective_value),
            );
        }
        OptimisationResult::Unsatisfiable => {
            println!("{MSG_UNSATISFIABLE}");
            log_statistics(
                &solver,
                &brancher,
                &resolver,
                options.verbose,
                init_time,
                None,
            );
        }
        OptimisationResult::Unknown => {
            println!("{MSG_UNKNOWN}");
            log_statistics(
                &solver,
                &brancher,
                &resolver,
                options.verbose,
                init_time,
                None,
            );
        }
    };

    Ok(())
}

#[allow(clippy::too_many_arguments, reason = "Should be refactored")]
fn satisfy(
    options: FlatZincOptions,
    solver: &mut Solver,
    mut brancher: impl Brancher,
    mut termination: impl TerminationCondition,
    outputs: Vec<Output>,
    init_time: Duration,
    mut resolver: impl ConflictResolver,
    full_solution: Option<(&Path, &FullSolution)>,
) {
    if options.compilation.all_solutions {
        let mut solution_iterator =
            solver.get_solution_iterator(&mut brancher, &mut termination, &mut resolver);
        let mut has_found_solution = false;
        loop {
            match solution_iterator.next_solution() {
                IteratedSolution::Solution(solution, solver, brancher, resolver) => {
                    has_found_solution = true;
                    write_full_solution(full_solution, solution.as_reference());
                    solution_callback(
                        brancher,
                        resolver,
                        None,
                        options.compilation.all_solutions,
                        &outputs,
                        solver,
                        solution.as_reference(),
                        options.verbose,
                        init_time,
                    );
                }
                IteratedSolution::Finished => {
                    assert!(has_found_solution);
                    println!("==========");
                    log_statistics(
                        solver,
                        &brancher,
                        &resolver,
                        options.verbose,
                        init_time,
                        None,
                    );
                    break;
                }
                IteratedSolution::Unknown => {
                    if !has_found_solution {
                        println!("{MSG_UNKNOWN}");
                    }
                    log_statistics(
                        solver,
                        &brancher,
                        &resolver,
                        options.verbose,
                        init_time,
                        None,
                    );
                    break;
                }
                IteratedSolution::Unsatisfiable => {
                    assert!(!has_found_solution);
                    println!("{MSG_UNSATISFIABLE}");
                    log_statistics(
                        solver,
                        &brancher,
                        &resolver,
                        options.verbose,
                        init_time,
                        None,
                    );
                    break;
                }
            }
        }
    } else {
        match solver.satisfy(&mut brancher, &mut termination, &mut resolver) {
            SatisfactionResult::Satisfiable(satisfiable) => {
                write_full_solution(full_solution, satisfiable.solution());
                solution_callback(
                    satisfiable.brancher(),
                    satisfiable.conflict_resolver(),
                    None,
                    options.compilation.all_solutions,
                    &outputs,
                    satisfiable.solver(),
                    satisfiable.solution(),
                    options.verbose,
                    init_time,
                )
            }
            SatisfactionResult::Unsatisfiable(solver, brancher, resolver) => {
                println!("{MSG_UNSATISFIABLE}");
                log_statistics(solver, brancher, resolver, options.verbose, init_time, None);
            }
            SatisfactionResult::Unknown(solver, brancher, resolver) => {
                println!("{MSG_UNKNOWN}");
                log_statistics(solver, brancher, resolver, options.verbose, init_time, None);
            }
        }
    }
}

/// Write the solution with the value of every FlatZinc variable to the file, when one is given;
/// a later solution replaces an earlier one.
fn write_full_solution(full_solution: Option<(&Path, &FullSolution)>, solution: SolutionReference) {
    let Some((path, full_solution)) = full_solution else {
        return;
    };
    let written = File::create(path).and_then(|file| {
        let mut writer = BufWriter::new(file);
        full_solution.write(solution, &mut writer)?;
        writer.flush()
    });
    if let Err(error) = written {
        eprintln!(
            "warning: the full solution could not be written to {}: {error}",
            path.display()
        );
    }
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
