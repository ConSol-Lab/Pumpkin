use std::fs::File;
use std::path::Path;
use std::time::Duration;

use pumpkin_solver::pumpkin_assert_simple;
pub(crate) mod encoders;
pub(crate) mod optimisation;
pub(crate) use encoders::PseudoBooleanEncoding;
use optimisation::linear_search::LinearSearch;
use optimisation::optimisation_result::MaxSatOptimisationResult;
use optimisation::optimisation_solver::OptimisationSolver;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::termination::TimeBudget;

use crate::parsers::dimacs::parse_wcnf;
use crate::parsers::dimacs::SolverArgs;
use crate::parsers::dimacs::SolverDimacsSink;
use crate::parsers::dimacs::WcnfInstance;
use crate::result::PumpkinError;
use crate::stringify_solution;

pub(crate) fn wcnf_problem(
    solver_options: SolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    encoding: PseudoBooleanEncoding,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let WcnfInstance {
        formula: solver,
        objective: objective_function,
        last_instance_variable,
    } = parse_wcnf::<SolverDimacsSink>(instance_file, SolverArgs::new(solver_options))?;

    pumpkin_assert_simple!(
        objective_function.get_terms().count() == 0,
        "Should not be any domain ids in the objective function for a MaxSAT problem"
    );

    let brancher = solver.default_brancher();
    let mut termination = time_limit.map(TimeBudget::starting_now);

    let mut solver =
        OptimisationSolver::new(solver, objective_function, LinearSearch::new(encoding));

    match solver.solve(&mut termination, brancher) {
        MaxSatOptimisationResult::Optimal { solution } => {
            println!("s OPTIMUM FOUND");
            println!(
                "v {}",
                stringify_solution(&solution, last_instance_variable, false)
            );
        }
        MaxSatOptimisationResult::Satisfiable { best_solution } => {
            println!("s SATISFIABLE");
            println!(
                "v {}",
                stringify_solution(&best_solution, last_instance_variable, false)
            );
        }
        MaxSatOptimisationResult::Infeasible => {
            println!("s UNSATISFIABLE");
        }
        MaxSatOptimisationResult::Unknown => {
            println!("s UNKNOWN");
        }
    }

    Ok(())
}
