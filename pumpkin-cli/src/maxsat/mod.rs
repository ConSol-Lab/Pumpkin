use std::fs::File;
use std::path::Path;
use std::time::Duration;
pub(crate) mod optimisation;

use optimisation::linear_search::LinearSearch;
use optimisation::optimisation_result::MaxSatOptimisationResult;
use optimisation::optimisation_solver::OptimisationSolver;
use pumpkin_lib::encodings::PseudoBooleanEncoding;
use pumpkin_lib::options::LearningOptions;
use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::termination::TimeBudget;

use crate::parsers::dimacs::parse_wcnf;
use crate::parsers::dimacs::CSPSolverArgs;
use crate::parsers::dimacs::SolverDimacsSink;
use crate::parsers::dimacs::WcnfInstance;
use crate::result::PumpkinError;
use crate::stringify_solution;

pub(crate) fn wcnf_problem(
    learning_options: LearningOptions,
    solver_options: SolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    upper_bound_encoding: PseudoBooleanEncoding,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let WcnfInstance {
        formula: solver,
        objective: objective_function,
        last_instance_variable,
    } = parse_wcnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(learning_options, solver_options),
    )?;

    let brancher = solver.default_brancher_over_all_propositional_variables();

    let mut solver = OptimisationSolver::new(
        solver,
        objective_function,
        LinearSearch::new(upper_bound_encoding),
    );

    let mut termination = time_limit.map(TimeBudget::starting_now);

    let result = match solver.solve(&mut termination, brancher) {
        MaxSatOptimisationResult::Optimal {
            solution,
            objective_value,
        } => {
            println!("s OPTIMAL");
            println!(
                "v {}",
                stringify_solution(&solution, last_instance_variable + 1, false)
            );
            Some((solution, objective_value))
        }
        MaxSatOptimisationResult::Satisfiable {
            best_solution,
            objective_value,
        } => {
            println!("s SATISFIABLE");
            println!(
                "v {}",
                stringify_solution(&best_solution, last_instance_variable + 1, false)
            );
            Some((best_solution, objective_value))
        }
        MaxSatOptimisationResult::Infeasible => {
            println!("s UNSATISFIABLE");
            None
        }
        MaxSatOptimisationResult::Unknown => {
            println!("s UNKNOWN");
            None
        }
    };

    Ok(())
}
