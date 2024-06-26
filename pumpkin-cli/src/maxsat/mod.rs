use std::{fs::File, path::Path, time::Duration};

use pumpkin_lib::{
    branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher,
    options::{LearningOptions, PseudoBooleanEncoding, SolverOptions},
};

use crate::{
    parsers::dimacs::{parse_wcnf, CSPSolverArgs, SolverDimacsSink, WcnfInstance},
    result::PumpkinError,
};

pub fn wcnf_problem(
    learning_options: LearningOptions,
    solver_options: SolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    upper_bound_encoding: PseudoBooleanEncoding,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let WcnfInstance {
        formula: csp_solver,
        objective: objective_function,
        last_instance_variable,
    } = parse_wcnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(learning_options, solver_options),
    )?;

    let brancher =
        IndependentVariableValueBrancher::default_over_all_propositional_variables(&csp_solver);

    todo!()

    // let mut solver = OptimisationSolver::new(
    //     csp_solver,
    //     objective_function,
    //     LinearSearch::new(upper_bound_encoding),
    // );

    // let mut termination = time_limit.map(TimeBudget::starting_now);

    // let result = match solver.solve(&mut termination, brancher) {
    //     OptimisationResult::Optimal {
    //         solution,
    //         objective_value,
    //     } => {
    //         println!("s OPTIMAL");
    //         println!(
    //             "v {}",
    //             stringify_solution(&solution, last_instance_variable + 1, false)
    //         );
    //         Some((solution, objective_value))
    //     }
    //     OptimisationResult::Satisfiable {
    //         best_solution,
    //         objective_value,
    //     } => {
    //         println!("s SATISFIABLE");
    //         println!(
    //             "v {}",
    //             stringify_solution(&best_solution, last_instance_variable + 1, false)
    //         );
    //         Some((best_solution, objective_value))
    //     }
    //     OptimisationResult::Infeasible => {
    //         println!("s UNSATISFIABLE");
    //         None
    //     }
    //     OptimisationResult::Unknown => {
    //         println!("s UNKNOWN");
    //         None
    //     }
    // };

    // Ok(())
}
