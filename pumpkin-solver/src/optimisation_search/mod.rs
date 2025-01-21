use crate::branching::Brancher;
use crate::results::OptimisationResult;
use crate::results::Solution;
use crate::results::SolutionCallbackArguments;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;
use crate::Solver;

pub(crate) mod lower_bounding_search;
pub(crate) mod upper_bounding_search;

pub trait OptimisationProcedure {
    fn minimise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
        is_maximising: bool,
        solver: &mut Solver,
    ) -> OptimisationResult;

    fn maximise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
        _is_minimising: bool,
        solver: &mut Solver,
    ) -> OptimisationResult {
        self.minimise(
            brancher,
            termination,
            objective_variable.scaled(-1),
            true,
            solver,
        )
    }

    /// Processes a solution when it is found, it consists of the following procedure:
    /// - Assigning `best_objective_value` the value assigned to `objective_variable` (multiplied by
    ///   `objective_multiplier`).
    /// - Storing the new best solution in `best_solution`.
    /// - Calling [`Brancher::on_solution`] on the provided `brancher`.
    /// - Logging the statistics using [`Solver::log_statistics_with_objective`].
    /// - Calling the solution callback stored with [`Solver::with_solution_callback`].
    fn update_best_solution_and_process(
        &self,
        objective_multiplier: i32,
        objective_variable: &impl IntegerVariable,
        best_objective_value: &mut i64,
        best_solution: &mut Solution,
        brancher: &mut impl Brancher,
        solver: &Solver,
    ) {
        *best_objective_value = (objective_multiplier
            * solver
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned")) as i64;
        *best_solution = solver.satisfaction_solver.get_solution_reference().into();

        self.internal_process_solution(best_solution, brancher, Some(*best_objective_value), solver)
    }

    fn internal_process_solution(
        &self,
        solution: &Solution,
        brancher: &mut impl Brancher,
        objective_value: Option<i64>,
        solver: &Solver,
    ) {
        brancher.on_solution(solution.as_reference());

        (solver.solution_callback)(SolutionCallbackArguments::new(
            solver,
            solution,
            objective_value,
        ));
    }
}
