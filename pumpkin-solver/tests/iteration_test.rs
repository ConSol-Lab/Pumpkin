#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use pumpkin_solver::constraints;
use pumpkin_solver::results::solution_iterator::IteratedSolution;
use pumpkin_solver::results::ProblemSolution;
use pumpkin_solver::termination::Indefinite;
use pumpkin_solver::Solver;

#[test]
fn iterator_finds_all_solutions() {
    // We create the solver with default options
    let mut solver = Solver::default();

    // We create 3 variables with domains within the range [0, 2]
    let x = solver.new_bounded_integer(0, 2);
    let y = solver.new_bounded_integer(0, 2);
    let z = solver.new_bounded_integer(0, 2);

    // We create the all-different constraint
    let _ = solver
        .add_constraint(constraints::all_different(vec![x, y, z]))
        .post();

    // We create a termination condition which allows the solver to run indefinitely
    let mut termination = Indefinite;
    // And we create a search strategy (in this case, simply the default)
    let mut brancher = solver.default_brancher();

    // Then we solve to satisfaction
    let mut solution_iterator = solver.get_solution_iterator(&mut brancher, &mut termination);

    let mut number_of_solutions = 0;

    // We keep track of a list of known solutions
    let mut known_solutions = Vec::new();

    loop {
        match solution_iterator.next_solution() {
            IteratedSolution::Solution(solution, _) => {
                number_of_solutions += 1;
                // We have found another solution, the same invariant should hold
                let value_x = solution.get_integer_value(x);
                let value_y = solution.get_integer_value(y);
                let value_z = solution.get_integer_value(z);
                assert!(x != y && x != z && y != z);

                // It should also be the case that we have not found this solution before
                assert!(!known_solutions.contains(&(value_x, value_y, value_z)));
                known_solutions.push((value_x, value_y, value_z));
            }
            IteratedSolution::Finished => {
                // No more solutions exist
                break;
            }
            IteratedSolution::Unknown => {
                // Our termination condition has caused the solver to terminate
                break;
            }
            IteratedSolution::Unsatisfiable => {
                panic!("Problem should be satisfiable")
            }
        }
    }
    // There are six possible solutions to this problem
    assert_eq!(number_of_solutions, 6)
}
