//! # Pumpkin
//! Pumpkin is a CP-SAT solver which aims to be well-documented and extensible.
//!
//! Pumpkin implements a variety of (Max)SAT and Constraint Programming (CP) techniques to create a
//! solver which is both understandable and efficient. The main implementation relies on the LCG
//! framework as introduced in \[1\] while utilising the 1-UIP conflict analysis scheme as
//! introduced in \[2\].
//!
//!  # Using Pumpkin to solve a satisfaction problem
//! Pumpkin can be used to solve satisfaction problems. It contains an API for adding a set of
//! pre-defined constraints to the solver.
//!
//!  ```rust
//!  # use pumpkin_lib::Solver;
//!  # use pumpkin_lib::results::SatisfactionResult;
//!  # use pumpkin_lib::termination::Indefinite;
//!  # use pumpkin_lib::results::ProblemSolution;
//!  // We create the solver with default options
//!  let mut solver = Solver::default();
//!
//!  // We create 3 variables with domains within the range [0, 10]
//!  let x = solver.new_bounded_integer(0, 2);
//!  let y = solver.new_bounded_integer(0, 2);
//!  let z = solver.new_bounded_integer(0, 2);
//!
//!  // We create the all-different constraint
//!  solver.all_different(vec![x, y, z]);
//!
//!  // We create a termination condition which allows the solver to run indefinitely
//!  let mut termination = Indefinite;
//!  // And we create a search strategy (in this case, simply the default)
//!  let mut brancher = solver.default_brancher_over_all_propositional_variables();
//!
//!  // Then we solve to satisfaction
//!  let result = solver.satisfy(&mut brancher, &mut termination);
//!
//!  if let SatisfactionResult::Satisfiable(solution) = result {
//!      let value_x = solution.get_integer_value(x);
//!      let value_y = solution.get_integer_value(y);
//!      let value_z = solution.get_integer_value(z);
//!
//!      // It should be the case that all of the assigned values are different.
//!      assert!(value_x != value_y && value_x != value_z && value_y != value_z);
//!  } else {
//!      panic!("This problem should be satisfiable")
//!  }
//!  ```
//!
//! # Using Pumpkin to solve an optimization problem
//! Pumpkin can be used to solve optimization problems. It expects the objective to be provided in
//! the form of a single integer variable.
//! ```rust
//!  # use pumpkin_lib::Solver;
//!  # use pumpkin_lib::results::OptimisationResult;
//!  # use pumpkin_lib::termination::Indefinite;
//!  # use pumpkin_lib::results::ProblemSolution;
//! # use std::cmp::max;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(5, 10);
//! let y = solver.new_bounded_integer(-3, 15);
//! let z = solver.new_bounded_integer(7, 25);
//! let objective = solver.new_bounded_integer(-10, 30);
//!
//! // We create the constraints:
//! // - x + y + z = 17
//! // - maximum(x, y, z) = objective
//! solver.linear_equals(vec![x, y, z], 17);
//! solver.maximum(vec![x, y, z], objective);
//!
//! // We create a termination condition which allows the solver to run indefinitely
//! let mut termination = Indefinite;
//! // And we create a search strategy (in this case, simply the default)
//! let mut brancher = solver.default_brancher_over_all_propositional_variables();
//!
//! // Then we solve to optimality,
//! let result = solver.minimise(&mut brancher, &mut termination, objective);
//!
//! if let OptimisationResult::Optimal(optimal_solution) = result {
//!     let value_x = optimal_solution.get_integer_value(x);
//!     let value_y = optimal_solution.get_integer_value(y);
//!     let value_z = optimal_solution.get_integer_value(z);
//!     // The maximum objective values is 7;
//!     // with one possible solution being: {x = 5, y = 5, z = 7, objective = 7}.
//!     assert!(value_x + value_y + value_z == 17);
//!     assert!(
//!         max(value_x, max(value_y, value_z)) == optimal_solution.get_integer_value(objective)
//!     );
//!     assert_eq!(optimal_solution.get_integer_value(objective), 7);
//! } else {
//!     panic!("This problem should have an optimal solution")
//! }
//! ```
//!
//! # Obtaining multiple solutions
//! Pumpkin supports obtaining multiple solutions from the [`Solver`] when solving satisfaction
//! problems. The same solution is prevented from occurring multiple times by adding blocking
//! clauses to the solver which means that after iterating over solutions, these solutions will
//! remain blocked if the solver is used again.
//! ```rust
//!  # use pumpkin_lib::Solver;
//!  # use pumpkin_lib::results::SatisfactionResult;
//!  # use pumpkin_lib::termination::Indefinite;
//!  # use pumpkin_lib::results::ProblemSolution;
//!  # use pumpkin_lib::results::solution_iterator::IteratedSolution;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(0, 2);
//! let y = solver.new_bounded_integer(0, 2);
//! let z = solver.new_bounded_integer(0, 2);
//!
//! // We create the all-different constraint
//! solver.all_different(vec![x, y, z]);
//!
//! // We create a termination condition which allows the solver to run indefinitely
//! let mut termination = Indefinite;
//! // And we create a search strategy (in this case, simply the default)
//! let mut brancher = solver.default_brancher_over_all_propositional_variables();
//!
//! // Then we solve to satisfaction
//! let mut solution_iterator = solver.get_solution_iterator(&mut brancher, &mut termination);
//!
//! let mut number_of_solutions = 0;
//!
//! // We keep track of a list of known solutions
//! let mut known_solutions = Vec::new();
//!
//! loop {
//!     match solution_iterator.next_solution() {
//!         IteratedSolution::Solution(solution) => {
//!             number_of_solutions += 1;
//!             // We have found another solution, the same invariant should hold
//!             let value_x = solution.get_integer_value(x);
//!             let value_y = solution.get_integer_value(y);
//!             let value_z = solution.get_integer_value(z);
//!             assert!(x != y && x != z && y != z);
//!
//!             // It should also be the case that we have not found this solution before
//!             assert!(!known_solutions.contains(&(value_x, value_y, value_z)));
//!             known_solutions.push((value_x, value_y, value_z));
//!         }
//!         IteratedSolution::Finished => {
//!             // No more solutions exist
//!             break;
//!         }
//!         IteratedSolution::Unknown => {
//!             // Our termination condition has caused the solver to terminate
//!             break;
//!         }
//!         IteratedSolution::Unsatisfiable => {
//!             panic!("Problem should be satisfiable")
//!         }
//!     }
//! }
//! // There are six possible solutions to this problem
//! assert_eq!(number_of_solutions, 6)
//!  ```
//!
//! # Obtaining an unsatisfiable core
//! Pumpkin allows the user to specify assumptions which can then be used to extract an
//! unsatisfiable core.
//! ```rust
//!  # use pumpkin_lib::Solver;
//!  # use pumpkin_lib::results::SatisfactionResultUnderAssumptions;
//!  # use pumpkin_lib::termination::Indefinite;
//!  # use pumpkin_lib::predicate;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(0, 2);
//! let y = solver.new_bounded_integer(0, 2);
//! let z = solver.new_bounded_integer(0, 2);
//!
//! // We create the all-different constraint
//! solver.all_different(vec![x, y, z]);
//!
//! // We create a termination condition which allows the solver to run indefinitely
//! let mut termination = Indefinite;
//! // And we create a search strategy (in this case, simply the default)
//! let mut brancher = solver.default_brancher_over_all_propositional_variables();
//!
//! // Then we solve to satisfaction
//! let assumptions = vec![
//!     solver.get_literal(predicate!(x == 1)),
//!     solver.get_literal(predicate!(y <= 1)),
//!     solver.get_literal(predicate!(y != 0)),
//! ];
//! let result =
//!     solver.satisfy_under_assumptions(&mut brancher, &mut termination, &assumptions);
//!
//! if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
//!     mut unsatisfiable,
//! ) = result
//! {
//!     {
//!         let core = unsatisfiable.extract_core();
//!
//!         // In this case, the core should be equal to the negation of all literals in the
//!         // assumptions
//!         assert!(assumptions
//!             .into_iter()
//!             .all(|literal| core.contains(&(!literal))));
//!     }
//! }
//!  ```
//! # Bibliography
//! \[1\] T. Feydy and P. J. Stuckey, ‘Lazy clause generation reengineered’, in International
//! Conference on Principles and Practice of Constraint Programming, 2009, pp. 352–366.
//!
//!  \[2\] J. Marques-Silva, I. Lynce, and S. Malik, ‘Conflict-driven clause learning SAT
//!  solvers’, in Handbook of satisfiability, IOS press, 2021
pub(crate) mod basic_types;
pub mod branching;
pub(crate) mod encoders;
pub(crate) mod engine;
pub(crate) mod math;
pub(crate) mod propagators;
pub(crate) mod pumpkin_asserts;
pub(crate) mod variable_names;

// We declare a private module with public use, so that all exports from API are exports directly
// from the crate.
//
// Example:
// `use pumpkin_lib::Solver;`
// vs.
// `use pumpkin_lib::api::Solver;`
mod api;

pub use api::*;

pub use crate::api::solver::DefaultBrancher;
pub use crate::api::solver::Solver;
pub use crate::basic_types::ConstraintOperationError;
pub use crate::basic_types::Random;
