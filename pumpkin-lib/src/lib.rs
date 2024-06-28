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
//!  if let SatisfactionResult::Satisfiable(satisfiable) = result {
//!      let solution = satisfiable.as_solution();
//!      let value_x = solution.get_integer_value(x);
//!      let value_y = solution.get_integer_value(y);
//!      let value_z = solution.get_integer_value(z);
//!
//!      // It should be the case that all of the assigned values are different.
//!      assert!(x != y && x != z && y != z);
//!  } else {
//!      panic!("This problem should be satisfiable")
//!  }
//!  ```
//!
//! # Obtaining multiple solutions
//! Pumpkin supports obtaining multiple solutions from the [`Solver`] when solving satisfaction
//! problems. The same solution is prevend from occurring multiple times by adding blocking clauses
//! to the solver which means that after iterating over solutions, these solutions will remain
//! blocked if the solver is used again.
//!
//! ```rust
//!  # use pumpkin_lib::Solver;
//!  # use pumpkin_lib::results::SatisfactionResult;
//!  # use pumpkin_lib::termination::Indefinite;
//!  # use pumpkin_lib::results::ProblemSolution;
//!  # use pumpkin_lib::results::satisfiable::IteratedSolution;
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
//! let result = solver.satisfy(&mut brancher, &mut termination);
//!
//! // We keep track of a list of known solutions
//! let mut known_solutions = Vec::new();
//!
//! if let SatisfactionResult::Satisfiable(satisfiable) = result {
//!     {
//!         let solution = satisfiable.as_solution();
//!         let value_x = solution.get_integer_value(x);
//!         let value_y = solution.get_integer_value(y);
//!         let value_z = solution.get_integer_value(z);
//!
//!         // It should be the case that all of the assigned values are different.
//!         assert!(x != y && x != z && y != z);
//!
//!         known_solutions.push((value_x, value_y, value_z));
//!     }
//!
//!     // Now we can iterate over the solutions
//!     let mut solution_iterator = satisfiable.iterate_solutions();
//!     loop {
//!         match solution_iterator.next_solution() {
//!             IteratedSolution::Solution(solution) => {
//!                 // We have found another solution, the same invariant should hold
//!                 let value_x = solution.get_integer_value(x);
//!                 let value_y = solution.get_integer_value(y);
//!                 let value_z = solution.get_integer_value(z);
//!                 assert!(x != y && x != z && y != z);
//!
//!                 // It should also be the case that we have not found this solution before
//!                 assert!(!known_solutions.contains(&(value_x, value_y, value_z)));
//!                 known_solutions.push((value_x, value_y, value_z));
//!             }
//!             IteratedSolution::Finished => {
//!                 // No more solutions exist
//!                 break;
//!             }
//!             IteratedSolution::Unknown => {
//!                 // Our termination condition has caused the solver to terminate
//!                 break;
//!             }
//!         }
//!     }
//! } else {
//!     panic!("This problem should be satisfiable")
//! }
//!  ```
//!
//! # Obtaining an unsatisfiable core
//! Pumpkin allows the user to specify assumptions which can then be used to extract an unsatisfiable core.
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
//!     solver.get_literal_for_predicate(predicate!(x == 1)),
//!     solver.get_literal_for_predicate(predicate!(y <= 1)),
//!     solver.get_literal_for_predicate(predicate!(y != 0)),
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
//!         // The core should be equal to the negation of all literals in the assumptions
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
