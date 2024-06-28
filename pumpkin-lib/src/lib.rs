//! # Pumpkin
//! Pumpkin is a CP-SAT solver which aims to be well-documented and extensible.
//!
//! Pumpkin implements a variety of (Max)SAT and Constraint Programming (CP) techniques to create a
//! solver which is both understandable and efficient. The main implementation relies on the LCG
//! framework as introduced in \[1\] while utilising the 1-UIP conflict analysis scheme as
//! introduced in \[2\].
//!
//!  # Example of how to use Pumpkin
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
