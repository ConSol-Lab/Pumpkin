//! # Pumpkin
//! Pumpkin is a combinatorial optimisation solver developed by the ConSol Lab at TU Delft. It is
//! based on the (lazy clause generation) constraint programming paradigm.
//!
//! Our goal is to keep the solver efficient, easy to use, and well-documented. The solver is
//! written in pure Rust and follows Rust best practices.
//!
//! A unique feature of Pumpkin is that it can produce a _certificate of unsatisfiability_. See our
//! CP'24 paper for more details.
//!
//! The solver currently supports integer variables and a number of (global) constraints:
//! * [Cumulative global constraint][crate::constraints::cumulative].
//! * [Element global constraint][crate::constraints::element].
//! * Arithmetic constraints: [linear integer
//!   (in)equalities][crate::constraints::less_than_or_equals], [integer
//!   division][crate::constraints::division], [integer multiplication][crate::constraints::times],
//!   [maximum][crate::constraints::maximum], [absolute value][crate::constraints::absolute].
//! * [Clausal constraints][Solver::add_clause].
//!
//! We are actively developing Pumpkin and would be happy to hear from you should you have any
//! questions or feature requests!
//!
//!  # Using Pumpkin
//! Pumpkin can be used to solve a variety of problems. The first step to solving a problem is
//! **adding variables**:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::OptimisationResult;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::ProblemSolution;
//! # use pumpkin_lib::constraints::Constraint;
//! # use std::cmp::max;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(5, 10);
//! let y = solver.new_bounded_integer(-3, 15);
//! let z = solver.new_bounded_integer(7, 25);
//! ```
//!
//! Then we can **add constraints** supported by the [`Solver`]:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::OptimisationResult;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::ProblemSolution;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! # use std::cmp::max;
//! # let mut solver = Solver::default();
//! # let x = solver.new_bounded_integer(5, 10);
//! # let y = solver.new_bounded_integer(-3, 15);
//! # let z = solver.new_bounded_integer(7, 25);
//! // We create the constraint:
//! // - x + y + z = 17
//! solver
//!     .add_constraint(constraints::equals(vec![x, y, z], 17))
//!     .post();
//! ```
//!
//! For finding a solution, a [`TerminationCondition`] and a [`Brancher`] should be specified, which
//! determine when the solver should stop searching and the variable/value selection strategy which
//! should be used:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::termination::Indefinite;
//! # let mut solver = Solver::default();
//! // We create a termination condition which allows the solver to run indefinitely
//! let mut termination = Indefinite;
//! // And we create a search strategy (in this case, simply the default)
//! let mut brancher = solver.default_brancher_over_all_propositional_variables();
//! ```
//!
//!
//! **Finding a solution** to this problem can be done by using [`Solver::satisfy`]:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::SatisfactionResult;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::ProblemSolution;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! # use std::cmp::max;
//! # let mut solver = Solver::default();
//! # let x = solver.new_bounded_integer(5, 10);
//! # let y = solver.new_bounded_integer(-3, 15);
//! # let z = solver.new_bounded_integer(7, 25);
//! # solver.add_constraint(constraints::equals(vec![x, y, z], 17)).post();
//! # let mut termination = Indefinite;
//! # let mut brancher = solver.default_brancher_over_all_propositional_variables();
//! // Then we find a solution to the problem
//! let result = solver.satisfy(&mut brancher, &mut termination);
//!
//! if let SatisfactionResult::Satisfiable(solution) = result {
//!     let value_x = solution.get_integer_value(x);
//!     let value_y = solution.get_integer_value(y);
//!     let value_z = solution.get_integer_value(z);
//!
//!     // The constraint should hold for this solution
//!     assert!(value_x + value_y + value_z == 17);
//! } else {
//!     panic!("This problem should have a solution")
//! }
//! ```
//!
//! **Optimizing an objective** can be done in a similar way using [`Solver::maximise`] or
//! [`Solver::minimise`]; first the objective variable and a constraint over this value are added:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! # let mut solver = Solver::default();
//! # let x = solver.new_bounded_integer(5, 10);
//! # let y = solver.new_bounded_integer(-3, 15);
//! # let z = solver.new_bounded_integer(7, 25);
//! // We add another variable, the objective
//! let objective = solver.new_bounded_integer(-10, 30);
//!
//! // We add a constraint which specifies the value of the objective
//! solver
//!     .add_constraint(constraints::maximum(vec![x, y, z], objective))
//!     .post();
//! ```
//!
//! Then we can find the optimal solution using [`Solver::minimise`] or [`Solver::maximise`]:
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::OptimisationResult;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::ProblemSolution;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! # use std::cmp::max;
//! # let mut solver = Solver::default();
//! # let x = solver.new_bounded_integer(5, 10);
//! # let y = solver.new_bounded_integer(-3, 15);
//! # let z = solver.new_bounded_integer(7, 25);
//! # let objective = solver.new_bounded_integer(-10, 30);
//! # solver.add_constraint(constraints::equals(vec![x, y, z], 17)).post();
//! # solver.add_constraint(constraints::maximum(vec![x, y, z], objective)).post();
//! # let mut termination = Indefinite;
//! # let mut brancher = solver.default_brancher_over_all_propositional_variables();
//! // Then we solve to optimality
//! let result = solver.minimise(&mut brancher, &mut termination, objective);
//!
//! if let OptimisationResult::Optimal(optimal_solution) = result {
//!     let value_x = optimal_solution.get_integer_value(x);
//!     let value_y = optimal_solution.get_integer_value(y);
//!     let value_z = optimal_solution.get_integer_value(z);
//!     // The maximum objective values is 7;
//!     // with one possible solution being: {x = 5, y = 5, z = 7, objective = 7}.
//!
//!     // We check whether the constraint holds again
//!     assert!(value_x + value_y + value_z == 17);
//!     // We check whether the newly added constraint for the objective value holds
//!     assert!(
//!         max(value_x, max(value_y, value_z)) == optimal_solution.get_integer_value(objective)
//!     );
//!     // We check whether this is actually an optimal solution
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
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::SatisfactionResult;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::results::ProblemSolution;
//! # use pumpkin_lib::results::solution_iterator::IteratedSolution;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(0, 2);
//! let y = solver.new_bounded_integer(0, 2);
//! let z = solver.new_bounded_integer(0, 2);
//!
//! // We create the all-different constraint
//! solver.add_constraint(constraints::all_different(vec![x, y, z])).post();
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
//! unsatisfiable core (see [`UnsatisfiableUnderAssumptions::extract_core`]).
//! ```rust
//! # use pumpkin_lib::Solver;
//! # use pumpkin_lib::results::SatisfactionResultUnderAssumptions;
//! # use pumpkin_lib::termination::Indefinite;
//! # use pumpkin_lib::predicate;
//! # use pumpkin_lib::constraints;
//! # use pumpkin_lib::constraints::Constraint;
//! // We create the solver with default options
//! let mut solver = Solver::default();
//!
//! // We create 3 variables with domains within the range [0, 10]
//! let x = solver.new_bounded_integer(0, 2);
//! let y = solver.new_bounded_integer(0, 2);
//! let z = solver.new_bounded_integer(0, 2);
//!
//! // We create the all-different constraint
//! solver.add_constraint(constraints::all_different(vec![x, y, z])).post();
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
//!         // We get a core from the unsatisfiable instance
//!         let core = unsatisfiable.extract_core();
//!
//!         // We know that the core is non-empty, non-unit and not at the root-level
//!         assert!(!core.is_empty());
//!         assert!(!core.is_unit());
//!         assert!(!core.is_a_root_level_core());
//!
//!         // There is two ways to check the contents of the core, first we take a look
//!         // at the negated assumption literals
//!         let core_negated_assumption_literals = core.get_negated_assumption_literals();
//!         assert!(assumptions
//!             .iter()
//!             .cloned()
//!             .all(|assumption| { core_negated_assumption_literals.contains(&(!assumption)) }));
//!
//!         // Then we check the original core
//!         let core = core.get_core();
//!         assert!(assumptions
//!             .iter()
//!             .all(|assumption| core.contains(assumption)));
//!     }
//! }
//!  ```
#[cfg(doc)]
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
pub(crate) mod basic_types;
pub(crate) mod encoders;
pub(crate) mod engine;
pub(crate) mod math;
pub(crate) mod propagators;
pub(crate) mod pumpkin_asserts;
pub(crate) mod variable_names;
#[cfg(doc)]
use crate::branching::Brancher;
#[cfg(doc)]
use crate::termination::TerminationCondition;

pub mod branching;
pub mod constraints;

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
