//! Randomised testing of the propagators of Pumpkin, with the checkers of their rules as the only
//! oracles: the rule is what is trusted, the propagator is what is tested.
//!
//! The examples are FlatZinc instances with one constraint, which the solver compiles itself. They
//! are extracted from FlatZinc files, one per constraint and deduplicated up to the names of
//! variables, generated at random from the signatures of the supported constraints, or mutated from
//! other examples. Each example is driven through random decisions and backtracks, and the runtime
//! checkers of the solver judge every propagation, every fixpoint and every solution. A failure is
//! shrunk and reported with a reproducer.
//!
//! The checkers are compiled in through the features of this crate; `checks` enables all of them.

pub mod driver;
pub mod example;
pub mod extraction;
pub mod generation;
pub mod logging;
pub mod report;
mod statements;

pub use driver::replay;
