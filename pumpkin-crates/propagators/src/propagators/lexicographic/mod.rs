//! The constraint `lex_less_or_equal(x, y)`: the vector `x` is lexicographically at most `y`.
//!
//! Let `alpha` be the first position at which `x` and `y` are not both fixed to the same value.
//! Every earlier position is equal, so the constraint requires `x[alpha] <= y[alpha]`, and
//! `x[alpha] < y[alpha]` if the positions after `alpha` cannot be lexicographically ordered: that
//! is the case exactly when the lower bounds of `x` after `alpha` are lexicographically greater
//! than the upper bounds of `y` after `alpha`. The propagator enforces these bounds on
//! `x[alpha]` and `y[alpha]` and repeats while `alpha` moves forward.
//!
//! For example, with `x = [2, x1]`, `y = [2, y1]`, `x1` in `3..5` and `y1` in `0..4`, the first
//! position is fixed and equal, so `alpha = 1`: `x1 <= 4` and `y1 >= 3`. With `x = [x0, 4]`,
//! `y = [y0, 3]` and `x0, y0` in `1..3`, the last position rules out `x0 = y0`, so `x0 <= 2` and
//! `y0 >= 2`.

mod conflict_rule;
mod constraint_description;
mod constructor;
mod propagator;
#[cfg(test)]
mod tests;

pub use conflict_rule::*;
pub use constraint_description::*;
pub use constructor::*;
pub use propagator::*;
