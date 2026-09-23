mod helpers;
mod inference;
#[cfg(test)]
mod tests;

pub use helpers::*;

/// Verifies that a claimed inference for `a * b = c` is actually implied by its premises.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationChecker<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}
