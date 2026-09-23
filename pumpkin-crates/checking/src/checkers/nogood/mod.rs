mod extended_retention;
mod inference;
#[cfg(test)]
mod tests;
mod unit_retention;

use crate::AtomicConstraint;
use crate::VariableState;

/// The checker of a nogood under unit propagation.
#[derive(Debug, Clone)]
pub struct NogoodChecker<Atomic> {
    pub nogood: Box<[Atomic]>,
}

/// The retention checker for extended nogood propagation:
/// when the atomic constraints over all but one variable hold,
/// that variable has no value left that satisfies its atomic constraints.
///
/// Its inferences are checked by [`NogoodChecker`].
#[derive(Debug, Clone)]
pub struct ExtendedNogoodChecker<Atomic> {
    pub nogood: Box<[Atomic]>,
}

/// `Some(true)` if the atomic constraint holds in the state, `Some(false)` if its negation
/// holds, and `None` otherwise.
fn truth_value<Atomic: AtomicConstraint>(
    atomic: &Atomic,
    state: &VariableState<Atomic>,
) -> Option<bool> {
    if state.is_true(atomic) {
        Some(true)
    } else if state.is_true(&atomic.negate()) {
        Some(false)
    } else {
        None
    }
}
