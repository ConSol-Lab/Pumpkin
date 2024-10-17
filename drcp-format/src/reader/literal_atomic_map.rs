use std::num::NonZero;

use crate::AtomicConstraint;
use crate::LiteralDefinitions;

/// A mapping from literals as they appear in a DRCP file, to atomic constraints.
///
/// The mapping should be total.
pub trait LiteralAtomicMap {
    /// The atomic constraint type to convert to.
    type Atomic;

    /// Create/obtain the atomic constraint associated with the given literal.
    fn to_atomic(&self, literal: NonZero<i32>) -> Self::Atomic;
}

impl<F, Out> LiteralAtomicMap for F
where
    F: Fn(NonZero<i32>) -> Out,
{
    type Atomic = Out;

    fn to_atomic(&self, literal: NonZero<i32>) -> Self::Atomic {
        self(literal)
    }
}

impl<Identifier> LiteralAtomicMap for LiteralDefinitions<Identifier>
where
    Identifier: Clone,
{
    type Atomic = AtomicConstraint<Identifier>;

    fn to_atomic(&self, literal: NonZero<i32>) -> Self::Atomic {
        let propositional = literal.unsigned_abs();
        let atomic = self.get(propositional).expect("all literals are present")[0].clone();

        if literal.is_positive() {
            atomic
        } else {
            !atomic
        }
    }
}
