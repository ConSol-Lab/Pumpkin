use std::num::NonZero;

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
