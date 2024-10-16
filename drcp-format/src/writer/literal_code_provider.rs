use std::num::NonZeroI32;

/// Describes a mapping from an arbitrary literal type to literal codes used in the proof.
pub trait LiteralCodeProvider {
    /// The literal type to provide codes for.
    type Literal;

    /// Convert a literal into the literal code.
    fn to_code(&mut self, literal: Self::Literal) -> NonZeroI32;
}

impl<F> LiteralCodeProvider for F
where
    F: Fn(NonZeroI32) -> NonZeroI32,
{
    type Literal = NonZeroI32;

    fn to_code(&mut self, literal: Self::Literal) -> NonZeroI32 {
        self(literal)
    }
}
