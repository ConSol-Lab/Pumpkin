use pumpkin_lib::variables::Literal;

#[derive(Clone, Copy, Debug)]
/// A struct containing a literal, weight and (optionally) the bound which the literal represents
pub(crate) struct WeightedLiteral {
    pub(crate) literal: Literal,
    pub(crate) weight: u64,
    /// The bound which the [`WeightedLiteral::literal`] represents (e.g. [x >= 5] would
    /// have bound Some(5))
    pub(crate) _bound: Option<i32>,
}
