use super::Literal;

#[derive(Clone, Copy, Debug)]
/// A struct containing a literal, weight and (optionally) the bound which the literal represents
pub struct WeightedLiteral {
    pub literal: Literal,
    pub weight: u64,
    /// The bound which the [literal][WeightedLiteral::literal] represents (e.g. [x >= 5] would have bound Some(5))
    pub bound: Option<i32>,
}
