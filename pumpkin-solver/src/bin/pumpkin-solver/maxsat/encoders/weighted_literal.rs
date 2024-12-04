use pumpkin_solver::variables::Literal;

#[derive(Copy, Clone, Debug)]
/// A struct containing a literal, weight and (optionally) the bound which the literal represents
pub(crate) struct WeightedLiteral {
    pub(crate) literal: Literal,
    pub(crate) weight: u64,
}
