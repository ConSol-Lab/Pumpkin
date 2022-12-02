use super::Literal;

#[derive(Clone, Copy)]
pub struct WeightedLiteral {
    pub literal: Literal,
    pub weight: u64,
}
