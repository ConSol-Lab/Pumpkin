use super::Literal;

#[derive(Clone, Copy, Debug)]
pub struct WeightedLiteral {
    pub literal: Literal,
    pub weight: u64,
}
