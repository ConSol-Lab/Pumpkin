use pumpkin_core::predicates::Predicate;

#[derive(Copy, Clone, Debug)]
/// A struct containing a literal, weight and (optionally) the bound which the literal represents
pub(crate) struct WeightedLiteral {
    pub(crate) literal: Predicate,
    pub(crate) weight: u64,
}
