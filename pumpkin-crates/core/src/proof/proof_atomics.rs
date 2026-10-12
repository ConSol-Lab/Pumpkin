use drcp_format::IntAtomic;
use drcp_format::IntComparison;

use crate::engine::VariableNames;
use crate::engine::predicates::predicate::PredicateType;
use crate::predicates::Predicate;

/// Convert a predicate to the [`IntAtomic`] that is used in the proof.
pub(crate) fn map_predicate_to_proof_atomic(
    predicate: Predicate,
    variable_names: &VariableNames,
) -> IntAtomic<&str, i32> {
    let name = variable_names
        .get_int_name(predicate.get_domain())
        .expect("integer domain is unnamed");
    let value = predicate.get_right_hand_side();
    let comparison = match predicate.get_predicate_type() {
        PredicateType::UpperBound => IntComparison::LessEqual,
        PredicateType::Equal => IntComparison::Equal,
        PredicateType::LowerBound => IntComparison::GreaterEqual,
        PredicateType::NotEqual => IntComparison::NotEqual,
    };

    IntAtomic::new(name, comparison, value)
}
