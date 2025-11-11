use drcp_format::IntAtomic;
use drcp_format::IntComparison;

use crate::containers::HashMap;
use crate::engine::VariableNames;
use crate::engine::predicates::predicate::PredicateType;
use crate::predicates::Predicate;
use crate::variables::DomainId;
use crate::variables::Literal;

#[derive(Default, Debug)]
pub(crate) struct ProofAtomics {
    /// Maps the domain id of a 0-1 integer `x` to the predicate `p` that it reifies:
    /// `[x == 1] <-> p`.
    ///
    /// Used in substituting the reification domain with the predicate when logging reasons.
    reification_domains: HashMap<DomainId, Predicate>,
}

impl ProofAtomics {
    /// Convert a predicate to the [`IntAtomic`] that is used in the proof.
    pub(crate) fn map_predicate_to_proof_atomic<'names>(
        &mut self,
        predicate: Predicate,
        variable_names: &'names VariableNames,
    ) -> IntAtomic<&'names str, i32> {
        let predicate = self
            .get_underlying_predicate(predicate)
            .unwrap_or(predicate);

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

    /// Given a literal, whenever it shows up in a proof step, substitute it with the provided
    /// predicate.
    pub(crate) fn reify_predicate(&mut self, literal: Literal, predicate: Predicate) {
        // Note: This only works because we assume `literal` is a fresh literal and we are given
        // the positive polarity. That assumption holds as the only place this can be called is
        // transitively through `new_literal_for_predicate`. As soon as this assumption is
        // violated, all hell will break loose.
        let domain = literal.get_true_predicate().get_domain();

        let _ = self.reification_domains.insert(domain, predicate);
    }

    /// The given predicate is a predicate over a literal. This function gets the associated
    /// predicate that was reified with [`Self::reify_predicate`] if it exists.
    fn get_underlying_predicate(&self, predicate: Predicate) -> Option<Predicate> {
        let domain_id = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();

        self.reification_domains
            .get(&domain_id)
            .map(|&reified_predicate| {
                assert!(rhs == 0 || rhs == 1);

                let value = predicate.get_right_hand_side();
                match predicate.get_predicate_type() {
                    PredicateType::LowerBound => reified_predicate,
                    PredicateType::UpperBound => !reified_predicate,
                    PredicateType::NotEqual => {
                        if value == 0 {
                            reified_predicate
                        } else {
                            !reified_predicate
                        }
                    }
                    PredicateType::Equal => {
                        if value == 0 {
                            !reified_predicate
                        } else {
                            reified_predicate
                        }
                    }
                }
            })
    }
}
