use drcp_format::IntAtomic;
use drcp_format::IntComparison;

use crate::basic_types::HashMap;
use crate::engine::VariableNames;
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
        let comparison = match predicate {
            Predicate::UpperBound { .. } => IntComparison::LessEqual,
            Predicate::Equal { .. } => IntComparison::Equal,
            Predicate::LowerBound { .. } => IntComparison::GreaterEqual,
            Predicate::NotEqual { .. } => IntComparison::NotEqual,
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

                match predicate {
                    // The `predicate` is false
                    Predicate::UpperBound { upper_bound: 0, .. }
                    | Predicate::Equal {
                        equality_constant: 0,
                        ..
                    }
                    | Predicate::NotEqual {
                        not_equal_constant: 1,
                        ..
                    } => !reified_predicate,

                    // The `predicate` is true
                    Predicate::LowerBound { lower_bound: 1, .. }
                    | Predicate::Equal {
                        equality_constant: 1,
                        ..
                    }
                    | Predicate::NotEqual {
                        not_equal_constant: 0,
                        ..
                    } => reified_predicate,

                    p => panic!("{p:?} is not a valid reification predicate"),
                }
            })
    }
}
