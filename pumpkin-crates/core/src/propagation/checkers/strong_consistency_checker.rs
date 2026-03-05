use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::containers::HashMap;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::propagation::checkers::Consistency;
use crate::propagation::checkers::ConsistencyChecker;
use crate::propagation::checkers::Witness;
use crate::propagation::checkers::WitnessGenerator;
use crate::variables::DomainId;

/// Tests for domain or bound consistency given a `checker`.
///
/// The checker is responsible for two things:
/// 1. It should generate the witnesses that support the values in the domain.
/// 3. It should identify conflicts in an assignment if that assignment violates the constraint.
#[derive(Clone, Debug)]
pub struct StrongConsistencyChecker<C> {
    checker: C,
    consistency: Consistency,
}

impl<C> StrongConsistencyChecker<C> {
    /// Create a new [`StrongConsistencyChecker`].
    pub fn new(checker: C, consistency: Consistency) -> Self {
        StrongConsistencyChecker {
            checker,
            consistency,
        }
    }

    /// Verifies that all values in the domain of `domain_id` occur in `supported_values`.
    fn verify_domain_consistency(
        &self,
        domains: Domains<'_>,
        domain_id: DomainId,
        supported_values: &[i32],
    ) -> bool {
        domains
            .iterate_domain(&domain_id)
            .all(|value| supported_values.contains(&value))
    }

    /// Verifies the bounds of the domain of `domain_id` occur in `supported_values`.
    fn verify_bounds_consistency(
        &self,
        domains: Domains<'_>,
        domain_id: DomainId,
        supported_values: &[i32],
    ) -> bool {
        let lower_bound = domains.lower_bound(&domain_id);
        let upper_bound = domains.upper_bound(&domain_id);

        supported_values.contains(&lower_bound) && supported_values.contains(&upper_bound)
    }
}

impl<C> StrongConsistencyChecker<C>
where
    C: InferenceChecker<Predicate>,
{
    /// Uses the checker to determine whether the given `witness` violates a constraint.
    fn validate_witness(&self, witness: &Witness) -> bool {
        let premises = witness
            .iter()
            .map(|(domain, value)| predicate![domain == value])
            .collect::<Vec<_>>();

        let state = VariableState::prepare_for_conflict_check(premises.clone(), None)
            .expect("the witness is consistent by construction");

        let state_is_conflicting = self.checker.check(state, &premises, None);

        !state_is_conflicting
    }
}

impl<C> ConsistencyChecker for StrongConsistencyChecker<C>
where
    C: WitnessGenerator + InferenceChecker<Predicate> + Clone,
{
    fn check_consistency(&self, mut domains: Domains<'_>, scope: &[DomainId]) -> bool {
        let mut supported_values: HashMap<DomainId, Vec<i32>> = HashMap::default();
        let witnesses = self.checker.support(domains.reborrow());

        for witness in witnesses {
            if !self.validate_witness(&witness) {
                dbg!(witness);
                panic!("witness should satisfy the constraint");
            }

            for (domain, value) in witness.iter() {
                let values = supported_values.entry(domain).or_insert(vec![]);
                values.push(value);
            }
        }

        scope.iter().copied().all(|domain_id| {
            let supported_values_for_domain = &supported_values[&domain_id];

            match self.consistency {
                Consistency::Domain => self.verify_domain_consistency(
                    domains.reborrow(),
                    domain_id,
                    supported_values_for_domain,
                ),
                Consistency::Bounds => self.verify_bounds_consistency(
                    domains.reborrow(),
                    domain_id,
                    supported_values_for_domain,
                ),
            }
        })
    }
}
