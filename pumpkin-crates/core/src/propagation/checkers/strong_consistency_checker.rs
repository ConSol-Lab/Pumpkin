use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::containers::HashMap;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::propagation::checkers::Consistency;
use crate::propagation::checkers::ConsistencyChecker;
use crate::propagation::checkers::Scope;
use crate::propagation::checkers::Witness;
use crate::propagation::checkers::WitnessGenerator;
use crate::variables::DomainId;

#[derive(Clone, Debug)]
pub struct StrongConsistencyChecker<C> {
    witness_generator: C,
    consistency: Consistency,
}

impl<C> StrongConsistencyChecker<C> {
    pub fn new(witness_generator: C, consistency: Consistency) -> Self {
        StrongConsistencyChecker {
            witness_generator,
            consistency,
        }
    }

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
    fn validate_witness(&self, witness: &Witness) -> bool {
        let premises = witness
            .iter()
            .map(|(domain, value)| predicate![domain == value])
            .collect::<Vec<_>>();

        let state = VariableState::prepare_for_conflict_check(premises.clone(), None)
            .expect("the witness is consistent by construction");

        let state_is_conflicting = self.witness_generator.check(state, &premises, None);

        !state_is_conflicting
    }
}

impl<C> ConsistencyChecker for StrongConsistencyChecker<C>
where
    C: WitnessGenerator + InferenceChecker<Predicate> + Clone,
{
    fn check_consistency(&self, mut domains: Domains<'_>, scope: &Scope) -> bool {
        let mut supported_values: HashMap<DomainId, Vec<i32>> = HashMap::default();
        let witnesses = self.witness_generator.support(domains.reborrow());

        for witness in witnesses {
            assert!(
                self.validate_witness(&witness),
                "witness should satisfy the constraint"
            );

            for (domain, value) in witness.iter() {
                let values = supported_values.entry(domain).or_insert(vec![]);
                values.push(value);
            }
        }

        scope.iter().all(|(_local_id, domain_id)| {
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
