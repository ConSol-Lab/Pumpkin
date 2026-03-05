use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::conjunction;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::propagation::checkers::Consistency;
use crate::propagation::checkers::ConsistencyChecker;
use crate::variables::DomainId;

#[derive(Clone, Debug)]
pub struct WeakConsistencyChecker<C> {
    witness_generator: C,
    consistency: Consistency,
}

impl<C: InferenceChecker<Predicate>> WeakConsistencyChecker<C> {
    pub fn new(witness_generator: C, consistency: Consistency) -> Self {
        WeakConsistencyChecker {
            witness_generator,
            consistency,
        }
    }

    fn verify_domain_consistency(
        &self,
        domains: Domains<'_>,
        premises: &[Predicate],
        domain_id: DomainId,
    ) -> bool {
        domains.iterate_domain(&domain_id).all(|value| {
            let consequent = Some(predicate![domain_id != value]);

            let state =
                VariableState::prepare_for_conflict_check(premises.iter().copied(), consequent)
                    .expect("domain is not inconsistent");

            !self
                .witness_generator
                .check(state, premises, consequent.as_ref())
        })
    }

    fn verify_bounds_consistency(
        &self,
        domains: Domains<'_>,
        premises: &[Predicate],
        domain_id: DomainId,
    ) -> bool {
        let lower_bound = domains.lower_bound(&domain_id);
        let upper_bound = domains.upper_bound(&domain_id);

        let lower_bound_supported =
            self.is_bound_supported(premises, predicate![domain_id >= lower_bound + 1]);

        let upper_bound_supported =
            self.is_bound_supported(premises, predicate![domain_id <= upper_bound - 1]);

        lower_bound_supported && upper_bound_supported
    }

    fn is_bound_supported(&self, premises: &[Predicate], consequent: Predicate) -> bool {
        let state =
            VariableState::prepare_for_conflict_check(premises.iter().copied(), Some(consequent))
                .expect("domain is not inconsistent");

        !self
            .witness_generator
            .check(state, premises, Some(&consequent))
    }
}

impl<C> ConsistencyChecker for WeakConsistencyChecker<C>
where
    C: InferenceChecker<Predicate> + Clone,
{
    fn check_consistency(&self, mut domains: Domains<'_>, scope: &[DomainId]) -> bool {
        // Get a description of the entire domain as the premise of a fact.
        let premises = scope
            .iter()
            .copied()
            .flat_map(|domain_id| {
                let lower_bound = domains.lower_bound(&domain_id);
                let upper_bound = domains.upper_bound(&domain_id);

                domains
                    .get_holes(&domain_id)
                    .map(|hole| predicate![domain_id != hole])
                    .chain(conjunction!(
                        [domain_id >= lower_bound] & [domain_id <= upper_bound]
                    ))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        scope
            .iter()
            .copied()
            .all(|domain_id| match self.consistency {
                Consistency::Domain => {
                    self.verify_domain_consistency(domains.reborrow(), &premises, domain_id)
                }
                Consistency::Bounds => {
                    self.verify_bounds_consistency(domains.reborrow(), &premises, domain_id)
                }
            })
    }
}
