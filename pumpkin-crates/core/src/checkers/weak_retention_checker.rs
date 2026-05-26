use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use super::Scope;
use crate::checkers::RetentionChecker;
use crate::conjunction;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;

/// The consistency level advertised by the propagator.
#[derive(Clone, Copy, Debug)]
pub enum WeakConsistency {
    Domain,
    Bounds,
}

#[derive(Debug, Clone)]
pub struct WeakRetentionChecker<Inferences: InferenceChecker<Predicate> + Clone> {
    /// The inference checker.
    inference_checker: Inferences,
    /// The consistency level to test for.
    consistency_level: WeakConsistency,
}

impl<Inferences: InferenceChecker<Predicate> + Clone> WeakRetentionChecker<Inferences> {
    pub fn new(consistency_level: WeakConsistency, inference_checker: Inferences) -> Self {
        WeakRetentionChecker {
            inference_checker,
            consistency_level,
        }
    }

    fn bound_not_updatable(&mut self, consequent: Predicate, premises: &[Predicate]) -> bool {
        let state =
            VariableState::prepare_for_conflict_check(premises.iter().copied(), Some(consequent))
                .expect("Domain should not be inconsistent");

        !self
            .inference_checker
            .check(state, premises, Some(&consequent))
    }
}

impl<Inferences: InferenceChecker<Predicate> + Clone> RetentionChecker
    for WeakRetentionChecker<Inferences>
{
    fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        let premises = scope
            .domains()
            .flat_map(|(_, domain_id)| {
                let lower_bound = domains.lower_bound(&domain_id);
                let upper_bound = domains.upper_bound(&domain_id);

                domains
                    .get_holes(&domain_id)
                    .filter(|&hole| lower_bound <= hole && hole <= upper_bound)
                    .map(|hole| predicate![domain_id != hole])
                    .chain(conjunction!(
                        [domain_id >= lower_bound] & [domain_id <= upper_bound]
                    ))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        scope
            .domains()
            .all(|(_, domain_id)| match self.consistency_level {
                WeakConsistency::Domain => domains.iterate_domain(&domain_id).all(|value| {
                    let consequent = Some(predicate!(domain_id != value));
                    let state = VariableState::prepare_for_conflict_check(
                        premises.iter().copied(),
                        consequent,
                    )
                    .expect("Domain should not be inconsistent");

                    !self
                        .inference_checker
                        .check(state, &premises.clone(), consequent.as_ref())
                }),
                WeakConsistency::Bounds => {
                    let lb = domains.lower_bound(&domain_id);
                    let lb_not_updatable =
                        self.bound_not_updatable(predicate![domain_id >= lb + 1], &premises);

                    let ub = domains.upper_bound(&domain_id);
                    let ub_not_updatable =
                        self.bound_not_updatable(predicate![domain_id <= ub - 1], &premises);

                    lb_not_updatable && ub_not_updatable
                }
            })
    }
}
