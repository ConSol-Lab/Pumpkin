use super::Scope;
use crate::checkers::ConsistencyChecker;
use crate::checkers::support::Support;
use crate::checkers::support::SupportGenerator;
use crate::checkers::support::SupportValue;
use crate::checkers::support::UnsupportedValue;
use crate::containers::HashSet;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::variables::DomainId;

/// The consistency level advertised by the propagator.
#[derive(Clone, Copy, Debug)]
pub enum StrongConsistency {
    Domain,
    Bounds,
}

#[derive(Clone, Debug)]
pub struct StrongConsistencyChecker<Supports: SupportGenerator> {
    supports: Supports,
    supported_values: HashSet<(DomainId, i32)>,
    consistency_level: StrongConsistency,

    support: Support<Supports::Value>,
}

impl<Supports: SupportGenerator> StrongConsistencyChecker<Supports> {
    pub fn new(consistency_level: StrongConsistency, supports: Supports) -> Self {
        StrongConsistencyChecker {
            consistency_level,
            supports,
            supported_values: HashSet::default(),
            support: Support::default(),
        }
    }
}

impl<Supports: SupportGenerator> ConsistencyChecker for StrongConsistencyChecker<Supports> {
    fn check_consistency(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        self.supported_values.clear();

        for (local_id, domain) in scope.domains() {
            let values_to_support = match self.consistency_level {
                StrongConsistency::Domain => {
                    itertools::Either::Left(domains.iterate_domain(&domain))
                }
                StrongConsistency::Bounds => itertools::Either::Right(
                    [domains.lower_bound(&domain), domains.upper_bound(&domain)].into_iter(),
                ),
            };

            for value in values_to_support {
                if self.supported_values.contains(&(domain, value)) {
                    continue;
                }

                self.supports.support(
                    &mut self.support,
                    local_id,
                    UnsupportedValue(value),
                    &domains,
                );

                if !self.process_support(&domains) {
                    return false;
                }
            }
        }

        true
    }
}

impl<Supports: SupportGenerator> StrongConsistencyChecker<Supports> {
    fn process_support(&mut self, domains: &Domains<'_>) -> bool {
        if !self.supports.is_solution(&self.support) {
            log::error!("Support is not a solution");
            return false;
        }

        for (domain, value) in self.support.drain() {
            if !value.is_in(domain, domains) {
                log::error!("Support value is not in the domain");
                return false;
            }

            if let Some(int) = value.as_int() {
                let _ = self.supported_values.insert((domain, int));
            }
        }

        true
    }
}
