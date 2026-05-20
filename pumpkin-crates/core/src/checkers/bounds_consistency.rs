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

#[derive(Clone, Debug)]
pub struct BoundsConsistencyChecker<Supports: SupportGenerator> {
    supports: Supports,
    supported_values: HashSet<(DomainId, i32)>,

    support: Support<Supports::Value>,
}

impl<Supports: SupportGenerator> BoundsConsistencyChecker<Supports> {
    pub fn new(supports: Supports) -> Self {
        BoundsConsistencyChecker {
            supports,
            supported_values: HashSet::default(),
            support: Support::default(),
        }
    }
}

impl<Supports: SupportGenerator> ConsistencyChecker for BoundsConsistencyChecker<Supports> {
    fn check_consistency(&mut self, scope: &Scope, mut domains: Domains<'_>) -> bool {
        self.supported_values.clear();

        for (local_id, domain) in scope.domains() {
            let values_to_support = [domains.lower_bound(&domain), domains.upper_bound(&domain)];

            for value in values_to_support {
                if self.supported_values.contains(&(domain, value)) {
                    continue;
                }

                self.supports.support(
                    &mut self.support,
                    local_id,
                    UnsupportedValue(value),
                    domains.reborrow(),
                );

                if !self.process_support(domains.reborrow()) {
                    return false;
                }
            }
        }

        true
    }
}

impl<Supports: SupportGenerator> BoundsConsistencyChecker<Supports> {
    fn process_support(&mut self, mut domains: Domains<'_>) -> bool {
        // TODO: Check that the support is a solution.
        if !self.supports.is_solution(&self.support) {
            log::error!("Support is not a solution");
            return false;
        }

        for (domain, value) in self.support.drain() {
            if !value.is_in(domain, domains.reborrow()) {
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
