use super::Scope;
use crate::checkers::RetentionChecker;
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

/// A [`ConsistencyChecker`] that enforces a strong consistency property.
///
/// The level of consistency is configured via [`StrongConsistency`].
#[derive(Clone, Debug)]
pub struct StrongRetentionChecker<Supports: SupportGenerator> {
    /// The generator of supports.
    supports: Supports,
    /// A cache of domain-value pairs that are supported.
    supported_values: HashSet<(DomainId, i32)>,
    /// The consistency level to test for.
    consistency_level: StrongConsistency,
    /// Re-usable buffer of the current support that is operated on.
    support: Support<Supports::Value>,
}

impl<Supports: SupportGenerator> StrongRetentionChecker<Supports> {
    pub fn new(consistency_level: StrongConsistency, supports: Supports) -> Self {
        StrongRetentionChecker {
            consistency_level,
            supports,
            supported_values: HashSet::default(),
            support: Support::default(),
        }
    }
}

impl<Supports: SupportGenerator> RetentionChecker for StrongRetentionChecker<Supports> {
    fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        // Make sure to clear the cache of supported values. At the beginning, no values are
        // supported.
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
                    // If this domain-value pair is already supported in this check
                    // then there is no need to generate a new support for it.
                    continue;
                }

                // Generate the support for this domain-value pair.
                self.supports.support(
                    &mut self.support,
                    local_id,
                    UnsupportedValue(value),
                    &domains,
                );

                if !self.process_support(&domains) {
                    // The support was incomplete or not a solution. Either way, the
                    // consistency check fails.
                    return false;
                }
            }
        }

        // All required values are successfully supported, so the check passes.
        true
    }
}

impl<Supports: SupportGenerator> StrongRetentionChecker<Supports> {
    /// Tests whether the [`StrongConsistencyChecker::support`] is a valid support.
    ///
    /// Drains the support in the process, so it can be used again by subsequent calls to
    /// [`SupportGenerator::support`].
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
