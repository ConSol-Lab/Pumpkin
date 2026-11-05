use crate::basic_types::PredicateId;
use crate::containers::KeyedVec;
use crate::engine::predicates::predicate::Predicate;
use crate::propagation::PropagationContext;
use crate::variables::DomainId;

/// Marks the end of the list of learned predicates over a domain.
const NO_PREDICATE: u32 = u32::MAX;

/// The learned predicates over a single domain.
#[derive(Clone, Copy, Debug)]
struct DomainEntry {
    /// The index of the first learned predicate over the domain (or [`NO_PREDICATE`]).
    first: u32,
    /// The number of learned predicates over the domain.
    num_predicates: u32,
    /// The number of learned predicates over the domain which are not yet implied by the
    /// candidate.
    num_not_implied: u32,
}

impl Default for DomainEntry {
    fn default() -> Self {
        DomainEntry {
            first: NO_PREDICATE,
            num_predicates: 0,
            num_not_implied: 0,
        }
    }
}

/// Checks whether nogoods are semantically subsumed by a learned nogood, i.e., whether every
/// predicate of the learned nogood is implied by a predicate of the candidate nogood.
///
/// The learned predicates are indexed by their domain, so that each predicate of a candidate is
/// only compared to the learned predicates over the same domain.
#[derive(Clone, Debug, Default)]
pub(crate) struct SemanticSubsumption {
    /// The predicates of the learned nogood.
    learned: Vec<Predicate>,
    /// For each learned predicate, the index of the next learned predicate over the same domain
    /// (or [`NO_PREDICATE`]).
    next_on_domain: Vec<u32>,
    /// For each learned predicate, whether it is implied by a predicate of the candidate.
    is_implied: Vec<bool>,
    /// The distinct domains of the learned predicates.
    learned_domains: Vec<DomainId>,
    /// The learned predicates per domain; only the entries of `learned_domains` are non-empty.
    domains: KeyedVec<DomainId, DomainEntry>,
}

impl SemanticSubsumption {
    /// Sets the learned nogood which candidates are checked against.
    pub(crate) fn set_learned(&mut self, nogood: &[Predicate]) {
        for &domain in &self.learned_domains {
            self.domains[domain] = DomainEntry::default();
        }
        self.learned_domains.clear();
        self.learned.clear();
        self.next_on_domain.clear();

        for (index, &predicate) in nogood.iter().enumerate() {
            let domain = predicate.get_domain();
            self.domains.accomodate(domain, DomainEntry::default());

            let entry = &mut self.domains[domain];
            if entry.first == NO_PREDICATE {
                self.learned_domains.push(domain);
            }
            self.next_on_domain.push(entry.first);
            entry.first = index as u32;
            entry.num_predicates += 1;
            self.learned.push(predicate);
        }
    }

    /// Returns whether every learned predicate is implied by a predicate of `candidate`.
    pub(crate) fn is_subsumed(
        &mut self,
        candidate: &[PredicateId],
        context: &mut PropagationContext,
    ) -> bool {
        // Every predicate of the candidate implies learned predicates over at most one domain.
        let mut num_domains_not_implied = self.learned_domains.len();
        if candidate.len() < num_domains_not_implied {
            return false;
        }

        self.is_implied.clear();
        self.is_implied.resize(self.learned.len(), false);
        for &domain in &self.learned_domains {
            let entry = &mut self.domains[domain];
            entry.num_not_implied = entry.num_predicates;
        }

        for (position, &predicate_id) in candidate.iter().enumerate() {
            let predicate = context.get_predicate(predicate_id);
            if let Some(entry) = self.domains.get_mut(predicate.get_domain()) {
                let mut index = entry.first;
                while index != NO_PREDICATE && entry.num_not_implied > 0 {
                    let learned_index = index as usize;
                    if !self.is_implied[learned_index]
                        && predicate.implies(self.learned[learned_index])
                    {
                        self.is_implied[learned_index] = true;
                        entry.num_not_implied -= 1;
                        if entry.num_not_implied == 0 {
                            num_domains_not_implied -= 1;
                            if num_domains_not_implied == 0 {
                                return true;
                            }
                        }
                    }
                    index = self.next_on_domain[learned_index];
                }
            }

            // The remaining predicates of the candidate cannot cover the remaining domains.
            if candidate.len() - position - 1 < num_domains_not_implied {
                return false;
            }
        }

        false
    }
}
