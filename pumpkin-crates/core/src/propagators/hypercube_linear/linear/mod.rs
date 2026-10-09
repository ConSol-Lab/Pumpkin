use std::num::NonZero;

use crate::containers::HashMap;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::TransformableVariable;

/// The linear inequality part of a hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct LinearInequality {
    terms: Box<[AffineView<DomainId>]>,
    bound: i32,
}

impl LinearInequality {
    /// Create a linear inequality that is trivially false.
    pub fn trivially_false() -> LinearInequality {
        LinearInequality {
            terms: [].into(),
            bound: -1,
        }
    }

    /// Construct a new linear inequality.
    ///
    /// If the terms simplify to 0 and the `bound` is at least 0, then `None` is returned.
    pub fn new(
        terms: impl IntoIterator<Item = (NonZero<i32>, DomainId)>,
        bound: i32,
    ) -> Option<Self> {
        // To merge terms with the same domain, we go through a HashMap mapping the weight to the
        // domain.
        let mut domain_to_weight = HashMap::new();

        for (weight, domain_id) in terms {
            let existing_weight = domain_to_weight.entry(domain_id).or_insert(0);
            *existing_weight += weight.get();
        }

        let terms = domain_to_weight
            .into_iter()
            .filter(|&(_, weight)| weight != 0)
            .map(|(domain, weight)| domain.scaled(weight))
            .collect::<Box<[_]>>();

        if terms.is_empty() && bound >= 0 {
            return None;
        }

        Some(LinearInequality { terms, bound })
    }

    /// Iterate over the terms in the linear inequality.
    pub fn terms(&self) -> impl Iterator<Item = AffineView<DomainId>> + '_ {
        self.terms.iter().copied()
    }

    /// The bound of the linear inequality.
    pub fn bound(&self) -> i32 {
        self.bound
    }

    /// Tests whether the left-hand side simplifies to 0 and the right-hand side is less than 0.
    pub fn is_trivially_false(&self) -> bool {
        self.terms.is_empty() && self.bound < 0
    }

    /// Get the term for the given domain.
    pub fn term_for_domain(&self, domain: DomainId) -> Option<AffineView<DomainId>> {
        self.terms().find(|view| view.inner == domain)
    }
}

#[cfg(test)]
mod tests;
