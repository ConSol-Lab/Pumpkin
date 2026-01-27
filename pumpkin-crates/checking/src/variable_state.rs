use std::collections::BTreeMap;
use std::collections::BTreeSet;

use crate::AtomicConstraint;
use crate::Comparison;
#[cfg(doc)]
use crate::InferenceChecker;
use crate::IntExt;

/// The domains of all variables in the problem.
///
/// Domains are initially unbounded. This is why bounds are represented as [`IntExt`].
///
/// Domains can be reduced through [`VariableState::apply`]. By default, the domain of every
/// variable is infinite.
#[derive(Clone, Debug)]
pub struct VariableState<Atomic: AtomicConstraint> {
    domains: BTreeMap<Atomic::Identifier, Domain>,
}

impl<Atomic: AtomicConstraint> Default for VariableState<Atomic> {
    fn default() -> Self {
        Self {
            domains: Default::default(),
        }
    }
}

impl<Atomic> VariableState<Atomic>
where
    Atomic: AtomicConstraint,
{
    /// Create a variable state that applies all the premises and, if present, the negation of the
    /// consequent.
    ///
    /// If `premises /\ !consequent` contain mutually exclusive atomic constraints (e.g., `[x >=
    /// 5]` and `[x <= 2]`) then `None` is returned.
    ///
    /// An [`InferenceChecker`] will receive a [`VariableState`] that conforms to this description.
    pub fn prepare_for_conflict_check(
        premises: impl IntoIterator<Item = Atomic>,
        consequent: Option<Atomic>,
    ) -> Result<Self, Atomic::Identifier> {
        let mut variable_state = VariableState::default();

        let negated_consequent = consequent.as_ref().map(AtomicConstraint::negate);

        // Apply all the premises and the negation of the consequent to the state.
        if let Some(premise) = premises
            .into_iter()
            .chain(negated_consequent)
            .find(|premise| !variable_state.apply(premise))
        {
            return Err(premise.identifier());
        }

        Ok(variable_state)
    }

    /// The domains for which at least one atomic is applied.
    pub fn domains<'this>(&'this self) -> impl Iterator<Item = &'this Atomic::Identifier> + 'this
    where
        Atomic::Identifier: 'this,
    {
        self.domains.keys()
    }

    /// Get the lower bound of a variable.
    pub fn lower_bound(&self, identifier: &Atomic::Identifier) -> IntExt {
        self.domains
            .get(identifier)
            .map(|domain| domain.lower_bound)
            .unwrap_or(IntExt::NegativeInf)
    }

    /// Get the upper bound of a variable.
    pub fn upper_bound(&self, identifier: &Atomic::Identifier) -> IntExt {
        self.domains
            .get(identifier)
            .map(|domain| domain.upper_bound)
            .unwrap_or(IntExt::PositiveInf)
    }

    /// Tests whether the given value is in the domain of the variable.
    pub fn contains(&self, identifier: &Atomic::Identifier, value: i32) -> bool {
        self.domains
            .get(identifier)
            .map(|domain| {
                value >= domain.lower_bound
                    && value <= domain.upper_bound
                    && !domain.holes.contains(&value)
            })
            .unwrap_or(true)
    }

    /// Get the holes within the lower and upper bound of the variable expression.
    pub fn holes<'a>(&'a self, identifier: &Atomic::Identifier) -> impl Iterator<Item = i32> + 'a
    where
        Atomic::Identifier: 'a,
    {
        self.domains
            .get(identifier)
            .map(|domain| domain.holes.iter().copied())
            .into_iter()
            .flatten()
    }

    /// Get the fixed value of this variable, if it is fixed.
    pub fn fixed_value(&self, identifier: &Atomic::Identifier) -> Option<i32> {
        let domain = self.domains.get(identifier)?;

        if domain.lower_bound == domain.upper_bound {
            let IntExt::Int(value) = domain.lower_bound else {
                panic!(
                    "lower can only equal upper if they are integers, otherwise the sign of infinity makes them different"
                );
            };

            Some(value)
        } else {
            None
        }
    }

    /// Obtain an iterator over the domain of the variable.
    ///
    /// If the domain is unbounded, then `None` is returned.
    pub fn iter_domain<'a>(&'a self, identifier: &Atomic::Identifier) -> Option<DomainIterator<'a>>
    where
        Atomic::Identifier: 'a,
    {
        let domain = self.domains.get(identifier)?;

        let IntExt::Int(lower_bound) = domain.lower_bound else {
            // If there is no lower bound, then the domain is unbounded.
            return None;
        };

        // Ensure there is also an upper bound.
        if !matches!(domain.upper_bound, IntExt::Int(_)) {
            return None;
        }

        Some(DomainIterator {
            domain,
            next_value: lower_bound,
        })
    }

    /// Apply the given `Atomic` to the state.
    ///
    /// Returns true if the state remains consistent, or false if the atomic cannot be true in
    /// conjunction with previously applied atomics.
    pub fn apply(&mut self, atomic: &Atomic) -> bool {
        let identifier = atomic.identifier();
        let domain = self
            .domains
            .entry(identifier)
            .or_insert(Domain::all_integers());

        match atomic.comparison() {
            Comparison::GreaterEqual => {
                domain.tighten_lower_bound(atomic.value());
            }

            Comparison::LessEqual => {
                domain.tighten_upper_bound(atomic.value());
            }

            Comparison::Equal => {
                domain.tighten_lower_bound(atomic.value());
                domain.tighten_upper_bound(atomic.value());
            }

            Comparison::NotEqual => {
                if domain.lower_bound == atomic.value() {
                    domain.tighten_lower_bound(atomic.value() + 1);
                }

                if domain.upper_bound == atomic.value() {
                    domain.tighten_upper_bound(atomic.value() - 1);
                }

                if domain.lower_bound < atomic.value() && domain.upper_bound > atomic.value() {
                    let _ = domain.holes.insert(atomic.value());
                }
            }
        }

        domain.is_consistent()
    }

    /// Is the given atomic true in the current state.
    pub fn is_true(&self, atomic: &Atomic) -> bool {
        let Some(domain) = self.domains.get(&atomic.identifier()) else {
            return false;
        };

        match atomic.comparison() {
            Comparison::GreaterEqual => domain.lower_bound >= atomic.value(),

            Comparison::LessEqual => domain.upper_bound <= atomic.value(),

            Comparison::Equal => {
                domain.lower_bound >= atomic.value() && domain.upper_bound <= atomic.value()
            }

            Comparison::NotEqual => {
                if domain.lower_bound >= atomic.value() {
                    return true;
                }

                if domain.upper_bound <= atomic.value() {
                    return true;
                }

                if domain.holes.contains(&atomic.value()) {
                    return true;
                }

                false
            }
        }
    }
}

/// A domain inside the variable state.
#[derive(Clone, Debug)]
pub struct Domain {
    lower_bound: IntExt,
    upper_bound: IntExt,
    holes: BTreeSet<i32>,
}

impl Domain {
    /// Create a domain that contains all integers.
    pub fn all_integers() -> Domain {
        Domain {
            lower_bound: IntExt::NegativeInf,
            upper_bound: IntExt::PositiveInf,
            holes: BTreeSet::default(),
        }
    }

    /// Create an empty/inconsistent domain.
    pub fn empty() -> Domain {
        Domain {
            lower_bound: IntExt::PositiveInf,
            upper_bound: IntExt::NegativeInf,
            holes: BTreeSet::default(),
        }
    }

    /// Construct a new domain.
    pub fn new(lower_bound: IntExt, upper_bound: IntExt, holes: BTreeSet<i32>) -> Self {
        let mut domain = Domain::all_integers();
        domain.holes = holes;

        if let IntExt::Int(bound) = lower_bound {
            domain.tighten_lower_bound(bound);
        }

        if let IntExt::Int(bound) = upper_bound {
            domain.tighten_upper_bound(bound);
        }

        domain
    }

    /// Get the holes in the domain.
    pub fn holes(&self) -> &BTreeSet<i32> {
        &self.holes
    }

    /// Get the lower bound of the domain.
    pub fn lower_bound(&self) -> IntExt {
        self.lower_bound
    }

    /// Get the upper bound of the domain.
    pub fn upper_bound(&self) -> IntExt {
        self.upper_bound
    }

    /// Tighten the lower bound and remove any holes that are no longer strictly larger than the
    /// lower bound.
    fn tighten_lower_bound(&mut self, bound: i32) {
        if self.lower_bound >= bound && !self.holes.contains(&bound) {
            return;
        }

        self.lower_bound = IntExt::Int(bound);
        self.holes = self.holes.split_off(&bound);

        // Take care of the condition where the new bound is already a hole in the domain.
        if self.holes.contains(&bound) {
            self.tighten_lower_bound(bound + 1);
        }
    }

    /// Tighten the upper bound and remove any holes that are no longer strictly smaller than the
    /// upper bound.
    fn tighten_upper_bound(&mut self, bound: i32) {
        if self.upper_bound <= bound && !self.holes.contains(&bound) {
            return;
        }

        self.upper_bound = IntExt::Int(bound);

        // Note the '+ 1' to keep the elements <= the upper bound instead of <
        // the upper bound.
        let _ = self.holes.split_off(&(bound + 1));

        // Take care of the condition where the new bound is already a hole in the domain.
        if self.holes.contains(&bound) {
            self.tighten_upper_bound(bound - 1);
        }
    }

    /// Returns true if the domain contains at least one value.
    pub fn is_consistent(&self) -> bool {
        // No need to check holes, as the invariant of `Domain` specifies the bounds are as tight
        // as possible, taking holes into account.

        self.lower_bound <= self.upper_bound
    }
}

/// An iterator over the values in the domain of a variable.
#[derive(Debug)]
pub struct DomainIterator<'a> {
    domain: &'a Domain,
    next_value: i32,
}

impl Iterator for DomainIterator<'_> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        let DomainIterator { domain, next_value } = self;

        let IntExt::Int(upper_bound) = domain.upper_bound else {
            panic!("Only finite domains can be iterated.")
        };

        loop {
            // We have completed iterating the domain.
            if *next_value > upper_bound {
                return None;
            }

            let value = *next_value;
            *next_value += 1;

            // The next value is not part of the domain.
            if domain.holes.contains(&value) {
                continue;
            }

            // Here the value is part of the domain, so we yield it.
            return Some(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TestAtomic;

    #[test]
    fn domain_iterator_unbounded() {
        let state = VariableState::<TestAtomic>::default();
        let iterator = state.iter_domain(&"x1");

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_unbounded_lower_bound() {
        let mut state = VariableState::default();

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 5,
        });

        let iterator = state.iter_domain(&"x1");

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_unbounded_upper_bound() {
        let mut state = VariableState::default();

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let iterator = state.iter_domain(&"x1");

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_bounded_no_holes() {
        let mut state = VariableState::default();

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 10,
        });

        let values = state
            .iter_domain(&"x1")
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![5, 6, 7, 8, 9, 10]);
    }

    #[test]
    fn domain_iterator_bounded_with_holes() {
        let mut state = VariableState::default();

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::NotEqual,
            value: 7,
        });

        let _ = state.apply(&TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 10,
        });

        let values = state
            .iter_domain(&"x1")
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![5, 6, 8, 9, 10]);
    }
}
