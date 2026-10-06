use std::collections::BTreeSet;

use fnv::FnvHashMap;

use crate::AtomicConstraint;
use crate::Comparison;
#[cfg(doc)]
use crate::ConflictChecker;
use crate::DomainView;
use crate::IntExt;

/// The domains of all variables in the problem.
///
/// Domains are initially unbounded. This is why bounds are represented as [`IntExt`].
///
/// Domains can be reduced through [`VariableState::apply`]. By default, the domain of every
/// variable is infinite.
#[derive(Clone, Debug)]
pub struct VariableState<Atomic: AtomicConstraint> {
    domains: FnvHashMap<Atomic::Identifier, Domain>,
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
    /// An [`ConflictChecker`] will receive a [`VariableState`] that conforms to this description.
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
                    match atomic.value().checked_add(1) {
                        Some(bound) => domain.tighten_lower_bound(bound),
                        None => *domain = Domain::empty(),
                    }
                }

                if domain.upper_bound == atomic.value() {
                    match atomic.value().checked_sub(1) {
                        Some(bound) => domain.tighten_upper_bound(bound),
                        None => *domain = Domain::empty(),
                    }
                }

                if domain.lower_bound < atomic.value() && domain.upper_bound > atomic.value() {
                    let _ = domain.holes.insert(atomic.value());
                }
            }
        }

        domain.is_consistent()
    }
}

impl<Atomic: AtomicConstraint> DomainView<Atomic> for VariableState<Atomic> {
    fn lower_bound(&self, identifier: &Atomic::Identifier) -> IntExt {
        self.domains
            .get(identifier)
            .map(|domain| domain.lower_bound)
            .unwrap_or(IntExt::NegativeInf)
    }

    fn upper_bound(&self, identifier: &Atomic::Identifier) -> IntExt {
        self.domains
            .get(identifier)
            .map(|domain| domain.upper_bound)
            .unwrap_or(IntExt::PositiveInf)
    }

    fn contains(&self, identifier: &Atomic::Identifier, value: i32) -> bool {
        self.domains
            .get(identifier)
            .map(|domain| {
                value >= domain.lower_bound
                    && value <= domain.upper_bound
                    && !domain.holes.contains(&value)
            })
            .unwrap_or(true)
    }

    fn holes<'a>(&'a self, identifier: &Atomic::Identifier) -> Box<dyn Iterator<Item = i32> + 'a> {
        Box::new(
            self.domains
                .get(identifier)
                .into_iter()
                .flat_map(|domain| domain.holes.iter().copied()),
        )
    }

    fn fixed_value(&self, identifier: &Atomic::Identifier) -> Option<i32> {
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

    fn iter_domain<'a>(
        &'a self,
        identifier: &Atomic::Identifier,
    ) -> Option<Box<dyn Iterator<Item = i32> + 'a>>
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

        Some(Box::new(DomainIterator {
            domain,
            next_value: i64::from(lower_bound),
        }))
    }

    fn is_true(&self, atomic: &Atomic) -> bool {
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
                if domain.lower_bound > atomic.value() {
                    return true;
                }

                if domain.upper_bound < atomic.value() {
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

        // Take care of the condition where the new bound is already a hole in the domain. No value
        // lies above i32::MAX, so then the domain is empty.
        if self.holes.contains(&bound) {
            match bound.checked_add(1) {
                Some(next) => self.tighten_lower_bound(next),
                None => *self = Domain::empty(),
            }
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
        // the upper bound. No hole lies above i32::MAX.
        if let Some(above) = bound.checked_add(1) {
            let _ = self.holes.split_off(&above);
        }

        // Take care of the condition where the new bound is already a hole in the domain. No value
        // lies below i32::MIN, so then the domain is empty.
        if self.holes.contains(&bound) {
            match bound.checked_sub(1) {
                Some(next) => self.tighten_upper_bound(next),
                None => *self = Domain::empty(),
            }
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
    next_value: i64,
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
            if *next_value > i64::from(upper_bound) {
                return None;
            }

            let value = i32::try_from(*next_value).expect("the value is at most the upper bound");
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

    #[test]
    fn upper_bound_at_i32_max_keeps_the_holes() {
        let mut state = VariableState::default();
        let x = |comparison, value| TestAtomic {
            name: "x1",
            comparison,
            value,
        };

        assert!(state.apply(&x(Comparison::NotEqual, 5)));
        assert!(state.apply(&x(Comparison::LessEqual, i32::MAX)));

        assert!(state.is_true(&x(Comparison::NotEqual, 5)));
    }

    #[test]
    fn holes_at_the_extremes_of_i32_move_the_bounds() {
        let mut state = VariableState::default();
        let x = |comparison, value| TestAtomic {
            name: "x1",
            comparison,
            value,
        };

        assert!(state.apply(&x(Comparison::NotEqual, i32::MIN)));
        assert!(state.apply(&x(Comparison::NotEqual, i32::MAX)));
        assert!(state.apply(&x(Comparison::GreaterEqual, i32::MIN)));
        assert!(state.apply(&x(Comparison::LessEqual, i32::MAX)));

        assert!(state.is_true(&x(Comparison::GreaterEqual, i32::MIN + 1)));
        assert!(state.is_true(&x(Comparison::LessEqual, i32::MAX - 1)));
    }

    #[test]
    fn removing_the_last_value_at_the_extremes_of_i32_empties_the_domain() {
        let x = |comparison, value| TestAtomic {
            name: "x1",
            comparison,
            value,
        };

        for value in [i32::MIN, i32::MAX] {
            let mut removed_last = VariableState::default();
            assert!(removed_last.apply(&x(Comparison::Equal, value)));
            assert!(!removed_last.apply(&x(Comparison::NotEqual, value)));

            let mut bound_on_hole = VariableState::default();
            assert!(bound_on_hole.apply(&x(Comparison::NotEqual, value)));
            let comparison = if value == i32::MAX {
                Comparison::GreaterEqual
            } else {
                Comparison::LessEqual
            };
            assert!(!bound_on_hole.apply(&x(comparison, value)));
        }
    }

    #[test]
    fn domain_iterator_ends_at_i32_max() {
        let mut state = VariableState::default();
        let x = |comparison, value| TestAtomic {
            name: "x1",
            comparison,
            value,
        };

        let _ = state.apply(&x(Comparison::GreaterEqual, i32::MAX - 2));
        let _ = state.apply(&x(Comparison::LessEqual, i32::MAX));
        let _ = state.apply(&x(Comparison::NotEqual, i32::MAX - 1));

        let values = state
            .iter_domain(&"x1")
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![i32::MAX - 2, i32::MAX]);
    }

    #[test]
    fn not_equals_is_correct() {
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

        assert!(!state.is_true(&TestAtomic {
            name: "x1",
            comparison: Comparison::NotEqual,
            value: 10
        }));
        assert!(!state.is_true(&TestAtomic {
            name: "x1",
            comparison: Comparison::NotEqual,
            value: 5
        }));

        assert!(state.is_true(&TestAtomic {
            name: "x1",
            comparison: Comparison::NotEqual,
            value: 4
        }));
        assert!(state.is_true(&TestAtomic {
            name: "x1",
            comparison: Comparison::NotEqual,
            value: 11
        }));
    }
}
