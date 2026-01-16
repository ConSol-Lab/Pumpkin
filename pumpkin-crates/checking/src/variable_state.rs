use std::collections::BTreeSet;
use std::collections::HashMap;
use std::hash::Hash;

use crate::AtomicConstraint;
use crate::Comparison;
use crate::I32Ext;

/// The domains of all variables in the problem.
///
/// Domains are initially unbounded. This is why bounds are represented as [`I32Ext`].
///
/// Domains can be reduced through [`VariableState::apply`]. By default, the domain of every
/// variable is infinite.
#[derive(Clone, Debug)]
pub struct VariableState<Atomic: AtomicConstraint> {
    domains: HashMap<Atomic::Identifier, Domain>,
}

impl<Atomic: AtomicConstraint> Default for VariableState<Atomic> {
    fn default() -> Self {
        Self {
            domains: Default::default(),
        }
    }
}

impl<Ident, Atomic> VariableState<Atomic>
where
    Ident: Hash + Eq,
    Atomic: AtomicConstraint<Identifier = Ident>,
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
    ) -> Option<Self> {
        let mut variable_state = VariableState::default();

        let negated_consequent = consequent.as_ref().map(AtomicConstraint::negate);

        // Apply all the premises and the negation of the consequent to the state.
        if !premises
            .into_iter()
            .chain(negated_consequent)
            .all(|premise| variable_state.apply(&premise))
        {
            return None;
        }

        Some(variable_state)
    }

    /// Get the lower bound of a variable.
    pub fn lower_bound(&self, identifier: &Ident) -> I32Ext {
        self.domains
            .get(identifier)
            .map(|domain| domain.lower_bound)
            .unwrap_or(I32Ext::NegativeInf)
    }

    /// Get the upper bound of a variable.
    pub fn upper_bound(&self, identifier: &Ident) -> I32Ext {
        self.domains
            .get(identifier)
            .map(|domain| domain.upper_bound)
            .unwrap_or(I32Ext::PositiveInf)
    }

    /// Get the holes within the lower and upper bound of the variable expression.
    pub fn holes<'a>(&'a self, identifier: &Ident) -> impl Iterator<Item = i32> + 'a
    where
        Ident: 'a,
    {
        self.domains
            .get(identifier)
            .map(|domain| domain.holes.iter().copied())
            .into_iter()
            .flatten()
    }

    /// Get the fixed value of this variable, if it is fixed.
    pub fn fixed_value(&self, identifier: &Ident) -> Option<i32> {
        let domain = self.domains.get(identifier)?;

        if domain.lower_bound == domain.upper_bound {
            let I32Ext::I32(value) = domain.lower_bound else {
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
    pub fn iter_domain<'a>(&'a self, identifier: &Ident) -> Option<DomainIterator<'a>>
    where
        Ident: 'a,
    {
        let domain = self.domains.get(identifier)?;

        let I32Ext::I32(lower_bound) = domain.lower_bound else {
            // If there is no lower bound, then the domain is unbounded.
            return None;
        };

        // Ensure there is also an upper bound.
        if !matches!(domain.upper_bound, I32Ext::I32(_)) {
            return None;
        }

        Some(DomainIterator {
            domain,
            next_value: lower_bound,
        })
    }

    /// Apply the given [`Atomic`] to the state.
    ///
    /// Returns true if the state remains consistent, or false if the atomic cannot be true in
    /// conjunction with previously applied atomics.
    pub fn apply(&mut self, atomic: &Atomic) -> bool {
        let identifier = atomic.identifier();
        let domain = self.domains.entry(identifier).or_insert(Domain::new());

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
struct Domain {
    lower_bound: I32Ext,
    upper_bound: I32Ext,
    holes: BTreeSet<i32>,
}

impl Domain {
    fn new() -> Domain {
        Domain {
            lower_bound: I32Ext::NegativeInf,
            upper_bound: I32Ext::PositiveInf,
            holes: BTreeSet::default(),
        }
    }

    /// Tighten the lower bound and remove any holes that are no longer strictly larger than the
    /// lower bound.
    fn tighten_lower_bound(&mut self, bound: i32) {
        if self.lower_bound >= bound {
            return;
        }

        self.lower_bound = I32Ext::I32(bound);
        self.holes = self.holes.split_off(&bound);

        // Take care of the condition where the new bound is already a hole in the domain.
        if self.holes.contains(&bound) {
            self.tighten_lower_bound(bound + 1);
        }
    }

    /// Tighten the upper bound and remove any holes that are no longer strictly smaller than the
    /// upper bound.
    fn tighten_upper_bound(&mut self, bound: i32) {
        if self.upper_bound <= bound {
            return;
        }

        self.upper_bound = I32Ext::I32(bound);

        // Note the '+ 1' to keep the elements <= the upper bound instead of <
        // the upper bound.
        let _ = self.holes.split_off(&(bound + 1));

        // Take care of the condition where the new bound is already a hole in the domain.
        if self.holes.contains(&bound) {
            self.tighten_upper_bound(bound - 1);
        }
    }

    /// Returns true if the domain contains at least one value.
    fn is_consistent(&self) -> bool {
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

        let I32Ext::I32(upper_bound) = domain.upper_bound else {
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

    #[test]
    fn domain_iterator_unbounded() {
        let state = VariableState::default();
        let iterator = state.iter_domain(&fzn_rs::VariableExpr::Identifier(Rc::from("x1")));

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_unbounded_lower_bound() {
        let mut state = VariableState::default();

        let variable_name = Rc::from("x1");
        let variable = fzn_rs::VariableExpr::Identifier(Rc::clone(&variable_name));

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: Comparison::LessEqual,
            value: 5,
        });

        let iterator = state.iter_domain(&variable);

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_unbounded_upper_bound() {
        let mut state = VariableState::default();

        let variable_name = Rc::from("x1");
        let variable = fzn_rs::VariableExpr::Identifier(Rc::clone(&variable_name));

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let iterator = state.iter_domain(&variable);

        assert!(iterator.is_none());
    }

    #[test]
    fn domain_iterator_bounded_no_holes() {
        let mut state = VariableState::default();

        let variable_name = Rc::from("x1");
        let variable = fzn_rs::VariableExpr::Identifier(Rc::clone(&variable_name));

        let _ = state.apply(&IntAtomic {
            name: Rc::clone(&variable_name),
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: Comparison::LessEqual,
            value: 10,
        });

        let values = state
            .iter_domain(&variable)
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![5, 6, 7, 8, 9, 10]);
    }

    #[test]
    fn domain_iterator_bounded_with_holes() {
        let mut state = VariableState::default();

        let variable_name = Rc::from("x1");
        let variable = fzn_rs::VariableExpr::Identifier(Rc::clone(&variable_name));

        let _ = state.apply(&IntAtomic {
            name: Rc::clone(&variable_name),
            comparison: Comparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&IntAtomic {
            name: Rc::clone(&variable_name),
            comparison: Comparison::NotEqual,
            value: 7,
        });

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: Comparison::LessEqual,
            value: 10,
        });

        let values = state
            .iter_domain(&variable)
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![5, 6, 8, 9, 10]);
    }
}
