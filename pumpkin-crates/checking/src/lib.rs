use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Add;
use std::ops::Mul;

use dyn_clone::DynClone;

pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    fn check(&self, state: VariableState<Atomic>) -> bool;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Comparison::GreaterEqual => ">=",
            Comparison::LessEqual => "<=",
            Comparison::Equal => "==",
            Comparison::NotEqual => "!=",
        };

        write!(f, "{s}")
    }
}

/// A variable.
pub trait CheckerVariable<Atomic: AtomicConstraint>: Debug + Clone {
    fn atomic_less_than(&self, value: i32) -> Atomic;
    fn atomic_greater_than(&self, value: i32) -> Atomic;
    fn atomic_equal(&self, value: i32) -> Atomic;
    fn atomic_not_equal(&self, value: i32) -> Atomic;

    /// Get the lower bound of the domain.
    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext;

    /// Get the upper bound of the domain.
    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext;

    /// Get the value the variable is fixed to, if the variable is fixed.
    fn induced_fixed_value(&self, variable_state: &VariableState<Atomic>) -> Option<i32>;

    /// Get the holes in the domain.
    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state;

    /// Iterate the domain of the variable.
    ///
    /// The order of the values is unspecified.
    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state;
}

/// The domains of all variables in the problem.
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

pub trait AtomicConstraint: Sized {
    type Identifier;

    fn identifier(&self) -> Self::Identifier;
    fn comparison(&self) -> Comparison;
    fn value(&self) -> i32;

    fn negate(&self) -> Self;
}

impl<Ident, Atomic> VariableState<Atomic>
where
    Ident: Hash + Eq,
    Atomic: AtomicConstraint<Identifier = Ident>,
{
    /// Create a variable state that applies all the premises and, if present, the negation of the
    /// consequent.
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

        Some(DomainIterator(DomainIteratorImpl::Domain {
            domain,
            next_value: lower_bound,
        }))
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

    fn is_consistent(&self) -> bool {
        // No need to check holes, as the invariant of `Domain` specifies the bounds are as tight
        // as possible, taking holes into account.

        self.lower_bound <= self.upper_bound
    }
}

/// An `i32` or infinity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum I32Ext {
    I32(i32),
    NegativeInf,
    PositiveInf,
}

impl From<i32> for I32Ext {
    fn from(value: i32) -> Self {
        I32Ext::I32(value)
    }
}

impl PartialEq<i32> for I32Ext {
    fn eq(&self, other: &i32) -> bool {
        match self {
            I32Ext::I32(v1) => v1 == other,
            I32Ext::NegativeInf | I32Ext::PositiveInf => false,
        }
    }
}

impl PartialOrd<I32Ext> for I32Ext {
    fn partial_cmp(&self, other: &I32Ext) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for I32Ext {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            I32Ext::I32(v1) => match other {
                I32Ext::I32(v2) => v1.cmp(v2),
                I32Ext::NegativeInf => Ordering::Greater,
                I32Ext::PositiveInf => Ordering::Less,
            },
            I32Ext::NegativeInf => match other {
                I32Ext::I32(_) => Ordering::Less,
                I32Ext::PositiveInf => Ordering::Less,
                I32Ext::NegativeInf => Ordering::Equal,
            },
            I32Ext::PositiveInf => match other {
                I32Ext::I32(_) => Ordering::Greater,
                I32Ext::NegativeInf => Ordering::Greater,
                I32Ext::PositiveInf => Ordering::Greater,
            },
        }
    }
}

impl PartialOrd<i32> for I32Ext {
    fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
        match self {
            I32Ext::I32(v1) => v1.partial_cmp(other),
            I32Ext::NegativeInf => Some(Ordering::Less),
            I32Ext::PositiveInf => Some(Ordering::Greater),
        }
    }
}

impl Add for I32Ext {
    type Output = I32Ext;

    fn add(self, rhs: I32Ext) -> Self::Output {
        match (self, rhs) {
            (I32Ext::I32(lhs), I32Ext::I32(rhs)) => I32Ext::I32(lhs + rhs),
            (I32Ext::NegativeInf, I32Ext::NegativeInf) => I32Ext::NegativeInf,
            (I32Ext::PositiveInf, I32Ext::PositiveInf) => I32Ext::PositiveInf,
            (lhs, rhs) => panic!("the result of {lhs:?} + {rhs:?} is indeterminate"),
        }
    }
}

impl Add<i32> for I32Ext {
    type Output = I32Ext;

    fn add(self, rhs: i32) -> Self::Output {
        match self {
            I32Ext::I32(lhs) => I32Ext::I32(lhs + rhs),
            I32Ext::NegativeInf => I32Ext::NegativeInf,
            I32Ext::PositiveInf => I32Ext::PositiveInf,
        }
    }
}

impl Mul<i32> for I32Ext {
    type Output = I32Ext;

    fn mul(self, rhs: i32) -> Self::Output {
        match self {
            I32Ext::I32(lhs) => I32Ext::I32(lhs * rhs),
            I32Ext::NegativeInf => {
                if rhs >= 0 {
                    I32Ext::NegativeInf
                } else {
                    I32Ext::PositiveInf
                }
            }
            I32Ext::PositiveInf => {
                if rhs >= 0 {
                    I32Ext::PositiveInf
                } else {
                    I32Ext::NegativeInf
                }
            }
        }
    }
}

/// An iterator over the values in the domain of a variable.
#[derive(Debug)]
pub struct DomainIterator<'a>(DomainIteratorImpl<'a>);

#[derive(Debug)]
enum DomainIteratorImpl<'a> {
    Domain { domain: &'a Domain, next_value: i32 },
}

impl Iterator for DomainIterator<'_> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            DomainIteratorImpl::Domain {
                domain,
                ref mut next_value,
            } => {
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
    }
}

#[cfg(test)]
mod tests {
    use drcp_format::IntAtomic;
    use drcp_format::IntComparison;

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
