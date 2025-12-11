use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Add;
use std::rc::Rc;

use crate::inferences::Fact;
use crate::model::Atomic;

/// The domains of all variables in the problem.
///
/// Domains can be reduced through [`VariableState::apply`]. By default, the domain of every
/// variable is infinite.
#[derive(Clone, Debug, Default)]
pub(crate) struct VariableState {
    domains: BTreeMap<Rc<str>, Domain>,
}

impl VariableState {
    /// Create a variable state that applies all the premises and, if present, the negation of the
    /// consequent.
    ///
    /// Used by inference checkers if they want to identify a conflict by negating the consequent.
    pub(crate) fn prepare_for_conflict_check(fact: &Fact) -> Option<Self> {
        let mut variable_state = VariableState::default();

        let negated_consequent = fact
            .consequent
            .as_ref()
            .map(|consequent| !consequent.clone());

        // Apply all the premises and the negation of the consequent to the state.
        if !fact
            .premises
            .iter()
            .chain(negated_consequent.as_ref())
            .all(|premise| variable_state.apply(premise))
        {
            return None;
        }

        Some(variable_state)
    }

    /// Get the lower bound of a variable.
    pub(crate) fn lower_bound(&self, variable: &fzn_rs::VariableExpr<i32>) -> I32Ext {
        let name = match variable {
            fzn_rs::VariableExpr::Identifier(name) => name,
            fzn_rs::VariableExpr::Constant(value) => return I32Ext::I32(*value),
        };

        self.domains
            .get(name)
            .map(|domain| domain.lower_bound)
            .unwrap_or(I32Ext::NegativeInf)
    }

    /// Get the upper bound of a variable.
    pub(crate) fn upper_bound(&self, variable: &fzn_rs::VariableExpr<i32>) -> I32Ext {
        let name = match variable {
            fzn_rs::VariableExpr::Identifier(name) => name,
            fzn_rs::VariableExpr::Constant(value) => return I32Ext::I32(*value),
        };

        self.domains
            .get(name)
            .map(|domain| domain.upper_bound)
            .unwrap_or(I32Ext::PositiveInf)
    }

    /// Get the holes within the lower and upper bound of the variable expression.
    pub(crate) fn holes(
        &self,
        variable: &fzn_rs::VariableExpr<i32>,
    ) -> impl Iterator<Item = i32> + '_ {
        #[allow(trivial_casts, reason = "without it we get a type error")]
        let name = match variable {
            fzn_rs::VariableExpr::Identifier(name) => name,
            fzn_rs::VariableExpr::Constant(_) => {
                return Box::new(std::iter::empty()) as Box<dyn Iterator<Item = i32>>;
            }
        };

        #[allow(trivial_casts, reason = "without it we get a type error")]
        self.domains
            .get(name)
            .map(|domain| Box::new(domain.holes.iter().copied()) as Box<dyn Iterator<Item = i32>>)
            .unwrap_or_else(|| Box::new(std::iter::empty()))
    }

    /// Get the fixed value of this variable, if it is fixed.
    pub(crate) fn fixed_value(&self, variable: &fzn_rs::VariableExpr<i32>) -> Option<i32> {
        let name = match variable {
            fzn_rs::VariableExpr::Identifier(name) => name,
            fzn_rs::VariableExpr::Constant(value) => return Some(*value),
        };

        let domain = self.domains.get(name)?;

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
    pub(crate) fn iter_domain(
        &self,
        variable: &fzn_rs::VariableExpr<i32>,
    ) -> Option<DomainIterator<'_>> {
        match variable {
            fzn_rs::VariableExpr::Identifier(name) => {
                let domain = self.domains.get(name)?;

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

            fzn_rs::VariableExpr::Constant(value) => {
                Some(DomainIterator(DomainIteratorImpl::Constant {
                    value: *value,
                    finished: false,
                }))
            }
        }
    }

    /// Apply the given [`Atomic`] to the state.
    ///
    /// Returns true if the state remains consistent, or false if the atomic cannot be true in
    /// conjunction with previously applied atomics.
    pub(crate) fn apply(&mut self, atomic: &Atomic) -> bool {
        let domain = self
            .domains
            .entry(Rc::clone(&atomic.name))
            .or_insert(Domain::new());

        match atomic.comparison {
            drcp_format::IntComparison::GreaterEqual => {
                domain.tighten_lower_bound(atomic.value);
            }

            drcp_format::IntComparison::LessEqual => {
                domain.tighten_upper_bound(atomic.value);
            }

            drcp_format::IntComparison::Equal => {
                domain.tighten_lower_bound(atomic.value);
                domain.tighten_upper_bound(atomic.value);
            }

            drcp_format::IntComparison::NotEqual => {
                if domain.lower_bound == atomic.value {
                    domain.tighten_lower_bound(atomic.value + 1);
                }

                if domain.upper_bound == atomic.value {
                    domain.tighten_upper_bound(atomic.value - 1);
                }

                if domain.lower_bound < atomic.value && domain.upper_bound > atomic.value {
                    let _ = domain.holes.insert(atomic.value);
                }
            }
        }

        domain.is_consistent()
    }

    /// Is the given atomic true in the current state.
    pub(crate) fn is_true(&self, atomic: &Atomic) -> bool {
        let Some(domain) = self.domains.get(&atomic.name) else {
            return false;
        };

        match atomic.comparison {
            drcp_format::IntComparison::GreaterEqual => domain.lower_bound >= atomic.value,

            drcp_format::IntComparison::LessEqual => domain.upper_bound <= atomic.value,

            drcp_format::IntComparison::Equal => {
                domain.lower_bound >= atomic.value && domain.upper_bound <= atomic.value
            }

            drcp_format::IntComparison::NotEqual => {
                if domain.lower_bound >= atomic.value {
                    return true;
                }

                if domain.upper_bound <= atomic.value {
                    return true;
                }

                if domain.holes.contains(&atomic.value) {
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
pub(crate) enum I32Ext {
    I32(i32),
    NegativeInf,
    PositiveInf,
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

/// An iterator over the values in the domain of a variable.
pub(crate) struct DomainIterator<'a>(DomainIteratorImpl<'a>);

enum DomainIteratorImpl<'a> {
    Constant { value: i32, finished: bool },
    Domain { domain: &'a Domain, next_value: i32 },
}

impl Iterator for DomainIterator<'_> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            // Iterating over a contant means only yielding the value once, and then
            // never again.
            DomainIteratorImpl::Constant {
                value,
                ref mut finished,
            } => {
                if *finished {
                    None
                } else {
                    *finished = true;
                    Some(value)
                }
            }

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
            comparison: IntComparison::LessEqual,
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
            comparison: IntComparison::GreaterEqual,
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
            comparison: IntComparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: IntComparison::LessEqual,
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
            comparison: IntComparison::GreaterEqual,
            value: 5,
        });

        let _ = state.apply(&IntAtomic {
            name: Rc::clone(&variable_name),
            comparison: IntComparison::NotEqual,
            value: 7,
        });

        let _ = state.apply(&IntAtomic {
            name: variable_name,
            comparison: IntComparison::LessEqual,
            value: 10,
        });

        let values = state
            .iter_domain(&variable)
            .expect("the domain is bounded")
            .collect::<Vec<_>>();

        assert_eq!(values, vec![5, 6, 8, 9, 10]);
    }
}
