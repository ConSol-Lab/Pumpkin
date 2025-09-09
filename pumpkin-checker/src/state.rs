use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::model::Atomic;

#[derive(Clone, Debug, Default)]
pub(crate) struct VariableState {
    domains: BTreeMap<Rc<str>, Domain>,
}

impl VariableState {
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

    /// Apply the given [`Atomic`] to the state.
    ///
    /// Returns true if the state remains consistent, or false if the atomic cannot be true in
    /// conjunction with previously applied atomics.
    pub(crate) fn apply(&mut self, atomic: Atomic) -> bool {
        let domain = self.domains.entry(atomic.name).or_insert(Domain::new());

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
                todo!()
            }
        }

        domain.is_consistent()
    }

    /// Is the given atomic true in the current state.
    pub(crate) fn is_true(&self, atomic: Atomic) -> bool {
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
        // TODO: Update with holes

        self.lower_bound <= self.upper_bound
    }
}

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
        match self {
            I32Ext::I32(v1) => match other {
                I32Ext::I32(v2) => v1.partial_cmp(v2),
                I32Ext::NegativeInf => Some(Ordering::Greater),
                I32Ext::PositiveInf => Some(Ordering::Less),
            },
            I32Ext::NegativeInf => match other {
                I32Ext::I32(_) => Some(Ordering::Less),
                I32Ext::PositiveInf => Some(Ordering::Less),
                I32Ext::NegativeInf => None,
            },
            I32Ext::PositiveInf => match other {
                I32Ext::I32(_) => Some(Ordering::Greater),
                I32Ext::NegativeInf => Some(Ordering::Greater),
                I32Ext::PositiveInf => None,
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
