use std::collections::BTreeSet;

use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::IntExt;
use crate::VariableState;

/// Calculates the union between multiple variable domains.
///
/// Can be used incrementally through [`Union::add`]. Using [`Union::reset`] the union is reset to
/// an empty state.
#[derive(Clone, Debug)]
pub struct Union {
    lower_bound: IntExt,
    upper_bound: IntExt,
    all_holes: BTreeSet<i32>,
}

impl Union {
    /// Create an empty union.
    pub fn empty() -> Self {
        Union {
            lower_bound: IntExt::PositiveInf,
            upper_bound: IntExt::NegativeInf,
            all_holes: BTreeSet::default(),
        }
    }

    /// Get the lower bound of the union of all added domains.
    ///
    /// If the union is empty, this is [`IntExt::PositiveInf`].
    pub fn lower_bound(&self) -> IntExt {
        self.lower_bound
    }

    /// Get the upper bound of the union of all added domains.
    ///
    /// If the union is empty, this is [`IntExt::NegativeInf`].
    pub fn upper_bound(&self) -> IntExt {
        self.upper_bound
    }

    /// Get the holes in the union of all added domains.
    pub fn holes(&self) -> impl Iterator<Item = i32> + '_ {
        self.all_holes.iter().copied()
    }

    /// Test whether `value` is in the union.
    pub fn contains(&self, value: i32) -> bool {
        self.lower_bound <= value && value <= self.upper_bound && !self.all_holes.contains(&value)
    }

    /// If the domain is finite, returns the number of elements.
    pub fn size(&self) -> Option<usize> {
        let IntExt::Int(lower_bound) = self.lower_bound else {
            return None;
        };
        let IntExt::Int(upper_bound) = self.upper_bound else {
            return None;
        };

        let size = upper_bound.abs_diff(lower_bound) as usize + 1 - self.all_holes.len();
        Some(size)
    }

    /// Rest the union to an empty state.
    pub fn reset(&mut self) {
        self.lower_bound = IntExt::PositiveInf;
        self.upper_bound = IntExt::NegativeInf;
        self.all_holes.clear();
    }

    /// Add a variable to the union.
    pub fn add<Atomic: AtomicConstraint>(
        &mut self,
        state: &VariableState<Atomic>,
        variable: &impl CheckerVariable<Atomic>,
    ) {
        // All holes that are already in the union but not in the new domain should be removed.
        self.all_holes
            .retain(|&value| !variable.induced_domain_contains(state, value));

        let other_holes = variable
            .induced_holes(state)
            .filter(|&value| value < self.lower_bound || value > self.upper_bound);

        self.all_holes.extend(other_holes);

        let variable_lb = variable.induced_lower_bound(state);
        let variable_ub = variable.induced_upper_bound(state);

        let additional_holes_below_self_lb = match (self.lower_bound, variable_ub) {
            (IntExt::Int(self_lb), IntExt::Int(variable_ub)) if self_lb > variable_ub => {
                variable_ub + 1..self_lb
            }

            _ => 0..0,
        };

        let additional_holes_above_self_ub = match (self.upper_bound, variable_lb) {
            (IntExt::Int(self_ub), IntExt::Int(variable_lb)) if self_ub < variable_lb => {
                self_ub + 1..variable_lb
            }

            _ => 0..0,
        };

        self.all_holes.extend(additional_holes_below_self_lb);
        self.all_holes.extend(additional_holes_above_self_ub);

        self.lower_bound = self.lower_bound.min(variable_lb);
        self.upper_bound = self.upper_bound.max(variable_ub);
    }

    /// Returns `true` if the union contains no elements.
    pub fn is_consistent(&self) -> bool {
        self.lower_bound <= self.upper_bound
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Comparison::*;
    use crate::TestAtomic;

    #[test]
    fn empty_union_is_inconsistent() {
        let union = Union::empty();
        assert!(!union.is_consistent());
    }

    #[test]
    fn reset_clear_domain() {
        let state = VariableState::prepare_for_conflict_check(
            [TestAtomic {
                name: "x",
                comparison: GreaterEqual,
                value: 4,
            }],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();

        union.add(&state, &"x");
        union.reset();
        assert!(!union.is_consistent());
    }

    #[test]
    fn adding_single_variable_copies_domain() {
        let state = VariableState::prepare_for_conflict_check(
            [TestAtomic {
                name: "x",
                comparison: GreaterEqual,
                value: 4,
            }],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();

        union.add(&state, &"x");
        assert!(union.is_consistent());

        assert_eq!(IntExt::<i32>::Int(4), union.lower_bound());
        assert_eq!(IntExt::<i32>::PositiveInf, union.upper_bound());
        assert!(union.holes().next().is_none());
    }

    #[test]
    fn union_of_non_overlapping_domains() {
        let state = VariableState::prepare_for_conflict_check(
            [
                TestAtomic {
                    name: "x",
                    comparison: GreaterEqual,
                    value: 4,
                },
                TestAtomic {
                    name: "y",
                    comparison: LessEqual,
                    value: 3,
                },
            ],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();

        union.add(&state, &"x");
        union.add(&state, &"y");

        assert_eq!(IntExt::<i32>::NegativeInf, union.lower_bound());
        assert_eq!(IntExt::<i32>::PositiveInf, union.upper_bound());
        assert!(union.holes().next().is_none());
    }

    #[test]
    fn union_of_domain_with_hole_outside_bounds() {
        let state = VariableState::prepare_for_conflict_check(
            [
                TestAtomic {
                    name: "x",
                    comparison: GreaterEqual,
                    value: 4,
                },
                TestAtomic {
                    name: "y",
                    comparison: NotEqual,
                    value: 2,
                },
            ],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();

        union.add(&state, &"x");
        union.add(&state, &"y");

        assert_eq!(IntExt::<i32>::NegativeInf, union.lower_bound());
        assert_eq!(IntExt::<i32>::PositiveInf, union.upper_bound());
        assert_eq!(vec![2], union.holes().collect::<Vec<_>>());
    }

    #[test]
    fn union_with_hole_removed_by_add() {
        let state = VariableState::prepare_for_conflict_check(
            [
                TestAtomic {
                    name: "x",
                    comparison: GreaterEqual,
                    value: 4,
                },
                TestAtomic {
                    name: "x",
                    comparison: NotEqual,
                    value: 5,
                },
                TestAtomic {
                    name: "y",
                    comparison: NotEqual,
                    value: 4,
                },
            ],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();

        union.add(&state, &"x");
        union.add(&state, &"y");

        assert_eq!(IntExt::<i32>::NegativeInf, union.lower_bound());
        assert_eq!(IntExt::<i32>::PositiveInf, union.upper_bound());
        assert!(union.holes().next().is_none());
    }

    #[test]
    fn holes_in_union_and_new_variable_should_be_kept() {
        let state = VariableState::prepare_for_conflict_check(
            [
                TestAtomic {
                    name: "x",
                    comparison: NotEqual,
                    value: 5,
                },
                TestAtomic {
                    name: "y",
                    comparison: NotEqual,
                    value: 5,
                },
            ],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();
        union.add(&state, &"x");
        union.add(&state, &"y");

        assert_eq!(IntExt::<i32>::NegativeInf, union.lower_bound());
        assert_eq!(IntExt::<i32>::PositiveInf, union.upper_bound());
        assert_eq!(vec![5], union.holes().collect::<Vec<_>>());
    }

    #[test]
    fn union_of_fixed_domains_with_gap() {
        let state = VariableState::prepare_for_conflict_check(
            [
                TestAtomic {
                    name: "x46",
                    comparison: Equal,
                    value: 4,
                },
                TestAtomic {
                    name: "x42",
                    comparison: Equal,
                    value: 1,
                },
            ],
            None,
        )
        .expect("not inconsistent");

        let mut union = Union::empty();
        assert_eq!(union.size(), None);

        union.add(&state, &"x46");
        assert_eq!(union.size(), Some(1));

        union.add(&state, &"x42");

        let holes = union.holes().collect::<BTreeSet<_>>();
        assert_eq!(BTreeSet::from([2, 3]), holes);

        assert_eq!(union.lower_bound(), IntExt::Int(1));
        assert_eq!(union.upper_bound(), IntExt::Int(4));
    }
}
