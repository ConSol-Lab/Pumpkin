use std::iter::Rev;
use std::ops::Deref;
use std::vec::Drain;

use crate::pumpkin_assert_simple;

#[derive(Clone, Debug)]
pub(crate) struct Trail<T> {
    current_decision_level: usize,
    /// At index i is the position where the i-th decision level ends (exclusive) on the trail
    trail_delimiter: Vec<usize>,
    trail: Vec<T>,
}

impl<T> Default for Trail<T> {
    fn default() -> Self {
        Trail {
            current_decision_level: Default::default(),
            trail_delimiter: Default::default(),
            trail: Default::default(),
        }
    }
}

impl<T> Trail<T> {
    pub(crate) fn increase_decision_level(&mut self) {
        self.current_decision_level += 1;
        self.trail_delimiter.push(self.trail.len());
    }

    pub(crate) fn get_decision_level(&self) -> usize {
        self.current_decision_level
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) -> Rev<Drain<T>> {
        pumpkin_assert_simple!(new_decision_level < self.current_decision_level);

        let new_trail_len = self.trail_delimiter[new_decision_level];

        self.current_decision_level = new_decision_level;
        self.trail_delimiter.truncate(new_decision_level);
        self.trail.drain(new_trail_len..).rev()
    }

    pub(crate) fn push(&mut self, elem: T) {
        self.trail.push(elem)
    }

    /// Only used in `crate::engine::cp::reason::ReasonStore` to replace a lazy reason with its
    ///   result.
    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.trail.get_mut(index)
    }
}

impl<T> Deref for Trail<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.trail
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pushed_values_are_observed_through_indexing() {
        let mut trail = Trail::default();

        let expected = [1, 2, 3, 4];
        for &elem in expected.iter() {
            trail.push(elem);
        }

        assert_eq!(&expected, trail.deref());
    }

    #[test]
    fn backtracking_removes_elements_beyond_decision_level() {
        let mut trail = Trail::default();

        trail.increase_decision_level();
        trail.push(1);
        let _ = trail.synchronise(0);

        assert!(trail.is_empty());
    }

    #[test]
    fn backtracking_is_nonchronological() {
        let mut trail = Trail::default();
        trail.push(1);

        trail.increase_decision_level();
        trail.push(2);
        trail.increase_decision_level();
        trail.push(3);
        trail.increase_decision_level();
        trail.push(4);

        let _ = trail.synchronise(1);

        assert_eq!(&[1, 2], trail.deref());
    }

    #[test]
    fn popped_elements_are_given_in_reverse_order_when_backtracking() {
        let mut trail = Trail::default();
        trail.push(1);

        trail.increase_decision_level();
        trail.push(2);
        trail.increase_decision_level();
        trail.push(3);
        trail.increase_decision_level();
        trail.push(4);

        let popped = trail.synchronise(0).collect::<Vec<_>>();
        assert_eq!(vec![4, 3, 2], popped);
    }
}
