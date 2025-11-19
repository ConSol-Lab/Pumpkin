use std::iter::Rev;
use std::ops::Deref;
use std::vec::Drain;

use crate::pumpkin_assert_simple;

#[derive(Clone, Debug)]
pub(crate) struct Trail<T> {
    current_checkpoint: usize,
    /// At index i is the position where the i-th decision level ends (exclusive) on the trail
    trail_delimiter: Vec<usize>,
    trail: Vec<T>,
}

// We explicitly implement the Default and not as a macro, because we want to avoid imposing Default
// on the generic type T.
impl<T> Default for Trail<T> {
    fn default() -> Self {
        Trail {
            current_checkpoint: Default::default(),
            trail_delimiter: Default::default(),
            trail: Default::default(),
        }
    }
}

impl<T> Trail<T> {
    pub(crate) fn new_checkpoint(&mut self) {
        self.current_checkpoint += 1;
        self.trail_delimiter.push(self.trail.len());
    }

    pub(crate) fn values_at_checkpoint(&self, checkpoint: usize) -> &[T] {
        assert!(checkpoint <= self.current_checkpoint);

        let start = if checkpoint == 0 {
            0
        } else {
            self.trail_delimiter[checkpoint - 1]
        };

        let end = if checkpoint == self.current_checkpoint {
            self.trail.len()
        } else {
            self.trail_delimiter[checkpoint]
        };

        &self.trail[start..end]
    }

    pub(crate) fn get_checkpoint(&self) -> usize {
        self.current_checkpoint
    }

    pub(crate) fn synchronise(&mut self, new_checkpoint: usize) -> Rev<Drain<'_, T>> {
        pumpkin_assert_simple!(new_checkpoint < self.current_checkpoint);

        let new_trail_len = self.trail_delimiter[new_checkpoint];

        self.current_checkpoint = new_checkpoint;
        self.trail_delimiter.truncate(new_checkpoint);
        self.trail.drain(new_trail_len..).rev()
    }

    pub(crate) fn push(&mut self, elem: T) {
        self.trail.push(elem)
    }

    /// This method pops an entry from the trail without doing any checks.
    ///
    /// Note that this method should *only* be used to prevent the assignments from being in an
    /// inconsistent state.
    pub(crate) fn pop(&mut self) -> Option<T> {
        self.trail.pop()
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
    fn backtracking_removes_elements_beyond_checkpoint() {
        let mut trail = Trail::default();

        trail.new_checkpoint();
        trail.push(1);
        let _ = trail.synchronise(0);

        assert!(trail.is_empty());
    }

    #[test]
    fn backtracking_is_nonchronological() {
        let mut trail = Trail::default();
        trail.push(1);

        trail.new_checkpoint();
        trail.push(2);
        trail.new_checkpoint();
        trail.push(3);
        trail.new_checkpoint();
        trail.push(4);

        let _ = trail.synchronise(1);

        assert_eq!(&[1, 2], trail.deref());
    }

    #[test]
    fn popped_elements_are_given_in_reverse_order_when_backtracking() {
        let mut trail = Trail::default();
        trail.push(1);

        trail.new_checkpoint();
        trail.push(2);
        trail.new_checkpoint();
        trail.push(3);
        trail.new_checkpoint();
        trail.push(4);

        let popped = trail.synchronise(0).collect::<Vec<_>>();
        assert_eq!(vec![4, 3, 2], popped);
    }

    #[test]
    fn elements_at_current_checkpoint() {
        let mut trail = Trail::default();
        trail.push(1);
        trail.push(2);

        trail.new_checkpoint();
        trail.push(3);
        trail.new_checkpoint();
        trail.push(4);
        trail.push(5);

        assert_eq!(&[1, 2], trail.values_at_checkpoint(0));
        assert_eq!(&[3], trail.values_at_checkpoint(1));
        assert_eq!(&[4, 5], trail.values_at_checkpoint(2));
    }
}
