//! A set for keeping track of which values are still part of the original domain, allows O(1)
//! removals and O(|D|) traversal of the domain (where D are the values which are currently in the
//! domain).
//!
//! # Theoretical
//! The idea of this structure is to allow efficient removal and traversal of the values which are
//! still in the domain at the "cost" of expensive queries to check whether a value is currently in
//! the domain.
//!
//! The idea is that the sparse-set keeps track of the number of elements which are in
//! the domain in ([`SparseSet::size`]) and it guarantees that the first [`SparseSet::size`] values
//! are in the domain. To remove a value, the element at index `i` is swapped with the element at
//! index [`SparseSet::size`] and [`SparseSet::size`] is afterwards decremented by 1. This does not
//! allow the reclamation of memory when an element is removed from the structure but it allows easy
//! backtracking by simply moving the [`SparseSet::size`] pointer.
//!
//! # Practical
//! Our implementation follows [\[1\]](https://hal.science/hal-01339250/document). The [`SparseSet`]
//! structure keeps track of a number of variables; the main practical consideration is that a
//! function `mapping` should be provided which maps every
//! value in the domain to an index in \[0..|domain|\) in a bijective manner.
//!
//! # Bibliography
//! \[1\] V. le C. de Saint-Marcq, P. Schaus, C. Solnon, and C. Lecoutre, ‘Sparse-sets for domain
//! implementation’, in CP workshop on Techniques foR Implementing Constraint programming Systems
//! (TRICS), 2013, pp. 1–10.

use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// A set for keeping track of which values are still part of the original domain based on [\[1\]](https://hal.science/hal-01339250/document).
/// See the module level documentation for more information.
///
/// It provides O(1) removals of values from the domain and O(|D|) traversal of the domain (where D
/// are the values which are currently in the domain).
///
/// Note that it is required that each element contained in the domain can be
/// uniquely mapped to an index in the range [0, |D|) (i.e. the mapping function is bijective)
///
/// # Bibliography
/// \[1\] V. le C. de Saint-Marcq, P. Schaus, C. Solnon, and C. Lecoutre, ‘Sparse-sets for domain
/// implementation’, in CP workshop on Techniques foR Implementing Constraint programming Systems
/// (TRICS), 2013, pp. 1–10.
#[derive(Debug, Clone)]
pub(crate) struct SparseSet<T> {
    /// The number of elements which are currently in the domain
    size: usize,
    /// The current state of the domain, this structure guarantees that the first
    /// [`size`][SparseSet::size] elements are part of the domain
    domain: Vec<T>,
    /// Stores for each value of T what its corresponding index is in
    /// [`domain`][`SparseSet::domain`]
    indices: Vec<usize>,
    /// A bijective function which takes as input an element `T` and returns an index in the range
    /// [0, |D_{original}|) to be used for retrieving values from
    /// [`indices`][`SparseSet::indices`]
    mapping: fn(&T) -> usize,
}

impl<T> SparseSet<T> {
    /// Assumption: It is assumed that `mapping` is a bijective function which
    /// will return an index which is in the range [0, |D_{original}|) (where D_{original} is
    /// the initial domain before any operations have been performed).
    pub(crate) fn new(input: Vec<T>, mapping: fn(&T) -> usize) -> Self {
        let input_len = input.len();
        SparseSet {
            size: input_len,
            domain: input,
            indices: (0..input_len).collect::<Vec<_>>(),
            mapping,
        }
    }

    pub(crate) fn set_to_empty(&mut self) {
        self.indices = vec![usize::MAX; self.indices.len()];
        self.domain.clear();
        self.size = 0;
    }

    /// Determines whether the domain represented by the [`SparseSet`] is empty
    pub(crate) fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Returns how many elements are part of the domain
    pub(crate) fn len(&self) -> usize {
        self.size
    }

    /// Returns the `index`th element in the domain; if `index` is larger than or equal to
    /// [`SparseSet::len`] then this method will panic.
    pub(crate) fn get(&self, index: usize) -> &T {
        pumpkin_assert_simple!(index < self.size);
        &self.domain[index]
    }

    /// Swaps the elements at positions `i` and `j` in [`domain`][SparseSet::domain] and swaps the
    /// corresponding indices in [`indices`][SparseSet::indices]
    fn swap(&mut self, i: usize, j: usize) {
        self.domain.swap(i, j);
        self.indices[(self.mapping)(&self.domain[i])] = i;
        self.indices[(self.mapping)(&self.domain[j])] = j;
    }

    /// Remove the value of `to_remove` from the domain; if the value is not in the domain then this
    /// method does not perform any operations.
    pub(crate) fn remove(&mut self, to_remove: &T) {
        if self.indices[(self.mapping)(to_remove)] < self.size {
            // The element is part of the domain and should be removed
            self.size -= 1;
            if self.size > 0 {
                self.swap(self.indices[(self.mapping)(to_remove)], self.size);
            }

            self.swap(
                self.indices[(self.mapping)(to_remove)],
                self.domain.len() - 1,
            );
            let element = self.domain.pop().expect("Has to have something to pop.");
            pumpkin_assert_moderate!((self.mapping)(&element) == (self.mapping)(to_remove));
            self.indices[(self.mapping)(to_remove)] = usize::MAX;
        } else if self.indices[(self.mapping)(to_remove)] < self.domain.len() {
            self.swap(
                self.indices[(self.mapping)(to_remove)],
                self.domain.len() - 1,
            );
            let element = self.domain.pop().expect("Has to have something to pop.");
            pumpkin_assert_moderate!((self.mapping)(&element) == (self.mapping)(to_remove));
            self.indices[(self.mapping)(to_remove)] = usize::MAX;
        }
        pumpkin_assert_simple!(
            self.indices
                .iter()
                .filter(|index| **index != usize::MAX)
                .count()
                >= self.size
        )
    }

    pub(crate) fn remove_temporarily(&mut self, to_remove: &T) {
        if self.indices[(self.mapping)(to_remove)] < self.size {
            // The element is part of the domain and should be removed
            self.size -= 1;
            self.swap(self.indices[(self.mapping)(to_remove)], self.size);
        }
    }

    pub(crate) fn restore_temporarily_removed(&mut self) {
        self.size = self.domain.len();
    }

    /// Determines whehter the `element` is contained in the domain of the sparse-set.
    pub(crate) fn contains(&self, element: &T) -> bool {
        (self.mapping)(element) < self.indices.len()
            && self.indices[(self.mapping)(element)] < self.size
    }

    /// Accomodates the `element`.
    pub(crate) fn accommodate(&mut self, element: &T) {
        let index = (self.mapping)(element);
        if self.indices.len() <= index {
            self.indices.resize(index + 1, usize::MAX);
        }
    }

    /// Inserts the element if it is not already contained in the sparse set.
    pub(crate) fn insert(&mut self, element: T) {
        if (self.mapping)(&element) >= self.indices.len() {
            self.accommodate(&element);
        }
        if self.indices[(self.mapping)(&element)] == usize::MAX {
            self.indices[(self.mapping)(&element)] = self.domain.len();
            self.domain.push(element);
            self.swap(self.size, self.domain.len() - 1);
            self.size += 1;
        } else if self.indices[(self.mapping)(&element)] >= self.size {
            self.swap(self.size, self.indices[(self.mapping)(&element)]);
            self.indices[(self.mapping)(&element)] = self.size;
            self.size += 1;
        }

        pumpkin_assert_simple!(
            self.indices
                .iter()
                .filter(|index| **index != usize::MAX)
                .count()
                >= self.size
        )
    }

    /// Returns an iterator which goes over the values in the domain of the sparse-set
    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.domain[..self.size].iter()
    }

    pub(crate) fn out_of_domain(&self) -> impl Iterator<Item = &T> {
        self.domain[self.size..].iter()
    }
}

impl<T> IntoIterator for SparseSet<T> {
    type Item = T;

    type IntoIter = std::iter::Take<std::vec::IntoIter<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.domain.into_iter().take(self.size)
    }
}

#[cfg(test)]
mod tests {
    use super::SparseSet;

    fn mapping_function(input: &u32) -> usize {
        *input as usize
    }

    #[test]
    fn test_len() {
        let sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        assert_eq!(sparse_set.len(), 3);
    }

    #[test]
    fn removal() {
        let mut sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        sparse_set.remove(&1);
        assert_eq!(sparse_set.domain, vec![0, 2]);
        assert_eq!(sparse_set.size, 2);
        assert_eq!(sparse_set.indices, vec![0, usize::MAX, 1]);
    }

    #[test]
    fn removal_adjusts_size() {
        let mut sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        assert_eq!(sparse_set.size, 3);
        sparse_set.remove(&0);
        assert_eq!(sparse_set.size, 2);
    }

    #[test]
    fn remove_all_elements_leads_to_empty_set() {
        let mut sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        sparse_set.remove(&0);
        sparse_set.remove(&1);
        sparse_set.remove(&2);
        assert!(sparse_set.is_empty());
    }
}
