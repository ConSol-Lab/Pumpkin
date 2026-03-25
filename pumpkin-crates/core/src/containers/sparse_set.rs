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
//! value in the domain to an index such that no two elements map to the same index.
//!
//! For performance, it is recommended to provide a mapping which maps an element to the
//! range `[0, |domain|]`.
//!
//! # Bibliography
//! \[1\] V. le C. de Saint-Marcq, P. Schaus, C. Solnon, and C. Lecoutre, ‘Sparse-sets for domain
//! implementation’, in CP workshop on Techniques foR Implementing Constraint programming Systems
//! (TRICS), 2013, pp. 1–10.

use crate::containers::HashSet;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// A set for keeping track of which values are still part of the original domain based on [\[1\]](https://hal.science/hal-01339250/document).
/// See the module level documentation for more information.
///
/// It provides O(1) removals of values from the domain and O(|D|) traversal of the domain (where D
/// are the values which are currently in the domain).
///
/// Note that it is required that each element contained in the domain can be
/// uniquely mapped to an index via the provided mapping.
///
/// # Bibliography
/// \[1\] V. le C. de Saint-Marcq, P. Schaus, C. Solnon, and C. Lecoutre, ‘Sparse-sets for domain
/// implementation’, in CP workshop on Techniques foR Implementing Constraint programming Systems
/// (TRICS), 2013, pp. 1–10.
#[derive(Debug, Clone)]
pub struct SparseSet<T> {
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
    mapping: fn(&T) -> i32,
    index_offset: i32,
}

impl<T> SparseSet<T> {
    /// Creates a new [`SparseSet`].
    ///
    /// If the provided `mapping` maps two elements in `input` to the same element then this method
    /// will panic.
    ///
    /// For performance, it is recommended to provide a mapping which maps all elements to the
    /// range `[0, |domain|]`.
    pub fn new(input: Vec<T>, mapping: fn(&T) -> i32) -> Self {
        let input_len = input.len();

        let mut min_index = 0;
        let mut max_index = 0;

        let mut used_indices = HashSet::new();

        for element in input.iter() {
            let index = (mapping)(element);
            let not_previously_inserted = used_indices.insert(index);

            pumpkin_assert_simple!(
                not_previously_inserted,
                "Two elements in the provided `input` map to the same index."
            );

            min_index = min_index.min(index);
            max_index = max_index.max(index);
        }

        pumpkin_assert_simple!(min_index <= max_index);

        // Now we need to adjust the indices; we first assign everything to usize::Max
        let mut indices =
            std::iter::repeat_n(usize::MAX, (max_index.abs() + min_index.abs()) as usize + 1)
                .collect::<Vec<_>>();
        // Then we go over all of the elements in the domain and assign them to their appropriate
        // indices
        for (i, element) in input.iter().enumerate().collect::<Vec<_>>() {
            indices[((mapping)(element) - min_index) as usize] = i;
        }

        SparseSet {
            size: input_len,
            domain: input,
            indices,
            mapping,
            index_offset: -min_index,
        }
    }

    fn get_mapping(&self, element: &T) -> usize {
        let output_index = (self.mapping)(element) + self.index_offset;
        output_index.try_into().unwrap()
    }

    pub fn set_to_empty(&mut self) {
        self.indices = vec![usize::MAX; self.indices.len()];
        self.domain.clear();
        self.size = 0;
    }

    pub fn restore_temporarily_removed(&mut self) {
        self.size = self.domain.len();
    }

    /// Determines whether the domain represented by the [`SparseSet`] is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Returns how many elements are part of the domain
    pub fn len(&self) -> usize {
        self.size
    }

    /// Returns the `index`th element in the domain; if `index` is larger than or equal to
    /// [`SparseSet::len`] then this method will panic.
    pub fn get(&self, index: usize) -> &T {
        pumpkin_assert_simple!(index < self.size);
        &self.domain[index]
    }

    /// Swaps the elements at positions `i` and `j` in [`domain`][SparseSet::domain] and swaps the
    /// corresponding indices in [`indices`][SparseSet::indices]
    fn swap(&mut self, i: usize, j: usize) {
        self.domain.swap(i, j);

        let index_i = self.get_mapping(&self.domain[i]);
        self.indices[index_i] = i;

        let index_j = self.get_mapping(&self.domain[j]);
        self.indices[index_j] = j;
    }

    /// Remove the value of `to_remove` from the domain; if the value is not in the domain then this
    /// method does not perform any operations.
    pub fn remove(&mut self, to_remove: &T) {
        if self.indices[self.get_mapping(to_remove)] < self.size {
            // The element is part of the domain and should be removed
            self.size -= 1;
            if self.size > 0 {
                self.swap(self.indices[self.get_mapping(to_remove)], self.size);
            }

            self.swap(
                self.indices[self.get_mapping(to_remove)],
                self.domain.len() - 1,
            );
            let element = self.domain.pop().expect("Has to have something to pop.");
            pumpkin_assert_moderate!((self.mapping)(&element) == (self.mapping)(to_remove));

            let to_remove_index = self.get_mapping(to_remove);
            self.indices[to_remove_index] = usize::MAX;
        } else if self.indices[self.get_mapping(to_remove)] < self.domain.len() {
            self.swap(
                self.indices[self.get_mapping(to_remove)],
                self.domain.len() - 1,
            );
            let element = self.domain.pop().expect("Has to have something to pop.");
            pumpkin_assert_moderate!((self.mapping)(&element) == (self.mapping)(to_remove));

            let to_remove_index = self.get_mapping(to_remove);
            self.indices[to_remove_index] = usize::MAX;
        }
    }

    pub fn remove_temporarily(&mut self, to_remove: &T) {
        if self.indices[self.get_mapping(to_remove)] < self.size {
            // The element is part of the domain and should be removed
            self.size -= 1;
            self.swap(self.indices[self.get_mapping(to_remove)], self.size);
        }
    }

    /// Determines whehter the `element` is contained in the domain of the sparse-set.
    pub fn contains(&self, element: &T) -> bool {
        self.get_mapping(element) < self.indices.len()
            && self.indices[self.get_mapping(element)] < self.size
    }

    /// Accomodates the `element`.
    pub fn accommodate(&mut self, element: &T) {
        let index = self.get_mapping(element);
        if self.indices.len() <= index {
            self.indices.resize(index + 1, usize::MAX);
        }
    }

    /// Inserts the element if it is not already contained in the sparse set.
    ///
    /// Note that this method does *not* check whether the insertion of this element causes clashes
    /// in the provided mapping (i.e., it does not check whether two elements are now mapped to the
    /// same index).
    pub fn insert(&mut self, element: T) {
        if !self.contains(&element) {
            self.accommodate(&element);

            let mut index = self.indices[self.get_mapping(&element)];

            // The index is outside of the domain, we need to readjust it
            //
            // If the index is `<= self.domain.len()`, then it could be that it is a temporarily
            // removed variable; in this case, we do not want to add a new element, but we just want
            // to place it inside of the domain again
            if index >= self.domain.len() {
                index = self.domain.len();

                let element_index = self.get_mapping(&element);
                self.indices[element_index] = index;

                self.domain.push(element);
            }

            self.swap(self.size, index);
            self.size += 1;
        }
    }

    /// Returns an iterator which goes over the values in the domain of the sparse-set
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.domain[..self.size].iter()
    }

    pub fn out_of_domain(&self) -> impl Iterator<Item = &T> {
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

    fn mapping_function(input: &i32) -> i32 {
        *input
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

    #[test]
    fn iter1() {
        let sparse_set = SparseSet::new(vec![5, 10, 2], mapping_function);
        let v: Vec<i32> = sparse_set.iter().copied().collect();
        assert_eq!(v.len(), 3);
        assert!(v.contains(&10));
        assert!(v.contains(&5));
        assert!(v.contains(&2));
    }

    #[test]
    fn iter2() {
        let mut sparse_set = SparseSet::new(vec![5, 10, 2], mapping_function); // 5, 10, 2
        sparse_set.insert(100); // 5, 10, 2, 100
        sparse_set.insert(2); // 5, 10, 2, 100
        sparse_set.insert(20); // 5, 10, 2, 100, 20
        sparse_set.remove(&10); // 5, 2, 100, 20
        sparse_set.insert(10); // 5, 10, 2, 100, 20
        sparse_set.remove(&10); // 5, 2, 100, 20

        let v: Vec<i32> = sparse_set.iter().copied().collect();
        assert_eq!(v.len(), 4);
        assert!(v.contains(&5));
        assert!(v.contains(&2));
        assert!(v.contains(&100));
        assert!(v.contains(&20));
        assert!(!v.contains(&10));
    }

    #[test]
    fn remove_temporarily_simple() {
        let mut sparse_set = SparseSet::new(vec![0], mapping_function);

        sparse_set.remove_temporarily(&0);
        sparse_set.insert(0);
        sparse_set.remove_temporarily(&0);

        assert!(sparse_set.is_empty())
    }

    #[test]
    fn remove_temporarily() {
        let mut sparse_set = SparseSet::new(vec![2, 0, 1], mapping_function);

        assert!(!sparse_set.is_empty());

        sparse_set.remove_temporarily(&0);
        sparse_set.insert(0);
        sparse_set.remove_temporarily(&0);
        assert!(!sparse_set.contains(&0));

        assert!(!sparse_set.is_empty());

        sparse_set.remove_temporarily(&0);
        assert!(!sparse_set.contains(&0));

        sparse_set.remove_temporarily(&0);
        sparse_set.remove_temporarily(&2);
        assert!(!sparse_set.contains(&2));
        sparse_set.remove_temporarily(&1);
        assert!(!sparse_set.contains(&1));

        assert!(sparse_set.is_empty());

        sparse_set.insert(1);

        assert!(sparse_set.contains(&1));

        assert!(!sparse_set.contains(&0));
        assert!(sparse_set.contains(&1));
        assert!(!sparse_set.contains(&2));

        sparse_set.restore_temporarily_removed();

        assert!(sparse_set.contains(&0));
        assert!(sparse_set.contains(&1));
        assert!(sparse_set.contains(&2));
    }

    #[test]
    fn remove_temporarily_non_continuous() {
        let mut sparse_set = SparseSet::new(vec![5, 10, 2], mapping_function);
        sparse_set.remove_temporarily(&10);
        assert!(!sparse_set.contains(&10));

        sparse_set.remove_temporarily(&5);
        sparse_set.remove_temporarily(&2);
        assert!(sparse_set.is_empty());
    }

    #[test]
    fn remove_temporarily_non_continuous_spanning() {
        let mut sparse_set = SparseSet::new(vec![5, 10, -2], mapping_function);
        sparse_set.remove_temporarily(&10);
        assert!(!sparse_set.contains(&10));

        sparse_set.remove_temporarily(&-2);
        assert!(!sparse_set.contains(&-2));

        sparse_set.remove_temporarily(&5);

        assert!(sparse_set.is_empty());
    }

    #[test]
    fn remove_temporarily_non_continuous_negative() {
        let mut sparse_set = SparseSet::new(vec![-5, -10, -2], mapping_function);
        sparse_set.remove_temporarily(&-10);
        assert!(!sparse_set.contains(&-10));

        sparse_set.remove_temporarily(&-2);
        assert!(!sparse_set.contains(&-2));

        sparse_set.remove_temporarily(&-5);

        assert!(sparse_set.is_empty());
    }
}
