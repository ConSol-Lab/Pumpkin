/// A set for keeping track of which values are still part of the original domain based on "Sparse-Sets for Domain Implementation - Le Clément et al. (2013)"
///
/// It provides O(1) removals of values from the domain and O(|D|) traversal of the domain
///
/// Note that it is required that each element contained in [SparseSet::domain] can be uniquely mapped to an index in the range [0, |D|) (i.e. the mapping function is bijective)
pub struct SparseSet<T> {
    /// * `size` - The number of elements which are currently in the domain
    size: usize,
    /// * `domain` - The current state of the domain, this structure guarantees that domain[..size] is in the actual domain
    domain: Vec<T>,
    /// * `indices` - The mapping of T to the indices in [SparseSet::domain]
    indices: Vec<usize>,
    /// * `mapping` - A bijective function which takes as input an element `T` and returns an index in the range [0, |D_{original}|)
    mapping: fn(&T) -> usize,
}

impl<T> SparseSet<T> {
    /// Assumption: It is assumed that `mapping` is a bijective function which will return an index which is in the range [0, |D_{original}|)
    pub fn new(input: Vec<T>, mapping: fn(&T) -> usize) -> Self {
        let input_len = input.len();
        SparseSet {
            size: input_len,
            domain: input,
            indices: (0..input_len).collect::<Vec<_>>(),
            mapping,
        }
    }

    /// Determines whether the domain represented by the [SparseSet] is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Determines whether the provided index is mapped to a non-removed value from the domain
    pub fn has_next(&self, index: usize) -> bool {
        index < self.size
    }

    /// Returns the element which is stored at the location of the index
    pub fn get(&self, index: usize) -> &T {
        &self.domain[index]
    }

    /// Swaps the elements at positions `i` and `j` and swaps the corresponding indices
    fn swap(&mut self, i: usize, j: usize) {
        self.domain.swap(i, j);
        self.indices[(self.mapping)(&self.domain[i])] = i;
        self.indices[(self.mapping)(&self.domain[j])] = j;
    }

    /// Remove the value of `to_remove` from the domain
    pub fn remove(&mut self, to_remove: &T) {
        if self.indices[(self.mapping)(to_remove)] < self.size {
            // The element is part of the domain and should be removed
            self.size -= 1;
            self.swap(self.indices[(self.mapping)(to_remove)], self.size);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SparseSet;

    fn mapping_function(input: &u32) -> usize {
        *input as usize
    }

    #[test]
    fn test_has_next() {
        let sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        assert!(sparse_set.has_next(0));
        assert!(sparse_set.has_next(1));
        assert!(sparse_set.has_next(2));
        assert!(!sparse_set.has_next(3));
    }

    #[test]
    fn removal() {
        let mut sparse_set = SparseSet::new(vec![0, 1, 2], mapping_function);
        sparse_set.remove(&1);
        assert_eq!(sparse_set.domain, vec![0, 2, 1]);
        assert_eq!(sparse_set.size, 2);
        assert_eq!(sparse_set.indices, vec![0, 2, 1]);
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
