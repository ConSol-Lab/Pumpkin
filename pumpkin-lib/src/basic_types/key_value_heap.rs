//! A heap where the keys range from [0, ..., n - 1] and the values are nonnegative floating points.
//! The heap can be queried to return key with the maximum value, and certain keys can be
//! (temporarily) removed/readded as necessary It allows increasing/decreasing the values of its
//! entries

// The implementation could be more efficient in the following ways:
//  - Currently more comparisons are done than necessary when sifting
//  - Possibly the recursion could be unrolled
use std::ops::AddAssign;
use std::ops::DivAssign;

use super::KeyedVec;
use super::StorageKey;
use crate::basic_types::HashSet;
use crate::pumpkin_assert_moderate;

/// A [max-heap](https://en.wikipedia.org/wiki/Min-max_heap)
/// which allows for generalised `Key`s (required to implement [StorageKey]) and `Value`s (which are
/// required to be ordered, divisible and addable).
#[derive(Debug)]
pub(crate) struct KeyValueHeap<Key: StorageKey, Value> {
    /// Contains the values stored as a heap; the value of key `i` is at index
    /// [`KeyValueHeap::map_key_to_position\[i\]`][KeyValueHeap::map_key_to_position]
    values: Vec<Value>,
    /// `map_key_to_position[i]` is the index of the value of the key `i` in
    /// [`KeyValueHeap::values`]
    map_key_to_position: KeyedVec<Key, usize>,
    /// `map_position_to_key[i]` is the key which is associated with `i` in
    /// [`KeyValueHeap::values`]
    map_position_to_key: Vec<Key>,
    /// The index of the last element in [`KeyValueHeap::values`]
    end_position: usize,
}

impl<Key: StorageKey, Value> Default for KeyValueHeap<Key, Value> {
    fn default() -> Self {
        Self {
            values: Default::default(),
            map_key_to_position: Default::default(),
            map_position_to_key: Default::default(),
            end_position: Default::default(),
        }
    }
}

impl<
        Key: StorageKey + Copy,
        Value: AddAssign<Value> + DivAssign<Value> + PartialOrd + Default + Copy,
    > KeyValueHeap<Key, Value>
{
    pub(crate) fn accomodate(&mut self, key: Key, default_value: Value) {
        let idx = key.index();

        while idx >= self.len() {
            self.grow(Key::create_from_index(self.len()), default_value);
        }
    }

    /// Return the key with maximum value from the heap, or None if the heap is empty. Note that
    /// this does not delete the key (see [`KeyValueHeap::pop_max`] to get and delete).
    ///
    /// The time-complexity of this operation is O(1)
    pub(crate) fn peek_max(&self) -> Option<(&Key, &Value)> {
        if self.is_empty() {
            None
        } else {
            Some((
                &self.map_position_to_key[0],
                &self.values[self.map_key_to_position[&self.map_position_to_key[0]]],
            ))
        }
    }

    pub(crate) fn get_value(&self, key: Key) -> &Value {
        pumpkin_assert_moderate!(
            key.index() < self.map_key_to_position.len(),
            "Attempted to get key with index {} for a map with length {}",
            key.index(),
            self.map_key_to_position.len()
        );
        &self.values[self.map_key_to_position[key]]
    }

    /// Deletes the key with maximum value from the heap and returns it, or None if the heap is
    /// empty.
    ///
    ///  The time-complexity of this operation is O(logn)
    pub(crate) fn pop_max(&mut self) -> Option<Key> {
        let best_key = self.map_position_to_key[0];
        pumpkin_assert_moderate!(0 == self.map_key_to_position[best_key]);
        self.delete_key(best_key);
        Some(best_key)
    }

    /// Increments the value of the element of 'key' by 'increment'
    ///
    /// The worst-case time-complexity of this operation is O(logn); average case is likely to be
    /// better
    pub(crate) fn increment(&mut self, key: Key, increment: Value) {
        let position = self.map_key_to_position[key];
        self.values[position] += increment;
        // Recall that increment may be applied to keys not present
        // So we only apply sift up in case the key is present
        if self.is_key_present(key) {
            self.sift_up(position);
        }
    }

    /// Restores the entry with key 'key' to the heap if the key is not present, otherwise does
    /// nothing. Its value is the previous value used before 'delete_key' was called.
    ///
    ///  The run-time complexity of this operation is O(logn)
    pub(crate) fn restore_key(&mut self, key: Key) {
        if !self.is_key_present(key) {
            // The key is somewhere in the range [end_position, max_size-1]
            // We place the key at the end of the heap, increase end_position, and sift up
            let position = self.map_key_to_position[key];
            pumpkin_assert_moderate!(position >= self.end_position);
            self.swap_positions(position, self.end_position);
            self.end_position += 1;
            self.sift_up(self.end_position - 1);
        }
    }

    /// Removes the entry with key 'key' (temporarily) from the heap if the key is present,
    /// otherwise does nothing. Its value remains recorded internally and is available upon
    /// calling [`KeyValueHeap::restore_key`]. The value can still be subjected to
    /// [`KeyValueHeap::divide_values`].
    ///
    /// The run-time complexity of this operation is O(logn)
    pub(crate) fn delete_key(&mut self, key: Key) {
        if self.is_key_present(key) {
            // Place the key at the end of the heap, decrement the heap, and sift down to ensure a
            // valid heap
            let position = self.map_key_to_position[key];
            self.swap_positions(position, self.end_position - 1);
            self.end_position -= 1;
            if position < self.end_position {
                self.sift_down(position);
            }
        }
    }

    /// Returns how many elements are in the heap (including the (temporarily) "removed" values)
    pub(crate) fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns whether there are elements left in the heap (excluding the "removed" values)
    pub(crate) fn is_empty(&self) -> bool {
        self.end_position == 0
    }

    /// Returns whether the key is currently not (temporarily) remove
    pub(crate) fn is_key_present(&self, key: Key) -> bool {
        self.map_key_to_position[key] < self.end_position
    }

    /// Increases the size of the heap by one and adjust the data structures appropriately by adding
    /// `Key` and `Value`
    pub(crate) fn grow(&mut self, key: Key, value: Value) {
        let last_index = self.values.len();
        self.values.push(value);
        // Initially the key is placed placed at the very end, will be placed in the correct
        // position below to ensure a valid heap structure
        self.map_key_to_position.push(last_index);
        self.map_position_to_key.push(key);
        pumpkin_assert_moderate!(
            self.map_position_to_key[last_index].index() == key.index()
                && self.map_key_to_position[key] == last_index
        );
        self.swap_positions(self.end_position, last_index);
        self.end_position += 1;
        self.sift_up(self.end_position - 1);
    }

    /// Divides all the values in the heap by 'divisor'. This will also affect the values of keys
    /// that have been [`KeyValueHeap::delete_key`].
    ///
    /// The run-time complexity of this operation is O(n)
    pub(crate) fn divide_values(&mut self, divisor: Value) {
        for value in self.values.iter_mut() {
            *value /= divisor;
        }
    }

    fn swap_positions(&mut self, a: usize, b: usize) {
        let key_i = self.map_position_to_key[a];
        pumpkin_assert_moderate!(self.map_key_to_position[key_i] == a);
        let key_j = self.map_position_to_key[b];
        pumpkin_assert_moderate!(self.map_key_to_position[key_j] == b);

        self.values.swap(a, b);
        self.map_position_to_key.swap(a, b);
        self.map_key_to_position.swap(key_i.index(), key_j.index());

        pumpkin_assert_moderate!(
            self.map_key_to_position[key_i] == b && self.map_key_to_position[key_j] == a
        );

        pumpkin_assert_moderate!(
            self.map_key_to_position
                .iter()
                .collect::<HashSet<&usize>>()
                .len()
                == self.map_key_to_position.len()
        )
    }

    fn sift_up(&mut self, position: usize) {
        // Only sift up if not at the root
        if position > 0 {
            let parent_position = KeyValueHeap::<Key, Value>::get_parent_position(position);
            // Continue sift up if the heap property is violated
            if self.values[parent_position] < self.values[position] {
                self.swap_positions(parent_position, position);
                self.sift_up(parent_position);
            }
        }
    }

    fn sift_down(&mut self, position: usize) {
        pumpkin_assert_moderate!(position < self.end_position);

        if !self.is_heap_locally(position) {
            let largest_child_position = self.get_largest_child_position(position);
            self.swap_positions(largest_child_position, position);
            self.sift_down(largest_child_position);
        }
    }

    fn is_heap_locally(&self, position: usize) -> bool {
        // Either the node is a left, or it satisfies the heap property (the value of the parent is
        // at least as large as the values of its child)
        let left_child_position = KeyValueHeap::<Key, Value>::get_left_child_position(position);
        let right_child_position = KeyValueHeap::<Key, Value>::get_right_child_position(position);

        self.is_leaf(position)
            || (self.values[position] >= self.values[left_child_position]
                && right_child_position < self.end_position
                && self.values[position] >= self.values[right_child_position])
    }

    fn is_leaf(&self, position: usize) -> bool {
        KeyValueHeap::<Key, Value>::get_left_child_position(position) >= self.end_position
    }

    fn get_largest_child_position(&self, position: usize) -> usize {
        pumpkin_assert_moderate!(!self.is_leaf(position));

        let left_child_position = KeyValueHeap::<Key, Value>::get_left_child_position(position);
        let right_child_position = KeyValueHeap::<Key, Value>::get_right_child_position(position);

        if right_child_position < self.end_position
            && self.values[right_child_position] > self.values[left_child_position]
        {
            right_child_position
        } else {
            left_child_position
        }
    }

    fn get_parent_position(child_position: usize) -> usize {
        pumpkin_assert_moderate!(child_position > 0, "Root has no parent.");
        (child_position - 1) / 2
    }

    fn get_left_child_position(position: usize) -> usize {
        2 * position + 1
    }

    fn get_right_child_position(position: usize) -> usize {
        2 * position + 2
    }
}
