/*
A heap class where the keys range from [0, ..., n-1] and the values are nonnegative floating points.
The heap can be queried to return key with the maximum value, and certain keys can be (temporarily) removed/readded as necessary
It allows increasing/decreasing the values of its entries
*/

//the implementation could be more efficient, currently more comparisons are done than necessary when sifting, and possibly the recursion could be unrolled

use crate::pumpkin_asserts::{pumpkin_assert_moderate, pumpkin_assert_simple};

#[derive(Default)]
pub struct KeyValueHeap {
    values: Vec<f64>, //contains the values stored as a heap. The value of key i is in position map_key_to_position[i]
    map_key_to_position: Vec<u32>, //[i] shows the location of the value of the key i in the values_ array
    map_position_to_key: Vec<u32>, //[i] shows which key is associated with values[i]
    end_position: u32,             //the index past the last element in the heap
}

impl KeyValueHeap {
    pub fn new() -> KeyValueHeap {
        KeyValueHeap {
            values: vec![],
            map_key_to_position: vec![],
            map_position_to_key: vec![],
            end_position: 0,
        }
    }

    //Return the key with maximum value from the heap, or None if the heap is empty. Note that this does not delete the key (see 'pop_max') to get and delete.
    //  O(1)
    pub fn peek_max(&self) -> Option<u32> {
        if self.is_empty() {
            None
        } else {
            Some(self.map_position_to_key[0])
        }
    }

    pub fn get_value(&self, key: u32) -> f64 {
        pumpkin_assert_moderate!((key as usize) < self.map_key_to_position.len());
        self.values[self.map_key_to_position[key as usize] as usize]
    }

    //Deletes the key with maximum value from the heap and returns it, or None if the heap is empty.
    //  O(logn)
    pub fn pop_max(&mut self) -> Option<u32> {
        let best_key = self.map_position_to_key[0];
        self.delete_key(best_key);
        Some(best_key)
    }

    //increments the value of the element of 'key_id' by 'increment'
    //  O(logn) worst case, but average case might be better.
    pub fn increment(&mut self, key: u32, increment: f64) {
        let position = self.map_key_to_position[key as usize];
        self.values[position as usize] += increment;
        //recall that increment may be applied to keys not present
        //   so we only apply sift up in case the key is present
        if self.is_key_present(key) {
            self.sift_up(position);
        }
    }

    //Restores the entry with key 'key' to the heap if the key is not present, otherwise does nothing
    //  its value is the previous value used before 'delete_key' was called.
    //  O(logn)
    pub fn restore_key(&mut self, key: u32) {
        if !self.is_key_present(key) {
            //key_id is somewhere in the range [end_position, max_size-1]
            //place the key at the end of the heap, increase end_position, and sift up
            let position = self.map_key_to_position[key as usize];
            pumpkin_assert_moderate!(position >= self.end_position);
            self.swap_positions(position, self.end_position);
            self.end_position += 1;
            self.sift_up(self.end_position - 1);
        }
    }

    //Removes the entry with key 'key' (temporarily) from the heap if the key is present, otherwise does nothing.
    //  its value remains recorded internally and is available upon calling 'restore_key'.
    //  the value can still be subjected to 'divide_values'.
    //  O(logn)
    pub fn delete_key(&mut self, key: u32) {
        if self.is_key_present(key) {
            //place the key at the end of the heap, decrement the heap, and sift down to ensure a valid heap
            let position = self.map_key_to_position[key as usize];
            self.swap_positions(position, self.end_position - 1);
            self.end_position -= 1;
            if position < self.end_position {
                self.sift_down(position);
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.end_position == 0
    }

    pub fn is_key_present(&self, key: u32) -> bool {
        self.map_key_to_position[key as usize] < self.end_position
    }

    //increases the size of the heap by one
    pub fn grow(&mut self, value: f64) {
        let new_key = self.values.len() as u32;
        self.values.push(value);
        self.map_key_to_position.push(new_key); //initially the key is placed placed at the very end, will be placed in the correct position below to ensure a valid heap structure
        self.map_position_to_key.push(new_key);
        self.swap_positions(self.end_position, new_key);
        self.end_position += 1;
        self.sift_up(self.end_position - 1);
    }

    //divides all the values in the heap by 'divisor'. This will affect the values of keys that have been removed.
    //  O(n)
    pub fn divide_values(&mut self, divisor: f64) {
        for value in self.values.iter_mut() {
            *value /= divisor;
        }
    }

    //Restores all keys, set their values to zero, and applies a randomised procedure on the keys possibly changing their order
    //  the random_seed controls the randomisation procedure in the following way:
    //      random_seed == -2: the order of the keys initially in the tree structure will be in a fixed increasing order
    //      random_seed == -1: the random_seed will be replaced by the current time, and then randomises the order
    //      random_seed > 0: the randomises the order using the given seen
    pub fn reset(&mut self, random_seed: i64) {
        pumpkin_assert_simple!(random_seed >= -2);

        self.end_position = self.values.len() as u32;
        for value in self.values.iter_mut() {
            *value = 0.0;
        }
        for iter in self.map_key_to_position.iter_mut().enumerate() {
            *iter.1 = iter.0 as u32;
        }
        for iter in self.map_position_to_key.iter_mut().enumerate() {
            *iter.1 = iter.0 as u32;
        }

        if random_seed != -2 {
            todo!("Random seeding not implemented yet.");
        }
    }

    fn swap_positions(&mut self, a: u32, b: u32) {
        //changing the type for convenience
        let a = a as usize;
        let b = b as usize;

        let key_i = self.map_position_to_key[a] as usize;
        let key_j = self.map_position_to_key[b] as usize;

        self.values.swap(a, b);
        self.map_position_to_key.swap(a, b);
        self.map_key_to_position.swap(key_i, key_j);
    }

    fn sift_up(&mut self, position: u32) {
        //only sift up if not at the root
        if position > 0 {
            let parent_position = KeyValueHeap::get_parent_position(position);
            //continue sift up if the heap property is violated
            if self.values[parent_position as usize] < self.values[position as usize] {
                self.swap_positions(parent_position, position);
                self.sift_up(parent_position);
            }
        }
    }

    fn sift_down(&mut self, position: u32) {
        pumpkin_assert_moderate!(position < self.end_position);

        if !self.is_heap_locally(position) {
            let largest_child_position = self.get_largest_child_position(position);
            self.swap_positions(largest_child_position, position);
            self.sift_down(largest_child_position);
        }
    }

    fn is_heap_locally(&self, position: u32) -> bool {
        //either the node is a left, or it satisfies the heap property (the value of the parent is at least as large as the values of its child)
        let left_child_position = KeyValueHeap::get_left_child_position(position);
        let right_child_position = KeyValueHeap::get_right_child_position(position);

        self.is_leaf(position)
            || (self.values[position as usize] >= self.values[left_child_position as usize]
                && right_child_position < self.end_position
                && self.values[position as usize] >= self.values[right_child_position as usize])
    }

    fn is_leaf(&self, position: u32) -> bool {
        KeyValueHeap::get_left_child_position(position) >= self.end_position
    }

    fn get_largest_child_position(&self, position: u32) -> u32 {
        pumpkin_assert_moderate!(!self.is_leaf(position));

        let left_child_position = KeyValueHeap::get_left_child_position(position);
        let right_child_position = KeyValueHeap::get_right_child_position(position);

        if right_child_position < self.end_position
            && self.values[right_child_position as usize]
                > self.values[left_child_position as usize]
        {
            right_child_position
        } else {
            left_child_position
        }
    }

    fn get_parent_position(child_position: u32) -> u32 {
        pumpkin_assert_moderate!(child_position > 0, "Root has no parent.");
        (child_position - 1) / 2
    }

    fn get_left_child_position(position: u32) -> u32 {
        //self is not needed, but we keep it for consistency with other methods
        2 * position + 1
    }

    fn get_right_child_position(position: u32) -> u32 {
        2 * position + 2
    }
}
