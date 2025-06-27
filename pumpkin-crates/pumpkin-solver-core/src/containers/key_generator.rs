use std::marker::PhantomData;

use super::StorageKey;

/// Generates a sequence of [`StorageKey`]s.
#[derive(Clone, Copy, Debug)]
pub struct KeyGenerator<Key> {
    key: PhantomData<Key>,
    counter: usize,
}

impl<Key> Default for KeyGenerator<Key> {
    fn default() -> Self {
        Self {
            key: Default::default(),
            counter: Default::default(),
        }
    }
}

impl<Key: StorageKey> KeyGenerator<Key> {
    /// Generate a new `Key`.
    pub fn next_key(&mut self) -> Key {
        let key = Key::create_from_index(self.counter);
        self.counter += 1;
        key
    }
}
