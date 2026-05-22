use std::marker::PhantomData;

use bit_set::BitSet;

use crate::containers::StorageKey;

/// A bit-set for types that implement [`StorageKey`].
#[derive(Debug)]
pub struct KeyedBitSet<Key> {
    bitset: BitSet,
    key: PhantomData<Key>,
}

impl<Key: StorageKey> KeyedBitSet<Key> {
    /// Add the key to the set.
    ///
    /// Returns `true` if the set did _not_ previously contain `key`.
    pub fn insert(&mut self, key: Key) -> bool {
        self.bitset.insert(key.index())
    }

    /// Remove the key from the set.
    ///
    /// If the key was present, returns true.
    pub fn remove(&mut self, key: Key) -> bool {
        self.bitset.remove(key.index())
    }

    /// Get all keys in the set and remove them.
    pub fn drain(&self) -> impl Iterator<Item = Key> {
        self.bitset.iter().map(Key::create_from_index)
    }

    /// Remove all keys in the set.
    pub fn clear(&mut self) {
        self.bitset.make_empty();
    }
}

impl<Key> Clone for KeyedBitSet<Key> {
    fn clone(&self) -> Self {
        Self {
            bitset: self.bitset.clone(),
            key: PhantomData,
        }
    }
}

impl<Key> Default for KeyedBitSet<Key> {
    fn default() -> Self {
        Self {
            bitset: BitSet::default(),
            key: PhantomData,
        }
    }
}
