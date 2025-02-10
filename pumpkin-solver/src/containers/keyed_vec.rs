use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

/// Structure for storing elements of type `Value`, the structure can only be indexed by structures
/// of type `Key`.
///
/// Almost all features of this structure require that `Key` implements the [StorageKey] trait.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct KeyedVec<Key, Value> {
    /// [PhantomData] to ensure that the [KeyedVec] is bound to the structure
    key: PhantomData<Key>,
    /// Storage of the elements of type `Value`
    elements: Vec<Value>,
}

impl<Key, Value: Clone> Clone for KeyedVec<Key, Value> {
    fn clone(&self) -> Self {
        Self {
            key: PhantomData,
            elements: self.elements.clone(),
        }
    }
}

impl<Key, Value> Default for KeyedVec<Key, Value> {
    fn default() -> Self {
        Self {
            key: PhantomData,
            elements: Vec::default(),
        }
    }
}

impl<Key, Value> KeyedVec<Key, Value> {
    pub(crate) const fn new() -> Self {
        Self {
            key: PhantomData,
            elements: Vec::new(),
        }
    }
}

impl<Key: StorageKey, Value> KeyedVec<Key, Value> {
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Add a new value to the vector.
    ///
    /// Returns the key for the inserted value.
    pub fn push(&mut self, value: Value) -> Key {
        self.elements.push(value);

        Key::create_from_index(self.elements.len() - 1)
    }

    /// Iterate over the values in the vector.
    pub fn iter(&self) -> impl Iterator<Item = &'_ Value> {
        self.elements.iter()
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = Key> {
        (0..self.elements.len()).map(Key::create_from_index)
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &'_ mut Value> {
        self.elements.iter_mut()
    }

    pub(crate) fn swap(&mut self, a: usize, b: usize) {
        self.elements.swap(a, b)
    }
}

impl<Key: StorageKey, Value: Clone> KeyedVec<Key, Value> {
    pub(crate) fn resize(&mut self, new_len: usize, value: Value) {
        self.elements.resize(new_len, value)
    }

    pub(crate) fn clear(&mut self) {
        self.elements.clear();
    }
}

impl<Key: StorageKey, Value> Index<Key> for KeyedVec<Key, Value> {
    type Output = Value;

    fn index(&self, index: Key) -> &Self::Output {
        &self.elements[index.index()]
    }
}

impl<Key: StorageKey, Value> Index<&Key> for KeyedVec<Key, Value> {
    type Output = Value;

    fn index(&self, index: &Key) -> &Self::Output {
        &self.elements[index.index()]
    }
}

impl<Key: StorageKey, Value> IndexMut<Key> for KeyedVec<Key, Value> {
    fn index_mut(&mut self, index: Key) -> &mut Self::Output {
        &mut self.elements[index.index()]
    }
}

impl StorageKey for usize {
    fn index(&self) -> usize {
        *self
    }

    fn create_from_index(index: usize) -> Self {
        index
    }
}
/// A simple trait which requires that the structures implementing this trait can generate an index.
pub trait StorageKey {
    fn index(&self) -> usize;

    fn create_from_index(index: usize) -> Self;
}
