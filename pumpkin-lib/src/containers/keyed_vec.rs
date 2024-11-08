use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

/// Structure for storing elements of type `Value`, the structure can only be indexed by structures
/// of type `Key`.
///
/// Almost all features of this structure require that `Key` implements the [StorageKey] trait.
#[derive(Debug, Hash, PartialEq, Eq)]
pub(crate) struct KeyedVec<Key, Value> {
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

impl<Key: StorageKey, Value> KeyedVec<Key, Value> {
    #[allow(dead_code)]
    pub(crate) fn new(elements: Vec<Value>) -> Self {
        KeyedVec {
            key: PhantomData,
            elements,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.elements.len()
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.elements.push(value)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &'_ Value> {
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

    #[allow(dead_code)]
    pub(crate) fn into_entries(self) -> impl Iterator<Item = (Key, Value)> {
        self.elements
            .into_iter()
            .enumerate()
            .map(|(idx, value)| (Key::create_from_index(idx), value))
    }
}

impl<Key: StorageKey, Value: Clone> KeyedVec<Key, Value> {
    pub(crate) fn resize(&mut self, new_len: usize, value: Value) {
        self.elements.resize(new_len, value)
    }

    pub(crate) fn clear(&mut self) {
        self.elements.clear();
    }

    /// Ensure the storage can accomodate the given key. Values for keys that are between the
    /// current last key and the given key will be `default_value`.
    #[allow(dead_code)]
    pub(crate) fn accomodate(&mut self, key: Key, default_value: Value) {
        let idx = key.index();

        if idx >= self.elements.len() {
            self.elements.resize(idx + 1, default_value);
        }
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
pub(crate) trait StorageKey {
    fn index(&self) -> usize;
    #[allow(dead_code)]
    fn create_from_index(index: usize) -> Self;
}
