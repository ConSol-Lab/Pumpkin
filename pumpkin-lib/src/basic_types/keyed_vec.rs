use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

/// Structure for storing elements of type `Value`, the structure can only be indexed by structures
/// of type `Key`.
///
/// Almost all features of this structure require that `Key` implements the [StorageKey] trait.
///
/// # Example Usage:
/// ```rust
/// # use pumpkin_lib::basic_types::KeyedVec;
/// # use pumpkin_lib::basic_types::DomainId;
/// // We create a list of integers
/// let elements: Vec<i32> = vec![1, 2, 3];
/// // Now we create a structure which can be indexed by DomainId
/// let keyed_vec: KeyedVec<DomainId, i32> = KeyedVec::new(elements);
///
/// // In the end, it should be the case that a DomainId with ID 1 corresponds to the element `2`
/// assert_eq!(keyed_vec[DomainId::new(1)], 2);
/// ```
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

impl<Key: StorageKey, Value> KeyedVec<Key, Value> {
    pub fn new(elements: Vec<Value>) -> Self {
        KeyedVec {
            key: PhantomData,
            elements,
        }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, value: Value) {
        self.elements.push(value)
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Value> {
        self.elements.iter()
    }
}

impl<Key: StorageKey, Value: Clone> KeyedVec<Key, Value> {
    pub fn resize(&mut self, new_len: usize, value: Value) {
        self.elements.resize(new_len, value)
    }
}

impl<Key: StorageKey, Value> Index<Key> for KeyedVec<Key, Value> {
    type Output = Value;

    fn index(&self, index: Key) -> &Self::Output {
        &self.elements[index.index()]
    }
}

impl<Key: StorageKey, Value> IndexMut<Key> for KeyedVec<Key, Value> {
    fn index_mut(&mut self, index: Key) -> &mut Self::Output {
        &mut self.elements[index.index()]
    }
}

/// A simple trait which requires that the structures implementing this trait can generate an index.
pub trait StorageKey {
    fn index(&self) -> usize;
}
