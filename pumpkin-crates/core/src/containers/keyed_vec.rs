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
    /// Pop the last element from the vec.
    pub(crate) fn pop(&mut self) -> Option<(Key, Value)> {
        self.elements.pop().map(|value| {
            let key = Key::create_from_index(self.elements.len());
            (key, value)
        })
    }

    pub(crate) fn len(&self) -> usize {
        self.elements.len()
    }

    /// Add a new value to the vector.
    ///
    /// Returns the key for the inserted value.
    pub fn push(&mut self, value: Value) -> Key {
        self.elements.push(value);

        Key::create_from_index(self.elements.len() - 1)
    }

    /// Create a new slot for a value, and populate it using [`Slot::populate()`].
    ///
    /// This allows initializing the value with the ID it will have in this vector.
    ///
    /// # Example
    /// ```
    /// # use pumpkin_solver::containers::StorageKey;
    /// # use pumpkin_solver::containers::KeyedVec;
    /// #[derive(Clone)]
    /// struct Key(usize);
    ///
    /// impl StorageKey for Key {
    ///     // ...
    /// #   fn create_from_index(index: usize) -> Self {
    /// #       Key(index)
    /// #   }
    /// #
    /// #   fn index(&self) -> usize {
    /// #       self.0
    /// #   }
    /// }
    ///
    /// struct Value;
    ///
    /// /// Create a value based on the specified key.
    /// fn create_value(key: Key) -> Value {
    ///     // ...
    /// #   Value
    /// }
    ///
    /// let mut keyed_vec: KeyedVec<Key, Value> = KeyedVec::default();
    ///
    /// // Reserve a slot.
    /// let slot = keyed_vec.new_slot();
    /// // Create the value.
    /// let value = create_value(slot.key());
    /// // Populate the slot.
    /// slot.populate(value);
    /// ```
    pub fn new_slot(&mut self) -> Slot<'_, Key, Value> {
        Slot { vec: self }
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

    pub(crate) fn accomodate(&mut self, key: Key, default_value: Value) {
        self.elements.resize(key.index() + 1, default_value);
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

impl StorageKey for u32 {
    fn index(&self) -> usize {
        *self as usize
    }

    fn create_from_index(index: usize) -> Self {
        index as u32
    }
}

/// A simple trait which requires that the structures implementing this trait can generate an index.
pub trait StorageKey: Clone {
    fn index(&self) -> usize;

    fn create_from_index(index: usize) -> Self;
}

/// A reserved slot for a new value in a [`KeyedVec`].
#[derive(Debug)]
pub struct Slot<'a, Key, Value> {
    vec: &'a mut KeyedVec<Key, Value>,
}

impl<Key: StorageKey, Value> Slot<'_, Key, Value> {
    /// The key this slot has.
    pub fn key(&self) -> Key {
        Key::create_from_index(self.vec.len())
    }

    /// Populate the slot with a value.
    pub fn populate(self, value: Value) -> Key {
        self.vec.push(value)
    }
}
