use std::ops::Deref;
use std::ops::DerefMut;

/// A mutable version of [`RefOrOwned`].
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum RefMutOrOwned<'a, T> {
    Ref(&'a mut T),
    Owned(T),
}

impl<T> RefMutOrOwned<'_, T> {
    pub(crate) fn reborrow(&mut self) -> RefMutOrOwned<'_, T> {
        match self {
            RefMutOrOwned::Ref(ref_to_t) => RefMutOrOwned::Ref(ref_to_t),
            RefMutOrOwned::Owned(value) => RefMutOrOwned::Ref(value),
        }
    }
}

impl<T> From<T> for RefMutOrOwned<'_, T> {
    fn from(value: T) -> Self {
        RefMutOrOwned::Owned(value)
    }
}

impl<T> Deref for RefMutOrOwned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            RefMutOrOwned::Ref(reference) => reference,
            RefMutOrOwned::Owned(value) => value,
        }
    }
}

impl<T> DerefMut for RefMutOrOwned<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            RefMutOrOwned::Ref(reference) => reference,
            RefMutOrOwned::Owned(value) => value,
        }
    }
}
