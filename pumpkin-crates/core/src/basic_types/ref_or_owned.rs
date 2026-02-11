use std::ops::Deref;
use std::ops::DerefMut;

/// Either owns a value or has a mutable reference to a value.
///
/// Used to store data in a reborrowed context that needs to be 'shared' with the original context
/// that was reborrowed from. For example, when dropping a reborrowed context, we want
/// [`PropagatorConstructorContext::get_next_local_id`] in the original context to 'know' about the
/// registered local ids in the reborrowed context.
#[derive(Debug)]
pub(crate) enum RefOrOwned<'a, T> {
    Ref(&'a mut T),
    Owned(T),
}

impl<T> RefOrOwned<'_, T> {
    pub(crate) fn reborrow(&mut self) -> RefOrOwned<'_, T> {
        match self {
            RefOrOwned::Ref(ref_to_t) => RefOrOwned::Ref(ref_to_t),
            RefOrOwned::Owned(value) => RefOrOwned::Ref(value),
        }
    }
}

impl<T> From<T> for RefOrOwned<'_, T> {
    fn from(value: T) -> Self {
        RefOrOwned::Owned(value)
    }
}

impl<T> Deref for RefOrOwned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            RefOrOwned::Ref(reference) => reference,
            RefOrOwned::Owned(value) => value,
        }
    }
}

impl<T> DerefMut for RefOrOwned<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            RefOrOwned::Ref(reference) => reference,
            RefOrOwned::Owned(value) => value,
        }
    }
}
