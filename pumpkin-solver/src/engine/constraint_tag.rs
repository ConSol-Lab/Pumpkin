use std::num::NonZero;

use crate::containers::StorageKey;
#[cfg(doc)]
use crate::Solver;

/// An identifier for constraints, which is used to relate constraints from the model to steps in
/// the proof. Under the hood, a tag is just a [`NonZero<u32>`]. The underlying integer can be
/// obtained through the [`Into`] implementation.
///
/// Constraint tags cannot be created by users of Pumpkin. Instead, they are created through
/// [`Solver::new_constraint_tag()`]. This is a concious decision, as derived constraints will also
/// need to be tagged, which means the solver has to be responsible for maintaining their
/// uniqueness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstraintTag(NonZero<u32>);

impl From<ConstraintTag> for NonZero<u32> {
    fn from(value: ConstraintTag) -> Self {
        value.0
    }
}

impl ConstraintTag {
    /// Create a new tag directly.
    ///
    /// *Note*: Be careful when doing this. Regular construction should only be done through the
    /// constraint satisfaction solver. It is important that constraint tags remain unique.
    pub(crate) fn from_non_zero(non_zero: NonZero<u32>) -> ConstraintTag {
        ConstraintTag(non_zero)
    }
}

impl StorageKey for ConstraintTag {
    fn index(&self) -> usize {
        self.0.get() as usize - 1
    }

    fn create_from_index(index: usize) -> Self {
        Self::from_non_zero(
            NonZero::new(index as u32 + 1).expect("the '+ 1' ensures the value is non-zero"),
        )
    }
}
