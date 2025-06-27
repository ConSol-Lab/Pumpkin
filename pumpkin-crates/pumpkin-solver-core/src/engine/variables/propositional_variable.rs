use crate::basic_types::StorageKey;
#[cfg(doc)]
use crate::variables::Literal;

/// A boolean variable in the solver; unlike [`Literal`], this representation does not use a
/// polarity.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct PropositionalVariable {
    index: u32,
}

impl PropositionalVariable {
    pub fn new(index: u32) -> PropositionalVariable {
        PropositionalVariable { index }
    }

    pub fn get_index(&self) -> u32 {
        self.index
    }
}

impl std::fmt::Display for PropositionalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "p{}", self.index)
    }
}

impl StorageKey for PropositionalVariable {
    fn index(&self) -> usize {
        self.index as usize
    }

    fn create_from_index(index: usize) -> Self {
        PropositionalVariable::new(index as u32)
    }
}
