use crate::basic_types::StorageKey;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct PropositionalVariable {
    index: u32,
}

impl PropositionalVariable {
    pub fn new(index: u32) -> PropositionalVariable {
        PropositionalVariable { index }
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
