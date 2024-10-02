use pumpkin_lib::containers::KeyedVec;
use pumpkin_lib::containers::StorageKey;
use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;
use pyo3::prelude::*;

#[pyclass]
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct IntVariable(usize);

impl StorageKey for IntVariable {
    fn index(&self) -> usize {
        self.0
    }

    fn create_from_index(index: usize) -> Self {
        IntVariable(index)
    }
}

#[pyclass]
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct BoolVariable(usize);

impl StorageKey for BoolVariable {
    fn index(&self) -> usize {
        self.0
    }

    fn create_from_index(index: usize) -> Self {
        BoolVariable(index)
    }
}

#[derive(Clone, Default)]
pub struct VariableMap {
    pub integers: KeyedVec<IntVariable, AffineView<DomainId>>,
    pub booleans: KeyedVec<BoolVariable, Literal>,
}

impl VariableMap {
    pub fn get_integer(&self, variable: IntVariable) -> AffineView<DomainId> {
        self.integers[variable]
    }

    pub fn get_boolean(&self, variable: BoolVariable) -> Literal {
        self.booleans[variable]
    }
}
