use pumpkin_lib::containers::KeyedVec;
use pumpkin_lib::containers::StorageKey;
use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;
use pumpkin_lib::variables::TransformableVariable;
use pyo3::prelude::*;

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
pub struct IntExpression {
    pub variable: IntVariable,
    pub offset: i32,
    pub scale: i32,
}

impl From<IntVariable> for IntExpression {
    fn from(variable: IntVariable) -> Self {
        IntExpression {
            variable,
            offset: 0,
            scale: 1,
        }
    }
}

impl IntExpression {
    pub fn to_affine_view(self, variable_map: &VariableMap) -> AffineView<DomainId> {
        let IntExpression {
            variable,
            offset,
            scale,
        } = self;

        variable_map
            .get_integer(variable)
            .scaled(scale)
            .offset(offset)
    }
}

#[pymethods]
impl IntExpression {
    fn offset(&self, add_offset: i32) -> IntExpression {
        let IntExpression {
            variable,
            offset,
            scale,
        } = *self;

        IntExpression {
            variable,
            offset: offset + add_offset,
            scale,
        }
    }

    fn scaled(&self, scaling: i32) -> IntExpression {
        let IntExpression {
            variable,
            offset,
            scale,
        } = *self;

        IntExpression {
            variable,
            offset: offset * scaling,
            scale: scale * scaling,
        }
    }
}

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

#[pyclass]
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct BoolExpression(BoolVariable, bool);

impl From<BoolVariable> for BoolExpression {
    fn from(value: BoolVariable) -> Self {
        BoolExpression(value, true)
    }
}

impl BoolExpression {
    pub fn to_literal(self, variable_map: &VariableMap) -> Literal {
        let literal = variable_map.get_boolean(self.0);

        if self.1 {
            literal
        } else {
            !literal
        }
    }
}

#[pymethods]
impl BoolExpression {
    fn negate(&self) -> Self {
        BoolExpression(self.0, !self.1)
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
