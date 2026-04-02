use pumpkin_solver::core::predicate;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::IntegerVariableEnum;
use pumpkin_solver::core::variables::Literal;
use pumpkin_solver::core::variables::TransformableVariable;
use pyo3::prelude::*;

#[pyclass(eq, hash, frozen)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntExpression(pub IntegerVariableEnum);

impl From<DomainId> for IntExpression {
    fn from(domain_id: DomainId) -> IntExpression {
        IntExpression(domain_id.into())
    }
}

impl From<Literal> for IntExpression {
    fn from(value: Literal) -> Self {
        IntExpression(value.into())
    }
}

impl From<BoolExpression> for IntExpression {
    fn from(value: BoolExpression) -> Self {
        IntExpression(value.0.into())
    }
}

#[pymethods]
impl IntExpression {
    fn offset(&self, add_offset: i32) -> IntExpression {
        IntExpression(self.0.offset(add_offset))
    }

    fn scaled(&self, scaling: i32) -> IntExpression {
        IntExpression(self.0.scaled(scaling))
    }
}

#[pyclass(eq, hash, frozen)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BoolExpression(pub Literal);

#[pymethods]
impl BoolExpression {
    pub fn scaled(&self, scale: i32) -> IntExpression {
        let int_expr: IntExpression = (*self).into();
        int_expr.scaled(scale)
    }

    pub fn offset(&self, offset: i32) -> IntExpression {
        let int_expr: IntExpression = (*self).into();
        int_expr.offset(offset)
    }

    pub fn negate(&self) -> BoolExpression {
        BoolExpression(!self.0)
    }

    pub fn as_expression(&self) -> IntExpression {
        (*self).into()
    }
}

impl From<Literal> for BoolExpression {
    fn from(literal: Literal) -> BoolExpression {
        BoolExpression(literal)
    }
}

#[pyclass(eq, eq_int, hash, frozen)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Comparator {
    NotEqual,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[pyclass(eq, get_all, hash, frozen)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub variable: IntExpression,
    pub comparator: Comparator,
    pub value: i32,
}

#[pymethods]
impl Predicate {
    #[new]
    fn new(variable: IntExpression, comparator: Comparator, value: i32) -> Self {
        Self {
            variable,
            comparator,
            value,
        }
    }
}

impl Predicate {
    pub(crate) fn into_solver_predicate(self) -> pumpkin_solver::core::predicates::Predicate {
        let affine_view = self.variable.0;

        match self.comparator {
            Comparator::NotEqual => predicate![affine_view != self.value],
            Comparator::Equal => predicate![affine_view == self.value],
            Comparator::LessThanOrEqual => predicate![affine_view <= self.value],
            Comparator::GreaterThanOrEqual => predicate![affine_view >= self.value],
        }
    }
}
