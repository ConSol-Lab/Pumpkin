use pumpkin_solver::core::predicate;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::Literal;
use pumpkin_solver::core::variables::TransformableVariable;
use pyo3::prelude::*;

#[pyclass(eq, hash, frozen)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntExpression(pub AffineView<DomainId>);

impl From<DomainId> for IntExpression {
    fn from(domain_id: DomainId) -> IntExpression {
        IntExpression(domain_id.into())
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
    pub fn as_integer(&self) -> IntExpression {
        IntExpression(self.0.get_integer_variable())
    }

    pub fn negate(&self) -> BoolExpression {
        BoolExpression(!self.0)
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
