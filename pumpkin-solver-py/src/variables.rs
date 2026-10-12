use std::hash::Hash;
use std::hash::Hasher;

use pumpkin_solver::core::predicate;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::Literal;
use pumpkin_solver::core::variables::TransformableVariable;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

#[pyclass(eq, hash, frozen, from_py_object)]
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

/// A Boolean expression, which is the [`Literal`] and optionally a 0-1 integer variable that is
/// equal to it.
///
/// Equality and hashing only consider the literal.
#[pyclass(eq, hash, frozen, from_py_object)]
#[derive(Clone, Copy, Debug)]
pub struct BoolExpression(pub Literal, pub Option<AffineView<DomainId>>);

impl PartialEq for BoolExpression {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for BoolExpression {}

impl Hash for BoolExpression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[pymethods]
impl BoolExpression {
    /// Get the integer which is 1 if the boolean is `true`, and 0 if the boolean is `false`.
    ///
    /// Booleans created with `Model.predicate_as_boolean` do not have an integer; use
    /// `Model.boolean_as_integer` for those instead.
    pub fn as_integer(&self) -> PyResult<IntExpression> {
        self.1.map(IntExpression).ok_or_else(|| {
            PyValueError::new_err(
                "This boolean has no integer variable; use `Model.boolean_as_integer` instead",
            )
        })
    }

    pub fn negate(&self) -> BoolExpression {
        BoolExpression(!self.0, self.1.map(|integer| integer.scaled(-1).offset(1)))
    }
}

impl From<Literal> for BoolExpression {
    fn from(literal: Literal) -> BoolExpression {
        BoolExpression(literal, None)
    }
}

#[pyclass(eq, eq_int, hash, frozen, from_py_object)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Comparator {
    NotEqual,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[pyclass(eq, get_all, hash, frozen, from_py_object)]
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
