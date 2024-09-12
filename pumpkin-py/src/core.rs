use std::num::NonZero;

use pumpkin_lib::constraints::Constraint;
use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;
use pumpkin_lib::variables::TransformableVariable;
use pyo3::pyclass;
use pyo3::pymethods;

/// An integer variable.
#[pyclass]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Variable(pub(crate) AffineView<DomainId>);

#[pymethods]
impl Variable {
    fn offset(&self, offset: i32) -> Variable {
        Variable(self.0.offset(offset))
    }
}

/// A boolean variable.
#[pyclass]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Boolean(pub(crate) Literal);

/// An opaque constraint implementation.
#[pyclass(unsendable)]
pub struct ConstraintDefinition(pub(crate) Box<dyn Constraint>);

impl Clone for ConstraintDefinition {
    fn clone(&self) -> Self {
        ConstraintDefinition(self.0.boxed_clone())
    }
}

impl Constraint for ConstraintDefinition {
    fn post(
        &self,
        solver: &mut pumpkin_lib::Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        self.0.post(solver, tag)
    }

    fn implied_by(
        &self,
        solver: &mut pumpkin_lib::Solver,
        reification_literal: pumpkin_lib::variables::Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        self.0.implied_by(solver, reification_literal, tag)
    }

    fn boxed_clone(&self) -> Box<dyn Constraint> {
        self.0.boxed_clone()
    }
}
