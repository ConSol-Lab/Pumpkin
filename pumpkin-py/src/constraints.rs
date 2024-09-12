use std::num::NonZero;

use pumpkin_lib::constraints::{self};
use pumpkin_lib::constraints::{Constraint, NegatableConstraint};
use pyo3::prelude::*;

use crate::core::Variable;

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
        todo!()
    }
}

#[pyfunction]
fn all_different(variables: Vec<Variable>) -> ConstraintDefinition {
    let variables = variables.into_iter().map(|var| var.0).collect::<Vec<_>>();
    ConstraintDefinition(Box::new(constraints::all_different(variables)))
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(all_different, m)?)?;
    m.add_class::<ConstraintDefinition>()?;
    Ok(())
}
