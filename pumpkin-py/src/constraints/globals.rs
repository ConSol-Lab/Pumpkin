use pyo3::pyfunction;

use crate::core::ConstraintDefinition;
use crate::core::Variable;

#[pyfunction]
pub fn all_different(variables: Vec<Variable>) -> ConstraintDefinition {
    let variables = variables.into_iter().map(|var| var.0).collect::<Vec<_>>();
    ConstraintDefinition(Box::new(pumpkin_lib::constraints::all_different(variables)))
}
