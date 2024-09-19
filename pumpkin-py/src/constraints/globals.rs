use pumpkin_lib::constraints;
use pyo3::pyclass;
use pyo3::pymethods;

use crate::core::Variable;

/// All variables have distinct values.
#[pyclass]
#[derive(Clone)]
pub struct AllDifferent(Vec<Variable>);

#[pymethods]
impl AllDifferent {
    #[new]
    fn new(variables: Vec<Variable>) -> Self {
        AllDifferent(variables)
    }
}

impl pumpkin_lib::constraints::Constraint for AllDifferent {
    fn post(
        self,
        solver: &mut pumpkin_lib::Solver,
        tag: Option<std::num::NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let variables = self.0.into_iter().map(|var| var.0).collect::<Vec<_>>();
        constraints::all_different(variables).post(solver, tag)
    }

    fn implied_by(
        self,
        solver: &mut pumpkin_lib::Solver,
        reification_literal: pumpkin_lib::variables::Literal,
        tag: Option<std::num::NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let variables = self.0.into_iter().map(|var| var.0).collect::<Vec<_>>();
        constraints::all_different(variables).implied_by(solver, reification_literal, tag)
    }
}
