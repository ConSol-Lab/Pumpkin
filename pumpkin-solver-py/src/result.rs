use pumpkin_solver::core::results::ProblemSolution;
use pyo3::prelude::*;

use crate::variables::BoolExpression;
use crate::variables::IntExpression;
use crate::variables::Predicate;

#[pyclass(from_py_object)]
#[allow(
    clippy::large_enum_variant,
    reason = "These will not be stored in bulk, so not an issue."
)]
#[derive(Clone)]
pub(crate) enum SatisfactionResult {
    Satisfiable(Solution),
    Unsatisfiable(),
    Unknown(),
}

#[pyclass(from_py_object)]
#[allow(
    clippy::large_enum_variant,
    reason = "These will not be stored in bulk, so not an issue"
)]
#[derive(Clone)]
pub(crate) enum SatisfactionUnderAssumptionsResult {
    Satisfiable(Solution),
    UnsatisfiableUnderAssumptions(Vec<Predicate>),
    Unsatisfiable(),
    Unknown(),
}

#[pyclass(from_py_object)]
#[derive(Clone)]
pub(crate) struct Solution(pumpkin_solver::core::results::Solution);

impl From<pumpkin_solver::core::results::Solution> for Solution {
    fn from(value: pumpkin_solver::core::results::Solution) -> Self {
        Solution(value)
    }
}

impl From<pumpkin_solver::core::results::SolutionReference<'_>> for Solution {
    fn from(value: pumpkin_solver::core::results::SolutionReference<'_>) -> Self {
        Solution(value.into())
    }
}

#[pymethods]
impl Solution {
    fn int_value(&self, variable: IntExpression) -> i32 {
        self.0.get_integer_value(variable.0)
    }

    fn bool_value(&self, variable: BoolExpression) -> bool {
        self.0.get_literal_value(variable.0)
    }
}
