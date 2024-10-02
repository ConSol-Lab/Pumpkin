use pumpkin_lib::results::ProblemSolution;
use pyo3::prelude::*;

use crate::variables::BoolVariable;
use crate::variables::IntExpression;
use crate::variables::VariableMap;

#[pyclass]
#[allow(clippy::large_enum_variant)]
pub enum SatisfactionResult {
    Satisfiable(Solution),
    Unsatisfiable(),
    Unknown(),
}

#[pyclass]
#[derive(Clone)]
pub struct Solution {
    pub solver_solution: pumpkin_lib::results::Solution,
    pub variable_map: VariableMap,
}

#[pymethods]
impl Solution {
    fn int_value(&self, variable: IntExpression) -> i32 {
        self.solver_solution
            .get_integer_value(variable.to_affine_view(&self.variable_map))
    }

    fn bool_value(&self, variable: BoolVariable) -> bool {
        self.solver_solution
            .get_literal_value(self.variable_map.get_boolean(variable))
    }
}
