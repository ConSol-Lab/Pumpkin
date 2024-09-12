use std::num::NonZero;

use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::termination::Indefinite;
use pyo3::prelude::*;

use crate::constraints::ConstraintDefinition;
use crate::core::Boolean;
use crate::core::Variable;

#[pyclass(unsendable)]
pub struct Solver {
    solver: pumpkin_lib::Solver,
}

#[pymethods]
impl Solver {
    #[new]
    fn new() -> Self {
        Solver {
            solver: pumpkin_lib::Solver::default(),
        }
    }

    fn new_variable(&mut self, lower_bound: i32, upper_bound: i32) -> Variable {
        Variable(
            self.solver
                .new_bounded_integer(lower_bound, upper_bound)
                .into(),
        )
    }

    #[pyo3(signature = (constraint, tag=None))]
    fn post(&mut self, constraint: ConstraintDefinition, tag: Option<NonZero<u32>>) -> bool {
        let mut poster = self.solver.add_constraint(constraint);

        if let Some(tag) = tag {
            poster = poster.with_tag(tag);
        }

        poster.post().is_ok()
    }

    #[pyo3(signature = (constraint, reification, tag=None))]
    fn imply(
        &mut self,
        constraint: ConstraintDefinition,
        reification: Boolean,
        tag: Option<NonZero<u32>>,
    ) -> bool {
        let mut poster = self.solver.add_constraint(constraint);

        if let Some(tag) = tag {
            poster = poster.with_tag(tag);
        }

        poster.implied_by(reification.0).is_ok()
    }

    fn satisfy(&mut self) -> SatisfactionResult {
        let mut brancher = self
            .solver
            .default_brancher_over_all_propositional_variables();

        match self.solver.satisfy(&mut brancher, &mut Indefinite) {
            pumpkin_lib::results::SatisfactionResult::Satisfiable(solution) => {
                SatisfactionResult::Satisfiable(Solution(solution))
            }
            pumpkin_lib::results::SatisfactionResult::Unsatisfiable => {
                SatisfactionResult::Unsatisfiable()
            }
            pumpkin_lib::results::SatisfactionResult::Unknown => SatisfactionResult::Unknown(),
        }
    }
}

#[pyclass]
#[allow(clippy::large_enum_variant)]
pub enum SatisfactionResult {
    Satisfiable(Solution),
    Unsatisfiable(),
    Unknown(),
}

#[pyclass]
#[derive(Clone)]
pub struct Solution(pumpkin_lib::results::Solution);

#[pymethods]
impl Solution {
    fn value(&self, variable: Variable) -> i32 {
        self.0.get_integer_value(variable.0)
    }
}
