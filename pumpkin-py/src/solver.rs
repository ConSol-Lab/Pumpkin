use std::num::NonZero;
use std::path::PathBuf;

use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::proof::Format;
use pumpkin_lib::proof::ProofLog;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::termination::Indefinite;
use pyo3::prelude::*;

use crate::constraints::Constraint;
use crate::core::Boolean;
use crate::core::Variable;

#[pyclass(unsendable)]
pub struct Solver {
    solver: pumpkin_lib::Solver,
}

#[pymethods]
impl Solver {
    #[new]
    #[pyo3(signature = (proof=None))]
    fn new(proof: Option<PathBuf>) -> Self {
        let proof_log = proof
            .map(|path| ProofLog::cp(&path, Format::Text, true))
            .transpose()
            .map(|proof| proof.unwrap_or_default())
            .expect("failed to create proof file");

        let options = SolverOptions {
            proof_log,
            ..Default::default()
        };

        Solver {
            solver: pumpkin_lib::Solver::with_options(options),
        }
    }

    /// Create a new integer variable.
    #[pyo3(signature = (lower_bound, upper_bound, name=None))]
    fn new_variable(&mut self, lower_bound: i32, upper_bound: i32, name: Option<&str>) -> Variable {
        let domain = match name {
            Some(name) => self
                .solver
                .new_named_bounded_integer(lower_bound, upper_bound, name),
            None => self.solver.new_bounded_integer(lower_bound, upper_bound),
        };

        Variable(domain.into())
    }

    /// Create a new boolean variable.
    #[pyo3(signature = (name=None))]
    fn new_boolean(&mut self, name: Option<&str>) -> Boolean {
        let literal = match name {
            Some(name) => self.solver.new_named_literal(name),
            None => self.solver.new_literal(),
        };

        Boolean(literal)
    }

    /// Post a constraint to the solver.
    #[pyo3(signature = (constraint, tag=None))]
    fn post(&mut self, constraint: Constraint, tag: Option<NonZero<u32>>) -> bool {
        let mut poster = self.solver.add_constraint(constraint);

        if let Some(tag) = tag {
            poster = poster.with_tag(tag);
        }

        poster.post().is_ok()
    }

    /// Post a constraint that is implied by a boolean variable.
    ///
    /// With this API, the solver can decide to "turn off" the constraint by setting `reification`
    /// to false.
    #[pyo3(signature = (constraint, reification, tag=None))]
    fn imply(
        &mut self,
        constraint: Constraint,
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
        let mut brancher = self.solver.default_brancher();

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
