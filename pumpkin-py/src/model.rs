use std::num::NonZero;
use std::path::PathBuf;

use pumpkin_lib::containers::KeyedVec;
use pumpkin_lib::options::LearningOptions;
use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::proof::Format;
use pumpkin_lib::proof::ProofLog;
use pumpkin_lib::termination::Indefinite;
use pumpkin_lib::ConstraintOperationError;
use pumpkin_lib::Solver;
use pyo3::prelude::*;

use crate::constraints::Constraint;
use crate::result::SatisfactionResult;
use crate::result::Solution;
use crate::variables::BoolExpression;
use crate::variables::BoolVariable;
use crate::variables::IntExpression;
use crate::variables::IntVariable;
use crate::variables::VariableMap;

#[pyclass]
#[derive(Default)]
pub struct Model {
    integer_variables: KeyedVec<IntVariable, ModelIntVar>,
    boolean_variables: KeyedVec<BoolVariable, Option<String>>,
    constraints: Vec<ModelConstraint>,
}

#[pymethods]
impl Model {
    #[new]
    fn new() -> Model {
        Model::default()
    }

    /// Create a new integer variable.
    #[pyo3(signature = (lower_bound, upper_bound, name=None))]
    fn new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<&str>,
    ) -> IntExpression {
        let variable = ModelIntVar {
            lower_bound,
            upper_bound,
            name: name.map(|n| n.to_owned()),
        };

        self.integer_variables.push(variable).into()
    }

    /// Create a new boolean variable.
    #[pyo3(signature = (name=None))]
    fn new_boolean_variable(&mut self, name: Option<&str>) -> BoolExpression {
        self.boolean_variables
            .push(name.map(|n| n.to_owned()))
            .into()
    }

    /// Add the given constraint to the model.
    #[pyo3(signature = (constraint, tag=None))]
    fn add_constraint(&mut self, constraint: Constraint, tag: Option<NonZero<u32>>) {
        self.constraints.push(ModelConstraint {
            constraint,
            premise: None,
            tag,
        });
    }

    /// Add `premise -> constraint` to the model.
    #[pyo3(signature = (constraint, premise, tag=None))]
    fn add_implication(
        &mut self,
        constraint: Constraint,
        premise: BoolExpression,
        tag: Option<NonZero<u32>>,
    ) {
        self.constraints.push(ModelConstraint {
            constraint,
            premise: Some(premise),
            tag,
        });
    }

    #[pyo3(signature = (proof=None))]
    fn satisfy(&self, proof: Option<PathBuf>) -> SatisfactionResult {
        let proof_log = proof
            .map(|path| ProofLog::cp(&path, Format::Text, true, true))
            .transpose()
            .map(|proof| proof.unwrap_or_default())
            .expect("failed to create proof file");

        let options = SolverOptions {
            proof_log,
            ..Default::default()
        };

        let mut solver = Solver::with_options(LearningOptions::default(), options);
        let variable_map = self.create_variable_map(&mut solver);

        if self.post_constraints(&mut solver, &variable_map).is_err() {
            return SatisfactionResult::Unsatisfiable();
        }

        let mut brancher = solver.default_brancher_over_all_propositional_variables();

        match solver.satisfy(&mut brancher, &mut Indefinite) {
            pumpkin_lib::results::SatisfactionResult::Satisfiable(solution) => {
                SatisfactionResult::Satisfiable(Solution {
                    solver_solution: solution,
                    variable_map,
                })
            }
            pumpkin_lib::results::SatisfactionResult::Unsatisfiable => {
                SatisfactionResult::Unsatisfiable()
            }
            pumpkin_lib::results::SatisfactionResult::Unknown => SatisfactionResult::Unknown(),
        }
    }
}

impl Model {
    fn create_variable_map(&self, solver: &mut Solver) -> VariableMap {
        let mut map = VariableMap::default();

        for ModelIntVar {
            lower_bound,
            upper_bound,
            name,
        } in self.integer_variables.iter()
        {
            let _ = map.integers.push(if let Some(name) = name {
                solver
                    .new_named_bounded_integer(*lower_bound, *upper_bound, name)
                    .into()
            } else {
                solver
                    .new_bounded_integer(*lower_bound, *upper_bound)
                    .into()
            });
        }

        for name in self.boolean_variables.iter() {
            let _ = map.booleans.push(if let Some(name) = name {
                solver.new_named_literal(name)
            } else {
                solver.new_literal()
            });
        }

        map
    }

    fn post_constraints(
        &self,
        solver: &mut Solver,
        variable_map: &VariableMap,
    ) -> Result<(), ConstraintOperationError> {
        for constraint in self.constraints.iter() {
            let ModelConstraint {
                constraint,
                premise,
                tag,
            } = constraint.clone();

            if let Some(premise) = premise {
                constraint.implied_by(
                    solver,
                    premise.to_literal(variable_map),
                    tag,
                    variable_map,
                )?;
            } else {
                constraint.post(solver, tag, variable_map)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
struct ModelConstraint {
    constraint: Constraint,
    premise: Option<BoolExpression>,
    tag: Option<NonZero<u32>>,
}

struct ModelIntVar {
    lower_bound: i32,
    upper_bound: i32,
    name: Option<String>,
}
