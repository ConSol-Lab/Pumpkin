use std::num::NonZero;
use std::path::PathBuf;

use pumpkin_lib::containers::KeyedVec;
use pumpkin_lib::options::LearningOptions;
use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::predicate;
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
    boolean_variables: KeyedVec<BoolVariable, ModelBoolVar>,
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
            .push(ModelBoolVar {
                name: name.map(|n| n.to_owned()),
                integer: None,
            })
            .into()
    }

    /// Get an integer variable for the given boolean.
    ///
    /// The integer is 1 if the boolean is `true`, and 0 if the boolean is `false`.
    fn boolean_as_integer(&mut self, boolean: BoolExpression) -> IntExpression {
        let bool_variable = boolean.get_variable();
        let polarity = boolean.get_polarity();

        if let Some(int_expr) = self.boolean_variables[bool_variable].integer {
            return int_expr;
        }

        let name = self.boolean_variables[bool_variable].name.clone();
        let mut int_expr = self.new_integer_variable(0, 1, name.as_deref());
        if !polarity {
            int_expr.scale = -1;
            int_expr.offset = 1;
        }

        self.boolean_variables[bool_variable].integer = Some(int_expr);

        int_expr
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

        for ModelBoolVar { name, integer } in self.boolean_variables.iter() {
            let literal = match (integer, name) {
                (Some(int_expr), _) => {
                    let solver_var = int_expr.to_affine_view(&map);
                    solver.get_literal(predicate![solver_var == 1])
                }

                (None, Some(name)) => solver.new_named_literal(name),
                (None, None) => solver.new_literal(),
            };

            map.booleans.push(literal);
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

struct ModelBoolVar {
    name: Option<String>,
    integer: Option<IntExpression>,
}
