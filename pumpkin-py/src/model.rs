use std::num::NonZero;
use std::path::PathBuf;

use pumpkin_lib::containers::KeyedVec;
use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::predicate;
use pumpkin_lib::proof::Format;
use pumpkin_lib::proof::ProofLog;
use pumpkin_lib::termination::Indefinite;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;
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

#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Comparator {
    NotEqual,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
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
                integer_equivalent: None,
                predicate: None,
            })
            .into()
    }

    /// Get an integer variable for the given boolean.
    ///
    /// The integer is 1 if the boolean is `true`, and 0 if the boolean is `false`.
    fn boolean_as_integer(&mut self, boolean: BoolExpression) -> IntExpression {
        let bool_variable = boolean.get_variable();

        let int_variable = match self.boolean_variables[bool_variable] {
            // If there is already an integer associated with the boolean variable, don't create a
            // new one.
            ModelBoolVar {
                integer_equivalent: Some(variable),
                ..
            } => variable,

            // Create a new integer variable which is equivalent to this boolean variable.
            ModelBoolVar {
                ref name,
                integer_equivalent: None,
                ..
            } => self.integer_variables.push(ModelIntVar {
                lower_bound: 0,
                upper_bound: 1,
                name: name.clone(),
            }),
        };

        // Link the integer variable to the boolean variable.
        self.boolean_variables[bool_variable].integer_equivalent = Some(int_variable);

        // Convert the integer variable to an appropriate integer expression based on the polarity
        // of the boolean expression.
        let polarity = boolean.get_polarity();

        IntExpression {
            variable: int_variable,
            offset: if polarity { 0 } else { 1 },
            scale: if polarity { 1 } else { -1 },
        }
    }

    #[pyo3(signature = (integer, comparator, value, name=None))]
    fn predicate_as_boolean(
        &mut self,
        integer: IntExpression,
        comparator: Comparator,
        value: i32,
        name: Option<&str>,
    ) -> BoolExpression {
        self.boolean_variables
            .push(ModelBoolVar {
                name: name.map(|n| n.to_owned()),
                integer_equivalent: None,
                predicate: Some(Predicate {
                    integer,
                    comparator,
                    value,
                }),
            })
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

        let mut solver = Solver::with_options(options);

        let solver_setup = self
            .create_variable_map(&mut solver)
            .and_then(|variable_map| {
                self.post_constraints(&mut solver, &variable_map)?;
                Ok(variable_map)
            });

        let Ok(variable_map) = solver_setup else {
            return SatisfactionResult::Unsatisfiable();
        };

        let mut brancher = solver.default_brancher();

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
    fn create_variable_map(
        &self,
        solver: &mut Solver,
    ) -> Result<VariableMap, ConstraintOperationError> {
        let mut map = VariableMap::default();

        for model_int_var in self.integer_variables.iter() {
            let _ = map
                .integers
                .push(model_int_var.create_domain(solver).into());
        }

        for model_bool_var in self.boolean_variables.iter() {
            let _ = map
                .booleans
                .push(model_bool_var.create_literal(solver, &map)?);
        }

        Ok(map)
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

impl ModelIntVar {
    fn create_domain(&self, solver: &mut Solver) -> DomainId {
        match self.name {
            Some(ref name) => {
                solver.new_named_bounded_integer(self.lower_bound, self.upper_bound, name)
            }

            None => solver.new_bounded_integer(self.lower_bound, self.upper_bound),
        }
    }
}

struct ModelBoolVar {
    name: Option<String>,
    /// If present, this is the 0-1 integer variable which is 1 if this boolean is `true`, and
    /// 0 if this boolean is `false`.
    integer_equivalent: Option<IntVariable>,
    /// If present, this boolean is true iff the predicate holds.
    predicate: Option<Predicate>,
}

impl ModelBoolVar {
    /// Convert a boolean variable to a solver literal.
    fn create_literal(
        &self,
        solver: &mut Solver,
        variable_map: &VariableMap,
    ) -> Result<Literal, ConstraintOperationError> {
        let literal = match self {
            ModelBoolVar {
                integer_equivalent: Some(int_var),
                predicate: Some(predicate),
                ..
            } => {
                // In case the boolean corresponds to both a predicate and a 0-1 integer, we have to
                // enforce equality between the integer variable and the truth of the predicate.

                let affine_view = variable_map.get_integer(*int_var);
                let int_eq_1 = predicate![affine_view == 1];

                let predicate_literal = predicate.to_solver_predicate(variable_map);

                solver.add_clause([!predicate_literal, int_eq_1])?;
                solver.add_clause([predicate_literal, !int_eq_1])?;

                solver.new_literal_for_predicate(int_eq_1)
            }

            ModelBoolVar {
                integer_equivalent: Some(int_var),
                predicate: None,
                ..
            } => {
                let affine_view = variable_map.get_integer(*int_var);
                solver.new_literal_for_predicate(predicate![affine_view == 1])
            }

            ModelBoolVar {
                predicate: Some(predicate),
                integer_equivalent: None,
                ..
            } => solver.new_literal_for_predicate(predicate.to_solver_predicate(variable_map)),

            ModelBoolVar {
                name: Some(name), ..
            } => solver.new_named_literal(name),

            ModelBoolVar { name: None, .. } => solver.new_literal(),
        };

        Ok(literal)
    }
}

struct Predicate {
    integer: IntExpression,
    comparator: Comparator,
    value: i32,
}

impl Predicate {
    /// Convert the predicate in the model domain to a predicate in the solver domain.
    fn to_solver_predicate(
        &self,
        variable_map: &VariableMap,
    ) -> pumpkin_lib::predicates::Predicate {
        let affine_view = self.integer.to_affine_view(variable_map);

        match self.comparator {
            Comparator::NotEqual => predicate![affine_view != self.value],
            Comparator::Equal => predicate![affine_view == self.value],
            Comparator::LessThanOrEqual => predicate![affine_view <= self.value],
            Comparator::GreaterThanOrEqual => predicate![affine_view >= self.value],
        }
    }
}
