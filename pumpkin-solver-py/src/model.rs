use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use pumpkin_solver::containers::KeyedVec;
use pumpkin_solver::optimisation::linear_sat_unsat::LinearSatUnsat;
use pumpkin_solver::optimisation::linear_unsat_sat::LinearUnsatSat;
use pumpkin_solver::optimisation::OptimisationDirection;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::predicate;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::results::SolutionReference;
use pumpkin_solver::termination::Indefinite;
use pumpkin_solver::termination::TerminationCondition;
use pumpkin_solver::termination::TimeBudget;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::Literal;
use pumpkin_solver::ConstraintOperationError;
use pumpkin_solver::DefaultBrancher;
use pumpkin_solver::Solver;
use pyo3::prelude::*;

use crate::constraints::Constraint;
use crate::optimisation::Direction;
use crate::optimisation::OptimisationResult;
use crate::optimisation::Optimiser;
use crate::result::SatisfactionResult;
use crate::result::SatisfactionUnderAssumptionsResult;
use crate::result::Solution;
use crate::variables::BoolExpression;
use crate::variables::BoolVariable;
use crate::variables::IntExpression;
use crate::variables::IntVariable;
use crate::variables::Predicate;
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
                integer_equivalent: Some(existing_equivalent),
                ..
            } => existing_equivalent,

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

    #[pyo3(signature = (predicate,  name=None))]
    fn predicate_as_boolean(&mut self, predicate: Predicate, name: Option<&str>) -> BoolExpression {
        self.boolean_variables
            .push(ModelBoolVar {
                name: name.map(|n| n.to_owned()),
                integer_equivalent: None,
                predicate: Some(predicate),
            })
            .into()
    }

    /// Add the given constraint to the model.
    #[pyo3(signature = (constraint))]
    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(ModelConstraint {
            constraint,
            premise: None,
        });
    }

    /// Add `premise -> constraint` to the model.
    #[pyo3(signature = (constraint, premise))]
    fn add_implication(&mut self, constraint: Constraint, premise: BoolExpression) {
        self.constraints.push(ModelConstraint {
            constraint,
            premise: Some(premise),
        });
    }

    #[pyo3(signature = (timeout=None,proof=None))]
    fn satisfy(&self, timeout: Option<f32>, proof: Option<PathBuf>) -> SatisfactionResult {
        let end_time = timeout.map(|secs| Instant::now() + Duration::from_secs_f32(secs));

        let solver_setup = self.create_solver(proof);

        let Ok((mut solver, variable_map)) = solver_setup else {
            return SatisfactionResult::Unsatisfiable();
        };

        let mut brancher = solver.default_brancher();
        let mut termination = get_termination(end_time);

        let result = match solver.satisfy(&mut brancher, &mut termination) {
            pumpkin_solver::results::SatisfactionResult::Satisfiable(satisfiable) => {
                SatisfactionResult::Satisfiable(Solution {
                    solver_solution: satisfiable.solution().into(),
                    variable_map,
                })
            }
            pumpkin_solver::results::SatisfactionResult::Unsatisfiable(_) => {
                SatisfactionResult::Unsatisfiable()
            }
            pumpkin_solver::results::SatisfactionResult::Unknown(_) => {
                SatisfactionResult::Unknown()
            }
        };

        result
    }

    #[pyo3(signature = (assumptions,timeout=None))]
    fn satisfy_under_assumptions(
        &self,
        assumptions: Vec<Predicate>,
        timeout: Option<f32>,
    ) -> SatisfactionUnderAssumptionsResult {
        let end_time = timeout.map(|secs| Instant::now() + Duration::from_secs_f32(secs));
        let solver_setup = self.create_solver(None);

        let Ok((mut solver, variable_map)) = solver_setup else {
            return SatisfactionUnderAssumptionsResult::Unsatisfiable();
        };

        let mut brancher = solver.default_brancher();
        let mut termination = get_termination(end_time);

        let solver_assumptions = assumptions
            .iter()
            .map(|pred| pred.to_solver_predicate(&variable_map))
            .collect::<Vec<_>>();

        let result = match solver.satisfy_under_assumptions(&mut brancher, &mut termination, &solver_assumptions) {
            pumpkin_solver::results::SatisfactionResultUnderAssumptions::Satisfiable(satisfiable) => {
                SatisfactionUnderAssumptionsResult::Satisfiable(Solution {
                    solver_solution: satisfiable.solution().into(),
                    variable_map,
                })
            }
            pumpkin_solver::results::SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(mut result) => {
                // Maarten: For now we assume that the core _must_ consist of the predicates that
                //     were the input to the solve call. In general this is not the case, e.g. when
                //     the assumptions can be semantically minized (the assumptions [y <= 1],
                //     [y >= 0] and [y != 0] will be compressed to [y == 1] which would end up in
                //     the core).
                //
                //     In the future, perhaps we should make the distinction between predicates and
                //     literals in the python wrapper as well. For now, this is the simplest way
                //     forward. I expect that the situation above almost never happens in practice.
                let core = result
                    .extract_core()
                    .iter()
                    .map(|predicate| assumptions
                         .iter()
                         .find(|pred| pred.to_solver_predicate(&variable_map) == *predicate)
                         .copied()
                         .expect("predicates in core must be part of the assumptions"))
                    .collect();

                SatisfactionUnderAssumptionsResult::UnsatisfiableUnderAssumptions(core)
            }
            pumpkin_solver::results::SatisfactionResultUnderAssumptions::Unsatisfiable(_) => {
                SatisfactionUnderAssumptionsResult::Unsatisfiable()
            }
            pumpkin_solver::results::SatisfactionResultUnderAssumptions::Unknown(_) => {
                SatisfactionUnderAssumptionsResult::Unknown()
            }
        };

        result
    }

    #[pyo3(signature = (objective, optimiser=Optimiser::LinearSatUnsat, direction=Direction::Minimise, proof=None, timeout=None))]
    fn optimise(
        &self,
        objective: IntExpression,
        optimiser: Optimiser,
        direction: Direction,
        proof: Option<PathBuf>,
        timeout: Option<f32>,
    ) -> OptimisationResult {
        let end_time = timeout.map(|secs| Instant::now() + Duration::from_secs_f32(secs));
        let solver_setup = self.create_solver(proof);

        let Ok((mut solver, variable_map)) = solver_setup else {
            return OptimisationResult::Unsatisfiable();
        };

        let mut brancher = solver.default_brancher();
        let mut termination = get_termination(end_time);

        let direction = match direction {
            Direction::Minimise => OptimisationDirection::Minimise,
            Direction::Maximise => OptimisationDirection::Maximise,
        };

        let objective = objective.to_affine_view(&variable_map);

        let callback: fn(&Solver, SolutionReference, &DefaultBrancher) = |_, _, _| {};

        let result = match optimiser {
            Optimiser::LinearSatUnsat => solver.optimise(
                &mut brancher,
                &mut termination,
                LinearSatUnsat::new(direction, objective, callback),
            ),
            Optimiser::LinearUnsatSat => solver.optimise(
                &mut brancher,
                &mut termination,
                LinearUnsatSat::new(direction, objective, callback),
            ),
        };

        match result {
            pumpkin_solver::results::OptimisationResult::Satisfiable(solution) => {
                OptimisationResult::Satisfiable(Solution {
                    solver_solution: solution,
                    variable_map,
                })
            }
            pumpkin_solver::results::OptimisationResult::Optimal(solution) => {
                OptimisationResult::Optimal(Solution {
                    solver_solution: solution,
                    variable_map,
                })
            }
            pumpkin_solver::results::OptimisationResult::Unsatisfiable => {
                OptimisationResult::Unsatisfiable()
            }
            pumpkin_solver::results::OptimisationResult::Unknown => OptimisationResult::Unknown(),
        }
    }
}

fn get_termination(end_time: Option<Instant>) -> Box<dyn TerminationCondition> {
    end_time
        .map(|end_time| end_time - Instant::now())
        .map(|duration| {
            Box::new(TimeBudget::starting_now(duration)) as Box<dyn TerminationCondition>
        })
        .unwrap_or(Box::new(Indefinite))
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
            } = constraint.clone();

            if let Some(premise) = premise {
                constraint.implied_by(solver, premise.to_literal(variable_map), variable_map)?;
            } else {
                constraint.post(solver, variable_map)?;
            }
        }

        Ok(())
    }

    fn create_solver(
        &self,
        proof: Option<PathBuf>,
    ) -> Result<(Solver, VariableMap), ConstraintOperationError> {
        let proof_log = proof
            .map(|path| ProofLog::cp(&path, true))
            .transpose()
            .map(|proof| proof.unwrap_or_default())
            .expect("failed to create proof file");

        let options = SolverOptions {
            proof_log,
            ..Default::default()
        };

        let mut solver = Solver::with_options(options);

        let variable_map = self
            .create_variable_map(&mut solver)
            .and_then(|variable_map| {
                self.post_constraints(&mut solver, &variable_map)?;
                Ok(variable_map)
            })?;

        Ok((solver, variable_map))
    }
}

#[derive(Clone)]
struct ModelConstraint {
    constraint: Constraint,
    premise: Option<BoolExpression>,
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
        let cs = solver.new_constraint_tag();

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

                solver.add_clause([!predicate_literal, int_eq_1], cs)?;
                solver.add_clause([predicate_literal, !int_eq_1], cs)?;

                solver.new_literal_for_predicate(int_eq_1, cs)
            }

            ModelBoolVar {
                integer_equivalent: Some(int_var),
                predicate: None,
                ..
            } => {
                let affine_view = variable_map.get_integer(*int_var);
                solver.new_literal_for_predicate(predicate![affine_view == 1], cs)
            }

            ModelBoolVar {
                predicate: Some(predicate),
                integer_equivalent: None,
                ..
            } => solver.new_literal_for_predicate(predicate.to_solver_predicate(variable_map), cs),

            ModelBoolVar {
                name: Some(name), ..
            } => solver.new_named_literal(name),

            ModelBoolVar { name: None, .. } => solver.new_literal(),
        };

        Ok(literal)
    }
}
