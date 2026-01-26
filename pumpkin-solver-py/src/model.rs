use std::num::NonZero;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use pumpkin_conflict_resolvers::resolvers::ResolutionResolver;
use pumpkin_solver::Solver;
use pumpkin_solver::core::DefaultBrancher;
use pumpkin_solver::core::branching::Brancher;
use pumpkin_solver::core::branching::branchers::warm_start::WarmStart;
use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::containers::StorageKey;
use pumpkin_solver::core::optimisation::OptimisationDirection;
use pumpkin_solver::core::optimisation::linear_sat_unsat::LinearSatUnsat;
use pumpkin_solver::core::optimisation::linear_unsat_sat::LinearUnsatSat;
use pumpkin_solver::core::options::SolverOptions;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::proof::ProofLog;
use pumpkin_solver::core::rand::SeedableRng;
use pumpkin_solver::core::rand::rngs::SmallRng;
use pumpkin_solver::core::results::SolutionReference;
use pumpkin_solver::core::termination::Indefinite;
use pumpkin_solver::core::termination::TerminationCondition;
use pumpkin_solver::core::termination::TimeBudget;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;

use crate::constraints::Constraint;
use crate::optimisation::Direction;
use crate::optimisation::OptimisationResult;
use crate::optimisation::Optimiser;
use crate::result::SatisfactionResult;
use crate::result::SatisfactionUnderAssumptionsResult;
use crate::result::Solution;
use crate::variables::BoolExpression;
use crate::variables::IntExpression;
use crate::variables::Predicate;

#[pyclass(unsendable)]
pub struct Model {
    solver: Solver,
    brancher: PythonBrancher,
}

#[pyclass]
#[derive(Clone, Debug)]
pub struct Tag(pub ConstraintTag);

#[pymethods]
impl Tag {
    fn __int__(&self) -> u32 {
        let non_zero: NonZero<u32> = self.0.into();
        non_zero.get()
    }
}

impl StorageKey for Tag {
    fn index(&self) -> usize {
        self.0.index()
    }

    fn create_from_index(index: usize) -> Self {
        Tag(ConstraintTag::create_from_index(index))
    }
}

#[pymethods]
impl Model {
    #[new]
    #[pyo3(signature = (proof=None, seed=None))]
    fn new(proof: Option<PathBuf>, seed: Option<u64>) -> PyResult<Model> {
        let proof_log = proof
            .map(|path| ProofLog::cp(&path, true))
            .transpose()
            .map(|proof| proof.unwrap_or_default())?;

        let options = SolverOptions {
            proof_log,
            // TODO: The solver options should probably be refactored to accept a seed instead the
            // the number generator instance. Now we cannot have a solver default that is shared
            // between the rust and python interfaces without duplicating the default seed as done
            // now.
            random_generator: SmallRng::seed_from_u64(seed.unwrap_or(42)),
            ..Default::default()
        };

        let solver = Solver::with_options(options);
        let brancher = PythonBrancher {
            warm_start: WarmStart::new(&[], &[]),
            default_brancher: solver.default_brancher(),
        };

        Ok(Model { solver, brancher })
    }

    /// Create a new integer variable.
    #[pyo3(signature = (lower_bound, upper_bound, name=None))]
    fn new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: Option<&str>,
    ) -> IntExpression {
        let domain_id = if let Some(name) = name {
            self.solver
                .new_named_bounded_integer(lower_bound, upper_bound, name)
        } else {
            self.solver.new_bounded_integer(lower_bound, upper_bound)
        };

        self.brancher.default_brancher.add_domain(domain_id);

        domain_id.into()
    }

    /// Create a new boolean variable.
    #[pyo3(signature = (name=None))]
    fn new_boolean_variable(&mut self, name: Option<&str>) -> BoolExpression {
        let literal = if let Some(name) = name {
            self.solver.new_named_literal(name)
        } else {
            self.solver.new_literal()
        };

        self.brancher
            .default_brancher
            .add_domain(literal.get_true_predicate().get_domain());

        literal.into()
    }

    /// Create a new constraint tag.
    fn new_constraint_tag(&mut self) -> Tag {
        Tag(self.solver.new_constraint_tag())
    }

    /// Get an integer variable for the given boolean.
    ///
    /// The integer is 1 if the boolean is `true`, and 0 if the boolean is `false`.
    ///
    /// A tag should be provided for this link to be identifiable in the proof.
    fn boolean_as_integer(&mut self, boolean: BoolExpression, tag: Tag) -> IntExpression {
        let new_domain = self.solver.new_bounded_integer(0, 1);
        self.brancher.default_brancher.add_domain(new_domain);

        let boolean_true = boolean.0.get_true_predicate();

        self.solver
            .add_clause([predicate![new_domain != 1], boolean_true], tag.0)
            .expect("created a new domain so this should never cause an empty domain");
        self.solver
            .add_clause([!boolean_true, predicate![new_domain == 1]], tag.0)
            .expect("created a new domain so this should never cause an empty domain");

        IntExpression::from(new_domain)
    }

    /// Reify a predicate as an explicit boolean expression.
    ///
    /// A tag should be provided for this link to be identifiable in the proof.
    #[pyo3(signature = (predicate, tag, name=None))]
    fn predicate_as_boolean(
        &mut self,
        predicate: Predicate,
        tag: Tag,
        name: Option<&str>,
    ) -> BoolExpression {
        let solver_predicate = predicate.into_solver_predicate();
        let Tag(tag) = tag;

        if let Some(name) = name {
            self.solver
                .new_named_literal_for_predicate(solver_predicate, tag, name)
                .into()
        } else {
            self.solver
                .new_literal_for_predicate(solver_predicate, tag)
                .into()
        }
    }

    /// Add the given constraint to the model.
    #[pyo3(signature = (constraint))]
    fn add_constraint(&mut self, constraint: Constraint) -> PyResult<()> {
        constraint
            .post(&mut self.solver)
            .map_err(|_| PyRuntimeError::new_err("inconsistency detected"))
    }

    /// Add `premise -> constraint` to the model.
    #[pyo3(signature = (constraint, premise))]
    fn add_implication(&mut self, constraint: Constraint, premise: BoolExpression) -> PyResult<()> {
        constraint
            .implied_by(&mut self.solver, premise.0)
            .map_err(|_| PyRuntimeError::new_err("inconsistency detected"))
    }

    #[pyo3(signature = (timeout=None))]
    fn satisfy(&mut self, timeout: Option<f32>) -> SatisfactionResult {
        let mut termination = get_termination(timeout);
        let mut resolver = ResolutionResolver::default();

        match self
            .solver
            .satisfy(&mut self.brancher, &mut termination, &mut resolver)
        {
            pumpkin_solver::core::results::SatisfactionResult::Satisfiable(satisfiable) => {
                SatisfactionResult::Satisfiable(Solution::from(satisfiable.solution()))
            }
            pumpkin_solver::core::results::SatisfactionResult::Unsatisfiable(_, _, _) => {
                SatisfactionResult::Unsatisfiable()
            }
            pumpkin_solver::core::results::SatisfactionResult::Unknown(_, _, _) => {
                SatisfactionResult::Unknown()
            }
        }
    }

    #[pyo3(signature = (assumptions,timeout=None))]
    fn satisfy_under_assumptions(
        &mut self,
        assumptions: Vec<Predicate>,
        timeout: Option<f32>,
    ) -> SatisfactionUnderAssumptionsResult {
        let mut termination = get_termination(timeout);
        let mut resolver = ResolutionResolver::default();

        let solver_assumptions = assumptions
            .iter()
            .map(|pred| pred.into_solver_predicate())
            .collect::<Vec<_>>();

        match self.solver.satisfy_under_assumptions(&mut self.brancher, &mut termination, &mut resolver, &solver_assumptions) {
            pumpkin_solver::core::results::SatisfactionResultUnderAssumptions::Satisfiable(satisfiable) => {
                SatisfactionUnderAssumptionsResult::Satisfiable(satisfiable.solution().into())
            }
            pumpkin_solver::core::results::SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(mut result) => {
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
                         .find(|pred| pred.into_solver_predicate() == *predicate)
                         .copied()
                         .expect("predicates in core must be part of the assumptions"))
                    .collect();

                SatisfactionUnderAssumptionsResult::UnsatisfiableUnderAssumptions(core)
            }
            pumpkin_solver::core::results::SatisfactionResultUnderAssumptions::Unsatisfiable(_) => {
                SatisfactionUnderAssumptionsResult::Unsatisfiable()
            }
            pumpkin_solver::core::results::SatisfactionResultUnderAssumptions::Unknown(_) => {
                SatisfactionUnderAssumptionsResult::Unknown()
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "this is common in many Python APIs"
    )]
    #[pyo3(signature = (
        objective,
        optimiser=Optimiser::LinearSatUnsat,
        direction=Direction::Minimise,
        timeout=None,
        on_solution=None,
        warm_start=HashMap::default(),
    ))]
    fn optimise(
        &mut self,
        py: Python<'_>,
        objective: IntExpression,
        optimiser: Optimiser,
        direction: Direction,
        timeout: Option<f32>,
        on_solution: Option<Py<PyAny>>,
        warm_start: HashMap<IntExpression, i32>,
    ) -> PyResult<OptimisationResult> {
        let mut termination = get_termination(timeout);
        let mut resolver = ResolutionResolver::default();

        let direction = match direction {
            Direction::Minimise => OptimisationDirection::Minimise,
            Direction::Maximise => OptimisationDirection::Maximise,
        };

        let objective = objective.0;

        let callback = move |_: &Solver,
                             solution: SolutionReference<'_>,
                             _: &PythonBrancher,
                             _: &ResolutionResolver| {
            let python_solution = crate::result::Solution::from(solution);

            // If there is a solution callback, unpack it.
            let Some(on_solution_callback) = on_solution.as_ref() else {
                return ControlFlow::Continue(());
            };

            // Call the callback, and if there is an error, unpack it.
            let Err(err) = on_solution_callback.call(py, (python_solution,), None) else {
                return ControlFlow::Continue(());
            };

            // Stop optimising and return the error.
            ControlFlow::Break(err)
        };

        self.update_warm_start(warm_start);

        let result = match optimiser {
            Optimiser::LinearSatUnsat => self.solver.optimise(
                &mut self.brancher,
                &mut termination,
                &mut resolver,
                LinearSatUnsat::new(direction, objective, callback),
            ),
            Optimiser::LinearUnsatSat => self.solver.optimise(
                &mut self.brancher,
                &mut termination,
                &mut resolver,
                LinearUnsatSat::new(direction, objective, callback),
            ),
        };

        match result {
            pumpkin_solver::core::results::OptimisationResult::Stopped(_, err) => Err(err),
            pumpkin_solver::core::results::OptimisationResult::Satisfiable(solution) => {
                Ok(OptimisationResult::Satisfiable(solution.into()))
            }
            pumpkin_solver::core::results::OptimisationResult::Optimal(solution) => {
                Ok(OptimisationResult::Optimal(solution.into()))
            }
            pumpkin_solver::core::results::OptimisationResult::Unsatisfiable => {
                Ok(OptimisationResult::Unsatisfiable())
            }
            pumpkin_solver::core::results::OptimisationResult::Unknown => {
                Ok(OptimisationResult::Unknown())
            }
        }
    }
}

impl Model {
    /// Update the warm start in the [`PythonBrancher`].
    fn update_warm_start(&mut self, warm_start: HashMap<IntExpression, i32>) {
        // First create the slice of variables to give to the WarmStart brancher.
        let warm_start_variables: Vec<_> = warm_start.keys().map(|variable| variable.0).collect();

        // For every variable collect the value into another slice.
        let warm_start_values: Vec<_> = warm_start_variables
            .iter()
            .map(|variable| {
                warm_start
                    .get(&IntExpression(*variable))
                    .copied()
                    .expect("all elements are keys")
            })
            .collect();

        self.brancher.warm_start = WarmStart::new(&warm_start_variables, &warm_start_values);
    }
}

fn get_termination(end_time: Option<f32>) -> Box<dyn TerminationCondition> {
    end_time
        .map(|secs| Instant::now() + Duration::from_secs_f32(secs))
        .map(|end_time| end_time - Instant::now())
        .map(|duration| {
            Box::new(TimeBudget::starting_now(duration)) as Box<dyn TerminationCondition>
        })
        .unwrap_or(Box::new(Indefinite))
}

struct PythonBrancher {
    warm_start: WarmStart<AffineView<DomainId>>,
    default_brancher: DefaultBrancher,
}

impl Brancher for PythonBrancher {
    fn next_decision(
        &mut self,
        context: &mut pumpkin_solver::core::branching::SelectionContext,
    ) -> Option<pumpkin_solver::core::predicates::Predicate> {
        if let Some(predicate) = self.warm_start.next_decision(context) {
            return Some(predicate);
        }

        self.default_brancher.next_decision(context)
    }

    fn subscribe_to_events(&self) -> Vec<pumpkin_solver::core::branching::BrancherEvent> {
        self.default_brancher.subscribe_to_events()
    }
}
