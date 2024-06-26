use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::branching::Brancher;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::termination::TerminationCondition;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::options::LearningOptions;
use crate::options::SolverOptions;

use super::results::OptimisationResult;
use super::results::SatisfactionResult;
use super::results::SatisfactionResultUnderAssumptions;

#[derive(Debug, Default)]
pub struct Solver {
    satisfaction_solver: ConstraintSatisfactionSolver,
}

impl Solver {
    pub fn with_options(learning_options: LearningOptions, solver_options: SolverOptions) -> Self {
        Solver {
            satisfaction_solver: ConstraintSatisfactionSolver::new(
                learning_options,
                solver_options,
            ),
        }
    }
}

/// Functions to create integer and propositional variables.
impl Solver {
    /// Create a fresh propositional variable and return the literal with positive polarity.
    pub fn new_literal(&mut self) -> Literal {
        Literal::new(
            self.satisfaction_solver
                .create_new_propositional_variable(None),
            true,
        )
    }

    /// Create a fresh propositional variable with a given name and return the literal with positive polarity.
    pub fn new_named_literal(&mut self, name: impl Into<String>) -> Literal {
        Literal::new(
            self.satisfaction_solver
                .create_new_propositional_variable(Some(name.into())),
            true,
        )
    }

    /// Return a [`DomainId`] with a singleton domain.
    pub fn get_constant_integer(&mut self, _value: i32) -> DomainId {
        todo!()
    }

    /// Create a new integer variable with the given bounds.
    pub fn new_bounded_integer(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        self.satisfaction_solver
            .create_new_integer_variable(lower_bound, upper_bound, None)
    }

    /// Create a new named integer variable with the given bounds.
    pub fn new_named_bounded_integer(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        name: impl Into<String>,
    ) -> DomainId {
        self.satisfaction_solver.create_new_integer_variable(
            lower_bound,
            upper_bound,
            Some(name.into()),
        )
    }

    /// Create a new integer variable which has a domain of predefined values.
    pub fn new_sparse_integer(&mut self, values: impl Into<HashSet<i32>>) -> DomainId {
        let values: HashSet<i32> = values.into();

        self.satisfaction_solver
            .create_new_integer_variable_sparse(values.into_iter().collect(), None)
    }

    /// Create a new named integer variable which has a domain of predefined values.
    pub fn new_named_sparse_integer(
        &mut self,
        values: impl Into<HashSet<i32>>,
        name: impl Into<String>,
    ) -> DomainId {
        let values: HashSet<i32> = values.into();

        self.satisfaction_solver
            .create_new_integer_variable_sparse(values.into_iter().collect(), Some(name.into()))
    }

    /// Get the literal corresponding to the given predicate. As the literal may need to be
    /// created, this possibly mutates the solver.
    pub fn get_predicate_literal(&mut self, predicate: Predicate) -> Literal {
        todo!()
    }
}

/// Functions for posting new constraints to the solver.
impl Solver {
    /// Creates a clause from `literals` and adds it to the current formula.
    ///
    /// If the formula becomes trivially unsatisfiable, a [`ConstraintOperationError`] will be
    /// returned. Subsequent calls to this method will always return an error, and no
    /// modification of the solver will take place.
    pub fn add_clause(
        &mut self,
        clause: impl IntoIterator<Item = Literal>,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver.add_clause(clause)
    }
}

// Functions which solve the model.
impl Solver {
    pub fn satisfy(
        &mut self,
        brancher: impl Brancher,
        termination: impl TerminationCondition,
    ) -> SatisfactionResult<'_> {
        // Find solutions to the model currently in the solver.
        //
        // The key idea here is that, when the `SolveResult` is dropped, the solver is
        // automatically reset to the root. This means we need no assertions anywhere
        // regarding the current decision level in the solver, because other methods in
        // its public API *cannot* be called unless the solver is at the root.

        todo!()
    }

    pub fn satisfy_under_assumptions<'this, 'brancher, B: Brancher>(
        &'this mut self,
        brancher: &'brancher mut B,
        termination: impl TerminationCondition,
        assumptions: impl IntoIterator<Item = Literal>,
    ) -> SatisfactionResultUnderAssumptions<'this, 'brancher, B> {
        todo!()
    }

    pub fn minimise(
        &mut self,
        brancher: impl Brancher,
        termination: impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult<'_> {
        todo!()
    }

    pub fn maximise(
        &mut self,
        brancher: impl Brancher,
        termination: impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult<'_> {
        self.minimise(brancher, termination, objective_variable.scaled(-1))
    }
}
