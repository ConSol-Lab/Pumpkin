use std::marker::PhantomData;

use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::basic_types::Solution;
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::branching::Brancher;
use crate::branching::PhaseSaving;
use crate::branching::SolutionGuidedValueSelector;
use crate::branching::Vsids;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::termination::TerminationCondition;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::options::LearningOptions;
use crate::options::SolverOptions;
use crate::predicate;
use crate::propagators::absolute_value::AbsoluteValueConstructor;
use crate::propagators::division::DivisionConstructor;
use crate::propagators::element::ElementConstructor;
use crate::propagators::integer_multiplication::IntegerMultiplicationConstructor;
use crate::propagators::linear_less_or_equal::LinearLessOrEqualConstructor;
use crate::propagators::linear_not_equal::LinearNotEqualConstructor;
use crate::propagators::maximum::MaximumConstructor;
use crate::propagators::ArgTask;
use crate::propagators::TimeTablePerPoint;
use crate::pumpkin_assert_simple;
use crate::results::SolutionReference;
use crate::statistics::log_statistic;
use crate::statistics::log_statistic_postfix;
use crate::statistics::log_statistics_with_objective;
use crate::variables::AffineView;
use crate::variables::PropositionalVariable;
use crate::variables::TransformableVariable;

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

    pub fn log_statistics_with_objective(&self, best_objective_value: i64) {
        log_statistic("objective", best_objective_value);
        self.log_statistics();
        log_statistic_postfix();
    }

    pub fn log_statistics(&self) {
        self.satisfaction_solver.log_statistics();
        log_statistic_postfix();
    }
}

/// Functions to create and retrieve integer and propositional variables.
impl Solver {
    pub fn lower_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.satisfaction_solver.get_lower_bound(variable)
    }

    pub fn upper_bound(&self, variable: &impl IntegerVariable) -> i32 {
        self.satisfaction_solver.get_upper_bound(variable)
    }

    pub fn get_solution_reference(&self) -> SolutionReference<'_> {
        self.satisfaction_solver.get_solution_reference()
    }

    /// Get a literal which is globally true.
    pub fn get_true_literal(&self) -> Literal {
        self.satisfaction_solver.get_true_literal()
    }

    /// Get a literal which is globally false.
    pub fn get_false_literal(&self) -> Literal {
        self.satisfaction_solver.get_false_literal()
    }

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
        values: impl Into<Vec<i32>>,
        name: impl Into<String>,
    ) -> DomainId {
        self.satisfaction_solver
            .create_new_integer_variable_sparse(values.into(), Some(name.into()))
    }

    /// Get the literal corresponding to the given predicate. As the literal may need to be
    /// created, this possibly mutates the solver.
    pub fn get_literal_for_predicate(&self, predicate: Predicate) -> Literal {
        self.satisfaction_solver.get_literal(predicate)
    }

    pub fn restore_state_at_root(&mut self, brancher: &mut impl Brancher) {
        self.satisfaction_solver.restore_state_at_root(brancher)
    }

    /// This is a temporary accessor to help refactoring.
    #[deprecated = "will be removed in favor of new state-based api"]
    pub fn is_at_the_root_level(&self) -> bool {
        self.satisfaction_solver.is_at_the_root_level()
    }

    /// Get the value of the given literal, which could be unassigned.
    pub fn get_literal_value(&self, literal: Literal) -> Option<bool> {
        self.satisfaction_solver.get_literal_value(literal)
    }
}

/// Functions for satisfaction_solver.add_propagatoring new constraints to the solver.
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

/// Functions which solve the model.
impl Solver {
    pub fn satisfy(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
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
        termination: &mut impl TerminationCondition,
        assumptions: impl IntoIterator<Item = Literal>,
    ) -> SatisfactionResultUnderAssumptions<'this, 'brancher, B> {
        todo!()
    }

    pub fn minimise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult {
        let initial_solve = self.satisfaction_solver.solve(termination, brancher);
        match initial_solve {
            CSPSolverExecutionFlag::Feasible => {
                #[allow(deprecated)]
                brancher.on_solution(self.satisfaction_solver.get_solution_reference());

                self.log_statistics_with_objective(
                    self.satisfaction_solver
                        .get_assigned_integer_value(&objective_variable)
                        .expect("Expected objective variable to be assigned")
                        as i64,
                );
                // #[allow(deprecated)]
                // print_solution_from_solver(self.csp_solver.get_solution_reference(), outputs)
            }
            CSPSolverExecutionFlag::Infeasible => return OptimisationResult::Unsatisfiable,
            CSPSolverExecutionFlag::Timeout => return OptimisationResult::Unknown,
        }

        let mut best_objective_value =
            self.satisfaction_solver
                .get_assigned_integer_value(&objective_variable)
                .expect("expected variable to be assigned") as i64;
        let mut best_solution: Solution = self.satisfaction_solver.get_solution_reference().into();

        loop {
            self.satisfaction_solver.restore_state_at_root(brancher);

            if self
                .strengthen(&objective_variable, best_objective_value)
                .is_err()
            {
                return OptimisationResult::Optimal(best_solution);
            }

            let solve_result = self.satisfaction_solver.solve(termination, brancher);
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    self.debug_bound_change(&objective_variable, best_objective_value);

                    best_objective_value = self
                        .satisfaction_solver
                        .get_assigned_integer_value(&objective_variable)
                        .expect("expected variable to be assigned")
                        as i64;
                    best_solution = self.satisfaction_solver.get_solution_reference().into();

                    #[allow(deprecated)]
                    brancher.on_solution(self.satisfaction_solver.get_solution_reference());

                    self.log_statistics_with_objective(best_objective_value);
                    // #[allow(deprecated)]
                    // print_solution_from_solver(self.csp_solver.get_solution_reference(), outputs)
                }
                CSPSolverExecutionFlag::Infeasible => {
                    return OptimisationResult::Optimal(best_solution);
                }
                CSPSolverExecutionFlag::Timeout => {
                    return OptimisationResult::Satisfiable(best_solution)
                }
            }
        }
    }

    pub fn maximise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
    ) -> OptimisationResult {
        self.minimise(brancher, termination, objective_variable.scaled(-1))
    }

    /// Given the current objective value `best_objective_value`, it adds a constraint specifying that the objective value should be at most `best_objective_value - 1`. Note that it is assumed that we are always minimising the variable.
    fn strengthen(
        &mut self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
    ) -> Result<(), ConstraintOperationError> {
        self.satisfaction_solver
            .add_clause([self.satisfaction_solver.get_literal(
                objective_variable.upper_bound_predicate((best_objective_value - 1) as i32),
            )])
    }

    fn debug_bound_change(
        &self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
    ) {
        pumpkin_assert_simple!(
            (self
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned") as i64)
                < best_objective_value,
            "{}",
            format!(
                "The current bound {} should be smaller than the previous bound {}",
                self.satisfaction_solver
                    .get_assigned_integer_value(objective_variable)
                    .expect("expected variable to be assigned"),
                best_objective_value
            )
        );
    }
}

/// Functions to add pre-defined constraints
impl Solver {
    /// Adds the constraint `array[index] = rhs`.
    pub fn element<ElementVar: IntegerVariable + 'static>(
        &mut self,
        index: impl IntegerVariable + 'static,
        array: impl Into<Box<[ElementVar]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        self.satisfaction_solver.add_propagator(ElementConstructor {
            index,
            array: array.into(),
            rhs,
        })
    }

    /// Adds the constraint `\sum terms_i != rhs`.
    pub fn linear_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(LinearNotEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> \sum terms_i != rhs`.
    pub fn half_reified_linear_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(LinearNotEqualConstructor::reified(terms.into(), rhs, reif))
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    pub fn linear_less_than_or_equal<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(LinearLessOrEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> (\sum terms_i <= rhs)`.
    pub fn half_reified_linear_less_than_or_equal<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(LinearLessOrEqualConstructor::reified(
                terms.into(),
                rhs,
                reif,
            ))
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    pub fn linear_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        let terms = terms.into();

        if !self.linear_less_than_or_equal(terms.clone(), rhs) {
            return false;
        }

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.linear_less_than_or_equal(negated, -rhs)
    }

    /// Adds the constraint `reif -> (\sum terms_i = rhs)`.
    pub fn half_reified_linear_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        let terms = terms.into();

        if !self.half_reified_linear_less_than_or_equal(terms.clone(), rhs, reif) {
            return false;
        }

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.half_reified_linear_less_than_or_equal(negated, -rhs, reif)
    }

    /// Adds the constraint `lhs != rhs`.
    pub fn binary_not_equal<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.linear_not_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> lhs != rhs`.
    pub fn half_reified_binary_not_equal<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.half_reified_linear_not_equals([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs <= rhs`.
    pub fn binary_less_than_or_equal<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
    ) -> bool {
        self.linear_less_than_or_equal([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs <= rhs)`.
    pub fn half_reified_binary_less_than_or_equal<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.half_reified_linear_less_than_or_equal([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs < rhs`.
    pub fn binary_less_than<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.binary_less_than_or_equal(lhs.scaled(1), rhs.offset(-1))
    }

    /// Adds the constraint `reif -> (lhs < rhs)`.
    pub fn half_reified_binary_less_than<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.half_reified_binary_less_than_or_equal(lhs.scaled(1), rhs.offset(-1), reif)
    }

    /// Adds the constraint `lhs = rhs`.
    pub fn binary_equals<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.linear_equals([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs = rhs)`.
    pub fn half_reified_binary_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.half_reified_linear_equals([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `a + b = c`.
    pub fn integer_plus<Var: IntegerVariable + 'static>(&mut self, a: Var, b: Var, c: Var) -> bool {
        self.linear_equals([a.scaled(1), b.scaled(1), c.scaled(-1)], 0)
    }

    /// Adds the constraint `a * b = c`.
    pub fn integer_multiplication(
        &mut self,
        a: impl IntegerVariable + 'static,
        b: impl IntegerVariable + 'static,
        c: impl IntegerVariable + 'static,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(IntegerMultiplicationConstructor { a, b, c })
    }

    /// A propagator for maintaining the constraint `numerator / denominator = rhs`; note that this
    /// propagator performs truncating division (i.e. rounding towards 0).
    ///
    /// The propagator assumes that the `denominator` is a non-zero integer.
    ///
    /// The implementation is ported from [OR-tools](https://github.com/google/or-tools/blob/870edf6f7bff6b8ff0d267d936be7e331c5b8c2d/ortools/sat/integer_expr.cc#L1209C1-L1209C19).
    pub fn integer_division(
        &mut self,
        numerator: impl IntegerVariable + 'static,
        denominator: impl IntegerVariable + 'static,
        rhs: impl IntegerVariable + 'static,
    ) -> bool
    where
        Self: Sized,
    {
        pumpkin_assert_simple!(
            !self
                .satisfaction_solver
                .integer_variable_contains(&denominator, 0),
            "We do not support a value of 0 in the domain of the denominator of `int_div`"
        );
        self.satisfaction_solver
            .add_propagator(DivisionConstructor {
                numerator,
                denominator,
                rhs,
            })
    }

    /// Adds the constraint `|signed| = absolute`.
    pub fn integer_absolute(
        &mut self,
        signed: impl IntegerVariable + 'static,
        absolute: impl IntegerVariable + 'static,
    ) -> bool {
        self.satisfaction_solver
            .add_propagator(AbsoluteValueConstructor { signed, absolute })
    }

    /// Adds the constraint that all variables must be distinct.
    pub fn all_different<Var: IntegerVariable + 'static>(
        &mut self,
        variables: impl Into<Box<[Var]>>,
    ) -> bool {
        let variables = variables.into();

        for i in 0..variables.len() {
            for j in i + 1..variables.len() {
                if !self.binary_not_equal(variables[i].clone(), variables[j].clone()) {
                    return false;
                }
            }
        }

        true
    }

    /// Posts the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint.
    /// This constraint ensures that at no point in time, the cumulative resource usage of the tasks
    /// exceeds `bound`. See [`crate::propagators`] for more information.
    ///
    /// The length of `start_times`, `durations` and `resource_requirements` should be the same; if
    /// this is not the case then this method will panic.
    ///
    /// For now we assume that the durations, resource requirements and bound are constant.
    pub fn cumulative<Var: IntegerVariable + 'static + std::fmt::Debug + Copy>(
        &mut self,
        start_times: &[Var],
        durations: &[i32],
        resource_requirements: &[i32],
        resource_capacity: i32,
    ) -> bool {
        pumpkin_assert_simple!(
            start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
            "The number of start variables, durations and resource requirements should be the same!car"
        );
        self.satisfaction_solver
            .add_propagator(TimeTablePerPoint::new(
                start_times
                    .iter()
                    .zip(durations)
                    .zip(resource_requirements)
                    .map(|((start_time, duration), resource_requirement)| ArgTask {
                        start_time: *start_time,
                        processing_time: *duration,
                        resource_usage: *resource_requirement,
                    })
                    .collect(),
                resource_capacity,
            ))
    }

    /// Posts the constraint `max(array) = m`.
    pub fn maximum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl Into<Box<[Var]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        self.satisfaction_solver.add_propagator(MaximumConstructor {
            array: array.into(),
            rhs,
        })
    }

    /// Posts the constraint `min(array) = m`.
    pub fn minimum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl IntoIterator<Item = Var>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        let array = array
            .into_iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<_>>();
        self.maximum(array, rhs.scaled(-1))
    }

    /// Adds the constraint `reif <-> \sum terms_i <= rhs`.
    pub fn reified_linear_less_than_or_equal<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        let terms = terms.into();
        self.half_reified_linear_less_than_or_equal(terms.clone(), rhs, reif)
            && self.half_reified_linear_less_than_or_equal(
                terms
                    .into_iter()
                    .map(|term| term.scaled(-1))
                    .collect::<Vec<_>>(),
                -rhs - 1,
                !reif,
            )
    }

    /// Adds the constraint `reif <-> \sum terms_i = rhs`.
    pub fn reified_linear_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        let terms = terms.into();
        self.half_reified_linear_equals(terms.clone(), rhs, reif)
            && self.half_reified_linear_not_equals(terms, rhs, !reif)
    }

    /// Adds the constraint `reif <-> clause`.
    pub fn array_bool_or(&mut self, clause: impl Into<Vec<Literal>>, reif: Literal) -> bool {
        let mut clause = clause.into();

        // \/clause -> r
        let all_implications = clause
            .iter()
            .all(|&literal| self.add_clause([!literal, reif]).is_ok());

        // r -> \/clause
        clause.insert(0, !reif);

        all_implications && self.add_clause(clause).is_ok()
    }

    /// Adds the constraint `reif <-> lhs != rhs`.
    pub fn reified_binary_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.half_reified_binary_not_equal(lhs.clone(), rhs.clone(), reif)
            && self.half_reified_binary_equals(lhs, rhs, reif)
    }

    /// Adds the constraint `reif <-> \sum terms_i != rhs`.
    pub fn reified_linear_not_equals<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        let terms = terms.into();
        self.half_reified_linear_not_equals(terms.clone(), rhs, reif)
            && self.half_reified_linear_equals(terms, rhs, !reif)
    }

    /// Adds the constraint `\sum weights_i * bools_i <= rhs`.
    pub fn linear_boolean_less_than_or_equal(
        &mut self,
        weights: &[i32],
        bools: &[Literal],
        rhs: i32,
    ) -> bool {
        let domains = bools
            .iter()
            .enumerate()
            .map(|(index, bool)| {
                let corresponding_domain_id = self.new_bounded_integer(0, 1);
                // bool -> [domain = 1]
                let true_literal =
                    self.get_literal_for_predicate(predicate![corresponding_domain_id >= 1]);
                let _ = self.add_clause([!*bool, true_literal]);
                // !bool -> [domain = 0]
                let false_literal =
                    self.get_literal_for_predicate(predicate![corresponding_domain_id <= 0]);
                let _ = self.add_clause([*bool, false_literal]);
                corresponding_domain_id.scaled(weights[index])
            })
            .collect::<Vec<_>>();
        self.linear_less_than_or_equal(domains, rhs)
    }

    /// Adds the constraint `\sum weights_i * bools_i = rhs`.
    pub fn linear_boolean_equals(
        &mut self,
        weights: &[i32],
        bools: &[Literal],
        rhs: DomainId,
    ) -> bool {
        let domains = bools
            .iter()
            .enumerate()
            .map(|(index, bool)| {
                let corresponding_domain_id = self.new_bounded_integer(0, 1);
                // bool -> [domain = 1]
                let true_literal =
                    self.get_literal_for_predicate(predicate![corresponding_domain_id >= 1]);
                let _ = self.add_clause([!*bool, true_literal]);
                // !bool -> [domain = 0]
                let false_literal =
                    self.get_literal_for_predicate(predicate![corresponding_domain_id <= 0]);
                let _ = self.add_clause([*bool, false_literal]);
                corresponding_domain_id.scaled(weights[index])
            })
            .chain(std::iter::once(rhs.scaled(-1)))
            .collect::<Vec<_>>();
        self.linear_equals(domains, 0)
    }
}

/// Default brancher implementation
impl Solver {
    /// Creates a default [`IndependentVariableValueBrancher`] which uses [`Vsids`] as
    /// [`VariableSelector`] and [`SolutionGuidedValueSelector`] (with [`PhaseSaving`] as its
    /// back-up selector) as its [`ValueSelector`]; it searches over all
    /// [`PropositionalVariable`]s defined in the provided `solver`.
    pub fn default_brancher_over_all_propositional_variables(&self) -> DefaultBrancher {
        self.satisfaction_solver
            .default_brancher_over_all_propositional_variables()
    }
}

/// Proof logging methods
impl Solver {
    /// Conclude the proof with the unsatisfiable claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_unsat(&mut self) -> std::io::Result<()> {
        self.satisfaction_solver.conclude_proof_unsat()
    }

    /// Conclude the proof with the optimality claim.
    ///
    /// This method will finish the proof. Any new operation will not be logged to the proof.
    pub fn conclude_proof_optimal(&mut self, bound: Literal) -> std::io::Result<()> {
        self.satisfaction_solver.conclude_proof_optimal(bound)
    }
}

pub type DefaultBrancher = IndependentVariableValueBrancher<
    PropositionalVariable,
    Vsids<PropositionalVariable>,
    SolutionGuidedValueSelector<
        PropositionalVariable,
        bool,
        PhaseSaving<PropositionalVariable, bool>,
    >,
>;
