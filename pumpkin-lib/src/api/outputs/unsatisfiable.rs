//! Contains the representation of a unsatisfiable solution.

use crate::branching::Brancher;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::Solver;

/// A struct which allows the retrieval of an unsatisfiable core consisting of the provided
/// assumptions passed to the initial [`Solver::satisfy_under_assumptions`]. Note that when this
/// struct is dropped (using [`Drop`]) then the [`Solver`] is reset.
#[derive(Debug)]
pub struct UnsatisfiableUnderAssumptions<'solver, 'brancher, B: Brancher> {
    pub(crate) solver: &'solver mut ConstraintSatisfactionSolver,
    pub(crate) brancher: &'brancher mut B,
}

impl<'solver, 'brancher, B: Brancher> UnsatisfiableUnderAssumptions<'solver, 'brancher, B> {
    pub fn new(
        solver: &'solver mut ConstraintSatisfactionSolver,
        brancher: &'brancher mut B,
    ) -> Self {
        UnsatisfiableUnderAssumptions { solver, brancher }
    }

    /// Extract an unsatisfiable core in terms of the assumptions.
    ///
    /// In general, a core is a (sub)set of the constraints which together are unsatisfiable; in the
    /// case of constraint programming, this means a set of constraints which are together
    /// unsatisfiable; for example, if we have one variable `x ∈ [0, 10]` and we have the two
    /// constraints `5 * x >= 25` and `[x <= 4]` then it can be observed that these constraints can
    /// never be true at the same time.
    ///
    /// In an assumption-based solver, a core is defined as a (sub)set of assumptions which, given a
    /// set of constraints, when set together will lead to an unsatisfiable instance. For
    /// example, if we two variables `x, y, z ∈ {0, 1, 2}` and we have the constraint
    /// `all-different(x, y, z)` then the assumptions `[[x = 1], [y <= 1], [y != 0]]` would
    /// constitute an unsatisfiable core since the constraint and the assumptions can never be
    /// satisfied at the same time.
    ///
    /// One of the main examples in which this assumption interface is used is MaxSAT in which
    /// core-guided solvers generate multiple cores while to solve the instance \[1\]. This
    /// core-guided search technique has been translated to CP in \[2\].
    ///
    /// # Example
    /// ```rust
    /// # use pumpkin_lib::Solver;
    /// # use pumpkin_lib::results::SatisfactionResultUnderAssumptions;
    /// # use pumpkin_lib::termination::Indefinite;
    /// # use pumpkin_lib::predicate;
    /// # use pumpkin_lib::constraints;
    /// # use pumpkin_lib::constraints::Constraint;
    /// // We create the solver with default options
    /// let mut solver = Solver::default();
    ///
    /// // We create 3 variables with domains within the range [0, 10]
    /// let x = solver.new_bounded_integer(0, 2);
    /// let y = solver.new_bounded_integer(0, 2);
    /// let z = solver.new_bounded_integer(0, 2);
    ///
    /// // We create the all-different constraint
    /// solver.add_constraint(constraints::all_different(vec![x, y, z])).post();
    ///
    /// // We create a termination condition which allows the solver to run indefinitely
    /// let mut termination = Indefinite;
    /// // And we create a search strategy (in this case, simply the default)
    /// let mut brancher = solver.default_brancher_over_all_propositional_variables();
    ///
    /// // Then we solve to satisfaction
    /// let assumptions = vec![
    ///     solver.get_literal(predicate!(x == 1)),
    ///     solver.get_literal(predicate!(y <= 1)),
    ///     solver.get_literal(predicate!(y != 0)),
    /// ];
    /// let result =
    ///     solver.satisfy_under_assumptions(&mut brancher, &mut termination, &assumptions);
    ///
    /// if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
    ///     mut unsatisfiable,
    /// ) = result
    /// {
    ///     {
    ///         let core = unsatisfiable.extract_core();
    ///
    ///         // In this case, the core should be equal to the negation of all literals in the
    ///         // assumptions
    ///         assert!(assumptions
    ///             .into_iter()
    ///             .all(|literal| core.contains(&(!literal))));
    ///     }
    /// }
    ///  ```
    ///
    /// # Bibliography
    /// \[1\] Z. Fu and S. Malik, ‘On solving the partial MAX-SAT problem’, in International
    /// Conference on Theory and Applications of Satisfiability Testing, 2006, pp. 252–265.
    ///
    /// \[2\] G. Gange, J. Berg, E. Demirović, and P. J. Stuckey, ‘Core-guided and core-boosted
    /// search for CP’, in Integration of Constraint Programming, Artificial Intelligence, and
    /// Operations Research: 17th International Conference, CPAIOR 2020, Vienna, Austria, September
    /// 21--24, 2020, Proceedings 17, 2020, pp. 205–221.
    pub fn extract_core(&mut self) -> Box<[Literal]> {
        self.solver
            .extract_clausal_core(self.brancher)
            .expect("expected consistent assumptions")
            .into()
    }
}

impl<'solver, 'brancher, B: Brancher> Drop
    for UnsatisfiableUnderAssumptions<'solver, 'brancher, B>
{
    fn drop(&mut self) {
        self.solver.restore_state_at_root(self.brancher)
    }
}
