//! Reverse propagation (RP) is a generalization of Reverse Unit Propagation (RUP). In the latter
//! case, a clause `c` is RUP with respect to a clause database `F` when `¬c ∧ F ⟹ false` and this
//! conflict can be detected through clausal (aka unit) propagation.
//! RP generalizes this property by dropping the requirement for `F` to be a
//! database of clauses. It can be a database of any constraint type.
//!
//! This concept is mostly useful when dealing with clausal proofs. In particular, the DRCP format,
//! which is the format used by Pumpkin to proofs when solving a CP problem.
//!
//! Since validating the RP property of a clause of predicates requires a CP propagation engine,
//! and given that Pumpkin implements such an engine, the [`RpEngine`] exposes an API to verify the
//! RP property of of clauses.

use log::warn;

use crate::basic_types::ClauseReference;
use crate::basic_types::HashMap;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::conflict_analysis::AnalysisStep;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagatorId;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::Solver;

/// An API for performing backwards reverse propagation of a clausal proof. The API allows the
/// reasons for all propagations that are used to derive the RP clause to be accessed.
///
/// To use the RpEngine, one can do the following:
/// 1. Initialise it with a base model against which the individual reverse propagating clauses will
///    be checked.
/// 2. Add reverse propagating clauses through [`RpEngine::add_rp_clause`]. The order in which this
///    happens matters.
/// 3. Check whether a propagation can derive a conflict under certain assumptions (probably the
///    negation of a reverse propagating clause which is no-longer in the engine).
/// 4. Remove the reverse propagating clauses with [`RpEngine::remove_last_rp_clause`] in reverse
///    order in which they were added.
#[derive(Debug)]
pub struct RpEngine {
    solver: ConstraintSatisfactionSolver,
    rp_clauses: Vec<RpClause>,
    rp_unit_clauses: HashMap<Literal, RpClauseHandle>,
    rp_allocated_clauses: HashMap<ClauseReference, RpClauseHandle>,
}

/// A handle to a reverse propagating clause. These clauses are added to the [`RpEngine`] through
/// [`RpEngine::add_rp_clause`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RpClauseHandle(usize);

/// One of the reasons contributing to unsatisfiability when calling
/// [`RpEngine::propagate_under_assumptions`].
#[derive(Debug)]
pub enum ConflictReason {
    Clause(RpClauseHandle),
    Propagator {
        propagator: PropagatorId,
        premises: Vec<Literal>,
        propagated: Literal,
    },
}

/// The reason for a conflict is a list of [`ConflictReason`]s.
pub type ReversePropagationConflict = Vec<ConflictReason>;

impl RpEngine {
    /// Create a new reverse propagating engine based on a [`Solver`] initialized with the model of
    /// the problem.
    pub fn new(solver: Solver) -> Self {
        RpEngine {
            solver: solver.into_satisfaction_solver(),
            rp_clauses: vec![],
            rp_unit_clauses: HashMap::default(),
            rp_allocated_clauses: HashMap::default(),
        }
    }

    /// Add a new reverse propagating clause to the engine. The clause should not be empty, and
    /// the engine should not be in an conflicting state.
    ///
    /// If the new clause causes a conflict under propagation, the engine will be in a conflicting
    /// state. A call to [`RpEngine::remove_last_rp_clause`] will remove the newly added clause and
    /// reset the engine to a useable state.
    pub fn add_rp_clause(
        &mut self,
        clause: impl IntoIterator<Item = Literal>,
    ) -> Result<RpClauseHandle, ReversePropagationConflict> {
        let clause: Vec<Literal> = clause.into_iter().collect();
        assert!(!clause.is_empty(), "cannot add the empty clause");

        let new_handle = RpClauseHandle(self.rp_clauses.len());

        if clause.len() == 1 {
            self.rp_clauses.push(RpClause::Unit(clause[0]));
            // todo remove, rp_unit clauses
            let _ = self.rp_unit_clauses.insert(clause[0], new_handle);

            self.solver.declare_new_decision_level();
            self.enqueue_and_propagate(clause[0])?;
        } else {
            let propagating_literal = self.get_propagating_literal(&clause);

            let reference = self.solver.add_allocated_deletable_clause(clause);

            let old_handle = self.rp_allocated_clauses.insert(reference, new_handle);
            assert!(old_handle.is_none());

            self.rp_clauses.push(RpClause::ClauseRef(reference));

            if let Some(propagating_literal) = propagating_literal {
                self.enqueue_and_propagate(propagating_literal)?;
            }
        }

        Ok(new_handle)
    }

    fn get_propagating_literal(&mut self, clause: &[Literal]) -> Option<Literal> {
        self.check_assigned_literals(clause);

        let false_count = clause
            .iter()
            .filter(|&&literal| self.solver.get_literal_value(literal) == Some(false))
            .count();

        if false_count == clause.len() - 1 {
            clause
                .iter()
                .find(|&&literal| self.solver.get_literal_value(literal).is_some())
                .copied()
        } else {
            None
        }
    }

    fn check_assigned_literals(&mut self, clause: &[Literal]) {
        if clause
            .iter()
            .any(|&literal| self.solver.get_literal_value(literal).is_some())
        {
            warn!("Adding RP clause with assigned literals.");
        }
    }

    /// Remove the last clause in the proof from consideration and return the literals it contains.
    pub fn remove_last_rp_clause(&mut self) -> Option<Vec<Literal>> {
        let last_rp_clause = self.rp_clauses.pop()?;

        let result = match last_rp_clause {
            RpClause::Unit(literal) => {
                self.backtrack_one_level();

                let _ = self.rp_unit_clauses.remove(&literal);

                Some(vec![literal])
            }

            RpClause::ClauseRef(reference) => {
                let clause = self.solver.delete_allocated_clause(reference);
                let _ = self
                    .rp_allocated_clauses
                    .remove(&reference)
                    .expect("the reference should be for an rp clause");
                Some(clause)
            }
        };

        // The now removed clause may have caused root-level unsatisfiability. Now that it is
        // removed, we should be able to use the solver again.
        self.solver.declare_ready();

        result
    }

    /// Perform unit propagation under assumptions.
    ///
    /// In case the engine discovers a conflict, the engine will be in a conflicting state. At this
    /// point, no new clauses can be added before a call to [`RpEngine::remove_last_rp_clause`].
    pub fn propagate_under_assumptions(
        &mut self,
        assumptions: impl IntoIterator<Item = Literal>,
    ) -> Result<(), Vec<ConflictReason>> {
        assert!(!self.solver.is_conflicting());

        self.solver.declare_new_decision_level();

        for assumption in assumptions.into_iter() {
            let enqueue_result = self.enqueue_and_propagate(assumption);

            if let Err(reasons) = enqueue_result {
                self.backtrack_one_level();
                return Err(reasons);
            }
        }

        self.backtrack_one_level();

        Ok(())
    }

    fn backtrack_one_level(&mut self) {
        self.solver
            .backtrack(self.solver.get_decision_level() - 1, &mut DummyBrancher);
    }

    fn enqueue_and_propagate(
        &mut self,
        literal: Literal,
    ) -> Result<(), ReversePropagationConflict> {
        if !self.solver.enqueue_assumption_literal(literal) {
            // technically this is fine, but it would be surprising to encounter this
            warn!("Unexpected conflict when assigning assumptions.");
            return Err(self.get_conflict_reasons());
        }

        self.solver.propagate_enqueued();

        if self.solver.is_conflicting() {
            Err(self.get_conflict_reasons())
        } else {
            Ok(())
        }
    }

    fn get_conflict_reasons(&mut self) -> Vec<ConflictReason> {
        let mut reasons = Vec::new();

        self.solver
            .get_conflict_reasons(&mut DummyBrancher, |step| match step {
                AnalysisStep::AllocatedClause(reference) => {
                    if let Some(handle) = self.rp_allocated_clauses.get(&reference) {
                        reasons.push(ConflictReason::Clause(*handle));
                    }
                }
                AnalysisStep::Propagation {
                    propagator,
                    conjunction,
                    propagated,
                } => {
                    reasons.push(ConflictReason::Propagator {
                        propagator,
                        premises: conjunction.into(),
                        propagated,
                    });
                }
                AnalysisStep::Unit(literal) => {
                    if let Some(handle) = self.rp_unit_clauses.get(&literal) {
                        reasons.push(ConflictReason::Clause(*handle));
                    }
                }
            });

        reasons
    }
}

#[derive(Debug)]
enum RpClause {
    Unit(Literal),
    ClauseRef(ClauseReference),
}

/// We need this to call [`ConstraintSatisfactionSolver::backtrack`], however, it does
/// not need to do anything because [`Brancher::next_decision`] will never be called.
struct DummyBrancher;

impl Brancher for DummyBrancher {
    fn next_decision(&mut self, _: &mut SelectionContext) -> Option<Predicate> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraints;
    use crate::engine::variables::DomainId;
    use crate::engine::variables::TransformableVariable;
    use crate::predicate;

    #[test]
    fn rp_clauses_are_removed_in_reverse_order_of_being_added() {
        let mut solver = Solver::default();
        let xs: Vec<Literal> = solver.new_literals().take(3).collect();

        let c1 = xs.clone();
        let c2 = vec![!xs[0], xs[1], !xs[2]];
        let c3 = vec![!xs[0], xs[2]];

        let mut checker = RpEngine::new(solver);
        let _ = checker.add_rp_clause(c1.clone()).unwrap();
        let _ = checker.add_rp_clause(c2.clone()).unwrap();
        let _ = checker.add_rp_clause(c3.clone()).unwrap();

        assert_eq!(Some(c3), checker.remove_last_rp_clause());
        assert_eq!(Some(c2), checker.remove_last_rp_clause());
        assert_eq!(Some(c1), checker.remove_last_rp_clause());
    }

    #[test]
    fn propositional_unsat_proof() {
        let mut solver = Solver::default();
        let xs: Vec<Literal> = solver.new_literals().take(2).collect();

        let _ = solver.add_clause(xs.clone());
        let _ = solver.add_clause([xs[0], !xs[1]]);
        let _ = solver.add_clause([!xs[0], xs[1]]);
        let _ = solver.add_clause([!xs[0], !xs[1]]);

        let mut checker = RpEngine::new(solver);
        let result = checker
            .add_rp_clause([xs[0]])
            .expect_err("no unit-propagation conflict");
        drop(result);

        let clause = checker.remove_last_rp_clause();
        assert_eq!(Some(vec![xs[0]]), clause);

        checker
            .propagate_under_assumptions([])
            .expect("without assumptions no conflict is detected with unit-propagation");

        let _ = checker
            .propagate_under_assumptions([!xs[0]])
            .expect_err("the assumptions should lead to unit propagation-detecting a conflict");
    }

    #[test]
    fn propositional_unsat_get_propagations() {
        let mut solver = Solver::default();
        let xs: Vec<Literal> = solver.new_literals().take(2).collect();

        let _ = solver.add_clause(xs.clone());
        let _ = solver.add_clause([xs[0], !xs[1]]);
        let _ = solver.add_clause([!xs[0], xs[1]]);
        let _ = solver.add_clause([!xs[0], !xs[1]]);

        let mut checker = RpEngine::new(solver);
        let result = checker
            .add_rp_clause([xs[0]])
            .expect_err("no unit-propagation conflict");
        drop(result);
    }

    #[test]
    fn fixing_a_queen_in_3queens_triggers_conflict_under_rp() {
        let (solver, queens) = create_3queens();

        let proof_c1 = [solver.get_literal(predicate![queens[0] == 0])];
        let mut checker = RpEngine::new(solver);

        let Err(conflict) = checker.propagate_under_assumptions(proof_c1) else {
            panic!("expected propagation to detect conflict")
        };

        assert_eq!(conflict.len(), 5);
    }

    #[test]
    fn with_deletable_clauses_3queens_is_unsat_under_propagation() {
        let (solver, queens) = create_3queens();

        let lit_q0_neq_0 = solver.get_literal(predicate![queens[0] != 0]);
        let lit_q0_neq_1 = solver.get_literal(predicate![queens[0] != 1]);

        let proof_c1 = [lit_q0_neq_0];
        let proof_c2 = [lit_q0_neq_1];

        let mut checker = RpEngine::new(solver);
        let _ = checker.add_rp_clause(proof_c1);

        let Err(conflict) = checker.add_rp_clause(proof_c2) else {
            panic!("expected propagation to detect conflict")
        };

        assert_eq!(conflict.len(), 5);
    }

    fn create_3queens() -> (Solver, Vec<DomainId>) {
        let mut solver = Solver::default();

        let queens = (0..3)
            .map(|_| solver.new_bounded_integer(0, 2))
            .collect::<Vec<_>>();
        let _ = solver
            .add_constraint(constraints::all_different(queens.clone()))
            .post();
        let _ = solver
            .add_constraint(constraints::all_different(
                queens
                    .iter()
                    .enumerate()
                    .map(|(i, var)| var.offset(i as i32))
                    .collect::<Vec<_>>(),
            ))
            .post();
        let _ = solver
            .add_constraint(constraints::all_different(
                queens
                    .iter()
                    .enumerate()
                    .map(|(i, var)| var.offset(-(i as i32)))
                    .collect::<Vec<_>>(),
            ))
            .post();

        (solver, queens)
    }
}
