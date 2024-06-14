use log::warn;

use crate::basic_types::ClauseReference;
use crate::basic_types::HashMap;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::conflict_analysis::AnalysisStep;
use crate::engine::propagation::PropagatorId;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;

/// An API for performing backwards reverse propagation of a clausal proof. The API allows the
/// reasons for all propagations that are used to derive the RP clause to be accessed.
#[derive(Debug)]
pub struct RpEngine {
    solver: ConstraintSatisfactionSolver,
    rp_clauses: Vec<RpClause>,
    rp_unit_clauses: HashMap<Literal, RpClauseHandle>,
    rp_allocated_clauses: HashMap<ClauseReference, RpClauseHandle>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RpClauseHandle(usize);

#[derive(Debug)]
pub enum ConflictReason {
    Clause(RpClauseHandle),
    Propagator {
        propagator: PropagatorId,
        premises: Vec<Literal>,
        propagated: Literal,
    },
}

pub type ReversePropagationConflict = Vec<ConflictReason>;

impl RpEngine {
    /// Create a new checker based on a `solver` initialized with the model of the problem.
    /// The `solver` should be at the root.
    pub fn new(solver: ConstraintSatisfactionSolver) -> Self {
        assert_eq!(
            0,
            solver.get_decision_level(),
            "the solver given to the checker must be at the root"
        );

        RpEngine {
            solver,
            rp_clauses: vec![],
            rp_unit_clauses: HashMap::default(),
            rp_allocated_clauses: HashMap::default(),
        }
    }

    /// Add a new reverse propagating clause to the checker. The clause should not be empty, and
    /// the checker should not be in an conflicting state.
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
            .filter(|&&literal| {
                self.solver
                    .get_propositional_assignments()
                    .is_literal_assigned_false(literal)
            })
            .count();

        if false_count == clause.len() - 1 {
            clause
                .iter()
                .find(|&&literal| {
                    !self
                        .solver
                        .get_propositional_assignments()
                        .is_literal_assigned(literal)
                })
                .copied()
        } else {
            None
        }
    }

    fn check_assigned_literals(&mut self, clause: &[Literal]) {
        if clause.iter().any(|&literal| {
            self.solver
                .get_propositional_assignments()
                .is_literal_assigned(literal)
        }) {
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
    fn next_decision(&mut self, _: &mut SelectionContext) -> Option<Literal> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraints::ConstraintsExt;
    use crate::engine::variables::DomainId;
    use crate::engine::variables::TransformableVariable;
    use crate::predicate;

    #[test]
    fn rp_clauses_are_removed_in_reverse_order_of_being_added() {
        let mut solver = ConstraintSatisfactionSolver::default();
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
        let mut solver = ConstraintSatisfactionSolver::default();
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
        let mut solver = ConstraintSatisfactionSolver::default();
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

        let proof_c1 = [solver.get_equality_literal(queens[0], 0)];
        let mut checker = RpEngine::new(solver);

        let Err(conflict) = checker.propagate_under_assumptions(proof_c1) else {
            panic!("expected propagation to detect conflict")
        };

        assert_eq!(conflict.len(), 5);
    }

    #[test]
    fn with_deletable_clauses_3queens_is_unsat_under_propagation() {
        let (solver, queens) = create_3queens();

        let lit_q0_neq_0 = solver.get_predicate_literal(predicate![queens[0] != 0]);
        let lit_q0_neq_1 = solver.get_predicate_literal(predicate![queens[0] != 1]);

        let proof_c1 = [lit_q0_neq_0];
        let proof_c2 = [lit_q0_neq_1];

        let mut checker = RpEngine::new(solver);
        let _ = checker.add_rp_clause(proof_c1);

        let Err(conflict) = checker.add_rp_clause(proof_c2) else {
            panic!("expected propagation to detect conflict")
        };

        assert_eq!(conflict.len(), 5);
    }

    fn create_3queens() -> (ConstraintSatisfactionSolver, Vec<DomainId>) {
        let mut solver = ConstraintSatisfactionSolver::default();

        let queens = (0..3)
            .map(|_| solver.create_new_integer_variable(0, 2))
            .collect::<Vec<_>>();
        let _ = solver.all_different(queens.clone());
        let _ = solver.all_different(
            queens
                .iter()
                .enumerate()
                .map(|(i, var)| var.offset(i as i32))
                .collect::<Vec<_>>(),
        );
        let _ = solver.all_different(
            queens
                .iter()
                .enumerate()
                .map(|(i, var)| var.offset(-(i as i32)))
                .collect::<Vec<_>>(),
        );

        (solver, queens)
    }
}
