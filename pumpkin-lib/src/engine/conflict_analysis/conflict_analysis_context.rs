use super::AnalysisStep;
use crate::basic_types::statistics::counters::Counters;
use crate::basic_types::ClauseReference;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::PropagationContext;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::ExplanationClauseManager;
use crate::engine::LearnedClauseManager;
use crate::engine::SatisfactionSolverOptions;
use crate::engine::VariableLiteralMappings;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_moderate;

/// Used during conflict analysis to provide the necessary information.
/// All fields are made public for the time being for simplicity. In the future that may change.
#[allow(missing_debug_implementations)]
pub(crate) struct ConflictAnalysisContext<'a> {
    pub(crate) clausal_propagator: &'a ClausalPropagatorType,
    pub(crate) variable_literal_mappings: &'a VariableLiteralMappings,
    pub(crate) assignments_integer: &'a AssignmentsInteger,
    pub(crate) assignments_propositional: &'a AssignmentsPropositional,
    pub(crate) internal_parameters: &'a mut SatisfactionSolverOptions,
    pub(crate) propagator_store: &'a PropagatorStore,
    pub(crate) assumptions: &'a Vec<Literal>,

    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) clause_allocator: &'a mut ClauseAllocator,
    pub(crate) explanation_clause_manager: &'a mut ExplanationClauseManager,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) counters: &'a mut Counters,
    pub(crate) learned_clause_manager: &'a mut LearnedClauseManager,
}

impl ConflictAnalysisContext<'_> {
    pub(crate) fn get_decision_level(&self) -> usize {
        pumpkin_assert_moderate!(
            self.assignments_propositional.get_decision_level()
                == self.assignments_integer.get_decision_level()
        );
        self.assignments_propositional.get_decision_level()
    }

    /// Given a propagated literal, returns a clause reference of the clause that propagates the
    /// literal. In case the literal was propagated by a clause, the propagating clause is
    /// returned. Otherwise, the literal was propagated by a propagator, in which case a new
    /// clause will be constructed based on the explanation given by the propagator.
    ///
    /// Note that information about the reason for propagation of root literals is not properly
    /// kept, so asking about the reason for a root propagation will cause a panic.
    pub(crate) fn get_propagation_clause_reference(
        &mut self,
        propagated_literal: Literal,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        pumpkin_assert_moderate!(
            !self
                .assignments_propositional
                .is_literal_root_assignment(propagated_literal),
            "Reasons are not kept properly for root propagations."
        );
        pumpkin_assert_moderate!(
            self.assignments_propositional
                .is_literal_assigned_true(propagated_literal),
            "Reason for propagation only makes sense for true literals."
        );

        let constraint_reference = self
            .assignments_propositional
            .get_variable_reason_constraint(propagated_literal.get_propositional_variable());

        // Case 1: the literal was propagated by the clausal propagator
        if constraint_reference.is_clause() {
            let reference = self
                .clausal_propagator
                .get_literal_propagation_clause_reference(
                    propagated_literal,
                    self.assignments_propositional,
                    self.clause_allocator,
                    self.explanation_clause_manager,
                );

            on_analysis_step(AnalysisStep::AllocatedClause(reference));

            reference
        }
        // Case 2: the literal was placed on the propositional trail while synchronising the CP
        // trail with the propositional trail
        else {
            self.create_clause_from_propagation_reason(
                propagated_literal,
                constraint_reference.get_reason_ref(),
                on_analysis_step,
            )
        }
    }

    /// Returns a clause reference of the clause that explains the current conflict in the solver.
    /// In case the conflict was caused by an unsatisfied clause, the conflict clause is returned.
    /// Otherwise, the conflict was caused by a propagator, in which case a new clause will be
    /// constructed based on the explanation given by the propagator.
    ///
    /// Note that the solver will panic in case the solver is not in conflicting state.
    pub(crate) fn get_conflict_reason_clause_reference(
        &mut self,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        match self.solver_state.get_conflict_info() {
            StoredConflictInfo::VirtualBinaryClause { lit1, lit2 } => self
                .explanation_clause_manager
                .add_explanation_clause_unchecked(vec![*lit1, *lit2], self.clause_allocator),
            StoredConflictInfo::Propagation { literal, reference } => {
                if reference.is_clause() {
                    let clause_ref = reference.as_clause_reference();
                    on_analysis_step(AnalysisStep::AllocatedClause(clause_ref));
                    clause_ref
                } else {
                    self.create_clause_from_propagation_reason(
                        *literal,
                        reference.get_reason_ref(),
                        on_analysis_step,
                    )
                }
            }
            StoredConflictInfo::Explanation {
                propagator,
                conjunction,
            } => {
                // create the explanation clause
                //  allocate a fresh vector each time might be a performance bottleneck
                //  todo better ways
                let explanation_literals: Vec<Literal> = conjunction
                    .iter()
                    .map(|&predicate| match predicate {
                        Predicate::IntegerPredicate(integer_predicate) => {
                            !self.variable_literal_mappings.get_literal(
                                integer_predicate,
                                self.assignments_propositional,
                                self.assignments_integer,
                            )
                        }
                        bool_predicate => !bool_predicate
                            .get_literal_of_bool_predicate(
                                self.assignments_propositional.true_literal,
                            )
                            .unwrap(),
                    })
                    .collect();

                on_analysis_step(AnalysisStep::Propagation {
                    propagator: *propagator,
                    conjunction: &explanation_literals,
                    propagated: self.assignments_propositional.false_literal,
                });

                self.explanation_clause_manager
                    .add_explanation_clause_unchecked(explanation_literals, self.clause_allocator)
            }
        }
    }

    /// Used internally to create a clause from a reason that references a propagator.
    /// This function also performs the necessary clausal allocation.
    fn create_clause_from_propagation_reason(
        &mut self,
        propagated_literal: Literal,
        reason_ref: ReasonRef,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        let propagation_context =
            PropagationContext::new(self.assignments_integer, self.assignments_propositional);
        let propagator = self.reason_store.get_propagator(reason_ref);
        let reason = self
            .reason_store
            .get_or_compute(reason_ref, &propagation_context)
            .expect("reason reference should not be stale");
        // create the explanation clause
        //  allocate a fresh vector each time might be a performance bottleneck
        //  todo better ways
        // important to keep propagated literal at the zero-th position
        let explanation_literals: Vec<Literal> = std::iter::once(propagated_literal)
            .chain(reason.iter().map(|&predicate| {
                match predicate {
                    Predicate::IntegerPredicate(integer_predicate) => {
                        !self.variable_literal_mappings.get_literal(
                            integer_predicate,
                            self.assignments_propositional,
                            self.assignments_integer,
                        )
                    }
                    bool_predicate => !bool_predicate
                        .get_literal_of_bool_predicate(self.assignments_propositional.true_literal)
                        .unwrap(),
                }
            }))
            .collect();

        let _ = self.internal_parameters.proof_log.log_inference(
            self.propagator_store.get_tag(propagator),
            explanation_literals.iter().skip(1).copied(),
            propagated_literal,
        );

        on_analysis_step(AnalysisStep::Propagation {
            propagator,
            conjunction: &explanation_literals[1..],
            propagated: propagated_literal,
        });

        self.explanation_clause_manager
            .add_explanation_clause_unchecked(explanation_literals, self.clause_allocator)
    }
}
