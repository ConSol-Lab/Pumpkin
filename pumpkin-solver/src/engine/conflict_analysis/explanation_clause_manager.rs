use super::AnalysisStep;
use super::ConflictAnalysisContext;
use crate::basic_types::ClauseReference;
use crate::basic_types::StoredConflictInfo;
use crate::engine::clause_allocators::ClauseAllocatorBasic;
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::propagation::PropagationContext;
use crate::engine::reason::ReasonRef;
use crate::predicates::Predicate;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_moderate;
use crate::variables::Literal;

#[derive(Debug, Default)]
// A structure for managing the explanations and conflicts encountered during conflict analysis
pub(crate) struct ExplanationClauseManager {
    // The clauses which are temporarily allocated during conflict analysis
    temporary_clauses: Vec<Vec<Literal>>,
}

impl ExplanationClauseManager {
    // Returns the clause (pointed to by the [`ClauseReference`]) as a slice of literals.
    pub(crate) fn get_clause<'a>(
        &'a self,
        clause_allocator: &'a ClauseAllocatorBasic,
        reference: &ClauseReference,
    ) -> &'a [Literal] {
        if reference.is_allocated_clause() {
            clause_allocator[*reference].get_literal_slice()
        } else {
            pumpkin_assert_moderate!(reference.is_explanation_clause());
            &self.temporary_clauses[reference.get_explanation_clause_index()]
        }
    }

    // If the clause (pointed to by the [`ClauseReference`]) is an allocated clause then it updates
    // the Lbd and bumps the activity; if this is not the case then the method does nothing
    pub(crate) fn update_clause_lbd_and_update_activity(
        reference: &ClauseReference,
        context: &mut ConflictAnalysisContext,
    ) {
        if reference.is_allocated_clause() {
            context
                .learned_clause_manager
                .update_clause_lbd_and_bump_activity(
                    *reference,
                    context.assignments_propositional,
                    context.clause_allocator,
                );
        }
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
        context: &mut ConflictAnalysisContext,
        propagated_literal: Literal,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        pumpkin_assert_moderate!(
            !context
                .assignments_propositional
                .is_literal_root_assignment(propagated_literal),
            "Reasons are not kept properly for root propagations."
        );
        pumpkin_assert_moderate!(
            context
                .assignments_propositional
                .is_literal_assigned_true(propagated_literal),
            "Reason for propagation only makes sense for true literals."
        );

        let constraint_reference = context
            .assignments_propositional
            .get_variable_reason_constraint(propagated_literal.get_propositional_variable());

        // Case 1: the literal was propagated by the clausal propagator
        if constraint_reference.is_clause() {
            let reference = context
                .clausal_propagator
                .get_literal_propagation_clause_reference(
                    propagated_literal,
                    context.assignments_propositional,
                    context.clause_allocator,
                );

            on_analysis_step(AnalysisStep::AllocatedClause(reference));

            reference
        }
        // Case 2: the literal was placed on the propositional trail while synchronising the CP
        // trail with the propositional trail
        else {
            self.create_clause_from_propagation_reason(
                context,
                propagated_literal,
                constraint_reference.get_reason_ref(),
                on_analysis_step,
            )
        }
    }

    /// Used internally to create a clause from a reason that references a propagator.
    ///
    /// This function also performs the necessary clausal allocation.
    fn create_clause_from_propagation_reason(
        &mut self,
        context: &mut ConflictAnalysisContext,
        propagated_literal: Literal,
        reason_ref: ReasonRef,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        let propagation_context = PropagationContext::new(
            context.assignments_integer,
            context.assignments_propositional,
        );
        let propagator = context.reason_store.get_propagator(reason_ref);
        let reason = context
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
                        !context.variable_literal_mappings.get_literal(
                            integer_predicate,
                            context.assignments_propositional,
                            context.assignments_integer,
                        )
                    }
                    bool_predicate => !bool_predicate
                        .get_literal_of_bool_predicate(
                            context.assignments_propositional.true_literal,
                        )
                        .unwrap(),
                }
            }))
            .collect();

        let _ = context.internal_parameters.proof_log.log_inference(
            context.propagator_store.get_tag(propagator),
            explanation_literals.iter().skip(1).map(|&lit| !lit),
            propagated_literal,
        );

        on_analysis_step(AnalysisStep::Propagation {
            propagator,
            conjunction: &explanation_literals[1..],
            propagated: propagated_literal,
        });

        self.temporary_clauses.push(explanation_literals);
        ClauseReference::create_explanation_clause_reference(
            (self.temporary_clauses.len() - 1) as u32,
        )
    }

    /// Returns a clause reference of the clause that explains the current conflict in the solver.
    /// In case the conflict was caused by an unsatisfied clause, the conflict clause is returned.
    /// Otherwise, the conflict was caused by a propagator, in which case a new clause will be
    /// constructed based on the explanation given by the propagator.
    ///
    /// Note that the solver will panic in case the solver is not in conflicting state.
    pub(crate) fn get_conflict_reason_clause_reference(
        &mut self,
        context: &mut ConflictAnalysisContext,
        on_analysis_step: &mut impl FnMut(AnalysisStep),
    ) -> ClauseReference {
        match context.solver_state.get_conflict_info() {
            StoredConflictInfo::VirtualBinaryClause { lit1, lit2 } => {
                self.temporary_clauses.push(vec![*lit1, *lit2]);
                ClauseReference::create_explanation_clause_reference(
                    (self.temporary_clauses.len() - 1) as u32,
                )
            }
            StoredConflictInfo::Propagation { literal, reference } => {
                if reference.is_clause() {
                    let clause_ref = reference.as_clause_reference();

                    if context.clause_allocator[clause_ref].is_learned() {
                        context.internal_parameters.proof_log.add_propagation(
                            context.nogood_step_ids[clause_ref]
                                .expect("must be a previously logged proof step"),
                        );
                    }

                    on_analysis_step(AnalysisStep::AllocatedClause(clause_ref));
                    clause_ref
                } else {
                    self.create_clause_from_propagation_reason(
                        context,
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
                            !context.variable_literal_mappings.get_literal(
                                integer_predicate,
                                context.assignments_propositional,
                                context.assignments_integer,
                            )
                        }
                        bool_predicate => !bool_predicate
                            .get_literal_of_bool_predicate(
                                context.assignments_propositional.true_literal,
                            )
                            .unwrap(),
                    })
                    .collect();

                let _ = context.internal_parameters.proof_log.log_inference(
                    context.propagator_store.get_tag(*propagator),
                    explanation_literals.iter().map(|&lit| !lit),
                    context.assignments_propositional.false_literal,
                );

                on_analysis_step(AnalysisStep::Propagation {
                    propagator: *propagator,
                    conjunction: &explanation_literals,
                    propagated: context.assignments_propositional.false_literal,
                });

                self.temporary_clauses.push(explanation_literals);
                ClauseReference::create_explanation_clause_reference(
                    (self.temporary_clauses.len() - 1) as u32,
                )
            }
        }
    }

    pub(crate) fn clean_up_explanation_clauses(&mut self) {
        self.temporary_clauses.clear();
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.temporary_clauses.is_empty()
    }
}
