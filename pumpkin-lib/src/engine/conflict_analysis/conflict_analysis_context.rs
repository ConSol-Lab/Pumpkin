use std::cmp::min;

use super::AnalysisStep;
use crate::basic_types::ClauseReference;
use crate::basic_types::ConstraintReference;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::constraint_satisfaction_solver::Counters;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::PropagationContext;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::ExplanationClauseManager;
use crate::engine::IntDomainEvent;
use crate::engine::LearnedClauseManager;
use crate::engine::PropagatorQueue;
use crate::engine::VariableLiteralMappings;
use crate::engine::WatchListCP;
use crate::proof::ProofLog;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// Used during conflict analysis to provide the necessary information.
/// All fields are made public for the time being for simplicity. In the future that may change.
#[allow(missing_debug_implementations)]
pub struct ConflictAnalysisContext<'a> {
    pub(crate) clausal_propagator: &'a mut ClausalPropagatorType,
    pub(crate) variable_literal_mappings: &'a VariableLiteralMappings,
    pub(crate) assignments_integer: &'a mut AssignmentsInteger,
    pub(crate) assignments_propositional: &'a mut AssignmentsPropositional,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) learning_clause_minimisation: &'a mut bool,
    pub(crate) propagator_store: &'a mut PropagatorStore,
    #[allow(dead_code)]
    pub(crate) assumptions: &'a Vec<Literal>,

    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) clause_allocator: &'a mut ClauseAllocator,
    pub(crate) explanation_clause_manager: &'a mut ExplanationClauseManager,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) counters: &'a mut Counters,
    pub(crate) learned_clause_manager: &'a mut LearnedClauseManager,

    pub(crate) propositional_trail_index: &'a mut usize,
    pub(crate) propagator_queue: &'a mut PropagatorQueue,
    pub(crate) watch_list_cp: &'a mut WatchListCP,
    pub(crate) sat_trail_synced_position: &'a mut usize,
    pub(crate) cp_trail_synced_position: &'a mut usize,
    pub(crate) event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,

    pub(crate) backtrack_event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,
    pub(crate) last_notified_cp_trail_index: &'a mut usize,
}

impl<'a> ConflictAnalysisContext<'a> {
    pub(crate) fn last_decision(&self) -> Literal {
        self.assignments_propositional.get_last_decision()
    }

    pub(crate) fn backtrack(&mut self, backtrack_level: usize) {
        pumpkin_assert_simple!(backtrack_level < self.get_decision_level());

        // We clear all of the unprocessed events from the watch list since synchronisation, we do
        // not need to process these events
        if self.watch_list_cp.is_watching_anything() {
            pumpkin_assert_simple!(self.event_drain.is_empty());
            self.assignments_integer
                .drain_domain_events()
                .for_each(drop);
        }

        // We synchronise the assignments propositional and for each unassigned literal, we notify
        // the brancher that it has been unassigned
        let unassigned_literals = self.assignments_propositional.synchronise(backtrack_level);
        unassigned_literals.for_each(|literal| {
            self.brancher.on_unassign_literal(literal);
        });

        // We synchronise the clausal propagator which sets the next variable on the trail to
        // propagate
        self.clausal_propagator
            .synchronise(self.assignments_propositional.num_trail_entries());
        pumpkin_assert_simple!(
            self.assignments_propositional.get_decision_level()
                < self.assignments_integer.get_decision_level(),
            "assignments_propositional must be backtracked _before_ CPEngineDataStructures"
        );

        // We also set the last processed trail entry of the propositional trail
        *self.propositional_trail_index = min(
            *self.propositional_trail_index,
            self.assignments_propositional.num_trail_entries(),
        );

        // We synchronise the assignments integer and for each of the unassigned integer variables,
        // we notify the brancher that it has been unassigned
        self.assignments_integer
            .synchronise(
                backtrack_level,
                self.watch_list_cp.is_watching_any_backtrack_events(),
                *self.last_notified_cp_trail_index,
            )
            .iter()
            .for_each(|(domain_id, previous_value)| {
                self.brancher
                    .on_unassign_integer(*domain_id, *previous_value)
            });
        pumpkin_assert_simple!(
            !self.watch_list_cp.is_watching_anything()
                || *self.last_notified_cp_trail_index
                    >= self.assignments_integer.num_trail_entries(),
        );
        *self.last_notified_cp_trail_index = self.assignments_integer.num_trail_entries();

        self.reason_store.synchronise(backtrack_level);
        //  note that variable_literal_mappings sync should be called after the sat/cp data
        // structures backtrack

        pumpkin_assert_simple!(
            *self.sat_trail_synced_position >= self.assignments_propositional.num_trail_entries()
        );
        pumpkin_assert_simple!(
            *self.cp_trail_synced_position >= self.assignments_integer.num_trail_entries()
        );
        *self.cp_trail_synced_position = self.assignments_integer.num_trail_entries();
        *self.sat_trail_synced_position = self.assignments_propositional.num_trail_entries();
        // for now all propagators are called to synchronise
        //  in the future this will be improved in two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator in self.propagator_store.iter_propagators_mut() {
            let context =
                PropagationContext::new(self.assignments_integer, self.assignments_propositional);
            propagator.synchronise(&context);
        }

        let _ = self.process_backtrack_events();
        self.propagator_queue.clear();
    }

    fn process_backtrack_events(&mut self) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self.watch_list_cp.is_watching_any_backtrack_events() {
            self.backtrack_event_drain
                .extend(self.assignments_integer.drain_backtrack_domain_events());

            if self.backtrack_event_drain.is_empty() {
                return false;
            }

            for (event, domain) in self.backtrack_event_drain.drain(..) {
                for propagator_var in self
                    .watch_list_cp
                    .get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut self.propagator_store[propagator_var.propagator];
                    let context = PropagationContext::new(
                        self.assignments_integer,
                        self.assignments_propositional,
                    );

                    propagator.notify_backtrack(&context, propagator_var.variable, event.into())
                }
            }
        }
        true
    }

    pub(crate) fn enqueue_propagated_literal(&mut self, propagated_literal: Literal) {
        assert!(self
            .assignments_propositional
            .enqueue_propagated_literal(propagated_literal, ConstraintReference::NON_REASON)
            .is_none());
    }

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

        let _ = self.proof_log.log_inference(
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
