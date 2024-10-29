use drcp_format::steps::StepId;

use crate::basic_types::ClauseReference;
use crate::basic_types::KeyedVec;
use crate::branching::Brancher;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonStore;
use crate::engine::solver_statistics::SolverStatistics;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::LearnedClauseManager;
use crate::engine::SatisfactionSolverOptions;
use crate::engine::VariableLiteralMappings;
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
    pub(crate) nogood_step_ids: &'a KeyedVec<ClauseReference, Option<StepId>>,

    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) clause_allocator: &'a mut ClauseAllocator,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) counters: &'a mut SolverStatistics,
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
}
