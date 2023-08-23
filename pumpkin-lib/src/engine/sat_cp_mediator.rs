use crate::basic_types::{
    ClauseReference, ConflictInfo, ConstraintReference, DomainId, Literal, Predicate,
    PropositionalVariable,
};

use crate::engine::{DebugHelper, Delta, PropagationContext, PropagatorId};
use crate::propagators::clausal_propagators::ClausalPropagatorInterface;
use crate::{pumpkin_assert_advanced, pumpkin_assert_moderate, pumpkin_assert_simple};

use super::constraint_satisfaction_solver::ClausalPropagator;
use super::{
    AssignmentsInteger, AssignmentsPropositional, CPEngineDataStructures,
    ConstraintProgrammingPropagator, ExplanationClauseManager, PropagatorVarId,
    SATEngineDataStructures,
};

pub struct SATCPMediator {
    synchronised_literal_to_predicate: Vec<(Predicate, Option<PropagatorVarId>)>, //todo explain
    mapping_integer_variable_to_equality_literals: Vec<Vec<Literal>>,
    mapping_integer_variable_to_lower_bound_literals: Vec<Vec<Literal>>,
    mapping_literal_to_predicates: Vec<Vec<Predicate>>,
    cp_trail_synced_position: usize, // assignments_integer.trail[cp_trail_synced_position] is the next entry that needs to be synchronised with the propositional assignment trail
    sat_trail_synced_position: usize, // this is the sat equivalent of the above, i.e., assignments_propositional.trail[sat_trail_synced_position] is the next literal on the trail that needs to be synchronised with the integer trail
    pub explanation_clause_manager: ExplanationClauseManager,
    pub true_literal: Literal,
    pub false_literal: Literal,
}

impl Default for SATCPMediator {
    fn default() -> SATCPMediator {
        let dummy_literal = Literal::new(PropositionalVariable::new(0), true);
        SATCPMediator {
            synchronised_literal_to_predicate: vec![],
            mapping_literal_to_predicates: vec![], //[literal] is the vector of predicates associated with the literal. Usually there is only one or two predicates associated with a literal, but due to preprocessing, it could be that one literal is associated with two or more predicates
            mapping_integer_variable_to_equality_literals: vec![],
            mapping_integer_variable_to_lower_bound_literals: vec![],
            cp_trail_synced_position: 0,
            sat_trail_synced_position: 0,
            explanation_clause_manager: ExplanationClauseManager::default(),
            true_literal: dummy_literal,
            false_literal: dummy_literal,
        }
    }
}

//methods for synchronising trails
impl SATCPMediator {
    pub fn synchronise_propositional_trail_based_on_integer_trail(
        &mut self,
        assignments_propositional: &mut AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) {
        //for each entry on the integer trail, we now add the equivalent propositional representation on the propositional trail
        //  note that only one literal per predicate will be stored
        //      since the clausal propagator will propagate other literals to ensure that the meaning of the literal is respected
        //          e.g., placing [x >= 5] will prompt the clausal propagator to set [x >= 4] [x >= 3] ... [x >= 1] to true
        for cp_trail_pos in self.cp_trail_synced_position..assignments_integer.num_trail_entries() {
            let entry = assignments_integer.get_trail_entry(cp_trail_pos);

            let propagator_id = assignments_integer
                .get_propagator_id_on_trail(cp_trail_pos)
                .expect(
                    "None is not expected for the propagator identifier here, strange, must abort.",
                );

            let literal = self.get_predicate_literal(entry.predicate);

            let constraint_reference =
                ConstraintReference::create_propagator_reference(propagator_id.0);

            assignments_propositional.enqueue_propagated_literal(literal, constraint_reference);
            self.synchronised_literal_to_predicate[literal] =
                (entry.predicate, entry.propagator_reason);
        }
        self.cp_trail_synced_position = assignments_integer.num_trail_entries();
    }

    pub fn synchronise_integer_trail_based_on_propositional_trail(
        &mut self,
        assignments_propositional: &AssignmentsPropositional,
        cp_data_structures: &mut CPEngineDataStructures,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) {
        pumpkin_assert_moderate!(
            self.cp_trail_synced_position == cp_data_structures.assignments_integer.num_trail_entries(),
            "We can only sychronise the propositional trail if the integer trail is already sychronised."
        );

        //this could possibly be improved if it shows up as a performance hotspot
        //  in some cases when we push e.g., [x >= a] on the stack, then we could also add the literals to the propositional stack
        //  and update the next_domain_trail_position pointer to go pass the entries that surely are not going to lead to any changes
        //  this would only work if the next_domain_trail pointer is already at the end of the stack, think about this, could be useful for propagators
        //      and might be useful for a custom domain propagator
        //  this would also simplify the code below, no additional checks would be needed? Not sure.

        if cp_data_structures
            .assignments_integer
            .num_integer_variables()
            == 0
            || cp_propagators.is_empty()
        {
            self.sat_trail_synced_position = assignments_propositional.trail.len();
            return;
        }

        for sat_trail_pos in self.sat_trail_synced_position..assignments_propositional.trail.len() {
            let literal = assignments_propositional.trail[sat_trail_pos];
            self.synchronise_literal(literal, cp_data_structures, cp_propagators);
        }
        self.sat_trail_synced_position = assignments_propositional.trail.len();
        //the newly added entries to the trail do not need to be synchronise with the propositional trail
        //  this is because the integer trail was already synchronise when this method was called
        //  and the newly added entries are already present on the propositional trail
        self.cp_trail_synced_position = cp_data_structures.assignments_integer.num_trail_entries();
    }

    fn synchronise_literal(
        &mut self,
        literal: Literal,
        cp_data_structures: &mut CPEngineDataStructures,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) {
        //recall that a literal may be linked to multiple predicates
        //  e.g., this may happen when in preprocessing two literals are detected to be equal
        //  so now we loop for each predicate and make necessary updates
        //  (although currently we do not have any serious preprocessing!)
        for j in 0..self.mapping_literal_to_predicates[literal].len() {
            let predicate = self.mapping_literal_to_predicates[literal][j];
            cp_data_structures.apply_predicate(&predicate, None, cp_propagators);
        }
    }

    pub fn synchronise(
        &mut self,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) {
        pumpkin_assert_simple!(
            self.sat_trail_synced_position >= assignments_propositional.trail.len()
        );
        pumpkin_assert_simple!(
            self.cp_trail_synced_position >= assignments_integer.num_trail_entries()
        );
        self.cp_trail_synced_position = assignments_integer.num_trail_entries();
        self.sat_trail_synced_position = assignments_propositional.trail.len();
    }
}

//methods for creating new variables
impl SATCPMediator {
    pub fn create_new_propositional_variable_with_predicate(
        &mut self,
        predicate: &Predicate,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> PropositionalVariable {
        let variable =
            self.create_new_propositional_variable(clausal_propagator, sat_data_structures);
        self.add_predicate_information_to_propositional_variable(variable, *predicate);
        variable
    }

    pub fn create_new_propositional_variable(
        &mut self,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> PropositionalVariable {
        let new_variable_index = sat_data_structures
            .assignments_propositional
            .num_propositional_variables();

        clausal_propagator.grow();

        sat_data_structures.assignments_propositional.grow();
        sat_data_structures.propositional_variable_selector.grow();
        sat_data_structures.propositional_value_selector.grow();

        //add an empty predicate vector for both polarities of the variable
        self.mapping_literal_to_predicates.push(vec![]);
        self.mapping_literal_to_predicates.push(vec![]);

        self.synchronised_literal_to_predicate
            .push((Predicate::get_dummy_predicate(), None));
        self.synchronised_literal_to_predicate
            .push((Predicate::get_dummy_predicate(), None));

        PropositionalVariable::new(new_variable_index)
    }

    pub fn create_new_integer_variable(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
    ) -> DomainId {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Inconsistent bounds.");
        pumpkin_assert_simple!(self.debug_check_consistency(cp_data_structures));

        let true_literal = sat_data_structures.assignments_propositional.true_literal;
        let false_literal = sat_data_structures.assignments_propositional.false_literal;

        //creating a variable entails three main operations:
        //	creating the domain of the variable
        //	creating literals to capture predicates (domain operations)
        //	creating a mapping of predicates to their corresponding domain operations

        let integer_variable = cp_data_structures
            .assignments_integer
            .grow(lower_bound, upper_bound);

        cp_data_structures.watch_list_cp.grow();

        //create the propositional representation of the integer variable
        //  this is done using a unary representation
        //	currently everything is done eagerly
        let mut lower_bound_literals = Vec::new(); //[i] is the literal [x >= i]

        //create lower bound literals [x >= k]

        //  set trivial cases below the lower bound
        for _i in 0..=lower_bound {
            lower_bound_literals.push(true_literal);
        }

        //  create propositional literals for the remaining lower bound literals
        for i in (lower_bound + 1)..=upper_bound {
            let lower_bound_predicate = Predicate::LowerBound {
                integer_variable,
                lower_bound: i,
            };

            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                &lower_bound_predicate,
                clausal_propagator,
                sat_data_structures,
            );

            lower_bound_literals.push(Literal::new(propositional_variable, true));
        }

        // Push the literal ~[x >= upper_bound + 1]
        lower_bound_literals.push(false_literal);
        pumpkin_assert_simple!(lower_bound_literals.len() == (upper_bound + 2) as usize);

        //add clauses to define the variables appropriately

        //	define lower bound literals
        //		[x >= value+1] -> [x >= value]
        //		special case (skipped in the loop): [x >= lower_bound+1] -> [x >= lower_bound], but since [x >= lower_bound] is trivially true this is ignored
        for i in ((lower_bound + 2) as usize)..=(upper_bound as usize) {
            clausal_propagator.add_permanent_implication_unchecked(
                lower_bound_literals[i],
                lower_bound_literals[i - 1],
                &mut sat_data_structures.clause_allocator,
            );
        }

        //create equality literals [x == k]

        //  set trivial cases below the lower bound
        let mut equality_literals: Vec<Literal> = Vec::new(); //[i] is the literal [x = i]
        for _i in 0..lower_bound {
            equality_literals.push(false_literal);
        }

        //  corner case #1: [x == lower_bound] <-> ~[x >= lower_bound+1]
        equality_literals.push(!lower_bound_literals[(lower_bound + 1) as usize]);
        //add the predicate information to the [x == lower_bound] literal
        //  in this case the variable [x == lower_bound] also has the meaning ![x >= lower_bound+1]
        //  note that meanings are attached to variables, so we need to be careful about the polarity of the literal when attaching
        //  here we say [x >= lower_bound+1] <-> [x != lower_bound]
        self.add_predicate_information_to_propositional_variable(
            lower_bound_literals[(lower_bound + 1) as usize].get_propositional_variable(),
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant: lower_bound,
            },
        );

        for i in (lower_bound + 1)..upper_bound {
            let equality_predicate = Predicate::Equal {
                integer_variable,
                equality_constant: i,
            };

            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                &equality_predicate,
                clausal_propagator,
                sat_data_structures,
            );

            equality_literals.push(Literal::new(propositional_variable, true));
        }

        if lower_bound < upper_bound {
            //  corner case #2: [x == upper_bound] <-> [x >= upper_bound]}
            equality_literals.push(lower_bound_literals[upper_bound as usize]);
            //add predicate information to the [x == upper_bound] literal
            self.add_predicate_information_to_propositional_variable(
                lower_bound_literals[upper_bound as usize].get_propositional_variable(),
                Predicate::Equal {
                    integer_variable,
                    equality_constant: upper_bound,
                },
            );
        }

        pumpkin_assert_simple!(equality_literals.len() == (upper_bound + 1) as usize);

        //	define equality literals
        //		[x == value] <-> [x >= value] AND ~[x >= value + 1]
        //		recall from above that [x == lower_bound] and [x == upper_bound] are effectively defined by being set to the corresponding lower bound literals, and so are skipped
        for i in ((lower_bound + 1) as usize)..(upper_bound as usize) {
            //one side of the implication <-
            clausal_propagator.add_permanent_ternary_clause_unchecked(
                !lower_bound_literals[i],
                lower_bound_literals[i + 1],
                equality_literals[i],
                &mut sat_data_structures.clause_allocator,
            );
            //the other side of the implication ->
            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[i],
                lower_bound_literals[i],
                &mut sat_data_structures.clause_allocator,
            );

            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[i],
                !lower_bound_literals[i + 1],
                &mut sat_data_structures.clause_allocator,
            );
        }

        self.mapping_integer_variable_to_lower_bound_literals
            .push(lower_bound_literals);

        self.mapping_integer_variable_to_equality_literals
            .push(equality_literals);

        integer_variable
    }

    pub fn add_predicate_information_to_propositional_variable(
        &mut self,
        variable: PropositionalVariable,
        predicate: Predicate,
    ) {
        pumpkin_assert_simple!(
            !self.mapping_literal_to_predicates[Literal::new(variable, false)].contains(&predicate),
            "The predicate is already attached to the _negative_ literal, cannot do this twice."
        );

        //create a closure for convenience that adds predicates to literals
        let closure_add_predicate_to_literal = |literal: Literal,
                                                predicate: Predicate,
                                                mapping_literal_to_predicates: &mut Vec<
            Vec<Predicate>,
        >| {
            pumpkin_assert_simple!(
                !mapping_literal_to_predicates[literal].contains(&predicate),
                "The predicate is already attached to the literal, cannot do this twice."
            );
            //resize the mapping vector if necessary
            if literal.to_u32() as usize >= mapping_literal_to_predicates.len() {
                mapping_literal_to_predicates.resize((literal.to_u32() + 1) as usize, Vec::new());
            }
            //append the predicate - note that the assert makes sure the same predicate is never added twice
            mapping_literal_to_predicates[literal].push(predicate);
        };

        //now use the closure to add the predicate to both the positive and negative literals

        let positive_literal = Literal::new(variable, true);
        closure_add_predicate_to_literal(
            positive_literal,
            predicate,
            &mut self.mapping_literal_to_predicates,
        );

        let negative_literal = Literal::new(variable, false);
        closure_add_predicate_to_literal(
            negative_literal,
            !predicate,
            &mut self.mapping_literal_to_predicates,
        );
    }
}

//methods for getting simple information on the interface of SAT and CP
impl SATCPMediator {
    pub fn get_lower_bound_literal(&self, integer_variable: DomainId, lower_bound: i32) -> Literal {
        if lower_bound as usize
            >= self.mapping_integer_variable_to_lower_bound_literals[integer_variable].len()
        {
            self.false_literal
        } else if lower_bound.is_negative() {
            self.true_literal
        } else {
            self.mapping_integer_variable_to_lower_bound_literals[integer_variable]
                [lower_bound as usize]
        }
    }

    pub fn get_upper_bound_literal(&self, integer_variable: DomainId, upper_bound: i32) -> Literal {
        !self.get_lower_bound_literal(integer_variable, upper_bound + 1)
    }

    pub fn get_equality_literal(
        &self,
        integer_variable: DomainId,
        equality_constant: i32,
    ) -> Literal {
        if equality_constant as usize
            >= self.mapping_integer_variable_to_equality_literals[integer_variable].len()
            || equality_constant.is_negative()
        {
            self.false_literal
        } else {
            self.mapping_integer_variable_to_equality_literals[integer_variable]
                [equality_constant as usize]
        }
    }

    pub fn get_inequality_literal(
        &self,
        integer_variable: DomainId,
        not_equal_constant: i32,
    ) -> Literal {
        !self.get_equality_literal(integer_variable, not_equal_constant)
    }

    pub fn get_predicate_literal(&self, predicate: Predicate) -> Literal {
        match predicate {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => self.get_lower_bound_literal(integer_variable, lower_bound),
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => self.get_upper_bound_literal(integer_variable, upper_bound),
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => self.get_inequality_literal(integer_variable, not_equal_constant),
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => self.get_equality_literal(integer_variable, equality_constant),
        }
    }

    pub fn get_conflict_reason_clause_reference(
        &mut self,
        conflict_info: &ConflictInfo,
        sat_data_structures: &mut SATEngineDataStructures,
        _cp_data_structures: &CPEngineDataStructures,
        _cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> ClauseReference {
        match conflict_info {
            ConflictInfo::VirtualBinaryClause { lit1, lit2 } => self
                .explanation_clause_manager
                .add_explanation_clause_unchecked(
                    vec![*lit1, *lit2],
                    &mut sat_data_structures.clause_allocator,
                ),
            ConflictInfo::StandardClause { clause_reference } => *clause_reference,
            ConflictInfo::Explanation {
                propositional_conjunction,
            } => {
                //create the explanation clause
                //  allocate a fresh vector each time might be a performance bottleneck
                //  todo better ways
                let explanation_literals = propositional_conjunction
                    .iter()
                    .map(|&p| !self.get_predicate_literal(p))
                    .collect();

                self.explanation_clause_manager
                    .add_explanation_clause_unchecked(
                        explanation_literals,
                        &mut sat_data_structures.clause_allocator,
                    )
            }
        }
    }

    pub fn get_propagation_clause_reference(
        &mut self,
        propagated_literal: Literal,
        clausal_propagator: &ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> ClauseReference {
        pumpkin_assert_moderate!(
            !sat_data_structures
                .assignments_propositional
                .is_literal_root_assignment(propagated_literal),
            "Reasons are not kept properly for root propagations."
        );

        pumpkin_assert_moderate!(
            sat_data_structures
                .assignments_propositional
                .is_literal_assigned_true(propagated_literal),
            "Reason for propagation only makes sense for true literals."
        );

        let constraint_reference = sat_data_structures
            .assignments_propositional
            .get_variable_reason_constraint(propagated_literal.get_propositional_variable());

        //Case 1: the literal was propagated by the clausal propagator
        if constraint_reference.is_clause() {
            clausal_propagator.get_literal_propagation_clause_reference(
                propagated_literal,
                &sat_data_structures.assignments_propositional,
                &mut sat_data_structures.clause_allocator,
                &mut self.explanation_clause_manager,
            )
        }
        //Case 2: the literal was placed on the propositional trail while synchronising the CP trail with the propositional trail
        else {
            let synchonised_entry = self.synchronised_literal_to_predicate[propagated_literal];
            let propagator_id = constraint_reference.get_propagator_id();

            let context = PropagationContext::new(
                &mut cp_data_structures.assignments_integer,
                PropagatorId(propagator_id),
            );

            let delta =
                Delta::from_predicate(synchonised_entry.1.unwrap().variable, synchonised_entry.0);

            let propagator = &mut cp_propagators[propagator_id as usize];
            let reason = propagator.get_reason_for_propagation(&context, delta);

            pumpkin_assert_advanced!(DebugHelper::debug_propagator_reason(
                synchonised_entry.0,
                &reason,
                &cp_data_structures.assignments_integer,
                propagator.as_ref(),
                propagator_id
            ));

            //create the explanation clause
            //  allocate a fresh vector each time might be a performance bottleneck
            //  todo better ways
            //important to keep propagated literal at the zero-th position
            let explanation_literals = std::iter::once(propagated_literal)
                .chain(reason.iter().map(|&p| !self.get_predicate_literal(p)))
                .collect();

            self.explanation_clause_manager
                .add_explanation_clause_unchecked(
                    explanation_literals,
                    &mut sat_data_structures.clause_allocator,
                )
        }
    }

    fn debug_check_consistency(&self, cp_data_structures: &CPEngineDataStructures) -> bool {
        pumpkin_assert_simple!(
            cp_data_structures
                .assignments_integer
                .num_integer_variables() as usize
                == self.mapping_integer_variable_to_lower_bound_literals.len()
        );
        pumpkin_assert_simple!(
            cp_data_structures
                .assignments_integer
                .num_integer_variables() as usize
                == self.mapping_integer_variable_to_equality_literals.len()
        );
        pumpkin_assert_simple!(
            cp_data_structures
                .assignments_integer
                .num_integer_variables()
                == cp_data_structures.watch_list_cp.num_integer_variables()
        );
        true
    }
}
