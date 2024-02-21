use crate::basic_types::ClauseReference;
use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintReference;
use crate::basic_types::DomainId;
use crate::basic_types::KeyedVec;
use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::basic_types::PropositionalVariable;
use crate::engine::constraint_satisfaction_solver::ClausalPropagator;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::reason::ReasonRef;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::CPEngineDataStructures;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::EmptyDomain;
use crate::engine::ExplanationClauseManager;
use crate::engine::SATEngineDataStructures;
use crate::engine::WatchListPropositional;
use crate::predicate;
use crate::propagators::clausal_propagators::ClausalPropagatorInterface;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Debug)]
pub struct SATCPMediator {
    mapping_domain_to_equality_literals: KeyedVec<DomainId, Box<[Literal]>>,
    mapping_domain_to_lower_bound_literals: KeyedVec<DomainId, Box<[Literal]>>,
    mapping_literal_to_predicates: KeyedVec<Literal, Vec<Predicate>>,
    /// [`AssignmentsInteger::trail`]
    /// [[cp_trail_synced_position][SATCPMediator::cp_trail_synced_position]] is the next entry
    /// that needs to be synchronised with [AssignmentsPropositional::trail].
    cp_trail_synced_position: usize,
    /// This is the SAT equivalent of the above, i.e., [AssignmentsPropositional::trail]
    /// [[sat_trail_synced_position][SATCPMediator::sat_trail_synced_position]] is the next
    /// [Literal] on the trail that needs to be synchronised with [AssignmentsInteger::trail].
    sat_trail_synced_position: usize,
    pub explanation_clause_manager: ExplanationClauseManager,
    pub true_literal: Literal,
    pub false_literal: Literal,
}

impl Default for SATCPMediator {
    fn default() -> SATCPMediator {
        // When the mediator is created for use in the ConstraintSatisfactionSolver, the true and
        // how are you doing this is gibberish false literals will be updated to match the
        // solver's true and false literals. However, we set this up here to facilitate
        // testing the mediator.
        let dummy_literal = Literal::new(PropositionalVariable::new(0), true);

        SATCPMediator {
            // `mapping_literal_to_predicates[literal]` is the vector of predicates associated with
            // the literal. Usually there is only one or two predicates associated with a literal,
            // but due to preprocessing, it could be that one literal is associated with two or more
            // predicates
            mapping_literal_to_predicates: KeyedVec::default(),
            mapping_domain_to_equality_literals: KeyedVec::default(),
            mapping_domain_to_lower_bound_literals: KeyedVec::default(),
            cp_trail_synced_position: 0,
            sat_trail_synced_position: 0,
            explanation_clause_manager: ExplanationClauseManager::default(),
            true_literal: dummy_literal,
            false_literal: !dummy_literal,
        }
    }
}

// methods for synchronising trails
impl SATCPMediator {
    pub fn synchronise_propositional_trail_based_on_integer_trail(
        &mut self,
        assignments_propositional: &mut AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
        clausal_propagator: &mut ClausalPropagator,
        clause_allocator: &mut ClauseAllocator,
    ) -> Option<ConflictInfo> {
        // for each entry on the integer trail, we now add the equivalent propositional
        // representation on the propositional trail  note that only one literal per
        // predicate will be stored      since the clausal propagator will propagate other
        // literals to ensure that the meaning of the literal is respected          e.g.,
        // placing [x >= 5] will prompt the clausal propagator to set [x >= 4] [x >= 3] ... [x >= 1]
        // to true
        for cp_trail_pos in self.cp_trail_synced_position..assignments_integer.num_trail_entries() {
            let entry = assignments_integer.get_trail_entry(cp_trail_pos);

            // It could be the case that the reason is `None`
            // due to a SAT propagation being put on the trail during
            // `synchronise_integer_trail_based_on_propositional_trail` In that case we
            // do not synchronise since we assume that the SAT trail is already aware of the
            // information
            if let Some(reason_ref) = entry.reason {
                let literal = self.get_predicate_literal(entry.predicate, assignments_integer);

                let constraint_reference = ConstraintReference::create_reason_reference(reason_ref);

                let conflict_info = assignments_propositional
                    .enqueue_propagated_literal(literal, constraint_reference);

                if conflict_info.is_some() {
                    self.cp_trail_synced_position = cp_trail_pos + 1;
                    return conflict_info;
                }

                // It could occur that one of these propagations caused a conflict in which case the
                // SAT-view and the CP-view are unsynchronised We need to ensure
                // that the views are synchronised up to the CP trail entry which caused the
                // conflict
                if let Err(e) =
                    clausal_propagator.propagate(assignments_propositional, clause_allocator)
                {
                    self.cp_trail_synced_position = cp_trail_pos + 1;
                    return Some(e);
                }
            }
        }
        self.cp_trail_synced_position = assignments_integer.num_trail_entries();
        None
    }

    pub fn synchronise_integer_trail_based_on_propositional_trail(
        &mut self,
        assignments_propositional: &mut AssignmentsPropositional,
        cp_data_structures: &mut CPEngineDataStructures,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> Result<(), EmptyDomain> {
        pumpkin_assert_moderate!(
            self.cp_trail_synced_position == cp_data_structures.assignments_integer.num_trail_entries(),
            "We can only sychronise the propositional trail if the integer trail is already sychronised."
        );

        // this could possibly be improved if it shows up as a performance hotspot
        //  in some cases when we push e.g., [x >= a] on the stack, then we could also add the
        // literals to the propositional stack  and update the next_domain_trail_position
        // pointer to go pass the entries that surely are not going to lead to any changes
        //  this would only work if the next_domain_trail pointer is already at the end of the
        // stack, think about this, could be useful for propagators      and might be useful
        // for a custom domain propagator  this would also simplify the code below, no
        // additional checks would be needed? Not sure.

        if cp_data_structures.assignments_integer.num_domains() == 0 || cp_propagators.is_empty() {
            self.sat_trail_synced_position = assignments_propositional.num_trail_entries();
            return Ok(());
        }

        for sat_trail_pos in
            self.sat_trail_synced_position..assignments_propositional.num_trail_entries()
        {
            let literal = assignments_propositional.get_trail_entry(sat_trail_pos);
            self.synchronise_literal(literal, cp_data_structures)?;
        }
        self.sat_trail_synced_position = assignments_propositional.num_trail_entries();
        // the newly added entries to the trail do not need to be synchronise with the propositional
        // trail  this is because the integer trail was already synchronise when this method
        // was called  and the newly added entries are already present on the propositional
        // trail
        self.cp_trail_synced_position = cp_data_structures.assignments_integer.num_trail_entries();

        let _ = cp_data_structures.process_domain_events(cp_propagators, assignments_propositional);

        Ok(())
    }

    fn synchronise_literal(
        &mut self,
        literal: Literal,
        cp_data_structures: &mut CPEngineDataStructures,
    ) -> Result<(), EmptyDomain> {
        // recall that a literal may be linked to multiple predicates
        //  e.g., this may happen when in preprocessing two literals are detected to be equal
        //  so now we loop for each predicate and make necessary updates
        //  (although currently we do not have any serious preprocessing!)
        for j in 0..self.mapping_literal_to_predicates[literal].len() {
            let predicate = self.mapping_literal_to_predicates[literal][j];
            cp_data_structures
                .assignments_integer
                .apply_predicate(predicate, None)?;
        }

        Ok(())
    }

    pub fn synchronise(
        &mut self,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) {
        pumpkin_assert_simple!(
            self.sat_trail_synced_position >= assignments_propositional.num_trail_entries()
        );
        pumpkin_assert_simple!(
            self.cp_trail_synced_position >= assignments_integer.num_trail_entries()
        );
        self.cp_trail_synced_position = assignments_integer.num_trail_entries();
        self.sat_trail_synced_position = assignments_propositional.num_trail_entries();
    }
}

// methods for creating new variables
impl SATCPMediator {
    pub fn create_new_propositional_variable_with_predicate(
        &mut self,
        watch_list_propositional: &mut WatchListPropositional,
        predicate: Predicate,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> PropositionalVariable {
        let variable = self.create_new_propositional_variable(
            watch_list_propositional,
            clausal_propagator,
            sat_data_structures,
        );
        self.add_predicate_information_to_propositional_variable(variable, predicate);
        variable
    }

    pub fn create_new_propositional_variable(
        &mut self,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> PropositionalVariable {
        let new_variable_index = sat_data_structures
            .assignments_propositional
            .num_propositional_variables();

        clausal_propagator.grow();

        watch_list_propositional.grow();

        sat_data_structures.assignments_propositional.grow();
        sat_data_structures.propositional_variable_selector.grow();
        sat_data_structures.propositional_value_selector.grow();

        // add an empty predicate vector for both polarities of the variable
        self.mapping_literal_to_predicates.push(vec![]);
        self.mapping_literal_to_predicates.push(vec![]);

        PropositionalVariable::new(new_variable_index)
    }

    /// Create a new integer variable and tie it to a fresh propositional representation. The given
    /// clausal propagator will be responsible for keeping the propositional representation
    /// consistent.
    pub fn create_new_domain(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
    ) -> DomainId {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Inconsistent bounds.");
        pumpkin_assert_simple!(self.debug_check_consistency(cp_data_structures));

        // 1. Create the integer domain.
        let domain_id = cp_data_structures.new_integer_domain(lower_bound, upper_bound);

        // 2. Create the propositional representation.
        self.create_propositional_representation(
            domain_id,
            clausal_propagator,
            sat_data_structures,
            cp_data_structures,
        );

        domain_id
    }

    /// Eagerly create the propositional representation of the integer variable. This is done using
    /// a unary representation.
    fn create_propositional_representation(
        &mut self,
        domain_id: DomainId,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
    ) {
        let lower_bound_literals = self.create_lower_bound_literals(
            cp_data_structures,
            domain_id,
            clausal_propagator,
            sat_data_structures,
        );

        let equality_literals = self.create_equality_literals(
            domain_id,
            &lower_bound_literals,
            cp_data_structures,
            clausal_propagator,
            sat_data_structures,
        );

        self.mapping_domain_to_lower_bound_literals
            .push(lower_bound_literals);

        self.mapping_domain_to_equality_literals
            .push(equality_literals.clone());

        // Add clause to select at least one equality.
        clausal_propagator
            .add_permanent_clause(
                equality_literals.into(),
                &mut sat_data_structures.assignments_propositional,
                &mut sat_data_structures.clause_allocator,
            )
            .expect("at least one equality must hold");
    }

    /// Create the literals representing [x == v] for all values v in the initial domain.
    fn create_equality_literals(
        &mut self,
        domain_id: DomainId,
        lower_bound_literals: &[Literal],
        cp_data_structures: &mut CPEngineDataStructures,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> Box<[Literal]> {
        assert!(
            lower_bound_literals.len() >= 2,
            "the lower bound literals should contain at least two literals"
        );

        let lower_bound = cp_data_structures
            .assignments_integer
            .get_lower_bound(domain_id);
        let upper_bound = cp_data_structures
            .assignments_integer
            .get_upper_bound(domain_id);

        // The literal at index i is [x == lb(x) + i].
        let mut equality_literals: Vec<Literal> = Vec::new();

        // Edge case where i = 0: [x == lb(x)] <-> ~[x >= lb(x) + 1]
        equality_literals.push(!lower_bound_literals[1]);

        // Add the predicate information to the [x == lower_bound] literal.
        //
        // Because the predicates are attached to propositional variables (which we treat as true
        // literals), we have to be mindful of the polarity of the predicate.
        self.add_predicate_information_to_propositional_variable(
            lower_bound_literals[1].get_propositional_variable(),
            predicate![domain_id != lower_bound],
        );

        for value in (lower_bound + 1)..upper_bound {
            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                &mut cp_data_structures.watch_list_propositional,
                predicate![domain_id == value],
                clausal_propagator,
                sat_data_structures,
            );

            equality_literals.push(Literal::new(propositional_variable, true));
        }

        if lower_bound < upper_bound {
            // Edge case [x == ub(x)] <-> [x >= ub(x)].
            let equals_ub = lower_bound_literals[lower_bound_literals.len() - 2];
            equality_literals.push(equals_ub);
            self.add_predicate_information_to_propositional_variable(
                equals_ub.get_propositional_variable(),
                predicate![domain_id == upper_bound],
            );
        }

        pumpkin_assert_eq_simple!(
            equality_literals.len(),
            (upper_bound - lower_bound + 1) as usize
        );

        // Enforce consistency of the equality literals through the following clauses:
        // [x == value] <-> [x >= value] AND ~[x >= value + 1]
        //
        // The equality literals for the bounds are skipped, as they are already defined above.
        for value in (lower_bound + 1)..upper_bound {
            let idx = value.abs_diff(lower_bound) as usize;

            // One side of the implication <-
            clausal_propagator.add_permanent_ternary_clause_unchecked(
                !lower_bound_literals[idx],
                lower_bound_literals[idx + 1],
                equality_literals[idx],
                &mut sat_data_structures.clause_allocator,
            );

            // The other side of the implication ->
            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[idx],
                lower_bound_literals[idx],
                &mut sat_data_structures.clause_allocator,
            );

            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[idx],
                !lower_bound_literals[idx + 1],
                &mut sat_data_structures.clause_allocator,
            );
        }

        equality_literals.into()
    }

    /// Eagerly create the literals that encode the bounds of the integer variable.
    fn create_lower_bound_literals(
        &mut self,
        cp_data_structures: &mut CPEngineDataStructures,
        domain_id: DomainId,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> Box<[Literal]> {
        let lower_bound = cp_data_structures
            .assignments_integer
            .get_lower_bound(domain_id);
        let upper_bound = cp_data_structures
            .assignments_integer
            .get_upper_bound(domain_id);

        // The literal at index i is [x >= lb(x) + i].
        let mut lower_bound_literals = Vec::new();

        // The integer variable will always be at least the lower bound of the initial domain.
        lower_bound_literals.push(self.true_literal);

        for value in (lower_bound + 1)..=upper_bound {
            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                &mut cp_data_structures.watch_list_propositional,
                predicate![domain_id >= value],
                clausal_propagator,
                sat_data_structures,
            );

            lower_bound_literals.push(Literal::new(propositional_variable, true));
        }

        // The integer variable is never bigger than the upper bound of the initial domain.
        lower_bound_literals.push(self.false_literal);

        pumpkin_assert_eq_simple!(
            lower_bound_literals.len(),
            (upper_bound - lower_bound + 2) as usize
        );

        // Enforce consistency over the lower bound literals by adding the following clause:
        // [x >= v + 1] -> [x >= v].
        //
        // Special case (skipped in the loop): [x >= lb(x) + 1] -> [x >= lb(x)], but
        // [x >= lb(x)] is trivially true.
        for v in (lower_bound + 2)..=upper_bound {
            let idx = v.abs_diff(lower_bound) as usize;

            clausal_propagator.add_permanent_implication_unchecked(
                lower_bound_literals[idx],
                lower_bound_literals[idx - 1],
                &mut sat_data_structures.clause_allocator,
            );
        }

        lower_bound_literals.into()
    }

    fn add_predicate_information_to_propositional_variable(
        &mut self,
        variable: PropositionalVariable,
        predicate: Predicate,
    ) {
        pumpkin_assert_simple!(
            !self.mapping_literal_to_predicates[Literal::new(variable, false)].contains(&predicate),
            "The predicate is already attached to the _negative_ literal, cannot do this twice."
        );

        // create a closure for convenience that adds predicates to literals
        let closure_add_predicate_to_literal =
            |literal: Literal,
             predicate: Predicate,
             mapping_literal_to_predicates: &mut KeyedVec<Literal, Vec<Predicate>>| {
                pumpkin_assert_simple!(
                    !mapping_literal_to_predicates[literal].contains(&predicate),
                    "The predicate is already attached to the literal, cannot do this twice."
                );
                // resize the mapping vector if necessary
                if literal.to_u32() as usize >= mapping_literal_to_predicates.len() {
                    mapping_literal_to_predicates
                        .resize((literal.to_u32() + 1) as usize, Vec::new());
                }
                // append the predicate - note that the assert makes sure the same predicate is
                // never added twice
                mapping_literal_to_predicates[literal].push(predicate);
            };

        // now use the closure to add the predicate to both the positive and negative literals

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

// methods for getting simple information on the interface of SAT and CP
impl SATCPMediator {
    pub fn get_lower_bound_literal(
        &self,
        domain: DomainId,
        lower_bound: i32,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        let initial_lower_bound = assignments_integer.get_initial_lower_bound(domain);
        let initial_upper_bound = assignments_integer.get_initial_upper_bound(domain);

        if lower_bound < initial_lower_bound {
            return self.true_literal;
        }

        if lower_bound > initial_upper_bound {
            return self.false_literal;
        }

        let literal_idx = lower_bound.abs_diff(initial_lower_bound) as usize;
        self.mapping_domain_to_lower_bound_literals[domain][literal_idx]
    }

    pub fn get_upper_bound_literal(
        &self,
        domain: DomainId,
        upper_bound: i32,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        !self.get_lower_bound_literal(domain, upper_bound + 1, assignments_integer)
    }

    pub fn get_equality_literal(
        &self,
        domain: DomainId,
        equality_constant: i32,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        let initial_lower_bound = assignments_integer.get_initial_lower_bound(domain);
        let initial_upper_bound = assignments_integer.get_initial_upper_bound(domain);

        if equality_constant < initial_lower_bound || equality_constant > initial_upper_bound {
            return self.false_literal;
        }

        let literal_idx = equality_constant.abs_diff(initial_lower_bound) as usize;
        self.mapping_domain_to_equality_literals[domain][literal_idx]
    }

    pub fn get_inequality_literal(
        &self,
        domain: DomainId,
        not_equal_constant: i32,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        !self.get_equality_literal(domain, not_equal_constant, assignments_integer)
    }

    pub fn get_predicate_literal(
        &self,
        predicate: Predicate,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        match predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.get_lower_bound_literal(domain_id, lower_bound, assignments_integer),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.get_upper_bound_literal(domain_id, upper_bound, assignments_integer),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.get_inequality_literal(domain_id, not_equal_constant, assignments_integer),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.get_equality_literal(domain_id, equality_constant, assignments_integer),
            Predicate::False => self.false_literal,
            Predicate::True => self.true_literal,
        }
    }

    pub fn get_conflict_reason_clause_reference(
        &mut self,
        conflict_info: &ConflictInfo,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
    ) -> ClauseReference {
        match conflict_info {
            ConflictInfo::VirtualBinaryClause { lit1, lit2 } => self
                .explanation_clause_manager
                .add_explanation_clause_unchecked(
                    vec![*lit1, *lit2],
                    &mut sat_data_structures.clause_allocator,
                ),
            ConflictInfo::Propagation { literal, reference } => {
                if reference.is_clause() {
                    reference.as_clause_reference()
                } else {
                    self.create_clause_from_propagation_reason(
                        *literal,
                        reference.get_reason_ref(),
                        cp_data_structures,
                        sat_data_structures,
                    )
                }
            }
            ConflictInfo::Explanation(propositional_conjunction) => {
                // create the explanation clause
                //  allocate a fresh vector each time might be a performance bottleneck
                //  todo better ways
                let explanation_literals = propositional_conjunction
                    .iter()
                    .map(|&p| {
                        !self.get_predicate_literal(p, &cp_data_structures.assignments_integer)
                    })
                    .chain(propositional_conjunction.iter_literals().map(|&l| !l))
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

        // Case 1: the literal was propagated by the clausal propagator
        if constraint_reference.is_clause() {
            clausal_propagator.get_literal_propagation_clause_reference(
                propagated_literal,
                &sat_data_structures.assignments_propositional,
                &mut sat_data_structures.clause_allocator,
                &mut self.explanation_clause_manager,
            )
        }
        // Case 2: the literal was placed on the propositional trail while synchronising the CP
        // trail with the propositional trail
        else {
            self.create_clause_from_propagation_reason(
                propagated_literal,
                constraint_reference.get_reason_ref(),
                cp_data_structures,
                sat_data_structures,
            )
        }
    }

    fn create_clause_from_propagation_reason(
        &mut self,
        propagated_literal: Literal,
        reason_ref: ReasonRef,
        cp_data_structures: &mut CPEngineDataStructures,
        sat_data_structures: &mut SATEngineDataStructures,
    ) -> ClauseReference {
        let (reason, assignments_integer) = cp_data_structures
            .compute_reason(reason_ref, &sat_data_structures.assignments_propositional);

        // create the explanation clause
        //  allocate a fresh vector each time might be a performance bottleneck
        //  todo better ways
        // important to keep propagated literal at the zero-th position
        let explanation_literals = std::iter::once(propagated_literal)
            .chain(
                reason
                    .iter()
                    .map(|&p| !self.get_predicate_literal(p, assignments_integer)),
            )
            .chain(reason.iter_literals().map(|&p| !p))
            .collect();

        self.explanation_clause_manager
            .add_explanation_clause_unchecked(
                explanation_literals,
                &mut sat_data_structures.clause_allocator,
            )
    }

    fn debug_check_consistency(&self, cp_data_structures: &CPEngineDataStructures) -> bool {
        pumpkin_assert_simple!(
            cp_data_structures.assignments_integer.num_domains() as usize
                == self.mapping_domain_to_lower_bound_literals.len()
        );
        pumpkin_assert_simple!(
            cp_data_structures.assignments_integer.num_domains() as usize
                == self.mapping_domain_to_equality_literals.len()
        );
        pumpkin_assert_simple!(
            cp_data_structures.assignments_integer.num_domains()
                == cp_data_structures.watch_list_cp.num_domains()
        );
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::PredicateConstructor;

    #[test]
    fn negative_upper_bound() {
        let mut mediator = SATCPMediator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();
        let domain_id = mediator.create_new_domain(
            0,
            10,
            &mut ClausalPropagator::default(),
            &mut sat_data_structures,
            &mut cp_data_structures,
        );
        let result = mediator.get_upper_bound_literal(
            domain_id,
            -2,
            &cp_data_structures.assignments_integer,
        );
        assert_eq!(
            result,
            sat_data_structures.assignments_propositional.false_literal
        );
    }

    #[test]
    fn lower_bound_literal_lower_than_lower_bound_should_be_true_literal() {
        let mut mediator = SATCPMediator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();
        let domain_id = mediator.create_new_domain(
            0,
            10,
            &mut ClausalPropagator::default(),
            &mut sat_data_structures,
            &mut cp_data_structures,
        );
        let result = mediator.get_lower_bound_literal(
            domain_id,
            -2,
            &cp_data_structures.assignments_integer,
        );
        assert_eq!(
            result,
            sat_data_structures.assignments_propositional.true_literal
        );
    }

    #[test]
    fn new_domain_with_negative_lower_bound() {
        let mut mediator = SATCPMediator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();

        let lb = -2;
        let ub = 2;

        let domain_id = mediator.create_new_domain(
            lb,
            ub,
            &mut ClausalPropagator::default(),
            &mut sat_data_structures,
            &mut cp_data_structures,
        );

        assert_eq!(
            lb,
            cp_data_structures
                .assignments_integer
                .get_lower_bound(domain_id)
        );

        assert_eq!(
            ub,
            cp_data_structures
                .assignments_integer
                .get_upper_bound(domain_id)
        );

        assert_eq!(
            mediator.true_literal,
            mediator.get_lower_bound_literal(
                domain_id,
                lb,
                &cp_data_structures.assignments_integer
            )
        );

        assert_eq!(
            mediator.false_literal,
            mediator.get_upper_bound_literal(
                domain_id,
                lb - 1,
                &cp_data_structures.assignments_integer
            )
        );

        assert!(sat_data_structures
            .assignments_propositional
            .is_literal_unassigned(mediator.get_equality_literal(
                domain_id,
                lb,
                &cp_data_structures.assignments_integer
            )));

        assert_eq!(
            mediator.false_literal,
            mediator.get_equality_literal(
                domain_id,
                lb - 1,
                &cp_data_structures.assignments_integer
            )
        );

        for value in (lb + 1)..ub {
            let literal = mediator.get_lower_bound_literal(
                domain_id,
                value,
                &cp_data_structures.assignments_integer,
            );

            assert!(sat_data_structures
                .assignments_propositional
                .is_literal_unassigned(literal));

            assert!(sat_data_structures
                .assignments_propositional
                .is_literal_unassigned(mediator.get_equality_literal(
                    domain_id,
                    value,
                    &cp_data_structures.assignments_integer
                )));
        }

        assert_eq!(
            mediator.false_literal,
            mediator.get_lower_bound_literal(
                domain_id,
                ub + 1,
                &cp_data_structures.assignments_integer
            )
        );
        assert_eq!(
            mediator.true_literal,
            mediator.get_upper_bound_literal(
                domain_id,
                ub,
                &cp_data_structures.assignments_integer
            )
        );
        assert!(sat_data_structures
            .assignments_propositional
            .is_literal_unassigned(mediator.get_equality_literal(
                domain_id,
                ub,
                &cp_data_structures.assignments_integer
            )));
        assert_eq!(
            mediator.false_literal,
            mediator.get_equality_literal(
                domain_id,
                ub + 1,
                &cp_data_structures.assignments_integer
            )
        );
    }

    #[test]
    fn clausal_propagation_is_synced_until_right_before_conflict() {
        let mut mediator = SATCPMediator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();
        let mut clausal_propagator = ClausalPropagator::default();

        let domain_id = mediator.create_new_domain(
            0,
            10,
            &mut clausal_propagator,
            &mut sat_data_structures,
            &mut cp_data_structures,
        );

        let dummy_reason = ReasonRef(0);

        let result = cp_data_structures.assignments_integer.tighten_lower_bound(
            domain_id,
            2,
            Some(dummy_reason),
        );
        assert!(result.is_ok());
        assert_eq!(
            cp_data_structures
                .assignments_integer
                .get_lower_bound(domain_id),
            2
        );

        let result = cp_data_structures.assignments_integer.tighten_lower_bound(
            domain_id,
            8,
            Some(dummy_reason),
        );
        assert!(result.is_ok());
        assert_eq!(
            cp_data_structures
                .assignments_integer
                .get_lower_bound(domain_id),
            8
        );

        let result = cp_data_structures.assignments_integer.tighten_lower_bound(
            domain_id,
            12,
            Some(dummy_reason),
        );
        assert!(result.is_err());
        assert_eq!(
            cp_data_structures
                .assignments_integer
                .get_lower_bound(domain_id),
            12
        );

        let _ = mediator.synchronise_propositional_trail_based_on_integer_trail(
            &mut sat_data_structures.assignments_propositional,
            &cp_data_structures.assignments_integer,
            &mut clausal_propagator,
            &mut sat_data_structures.clause_allocator,
        );

        for lower_bound in 0..=8 {
            let literal = mediator.get_predicate_literal(
                domain_id.lower_bound_predicate(lower_bound),
                &cp_data_structures.assignments_integer,
            );
            assert!(
                sat_data_structures
                    .assignments_propositional
                    .is_literal_assigned_true(literal),
                "Literal for lower-bound {lower_bound} is not assigned"
            );
        }
    }

    #[test]
    fn check_correspondence_predicates_creating_new_int_domain() {
        let mut mediator = SATCPMediator::default();
        let mut sat_data_structures = SATEngineDataStructures::default();
        let mut cp_data_structures = CPEngineDataStructures::default();
        let mut clausal_propagator = ClausalPropagator::default();

        let lower_bound = 0;
        let upper_bound = 10;
        let domain_id = mediator.create_new_domain(
            lower_bound,
            upper_bound,
            &mut clausal_propagator,
            &mut sat_data_structures,
            &mut cp_data_structures,
        );

        for bound in lower_bound + 1..upper_bound {
            let lower_bound_predicate = predicate![domain_id >= bound];
            let equality_predicate = predicate![domain_id == bound];
            for predicate in [lower_bound_predicate, equality_predicate] {
                let literal = mediator
                    .get_predicate_literal(predicate, &cp_data_structures.assignments_integer);
                assert!(mediator.mapping_literal_to_predicates[literal].contains(&predicate))
            }
        }
    }
}
