//! Used to create propositional and integer variables. Holds information about mapping literals
//! to predicates (atomic constraints) and vice versa.
//!
//! Note that when integer variables are created, the solver also creates propositional variables
//! corresponding to atomic constraints (predicates).

use crate::basic_types::KeyedVec;
use crate::basic_types::StorageKey;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::cp::WatchListCP;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::WatchListPropositional;
use crate::predicate;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_simple;

#[derive(Debug, Default)]
pub struct VariableLiteralMappings {
    /// `domain_to_equality_literals[DomainId x][i]` is the [`Literal`]
    /// that represents `[x == i + initial_lb(x)]`, where `initial_lb(x)` is
    /// the lower bound of [`DomainId`] `x` at the time of its creation.
    pub domain_to_equality_literals: KeyedVec<DomainId, Box<[Literal]>>,
    /// `domain_to_lower_bound_literals[DomainId x][i]` is the [`Literal`]
    /// that represents `[x >= i + initial_lb(x)]`, where `initial_lb(x)` is
    /// the lower bound of [`DomainId`] `x` at the time of its creation.
    /// Note that the [`Literal`]s representing `[x <= k]` are obtained by negating `[x >= k+1]`.
    pub domain_to_lower_bound_literals: KeyedVec<DomainId, Box<[Literal]>>,
    /// `literal_to_predicates[literal]` is the vector of [`IntegerPredicate`]s associated with
    /// the `literal`. Usually there are one or two [`IntegerPredicate`]s associated with a
    /// [`Literal`], but due to preprocessing (not currently implemented), it could be that one
    /// [`Literal`] is associated with three or more [`IntegerPredicate`]s.
    pub literal_to_predicates: KeyedVec<Literal, Vec<IntegerPredicate>>,
}

// methods for creating new variables
impl VariableLiteralMappings {
    /// Creates a new propositional literals, and registers the variable to the given predicate.
    ///
    /// Note that this function does not guarantee that the literal is appropriately propagated
    /// depending on the predicate. This function merely established a link in the internal data
    /// structures.
    fn create_new_propositional_variable_with_predicate(
        &mut self,
        watch_list_propositional: &mut WatchListPropositional,
        predicate: IntegerPredicate,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
    ) -> PropositionalVariable {
        let variable = self.create_new_propositional_variable(
            watch_list_propositional,
            clausal_propagator,
            assignments_propositional,
        );
        self.add_predicate_information_to_propositional_variable(variable, predicate);
        variable
    }

    /// Creates a new propositional variables.
    ///
    /// Note that the variable is not registered with any predicate.
    pub fn create_new_propositional_variable(
        &mut self,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
    ) -> PropositionalVariable {
        let new_variable_index = assignments_propositional.num_propositional_variables();

        clausal_propagator.grow();

        watch_list_propositional.grow();

        assignments_propositional.grow();

        // add an empty predicate vector for both polarities of the variable
        self.literal_to_predicates.push(vec![]);
        self.literal_to_predicates.push(vec![]);

        PropositionalVariable::new(new_variable_index)
    }

    /// Create a new integer variable and tie it to a fresh propositional representation. The given
    /// clausal propagator will be responsible for keeping the propositional representation
    /// consistent.
    #[allow(clippy::too_many_arguments)]
    pub fn create_new_domain(
        &mut self,
        lower_bound: i32,
        upper_bound: i32,
        assignments_integer: &mut AssignmentsInteger,
        watch_list_cp: &mut WatchListCP,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> DomainId {
        pumpkin_assert_simple!(lower_bound <= upper_bound, "Inconsistent bounds.");
        // pumpkin_assert_simple!(self.debug_check_consistency(cp_data_structures));

        // 1. Create the integer/domain representation.
        let domain_id = assignments_integer.grow(lower_bound, upper_bound);
        watch_list_cp.grow();

        // 2. Create the propositional representation.
        self.create_propositional_representation(
            domain_id,
            assignments_integer,
            watch_list_propositional,
            clausal_propagator,
            assignments_propositional,
            clause_allocator,
        );

        domain_id
    }

    /// Eagerly create the propositional representation of the integer variable. This is done using
    /// a unary representation.
    fn create_propositional_representation(
        &mut self,
        domain_id: DomainId,
        assignments_integer: &AssignmentsInteger,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) {
        let lower_bound_literals = self.create_lower_bound_literals(
            domain_id,
            assignments_integer,
            watch_list_propositional,
            clausal_propagator,
            assignments_propositional,
            clause_allocator,
        );

        let equality_literals = self.create_equality_literals(
            domain_id,
            &lower_bound_literals,
            assignments_integer,
            watch_list_propositional,
            clausal_propagator,
            assignments_propositional,
            clause_allocator,
        );

        self.domain_to_lower_bound_literals
            .push(lower_bound_literals);

        self.domain_to_equality_literals
            .push(equality_literals.clone());

        // Add clause to select at least one equality.
        // clausal_propagator
        // .add_permanent_clause(
        // equality_literals.into(),
        // assignments_propositional,
        // clause_allocator,
        // )
        // .expect("at least one equality must hold");
        if equality_literals.len() > 1 {
            let _ = clausal_propagator.add_clause_unchecked(
                equality_literals.into(),
                false,
                clause_allocator,
            );
        }
    }

    /// Create the literals representing [x == v] for all values v in the initial domain.
    #[allow(clippy::too_many_arguments)]
    fn create_equality_literals(
        &mut self,
        domain_id: DomainId,
        lower_bound_literals: &[Literal],
        assignments_integer: &AssignmentsInteger,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> Box<[Literal]> {
        assert!(
            lower_bound_literals.len() >= 2,
            "the lower bound literals should contain at least two literals"
        );

        let lower_bound = assignments_integer.get_lower_bound(domain_id);
        let upper_bound = assignments_integer.get_upper_bound(domain_id);

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
            predicate![domain_id != lower_bound].try_into().unwrap(),
        );

        for value in (lower_bound + 1)..upper_bound {
            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                watch_list_propositional,
                predicate![domain_id == value].try_into().unwrap(),
                clausal_propagator,
                assignments_propositional,
            );

            equality_literals.push(Literal::new(propositional_variable, true));
        }

        if lower_bound < upper_bound {
            assert!(lower_bound_literals.last().unwrap().index() == 0);

            // Edge case [x == ub(x)] <-> [x >= ub(x)].
            // Note the -2: this is because the last literal
            // is reserved for a trivially false literal.
            let equals_ub = lower_bound_literals[lower_bound_literals.len() - 2];
            equality_literals.push(equals_ub);
            self.add_predicate_information_to_propositional_variable(
                equals_ub.get_propositional_variable(),
                predicate![domain_id == upper_bound].try_into().unwrap(),
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
                clause_allocator,
            );

            // The other side of the implication ->
            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[idx],
                lower_bound_literals[idx],
                clause_allocator,
            );

            clausal_propagator.add_permanent_implication_unchecked(
                equality_literals[idx],
                !lower_bound_literals[idx + 1],
                clause_allocator,
            );
        }

        equality_literals.into()
    }

    /// Eagerly create the literals that encode the bounds of the integer variable.
    #[allow(clippy::too_many_arguments)]
    fn create_lower_bound_literals(
        &mut self,
        domain_id: DomainId,
        assignments_integer: &AssignmentsInteger,
        watch_list_propositional: &mut WatchListPropositional,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments_propositional: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> Box<[Literal]> {
        let lower_bound = assignments_integer.get_lower_bound(domain_id);
        let upper_bound = assignments_integer.get_upper_bound(domain_id);

        // The literal at index i is [x >= lb(x) + i].
        let mut lower_bound_literals = Vec::new();

        // The integer variable will always be at least the lower bound of the initial domain.
        lower_bound_literals.push(assignments_propositional.true_literal);

        for value in (lower_bound + 1)..=upper_bound {
            let propositional_variable = self.create_new_propositional_variable_with_predicate(
                watch_list_propositional,
                predicate![domain_id >= value].try_into().unwrap(),
                clausal_propagator,
                assignments_propositional,
            );

            lower_bound_literals.push(Literal::new(propositional_variable, true));
        }

        // The integer variable is never bigger than the upper bound of the initial domain.
        lower_bound_literals.push(assignments_propositional.false_literal);

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
                clause_allocator,
            );
        }

        lower_bound_literals.into()
    }

    fn add_predicate_information_to_propositional_variable(
        &mut self,
        variable: PropositionalVariable,
        predicate: IntegerPredicate,
    ) {
        pumpkin_assert_simple!(
            !self.literal_to_predicates[Literal::new(variable, false)].contains(&predicate),
            "The predicate is already attached to the _negative_ literal, cannot do this twice."
        );

        // create a closure for convenience that adds predicates to literals
        let closure_add_predicate_to_literal = |literal: Literal,
                                                predicate: IntegerPredicate,
                                                mapping_literal_to_predicates: &mut KeyedVec<
            Literal,
            Vec<IntegerPredicate>,
        >| {
            pumpkin_assert_simple!(
                !mapping_literal_to_predicates[literal].contains(&predicate),
                "The predicate is already attached to the literal, cannot do this twice."
            );
            // resize the mapping vector if necessary
            if literal.to_u32() as usize >= mapping_literal_to_predicates.len() {
                mapping_literal_to_predicates.resize((literal.to_u32() + 1) as usize, Vec::new());
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
            &mut self.literal_to_predicates,
        );

        let negative_literal = Literal::new(variable, false);
        closure_add_predicate_to_literal(
            negative_literal,
            !predicate,
            &mut self.literal_to_predicates,
        );
    }

    /// Get integer predicates for a literal.
    pub(crate) fn get_predicates(
        &self,
        literal: Literal,
    ) -> impl Iterator<Item = IntegerPredicate> + '_ {
        self.literal_to_predicates[literal].iter().copied()
    }
}

// methods for getting simple information on the interface of SAT and CP
impl VariableLiteralMappings {
    /// Returns the [`DomainId`] of the first [`IntegerPredicate`] which the provided `literal` is
    /// linked to or [`None`] if no such [`DomainId`] exists.
    pub fn get_domain_literal(&self, literal: Literal) -> Option<DomainId> {
        self.literal_to_predicates[literal]
            .first()
            .map(|predicate| predicate.get_domain())
    }

    ///  Returns a literal which corresponds to the provided [`IntegerPredicate`].
    pub fn get_literal(
        &self,
        predicate: IntegerPredicate,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        match predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.get_lower_bound_literal(
                domain_id,
                lower_bound,
                assignments_propositional,
                assignments_integer,
            ),
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.get_upper_bound_literal(
                domain_id,
                upper_bound,
                assignments_propositional,
                assignments_integer,
            ),
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.get_inequality_literal(
                domain_id,
                not_equal_constant,
                assignments_propositional,
                assignments_integer,
            ),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => self.get_equality_literal(
                domain_id,
                equality_constant,
                assignments_propositional,
                assignments_integer,
            ),
        }
    }

    pub fn get_lower_bound_literal(
        &self,
        domain: DomainId,
        lower_bound: i32,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        let initial_lower_bound = assignments_integer.get_initial_lower_bound(domain);
        let initial_upper_bound = assignments_integer.get_initial_upper_bound(domain);

        if lower_bound < initial_lower_bound {
            return assignments_propositional.true_literal;
        }

        if lower_bound > initial_upper_bound {
            return assignments_propositional.false_literal;
        }

        let literal_idx = lower_bound.abs_diff(initial_lower_bound) as usize;
        self.domain_to_lower_bound_literals[domain][literal_idx]
    }

    pub fn get_upper_bound_literal(
        &self,
        domain: DomainId,
        upper_bound: i32,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        !self.get_lower_bound_literal(
            domain,
            upper_bound + 1,
            assignments_propositional,
            assignments_integer,
        )
    }

    pub fn get_equality_literal(
        &self,
        domain: DomainId,
        equality_constant: i32,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        let initial_lower_bound = assignments_integer.get_initial_lower_bound(domain);
        let initial_upper_bound = assignments_integer.get_initial_upper_bound(domain);

        if equality_constant < initial_lower_bound || equality_constant > initial_upper_bound {
            return assignments_propositional.false_literal;
        }

        let literal_idx = equality_constant.abs_diff(initial_lower_bound) as usize;
        self.domain_to_equality_literals[domain][literal_idx]
    }

    pub fn get_inequality_literal(
        &self,
        domain: DomainId,
        not_equal_constant: i32,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Literal {
        !self.get_equality_literal(
            domain,
            not_equal_constant,
            assignments_propositional,
            assignments_integer,
        )
    }
}

#[cfg(test)]
mod tests {
    use assignments_integer::AssignmentsInteger;
    use watch_list_cp::WatchListCP;
    use watch_list_propositional::WatchListPropositional;

    use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
    use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
    use crate::engine::cp::assignments_integer;
    use crate::engine::cp::watch_list_cp;
    use crate::engine::cp::watch_list_propositional;
    use crate::engine::AssignmentsPropositional;
    use crate::engine::VariableLiteralMappings;
    use crate::predicate;

    #[test]
    fn negative_upper_bound() {
        let mut variable_literal_mappings = VariableLiteralMappings::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut watch_list_cp = WatchListCP::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut clausal_propagator = ClausalPropagatorType::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut clausal_allocator = ClauseAllocator::default();

        let domain_id = variable_literal_mappings.create_new_domain(
            0,
            10,
            &mut assignments_integer,
            &mut watch_list_cp,
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
            &mut clausal_allocator,
        );

        let result = variable_literal_mappings.get_upper_bound_literal(
            domain_id,
            -2,
            &assignments_propositional,
            &assignments_integer,
        );

        assert_eq!(result, assignments_propositional.false_literal);
    }

    #[test]
    fn lower_bound_literal_lower_than_lower_bound_should_be_true_literal() {
        let mut variable_literal_mappings = VariableLiteralMappings::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut watch_list_cp = WatchListCP::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut clausal_propagator = ClausalPropagatorType::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut clausal_allocator = ClauseAllocator::default();

        let domain_id = variable_literal_mappings.create_new_domain(
            0,
            10,
            &mut assignments_integer,
            &mut watch_list_cp,
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
            &mut clausal_allocator,
        );
        let result = variable_literal_mappings.get_lower_bound_literal(
            domain_id,
            -2,
            &assignments_propositional,
            &assignments_integer,
        );
        assert_eq!(result, assignments_propositional.true_literal);
    }

    #[test]
    fn new_domain_with_negative_lower_bound() {
        let mut variable_literal_mappings = VariableLiteralMappings::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut watch_list_cp = WatchListCP::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut clausal_propagator = ClausalPropagatorType::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut clausal_allocator = ClauseAllocator::default();

        let lb = -2;
        let ub = 2;

        let domain_id = variable_literal_mappings.create_new_domain(
            lb,
            ub,
            &mut assignments_integer,
            &mut watch_list_cp,
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
            &mut clausal_allocator,
        );

        assert_eq!(lb, assignments_integer.get_lower_bound(domain_id));
        assert_eq!(ub, assignments_integer.get_upper_bound(domain_id));

        assert_eq!(
            assignments_propositional.true_literal,
            variable_literal_mappings.get_lower_bound_literal(
                domain_id,
                lb,
                &assignments_propositional,
                &assignments_integer,
            )
        );

        assert_eq!(
            assignments_propositional.false_literal,
            variable_literal_mappings.get_upper_bound_literal(
                domain_id,
                lb - 1,
                &assignments_propositional,
                &assignments_integer,
            )
        );

        assert!(assignments_propositional.is_literal_unassigned(
            variable_literal_mappings.get_equality_literal(
                domain_id,
                lb,
                &assignments_propositional,
                &assignments_integer,
            )
        ));

        assert_eq!(
            assignments_propositional.false_literal,
            variable_literal_mappings.get_equality_literal(
                domain_id,
                lb - 1,
                &assignments_propositional,
                &assignments_integer,
            )
        );

        for value in (lb + 1)..ub {
            let literal = variable_literal_mappings.get_lower_bound_literal(
                domain_id,
                value,
                &assignments_propositional,
                &assignments_integer,
            );

            assert!(assignments_propositional.is_literal_unassigned(literal));

            assert!(assignments_propositional.is_literal_unassigned(
                variable_literal_mappings.get_equality_literal(
                    domain_id,
                    value,
                    &assignments_propositional,
                    &assignments_integer,
                )
            ));
        }

        assert_eq!(
            assignments_propositional.false_literal,
            variable_literal_mappings.get_lower_bound_literal(
                domain_id,
                ub + 1,
                &assignments_propositional,
                &assignments_integer,
            )
        );
        assert_eq!(
            assignments_propositional.true_literal,
            variable_literal_mappings.get_upper_bound_literal(
                domain_id,
                ub,
                &assignments_propositional,
                &assignments_integer,
            )
        );
        assert!(assignments_propositional.is_literal_unassigned(
            variable_literal_mappings.get_equality_literal(
                domain_id,
                ub,
                &assignments_propositional,
                &assignments_integer,
            )
        ));
        assert_eq!(
            assignments_propositional.false_literal,
            variable_literal_mappings.get_equality_literal(
                domain_id,
                ub + 1,
                &assignments_propositional,
                &assignments_integer,
            )
        );
    }

    #[test]
    fn check_correspondence_predicates_creating_new_int_domain() {
        let mut variable_literal_mappings = VariableLiteralMappings::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut watch_list_cp = WatchListCP::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut clausal_propagator = ClausalPropagatorType::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut clausal_allocator = ClauseAllocator::default();

        let lower_bound = 0;
        let upper_bound = 10;
        let domain_id = variable_literal_mappings.create_new_domain(
            lower_bound,
            upper_bound,
            &mut assignments_integer,
            &mut watch_list_cp,
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
            &mut clausal_allocator,
        );

        for bound in lower_bound + 1..upper_bound {
            let lower_bound_predicate = predicate![domain_id >= bound];
            let equality_predicate = predicate![domain_id == bound];
            for predicate in [lower_bound_predicate, equality_predicate] {
                let literal = variable_literal_mappings.get_literal(
                    predicate.try_into().unwrap(),
                    &assignments_propositional,
                    &assignments_integer,
                );
                assert!(variable_literal_mappings.literal_to_predicates[literal]
                    .contains(&predicate.try_into().unwrap()))
            }
        }
    }
}
