use std::cmp;

use crate::basic_types::HashSet;
use crate::basic_types::KeyedVec;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::VariableLiteralMappings;
use crate::predicate;
use crate::predicates::IntegerPredicate;
use crate::propagators::SparseSet;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;
use crate::variables::Literal;

/// Performs semantic minimisation; this minmiser attempts to remove redundant literals from
/// nogoods.
///
/// For examle, if we have the nogood `[x >= 3] /\ [x >= 5] /\ ...` then we can remove `[x >= 3]`
/// from the nogood as it is implied by `[x >= 5]`.
///
/// This minimiser achieves this by applying the predicates of the provided nogood to the domain and
/// then creating a nogood which consists only of the changed bounds of the [`DomainId`]s involved
/// in the nogood.
#[derive(Clone, Debug)]
pub(crate) struct SemanticMinimiser {
    /// The original domains of the [`DomainId`]s.
    ///
    /// These are stored to ensure that no [`IntegerPredicate`]s which are trivially true are added
    /// to the nogood.
    original_domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    /// The domains which are created while minimising.
    domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    /// The [`DomainId`]s which are present in the current nogood.
    present_ids: SparseSet<DomainId>,
    /// Stores the final nogood which is created after minimisation.
    final_nogood: Vec<Literal>,
}

impl Default for SemanticMinimiser {
    fn default() -> Self {
        let mapping = |x: &DomainId| x.id as usize;
        Self {
            original_domains: Default::default(),
            domains: Default::default(),
            present_ids: SparseSet::new(vec![], mapping),
            final_nogood: Vec::default(),
        }
    }
}

impl SemanticMinimiser {
    pub(crate) fn minimise(
        &mut self,
        learned_clause: impl Iterator<Item = Literal>,
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
        variable_literal_mapping: &VariableLiteralMappings,
    ) -> Vec<Literal> {
        // We get a clause and we turn it into a nogood by negating
        let nogood = learned_clause.map(|literal| !literal).collect();
        self.accommodate(assignments_integer);
        self.clean_up();
        self.apply_predicates(&nogood, variable_literal_mapping, assignments_propositional);

        // Compile the nogood based on the internal state.
        // Add domain description to the helper.
        for domain_id in self.present_ids.iter() {
            // If at least one domain is inconsistent, we can stop.
            if self.domains[domain_id].inconsistent {
                return vec![assignments_propositional.true_literal];
            }
            self.domains[domain_id].add_domain_description_to_vector(
                *domain_id,
                &self.original_domains[domain_id],
                &mut self.final_nogood,
                variable_literal_mapping,
                assignments_propositional,
                assignments_integer,
            );
        }

        // We store a nogood in the helper and we turn it into a clause by negating
        self.final_nogood
            .iter()
            .map(|literal| !(*literal))
            .collect::<Vec<_>>()
    }

    fn apply_predicates(
        &mut self,
        nogood: &Vec<Literal>,
        variable_literal_mapping: &VariableLiteralMappings,
        assignments_propositional: &AssignmentsPropositional,
    ) {
        // Apply the predicates to the domains in a straight-forward way.
        // Later we will take into account the effect of holes on the domain.
        for literal in nogood {
            let predicate = variable_literal_mapping.get_predicates(*literal).next();
            if let Some(predicate) = predicate {
                self.present_ids.insert(predicate.get_domain());

                match predicate {
                    IntegerPredicate::LowerBound {
                        domain_id,
                        lower_bound,
                    } => {
                        self.domains[domain_id].tighten_lower_bound(lower_bound);
                    }
                    IntegerPredicate::UpperBound {
                        domain_id,
                        upper_bound,
                    } => {
                        self.domains[domain_id].tighten_upper_bound(upper_bound);
                    }
                    IntegerPredicate::NotEqual {
                        domain_id,
                        not_equal_constant,
                    } => {
                        self.domains[domain_id].add_hole(not_equal_constant);
                    }
                    IntegerPredicate::Equal {
                        domain_id,
                        equality_constant,
                    } => {
                        self.domains[domain_id].assign(equality_constant);
                    }
                }
            } else if *literal != assignments_propositional.true_literal
                && *literal != assignments_propositional.false_literal
            {
                self.final_nogood.push(*literal);
            }
        }

        for domain_id in self.present_ids.iter() {
            self.domains[*domain_id].propagate_holes_on_lower_bound();
            self.domains[*domain_id].propagate_holes_on_upper_bound();
            self.domains[*domain_id].remove_redundant_holes();
            self.domains[*domain_id].update_consistency();
        }
    }

    fn accommodate(&mut self, assignments: &AssignmentsInteger) {
        pumpkin_assert_simple!(self.domains.len() == self.original_domains.len());

        while (self.domains.len() as u32) < assignments.num_domains() {
            let domain_id = DomainId {
                id: self.domains.len() as u32,
            };
            let lower_bound = assignments.get_initial_lower_bound(domain_id);
            let upper_bound = assignments.get_initial_upper_bound(domain_id);
            let holes = assignments.get_initial_holes(domain_id).collect();
            self.grow(lower_bound, upper_bound, holes);
        }
    }

    fn grow(&mut self, lower_bound: i32, upper_bound: i32, holes: Vec<i32>) {
        let initial_domain = SimpleIntegerDomain {
            lower_bound,
            upper_bound,
            holes: HashSet::from_iter(holes.iter().cloned()),
            inconsistent: false,
        };
        self.original_domains.push(initial_domain.clone());
        self.domains.push(initial_domain);
    }

    fn clean_up(&mut self) {
        // Remove the domain ids from the present domain ids.
        let vals: Vec<DomainId> = self.present_ids.iter().copied().collect();
        self.present_ids.clear();
        for domain_id in vals {
            self.domains[domain_id] = self.original_domains[domain_id].clone();
        }
        self.final_nogood.clear();
    }
}

#[derive(Clone, Default, Debug)]
struct SimpleIntegerDomain {
    lower_bound: i32,
    upper_bound: i32,
    holes: HashSet<i32>,
    inconsistent: bool,
}

impl SimpleIntegerDomain {
    fn tighten_lower_bound(&mut self, lower_bound: i32) {
        self.lower_bound = cmp::max(self.lower_bound, lower_bound);
    }

    fn tighten_upper_bound(&mut self, upper_bound: i32) {
        self.upper_bound = cmp::min(self.upper_bound, upper_bound);
    }

    fn add_hole(&mut self, hole: i32) {
        // Add the hole if it is within the domain.
        // Note that we do not adjust bounds due to holes being at the border. This is taken care of
        // by other functions (propagate bounds based on holes).
        if self.lower_bound <= hole && hole <= self.upper_bound {
            let _ = self.holes.insert(hole);
        }
    }

    fn assign(&mut self, value: i32) {
        // If the domains are inconsistent, or if the assigned value would make the domain
        // inconsistent, declare inconsistency and stop.
        if self.lower_bound > self.upper_bound
            || self.lower_bound > value
            || self.upper_bound < value
        {
            self.inconsistent = true;
        }
        // Otherwise, it is safe to apply the predicate.
        // Note that we do not take into account holes here.
        else {
            self.lower_bound = value;
            self.upper_bound = value;
        }
    }

    fn propagate_holes_on_lower_bound(&mut self) {
        while self.holes.contains(&self.lower_bound) && self.lower_bound <= self.upper_bound {
            self.lower_bound += 1;
        }
    }

    fn propagate_holes_on_upper_bound(&mut self) {
        while self.holes.contains(&self.upper_bound) && self.lower_bound <= self.upper_bound {
            self.upper_bound -= 1;
        }
    }

    fn update_consistency(&mut self) {
        // The domain may have already gotten in an inconsistent state due to equality predicates.
        // Make sure not to make any changes if already inconsistent.
        if !self.inconsistent {
            self.inconsistent = self.lower_bound > self.upper_bound;
        }
    }

    fn remove_redundant_holes(&mut self) {
        // Do nothing if inconsistent.
        if self.inconsistent {
            return;
        }
        // Only keep holes that fall within the current bounds.
        self.holes
            .retain(|hole| self.lower_bound < *hole && *hole < self.upper_bound);
    }

    fn add_domain_description_to_vector(
        &self,
        domain_id: DomainId,
        original_domain: &SimpleIntegerDomain,
        description: &mut Vec<Literal>,
        variable_literal_mappings: &VariableLiteralMappings,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) {
        // If the domain assigned at a nonroot level, this is just one predicate.
        if self.lower_bound == self.upper_bound
            && self.lower_bound != original_domain.lower_bound
            && self.upper_bound != original_domain.upper_bound
        {
            description.push(
                variable_literal_mappings.get_literal(
                    predicate![domain_id == self.lower_bound]
                        .try_into()
                        .unwrap(),
                    assignments_propositional,
                    assignments_integer,
                ),
            );
            return;
        }

        // Add bounds but avoid root assignments.
        if self.lower_bound != original_domain.lower_bound {
            description.push(
                variable_literal_mappings.get_literal(
                    predicate![domain_id >= self.lower_bound]
                        .try_into()
                        .unwrap(),
                    assignments_propositional,
                    assignments_integer,
                ),
            );
        }

        if self.upper_bound != original_domain.upper_bound {
            description.push(
                variable_literal_mappings.get_literal(
                    predicate![domain_id <= self.upper_bound]
                        .try_into()
                        .unwrap(),
                    assignments_propositional,
                    assignments_integer,
                ),
            );
        }

        // Add nonroot holes.
        for hole in self.holes.iter() {
            // Only record holes that are within the lower and upper bound,
            // that are not root assignments.
            // Since bound values cannot be in the holes,
            // we can use '<' or '>'.
            if self.lower_bound < *hole
                && *hole < self.upper_bound
                && !original_domain.holes.contains(hole)
            {
                description.push(variable_literal_mappings.get_literal(
                    predicate![domain_id != *hole].try_into().unwrap(),
                    assignments_propositional,
                    assignments_integer,
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::conflict_analysis::SemanticMinimiser;
    use crate::engine::AssignmentsInteger;
    use crate::engine::AssignmentsPropositional;
    use crate::engine::VariableLiteralMappings;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::variables::Literal;

    fn create_for_testing(
        num_integer_variables: usize,
        num_propositional_variables: usize,
        domains: Option<Vec<(i32, i32)>>,
    ) -> (
        AssignmentsInteger,
        AssignmentsPropositional,
        VariableLiteralMappings,
    ) {
        use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
        use crate::engine::variables::Literal;
        use crate::engine::VariableLiteralMappings;
        use crate::engine::WatchListCP;
        use crate::engine::WatchListPropositional;
        use crate::propagators::clausal::BasicClausalPropagator;
        use crate::pumpkin_assert_simple;

        pumpkin_assert_simple!({
            if let Some(domains) = domains.as_ref() {
                num_integer_variables == domains.len()
            } else {
                true
            }
        });

        let mut mediator = VariableLiteralMappings::default();
        let mut clausal_propagator = BasicClausalPropagator::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        let mut assignments_integer = AssignmentsInteger::default();
        let mut clause_allocator = ClauseAllocator::default();
        let mut watch_list_propositional = WatchListPropositional::default();
        let mut watch_list_cp = WatchListCP::default();

        let root_variable = mediator.create_new_propositional_variable(
            &mut watch_list_propositional,
            &mut clausal_propagator,
            &mut assignments_propositional,
        );
        let true_literal = Literal::new(root_variable, true);

        assignments_propositional.true_literal = true_literal;

        assignments_propositional.false_literal = !true_literal;

        assignments_propositional.enqueue_decision_literal(true_literal);

        if let Some(domains) = domains.as_ref() {
            for (_, (lower_bound, upper_bound)) in (0..num_integer_variables).zip(domains) {
                let _ = mediator.create_new_domain(
                    *lower_bound,
                    *upper_bound,
                    &mut assignments_integer,
                    &mut watch_list_cp,
                    &mut watch_list_propositional,
                    &mut clausal_propagator,
                    &mut assignments_propositional,
                    &mut clause_allocator,
                );
            }
        } else {
            for _ in 0..num_integer_variables {
                let _ = mediator.create_new_domain(
                    0,
                    10,
                    &mut assignments_integer,
                    &mut watch_list_cp,
                    &mut watch_list_propositional,
                    &mut clausal_propagator,
                    &mut assignments_propositional,
                    &mut clause_allocator,
                );
            }
        }

        for _ in 0..num_propositional_variables {
            // We create an additional variable to ensure that the generator returns the correct
            // variables
            let _ = mediator.create_new_propositional_variable(
                &mut watch_list_propositional,
                &mut clausal_propagator,
                &mut assignments_propositional,
            );
        }

        (assignments_integer, assignments_propositional, mediator)
    }

    fn assert_elements_equal(first: Vec<Literal>, second: Vec<Literal>) {
        assert_eq!(first.len(), second.len());
        assert!(first.iter().all(|literal| second.contains(literal)));
        assert!(second.iter().all(|literal| first.contains(literal)));
    }

    fn nogood_to_clause(
        nogood: Vec<Predicate>,
        variable_literal_mappings: &VariableLiteralMappings,
        assignments_integer: &AssignmentsInteger,
        assignments_propositional: &AssignmentsPropositional,
    ) -> Vec<Literal> {
        nogood
            .iter()
            .map(|predicate| {
                variable_literal_mappings.get_literal(
                    (!*predicate).try_into().unwrap(),
                    assignments_propositional,
                    assignments_integer,
                )
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn trivial_nogood() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood = vec![predicate!(domain_id >= 0), predicate!(domain_id <= 10)];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let p = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert!(p.is_empty());
    }

    #[test]
    fn trivial_conflict_bounds() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood: Vec<Predicate> = vec![predicate!(domain_id >= 5), predicate!(domain_id <= 4)];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let p = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], assignments_propositional.true_literal);
    }

    #[test]
    fn trivial_conflict_holes() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood: Vec<Predicate> = vec![
            predicate!(domain_id != 5),
            predicate!(domain_id >= 5),
            predicate!(domain_id <= 5),
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let p = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], assignments_propositional.true_literal);
    }

    #[test]
    fn trivial_conflict_assignment() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood: Vec<Predicate> = vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let p = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], assignments_propositional.true_literal);
    }

    #[test]
    fn trivial_conflict_bounds_reset() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood: Vec<Predicate> = vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let _ = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        let p = p.minimise(
            vec![].into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert!(p.is_empty());
    }

    #[test]
    fn simple_bound1() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(2, 0, Some(vec![(0, 10), (0, 5)]));
        let domain_0 = assignments_integer.get_domains().next().unwrap();
        let domain_1 = assignments_integer.get_domains().nth(1).unwrap();
        let nogood: Vec<Predicate> = vec![
            predicate![domain_0 >= 5],
            predicate![domain_0 <= 9],
            predicate![domain_1 >= 0],
            predicate![domain_1 <= 4],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 3);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![
                    predicate![domain_0 >= 5],
                    predicate![domain_0 <= 9],
                    predicate![domain_1 <= 4],
                ],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        );
    }

    #[test]
    fn simple_bound2() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(2, 0, Some(vec![(0, 10), (0, 5)]));
        let domain_0 = assignments_integer.get_domains().next().unwrap();
        let domain_1 = assignments_integer.get_domains().nth(1).unwrap();

        let nogood = vec![
            predicate![domain_0 >= 5],
            predicate![domain_0 <= 9],
            predicate![domain_1 >= 0],
            predicate![domain_1 <= 4],
            predicate![domain_0 != 7],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 4);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![
                    predicate![domain_0 >= 5],
                    predicate![domain_0 <= 9],
                    predicate![domain_1 <= 4],
                    predicate![domain_0 != 7],
                ],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        );
    }

    #[test]
    fn simple_bound3() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(2, 0, Some(vec![(0, 10), (0, 5)]));
        let domain_0 = assignments_integer.get_domains().next().unwrap();
        let domain_1 = assignments_integer.get_domains().nth(1).unwrap();

        let nogood = vec![
            predicate![domain_0 >= 5],
            predicate![domain_0 <= 9],
            predicate![domain_1 >= 0],
            predicate![domain_1 <= 4],
            predicate![domain_0 != 7],
            predicate![domain_0 != 7],
            predicate![domain_0 != 8],
            predicate![domain_0 != 6],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 6);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![
                    predicate![domain_0 >= 5],
                    predicate![domain_0 <= 9],
                    predicate![domain_1 <= 4],
                    predicate![domain_0 != 7],
                    predicate![domain_0 != 6],
                    predicate![domain_0 != 8],
                ],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        )
    }

    #[test]
    fn simple_assign() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(2, 0, Some(vec![(0, 10), (0, 5)]));
        let domain_0 = assignments_integer.get_domains().next().unwrap();
        let domain_1 = assignments_integer.get_domains().nth(1).unwrap();

        let nogood = vec![
            predicate![domain_0 >= 5],
            predicate![domain_0 <= 9],
            predicate![domain_1 >= 0],
            predicate![domain_1 <= 4],
            predicate![domain_0 != 7],
            predicate![domain_0 != 7],
            predicate![domain_0 != 6],
            predicate![domain_0 == 5],
            predicate![domain_0 != 7],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 2);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![predicate![domain_0 == 5], predicate![domain_1 <= 4]],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        )
    }

    #[test]
    fn simple_lb_override1() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood = vec![
            predicate![domain_id >= 2],
            predicate![domain_id >= 1],
            predicate![domain_id >= 5],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 1);
        assert_eq!(
            literals[0],
            variable_literal_mappings.get_literal(
                (!predicate!(domain_id >= 5)).try_into().unwrap(),
                &assignments_propositional,
                &assignments_integer
            )
        );
    }

    #[test]
    fn hole_lb_override() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood = vec![
            predicate![domain_id != 2],
            predicate![domain_id != 3],
            predicate![domain_id >= 5],
            predicate![domain_id >= 1],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 1);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![predicate!(domain_id >= 5)],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        )
    }

    #[test]
    fn hole_push_lb() {
        let mut p = SemanticMinimiser::default();
        let (assignments_integer, assignments_propositional, variable_literal_mappings) =
            create_for_testing(1, 0, None);
        let domain_id = assignments_integer.get_domains().next().unwrap();
        let nogood = vec![
            predicate![domain_id != 2],
            predicate![domain_id != 3],
            predicate![domain_id >= 1],
            predicate![domain_id != 1],
        ];
        let learned_clause = nogood_to_clause(
            nogood,
            &variable_literal_mappings,
            &assignments_integer,
            &assignments_propositional,
        );

        let literals = p.minimise(
            learned_clause.into_iter(),
            &assignments_integer,
            &assignments_propositional,
            &variable_literal_mappings,
        );

        assert_eq!(literals.len(), 1);
        assert_elements_equal(
            literals,
            nogood_to_clause(
                vec![predicate![domain_id >= 4]],
                &variable_literal_mappings,
                &assignments_integer,
                &assignments_propositional,
            ),
        )
    }
}
