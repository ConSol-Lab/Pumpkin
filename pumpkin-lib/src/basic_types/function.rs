use std::collections::HashMap;

use crate::{
    engine::{AssignmentsInteger, AssignmentsPropositional, ConstraintSatisfactionSolver},
    pumpkin_assert_moderate,
};

use super::{DomainId, Literal, Solution, WeightedLiteral};

#[derive(Clone, Default)]
pub struct Function {
    weighted_literals: HashMap<Literal, u64>,
    weighted_integers: HashMap<DomainId, u64>,
    constant_term: u64,
}

impl Function {
    pub fn add_weighted_literal(&mut self, literal: Literal, weight: u64) {
        //we want to avoid the situation where both polarities of a variable have a weight
        //  in case that happens, we keep a weight for one of the two polarity, and factor in the obligatory cost in the constant term

        let negative_literal = !literal;
        if let Some(opposite_weight) = self.weighted_literals.get_mut(&negative_literal) {
            pumpkin_assert_moderate!(*opposite_weight != 0);
            match weight.cmp(opposite_weight) {
                std::cmp::Ordering::Less => {
                    *opposite_weight -= weight;
                    self.constant_term += weight;
                }
                std::cmp::Ordering::Equal => {
                    self.weighted_literals.remove(&negative_literal);
                    self.constant_term += weight;
                }
                std::cmp::Ordering::Greater => {
                    let diff = weight - *opposite_weight;
                    self.constant_term += *opposite_weight;
                    self.weighted_literals.remove(&negative_literal);
                    self.weighted_literals.insert(literal, diff);
                }
            }
        } else {
            *self.weighted_literals.entry(literal).or_insert(0) += weight;
        }
    }

    pub fn add_weighted_integer(&mut self, domain_id: DomainId, weight: u64) {
        *self.weighted_integers.entry(domain_id).or_insert(0) += weight;
    }

    pub fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub fn get_weighted_literals(&self) -> std::collections::hash_map::Iter<Literal, u64> {
        self.weighted_literals.iter()
    }

    pub fn get_weighted_integers(&self) -> std::collections::hash_map::Iter<DomainId, u64> {
        self.weighted_integers.iter()
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn is_empty(&self) -> bool {
        self.weighted_integers.is_empty()
            && self.weighted_literals.is_empty()
            && self.constant_term == 0
    }

    pub fn evaluate_solution(&self, solution: &Solution) -> u64 {
        let mut value: u64 = self.constant_term;
        //add the contribution of the propositional part
        for term in self.get_weighted_literals() {
            let literal = *term.0;
            let weight = *term.1;
            value += weight * (solution.get_literal_value(literal) as u64);
        }
        //add the contribution of the integer part
        for term in self.get_weighted_integers() {
            let domain_id = *term.0;
            let weight = *term.1;
            value += weight * solution[domain_id] as u64;
        }
        value
    }

    pub fn evaluate_assignment(
        &self,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> u64 {
        let mut value: u64 = self.constant_term;
        //add the contribution of the propositional part
        for term in self.get_weighted_literals() {
            let literal = *term.0;
            let weight = *term.1;
            pumpkin_assert_moderate!(assignments_propositional.is_literal_assigned(literal));
            value += weight * (assignments_propositional.is_literal_assigned_true(literal) as u64);
        }
        //add the contribution of the integer part
        for term in self.get_weighted_integers() {
            let domain_id = *term.0;
            let weight = *term.1;
            pumpkin_assert_moderate!(assignments_integer.is_domain_assigned(domain_id));
            value += weight * assignments_integer.get_assigned_value(domain_id) as u64;
        }
        value
    }

    pub fn get_function_as_weighted_literals_vector(
        &self,
        csp_solver: &ConstraintSatisfactionSolver,
    ) -> Vec<WeightedLiteral> {
        let mut weighted_literals: Vec<WeightedLiteral> = self
            .get_weighted_literals()
            .map(|p| WeightedLiteral {
                literal: *p.0,
                weight: *p.1,
            })
            .collect();

        for term in self.get_weighted_integers() {
            let domain_id = *term.0;
            let weight = *term.1;

            let lower_bound = csp_solver
                .get_integer_assignments()
                .get_initial_lower_bound(domain_id);
            let upper_bound = csp_solver
                .get_integer_assignments()
                .get_upper_bound(domain_id);

            //note that we only needs lower bound literals starting from lower_bound+1
            //  the literals before those contribute to the objective function but not in a way that can be changed
            for i in (lower_bound + 1)..=upper_bound {
                let literal = csp_solver.get_lower_bound_literal(domain_id, i);
                weighted_literals.push(WeightedLiteral { literal, weight });
            }
        }

        //this was introduced to eliminate the randomness caused by the hashmap that is internally used in 'Function'
        //  hashmaps internally use randomisation when sorting keys, which influences the order in which elements are tranversed when going through all elements in the hashmap
        //  this can in turn have an impact on the solver since the order in which literals are stored influences the encoding
        //  todo this can be seen as a temporary solution, need to rethink if there is a better way and whether other hashmaps in the solver can cause similar problems
        weighted_literals.sort_by_key(|wl| wl.literal.to_u32());

        weighted_literals
    }
}

#[derive(Clone)]
pub struct WeightedInteger {
    pub domain_id: DomainId,
    pub weight: u64,
}
