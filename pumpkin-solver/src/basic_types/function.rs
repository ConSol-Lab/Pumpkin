use super::solution::ProblemSolution;
use super::Solution;
use crate::basic_types::HashMap;
use crate::basic_types::SolutionReference;
use crate::engine::variables::DomainId;
use crate::variables::Literal;

/// A struct which represents a linear function over weighted [`DomainId`]s, and a
/// constant term.
#[derive(Clone, Default, Debug)]
pub struct Function {
    term: HashMap<DomainId, u64>,
    literals: HashMap<Literal, u64>,
    constant_term: u64,
}

impl Function {
    pub fn get_sum_of_literal_weights(&self) -> u64 {
        self.literals.values().sum()
    }

    pub fn add_term(&mut self, domain_id: DomainId, weight: u64) {
        *self.term.entry(domain_id).or_insert(0) += weight;
    }

    pub fn add_weighted_literal(&mut self, literal: Literal, weight: u64) {
        *self.literals.entry(!literal).or_insert(0) += weight;
    }

    pub fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub fn get_terms(&self) -> impl Iterator<Item = (&DomainId, &u64)> {
        self.term.iter()
    }

    pub fn get_literal_terms(&self) -> impl Iterator<Item = (&Literal, &u64)> {
        self.literals.iter()
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn is_empty(&self) -> bool {
        self.term.is_empty() && self.literals.is_empty() && self.constant_term == 0
    }

    pub fn evaluate_solution(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for (domain_id, weight) in self.get_terms() {
            value += weight * solution.get_integer_value(*domain_id) as u64;
        }
        for (literal, weight) in self.get_literal_terms() {
            value += weight * (solution.get_literal_value(*literal)) as u64;
        }
        value
    }

    pub fn evaluate_assignment(&self, solution: &Solution) -> u64 {
        let mut value: u64 = self.constant_term;
        for (domain_id, weight) in self.get_terms() {
            value += weight * solution.get_integer_value(*domain_id) as u64;
        }
        for (literal, weight) in self.get_literal_terms() {
            value += weight * solution.get_literal_value(*literal) as u32 as u64;
        }
        value
    }
}
