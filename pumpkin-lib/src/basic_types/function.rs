use super::solution::ProblemSolution;
use super::Solution;
use crate::basic_types::HashMap;
use crate::basic_types::SolutionReference;
use crate::engine::variables::DomainId;

/// A struct which represents a linear function over weighted [`DomainId`]s, and a
/// constant term.
#[derive(Clone, Default, Debug)]
pub struct Function {
    term: HashMap<DomainId, u64>,
    constant_term: u64,
}

impl Function {
    pub fn add_term(&mut self, domain_id: DomainId, weight: u64) {
        *self.term.entry(domain_id).or_insert(0) += weight;
    }

    pub fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub fn get_terms(&self) -> impl Iterator<Item = (&DomainId, &u64)> {
        self.term.iter()
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn is_empty(&self) -> bool {
        self.term.is_empty() && self.constant_term == 0
    }

    pub fn evaluate_solution(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for term in self.get_terms() {
            let domain_id = *term.0;
            let weight = *term.1;
            value += weight * solution.get_integer_value(domain_id) as u64;
        }
        value
    }

    pub fn evaluate_assignment(&self, solution: &Solution) -> u64 {
        let mut value: u64 = self.constant_term;
        for term in self.get_terms() {
            let domain_id = *term.0;
            let weight = *term.1;
            value += weight * solution.get_integer_value(domain_id) as u64;
        }
        value
    }
}
