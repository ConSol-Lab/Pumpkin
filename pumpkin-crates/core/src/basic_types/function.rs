use super::solution::ProblemSolution;
use crate::basic_types::SolutionReference;
use crate::containers::HashMap;
use crate::predicates::Predicate;

/// A struct which represents a linear function over weighted [`Literal`]s, and a
/// constant term.
#[derive(Clone, Default, Debug)]
pub struct Function {
    literals: HashMap<Predicate, u64>,
    constant_term: u64,
}

impl Function {
    pub fn get_sum_of_literal_weights(&self) -> u64 {
        self.literals.values().sum()
    }

    pub fn add_weighted_literal(&mut self, predicate: Predicate, weight: u64) {
        *self.literals.entry(!predicate).or_insert(0) += weight;
    }

    pub fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub fn get_literal_terms(&self) -> impl Iterator<Item = (&Predicate, &u64)> {
        self.literals.iter()
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn evaluate_solution(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for (literal, weight) in self.get_literal_terms() {
            value += weight * (solution.get_predicate_value(*literal)) as u64;
        }
        value
    }

    pub fn evaluate_assignment(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for (literal, weight) in self.get_literal_terms() {
            value += weight * solution.get_predicate_value(*literal) as u32 as u64;
        }
        value
    }
}
