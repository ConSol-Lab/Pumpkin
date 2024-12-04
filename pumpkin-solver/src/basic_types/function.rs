use super::solution::ProblemSolution;
use super::Solution;
use crate::basic_types::HashMap;
use crate::basic_types::SolutionReference;
use crate::variables::Literal;

/// A struct which represents a linear function over weighted [`Literal`]s, and a
/// constant term.
#[derive(Clone, Default, Debug)]
pub struct Function {
    literals: HashMap<Literal, u64>,
    constant_term: u64,
}

impl Function {
    pub fn get_sum_of_literal_weights(&self) -> u64 {
        self.literals.values().sum()
    }

    pub fn add_weighted_literal(&mut self, literal: Literal, weight: u64) {
        *self.literals.entry(!literal).or_insert(0) += weight;
    }

    pub fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub fn get_literal_terms(&self) -> impl Iterator<Item = (&Literal, &u64)> {
        self.literals.iter()
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn evaluate_solution(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for (literal, weight) in self.get_literal_terms() {
            value += weight * (solution.get_literal_value(*literal)) as u64;
        }
        value
    }

    pub fn evaluate_assignment(&self, solution: &Solution) -> u64 {
        let mut value: u64 = self.constant_term;
        for (literal, weight) in self.get_literal_terms() {
            value += weight * solution.get_literal_value(*literal) as u32 as u64;
        }
        value
    }
}
