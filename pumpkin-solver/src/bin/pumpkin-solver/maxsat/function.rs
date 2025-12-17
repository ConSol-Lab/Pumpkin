use pumpkin_core::containers::HashMap;
use pumpkin_core::results::ProblemSolution;
use pumpkin_core::results::SolutionReference;
use pumpkin_core::variables::Literal;

/// A struct which represents a linear function over weighted [`Literal`]s, and a
/// constant term.
#[derive(Clone, Default, Debug)]
pub(crate) struct Function {
    literals: HashMap<Literal, u64>,
    constant_term: u64,
}

impl Function {
    pub(crate) fn add_weighted_literal(&mut self, literal: Literal, weight: u64) {
        *self.literals.entry(!literal).or_insert(0) += weight;
    }

    pub(crate) fn add_constant_term(&mut self, value: u64) {
        self.constant_term += value;
    }

    pub(crate) fn get_literal_terms(&self) -> impl Iterator<Item = (&Literal, &u64)> {
        self.literals.iter()
    }

    pub(crate) fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub(crate) fn evaluate_assignment(&self, solution: SolutionReference) -> u64 {
        let mut value: u64 = self.constant_term;
        for (literal, weight) in self.get_literal_terms() {
            value += weight * solution.get_literal_value(*literal) as u32 as u64;
        }
        value
    }
}
