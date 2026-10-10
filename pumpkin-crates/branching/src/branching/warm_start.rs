use pumpkin_core::asserts::pumpkin_assert_eq_simple;
use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::results::SolutionReference;
use pumpkin_core::variables::IntegerVariable;

/// A [`Brancher`] which represents warm starting.
///
/// It takes a list of variables and values (of equal size) and attempts to assign each variable
/// `variables[i]` to the value `values[i]`. If this is not possible, then it will attempt to assign
/// the next variable and so on, until it either runs out of variables or a solution is found.
#[derive(Debug)]
pub struct WarmStart<Var> {
    variables: Vec<Var>,
    values: Vec<i32>,

    has_found_solution: bool,
    index: usize,
}

impl<Var: Clone> WarmStart<Var> {
    pub fn new(variables: &[Var], values: &[i32]) -> Self {
        pumpkin_assert_eq_simple!(variables.len(), values.len());
        Self {
            variables: variables.iter().rev().cloned().collect(),
            values: values.iter().rev().cloned().collect(),

            has_found_solution: false,
            index: 0,
        }
    }
}

impl<Var: IntegerVariable> Brancher for WarmStart<Var> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        if self.has_found_solution {
            return None;
        }

        while self.index < self.variables.len() {
            let variable = &self.variables[self.index];
            let value = self.values[self.index];

            let predicate = predicate!(variable == value);

            self.index += 1;

            if !context.is_predicate_assigned(predicate) {
                return Some(predicate);
            }
        }

        None
    }

    fn synchronise(&mut self, _context: &mut SelectionContext) {
        self.index = 0
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        self.has_found_solution = true;
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Solution, BrancherEvent::Synchronise]
    }
}
