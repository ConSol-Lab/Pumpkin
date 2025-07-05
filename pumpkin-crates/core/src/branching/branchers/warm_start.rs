use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::pumpkin_assert_eq_simple;
use crate::variables::IntegerVariable;

pub struct WarmStart<Var> {
    variables: Vec<Var>,
    values: Vec<i32>,
}

impl<Var: Clone> WarmStart<Var> {
    pub fn new(variables: &[Var], values: &[i32]) -> Self {
        pumpkin_assert_eq_simple!(variables.len(), values.len());
        Self {
            variables: variables.iter().rev().cloned().collect(),
            values: values.iter().rev().cloned().collect(),
        }
    }
}

impl<Var: IntegerVariable> Brancher for WarmStart<Var> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        while !self.variables.is_empty() {
            let variable = self.variables.pop().unwrap();
            let value = self.values.pop().unwrap();

            let predicate = predicate!(variable == value);

            if context.assignments.evaluate_predicate(predicate).is_none() {
                return Some(predicate);
            }
        }

        return None;
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}
