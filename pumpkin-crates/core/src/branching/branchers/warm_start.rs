use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::pumpkin_assert_eq_simple;
use crate::variables::IntegerVariable;

/// A [`Brancher`] which represents warm starting.
///
/// It takes a list of variables and values (of equal size) and attempts to assign each variable to
/// the value *once* if possible. If it is not possible, then this [`Brancher`] will not attempt to
/// assign that variable to that value again (unless it is specified multiple times in the warm
/// start).
#[derive(Debug)]
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
        pumpkin_assert_eq_simple!(self.variables.len(), self.values.len());
        while let (Some(variable), Some(value)) = (self.variables.pop(), self.values.pop()) {
            let predicate = predicate!(variable == value);

            if context.assignments.evaluate_predicate(predicate).is_none() {
                return Some(predicate);
            }
        }

        None
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}
