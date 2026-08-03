use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::predicates::Predicate;

#[derive(Debug, Clone, Copy)]
pub struct NoDecisionBrancher;

impl Brancher for NoDecisionBrancher {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> Option<Predicate> {
        None
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}
