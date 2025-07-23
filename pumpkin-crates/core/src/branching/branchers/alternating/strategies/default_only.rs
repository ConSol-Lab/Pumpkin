use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
#[cfg(doc)]
use crate::DefaultBrancher;

/// Specifies that the [`AlternatingBrancher`] should always use the [`DefaultBrancher`].
#[derive(Default, Debug, Clone, Copy)]
pub struct DefaultOnly;

impl AlternatingStrategy for DefaultOnly {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> bool {
        true
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }

    fn will_always_use_default(&self) -> bool {
        true
    }

    fn is_using_default_brancher(&self) -> bool {
        true
    }
}
