use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;

#[cfg(doc)]
use crate::branching::alternating::AlternatingBrancher;
use crate::branching::alternating::AlternatingStrategy;
use crate::branching::alternating::BrancherToUse;

/// Specifies that the [`AlternatingBrancher`] should always use the other strategy.
#[derive(Default, Debug, Clone, Copy)]
pub struct OtherOnly;

impl AlternatingStrategy for OtherOnly {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> BrancherToUse {
        BrancherToUse::Other
    }

    fn is_using_default_brancher(&self) -> bool {
        false
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }

    fn is_restart_pointless(
        &mut self,
        _default_brancher: &mut impl Brancher,
        other_brancher: &mut impl Brancher,
    ) -> bool {
        other_brancher.is_restart_pointless()
    }

    fn will_always_use_default(&self) -> bool {
        false
    }
}
