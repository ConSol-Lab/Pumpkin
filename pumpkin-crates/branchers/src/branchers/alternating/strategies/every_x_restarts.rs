use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;

#[cfg(doc)]
use crate::branchers::alternating::AlternatingBrancher;
use crate::branchers::alternating::BrancherToUse;
use crate::branchers::alternating::strategies::AlternatingStrategy;

/// Specifies that the [`AlternatingBrancher`] should switch between
/// [`BrancherToUse::Default`] and the provided brancher every `x`th considered restart.
#[derive(Debug, Clone, Copy)]
pub struct EveryXRestarts {
    use_default_brancher: bool,
    num_restarts_considered: u32,
    x: u32,
    restart_considered: bool,
}

impl EveryXRestarts {
    pub fn new(x: u32) -> Self {
        Self {
            use_default_brancher: false,
            num_restarts_considered: 0,
            x,
            restart_considered: false,
        }
    }
}

impl AlternatingStrategy for EveryXRestarts {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> BrancherToUse {
        if self.restart_considered {
            self.num_restarts_considered += 1;
        }

        if self.num_restarts_considered.is_multiple_of(self.x) {
            self.use_default_brancher = !self.use_default_brancher
        }

        if self.use_default_brancher {
            BrancherToUse::Default
        } else {
            BrancherToUse::Other
        }
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Restart]
    }

    fn on_restart(&mut self) {
        self.restart_considered = true;
    }

    fn will_always_use_default(&self) -> bool {
        false
    }

    fn is_restart_pointless(
        &mut self,
        default_brancher: &mut impl Brancher,
        other_brancher: &mut impl Brancher,
    ) -> bool {
        // We indicate that we have considered a restart, this can then be used by the
        // AlternatingStrategy to determine when to switch
        self.restart_considered = true;

        // Note that we could switch to the other strategy and then the restart is performed
        // so we check whether restarting for the other brancher is
        // pointless
        if self.use_default_brancher {
            other_brancher.is_restart_pointless()
        } else {
            default_brancher.is_restart_pointless()
        }
    }

    fn is_using_default_brancher(&self) -> bool {
        self.use_default_brancher
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::Solver;
    use pumpkin_core::branching::Brancher;
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::testing::TestRandom;

    use crate::DefaultBrancher;
    use crate::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branchers::alternating::every_x_restarts::EveryXRestarts;

    #[test]
    fn test_every_restart() {
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::create_for_testing(vec![], &mut test_rng);
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            DefaultBrancher::default_over_all_variables(solver.get_domains()),
            EveryXRestarts::new(1),
        );

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);

        assert!(brancher.is_using_default_brancher());
    }

    #[test]
    fn test_every_other_restart() {
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::create_for_testing(vec![], &mut test_rng);
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            DefaultBrancher::default_over_all_variables(solver.get_domains()),
            EveryXRestarts::new(2),
        );

        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);

        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);

        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);

        assert!(!brancher.is_using_default_brancher());
    }
}
