use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;

/// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
/// the provided brancher every restart.
#[derive(Default, Debug, Clone, Copy)]
pub struct EveryRestart {
    use_default_brancher: bool,
    considered_restart: bool,
}

impl AlternatingStrategy for EveryRestart {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> bool {
        if self.considered_restart {
            self.use_default_brancher = !self.use_default_brancher
        }

        self.use_default_brancher
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Restart]
    }

    fn on_restart(&mut self) {
        self.considered_restart = true;
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
        self.considered_restart = true;

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
    use crate::basic_types::tests::TestRandom;
    use crate::branching::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branching::branchers::alternating::strategies::every_restart::EveryRestart;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::Assignments;
    use crate::Solver;

    #[test]
    fn test_every_other_restart() {
        let assignments = Assignments::default();
        let solver = Solver::default();
        let mut brancher =
            AlternatingBrancher::new(&solver, solver.default_brancher(), EveryRestart::default());

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));

        assert!(brancher.is_using_default_brancher());
    }
}
