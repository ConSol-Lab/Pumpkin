use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;

#[cfg(doc)]
use crate::branching::alternating::AlternatingBrancher;
use crate::branching::alternating::BrancherToUse;
use crate::branching::alternating::strategies::AlternatingStrategy;

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
            // Only switch at the moment a new restart is registered; otherwise, every decision
            // after the `x`th restart would cause a switch.
            self.restart_considered = false;
            self.num_restarts_considered += 1;

            if self.num_restarts_considered.is_multiple_of(self.x) {
                self.use_default_brancher = !self.use_default_brancher
            }
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

        if (self.num_restarts_considered + 1).is_multiple_of(self.x) {
            // This restart will cause a switch to the other brancher; this is never pointless
            // since a different brancher will be used after the restart.
            //
            // Note that if we were to report that the restart is pointless, then the switch
            // would happen without backtracking, which is not the intended behaviour.
            false
        } else {
            // No switch will take place, so the restart is pointless if it is pointless for the
            // brancher which is currently in use
            if self.use_default_brancher {
                default_brancher.is_restart_pointless()
            } else {
                other_brancher.is_restart_pointless()
            }
        }
    }

    fn is_using_default_brancher(&self) -> bool {
        self.use_default_brancher
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::branching::Brancher;
    use pumpkin_core::branching::BrancherEvent;
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::state::State;

    use crate::DefaultBrancher;
    use crate::branching::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branching::alternating::every_x_restarts::EveryXRestarts;
    use crate::testing::TestRandom;

    #[test]
    fn test_every_restart() {
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
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
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
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

    #[test]
    fn test_multiple_decisions_between_restarts_do_not_switch() {
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
            EveryXRestarts::new(1),
        );

        for _ in 0..5 {
            let _ = brancher.next_decision(&mut context);
            assert!(!brancher.is_using_default_brancher());
        }

        brancher.on_restart();
        for _ in 0..5 {
            let _ = brancher.next_decision(&mut context);
            assert!(brancher.is_using_default_brancher());
        }

        brancher.on_restart();
        for _ in 0..5 {
            let _ = brancher.next_decision(&mut context);
            assert!(!brancher.is_using_default_brancher());
        }
    }

    #[test]
    fn test_restart_is_not_pointless_when_switching_to_static_brancher() {
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            StaticBrancher,
            EveryXRestarts::new(2),
        );

        // First considered restart: no switch, so the restart is pointless for the static
        // brancher which is in use.
        assert!(brancher.is_restart_pointless());
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        // Second considered restart: switch to the default brancher.
        assert!(!brancher.is_restart_pointless());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        // Third considered restart: no switch, the default brancher is not static.
        assert!(!brancher.is_restart_pointless());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        // Fourth considered restart: switch back to the static brancher, which should not be
        // considered pointless.
        assert!(!brancher.is_restart_pointless());
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());
    }

    #[derive(Debug)]
    struct StaticBrancher;

    impl Brancher for StaticBrancher {
        fn next_decision(&mut self, _context: &mut SelectionContext) -> Option<Predicate> {
            None
        }

        fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
            vec![]
        }
    }
}
