use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use pumpkin_solver::core::termination::TerminationCondition;

/// A [`TerminationCondition`] which triggers due to a SIGINT signal.
#[derive(Clone, Debug)]
pub(crate) struct OsSignal {
    signal_received: Arc<AtomicBool>,
}

impl OsSignal {
    /// Create a termination and install the event listeners.
    pub(crate) fn install() -> OsSignal {
        // The signals to listen to for termination.
        const TERMINATION_SIGNALS: &[std::ffi::c_int] =
            &[signal_hook::consts::SIGINT, signal_hook::consts::SIGTERM];

        let signal_termination = OsSignal {
            signal_received: Arc::new(AtomicBool::new(false)),
        };

        for &signal in TERMINATION_SIGNALS {
            let _ = signal_hook::flag::register(
                signal,
                Arc::clone(&signal_termination.signal_received),
            )
            .expect("failed to register signal listener");
        }

        signal_termination
    }
}

impl TerminationCondition for OsSignal {
    fn should_stop(&mut self) -> bool {
        self.signal_received.load(Ordering::Relaxed)
    }
}
