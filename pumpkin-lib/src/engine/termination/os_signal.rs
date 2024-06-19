use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use super::TerminationCondition;

/// A [`TerminationCondition`] which triggers due to a SIGINT signal.
#[derive(Clone, Debug)]
pub struct OsSignal {
    signal_received: Arc<AtomicBool>,
}

impl OsSignal {
    /// Create a termination and install the event listeners.
    pub fn install() -> OsSignal {
        let signal = OsSignal {
            signal_received: Arc::new(AtomicBool::new(false)),
        };

        let _ = signal_hook::flag::register(
            signal_hook::consts::SIGINT,
            Arc::clone(&signal.signal_received),
        )
        .expect("failed to register signal listener");

        signal
    }
}

impl TerminationCondition for OsSignal {
    fn should_stop(&mut self) -> bool {
        self.signal_received.load(Ordering::Relaxed)
    }
}
