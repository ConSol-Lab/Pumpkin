use std::time::Duration;
use std::time::Instant;

/// A time keeping utility which keeps track of the elapsed time since creation.
#[derive(Debug, Copy, Clone)]
pub(crate) struct Stopwatch {
    time_start: Instant,
}

impl Stopwatch {
    /// Create a new [`Stopwatch`] which starts keeping track of time immediately.
    pub(crate) fn starting_now() -> Stopwatch {
        Stopwatch {
            time_start: Instant::now(),
        }
    }

    /// Get the duration since the [`Stopwatch`] was created.
    pub(crate) fn elapsed(&self) -> Duration {
        self.time_start.elapsed()
    }
}
