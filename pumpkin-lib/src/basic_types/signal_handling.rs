//! Responsible for behaviour related to handling signals
//! (e.g. [SIGINT](https://man7.org/linux/man-pages/man7/signal.7.html#:~:text=synonym%20for%20SIGPWR-,SIGINT,-P1990%20%20%20%20%20%20Term%20%20%20%20Interrupt)).
//!
//! This is used to gracefully shut down the solver when, for example, a time-limit is reached (e.g.
//! the [MiniZinc](https://www.minizinc.org/) runner sends such a signal when the solver is running past its allowed time-limit).

pub mod signal_handler {
    use std::io::Error;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    use once_cell::sync::Lazy;
    use signal_hook::consts::TERM_SIGNALS;

    static SIGNAL_RECEIVED: Lazy<Arc<AtomicBool>> = Lazy::new(|| Arc::new(AtomicBool::new(false)));

    /// Registers the appropriate signals in [`TERM_SIGNALS`]; note that if *any* of the signals is
    /// received twice then the application will shut down.
    pub fn register_signals() -> Result<(), Error> {
        for signal in TERM_SIGNALS {
            // If we received a signal twice (e.g. when the user presses CTRL+C twice) then the
            // application will be terminated
            let _ = signal_hook::flag::register_conditional_shutdown(
                *signal,
                1,
                Arc::clone(&SIGNAL_RECEIVED),
            )?;

            // Registers that `SIGNAL_RECEIVED` will be set to true whenever `signal` is received
            let _ = signal_hook::flag::register(*signal, Arc::clone(&SIGNAL_RECEIVED))?;
        }

        Ok(())
    }

    /// Returns `true` if the application has received the signal to shut down and false otherwise.
    pub fn should_terminate() -> bool {
        SIGNAL_RECEIVED.load(Ordering::Relaxed)
    }
}
