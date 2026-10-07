//! A logger that keeps the messages of the solver, so that the report of a failure can quote what a
//! checker logged before it panicked.

use std::sync::Mutex;

use log::LevelFilter;
use log::Log;
use log::Metadata;
use log::Record;

static MESSAGES: Mutex<Vec<String>> = Mutex::new(Vec::new());
static LOGGER: Capture = Capture;

#[derive(Debug)]
struct Capture;

impl Log for Capture {
    fn enabled(&self, _: &Metadata<'_>) -> bool {
        true
    }

    fn log(&self, record: &Record<'_>) {
        if let Ok(mut messages) = MESSAGES.lock() {
            messages.push(format!("[{}] {}", record.level(), record.args()));
        }
    }

    fn flush(&self) {}
}

/// Installs the logger, keeping the messages up to the warning level.
pub fn init() {
    if log::set_logger(&LOGGER).is_ok() {
        log::set_max_level(LevelFilter::Warn);
    }
}

/// The messages logged since the last call.
pub(crate) fn take() -> Vec<String> {
    MESSAGES
        .lock()
        .map(|mut messages| std::mem::take(&mut *messages))
        .unwrap_or_default()
}
