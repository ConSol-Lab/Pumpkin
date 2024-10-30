//! Responsible for behaviour related to logging statistics with a specific pre-fix and closing
//! lines.

use std::fmt::Debug;
use std::fmt::Display;
use std::io::stdout;
use std::io::Write;
use std::sync::OnceLock;
use std::sync::RwLock;

use convert_case::Case;
use convert_case::Casing;
use log::debug;

/// The options for statistic logging containing the statistic prefix, the (optional) line which is
/// printed after the statistics, and the (optional) casing of the statistics.
pub struct StatisticOptions<'a> {
    // What is printed before a statistic is printed, the statistics will be printed in the
    // form `{PREFIX} {VALUE}={NAME}`
    statistic_prefix: &'a str,
    // A closing line which is printed after all of the statistics have been printed
    after_statistics: Option<&'a str>,
    // The casing of the name of the statistic
    statistics_casing: Option<Case>,
    // The writer TODO
    statistics_writer: Box<dyn Write + Send + Sync>,
}

static STATISTIC_OPTIONS: OnceLock<RwLock<StatisticOptions>> = OnceLock::new();

/// Configures the logging of the statistics.
///
/// It specifies the (optional) prefix and a closing line (postfix) which
/// can be printed after all of the statistics have been logged. Statistics will only be printed
/// if `log_statistics` is true.
pub fn configure_statistic_logging(
    prefix: &'static str,
    after: Option<&'static str>,
    casing: Option<Case>,
    writer: Option<Box<dyn Write + Send + Sync>>,
) {
    let _ = STATISTIC_OPTIONS.get_or_init(|| {
        RwLock::from(StatisticOptions {
            statistic_prefix: prefix,
            after_statistics: after,
            statistics_casing: casing,
            statistics_writer: writer.unwrap_or(Box::new(stdout())),
        })
    });
}

/// Logs the provided statistic with name `name` and value `value`. At the moment it will log in
/// the format `STATISTIC_PREFIX NAME=VALUE`.
pub fn log_statistic(name: impl Display, value: impl Display) {
    if let Some(statistic_options_lock) = STATISTIC_OPTIONS.get() {
        if let Ok(mut statistic_options) = statistic_options_lock.write() {
            let name = if let Some(casing) = &statistic_options.statistics_casing {
                name.to_string().to_case(*casing)
            } else {
                name.to_string()
            };
            let prefix = statistic_options.statistic_prefix;
            if let Err(e) = write!(
                statistic_options.statistics_writer,
                "{} {name}={value}\n",
                prefix
            ) {
                debug!("Could not write statistic: {e}")
            };
        }
    }
}

/// Logs the postfix of the statistics (if it has been set).
///
/// Certain formats (e.g. the [MiniZinc](https://www.minizinc.org/doc-2.7.6/en/fzn-spec.html#statistics-output)
/// output format) require that a block of statistics is followed by a closing line; this
/// function outputs this closing line **if** it is configued.
pub fn log_statistic_postfix() {
    if let Some(statistic_options_lock) = STATISTIC_OPTIONS.get() {
        if let Ok(mut statistic_options) = statistic_options_lock.write() {
            if let Some(post_fix) = statistic_options.after_statistics {
                if let Err(e) = write!(statistic_options.statistics_writer, "{post_fix}\n") {
                    debug!("Could not write statistic: {e}");
                }
            }
        }
    }
}

/// Returns whether or not statistics should be logged by determining whether the
/// [`StatisticOptions`] have been configured.
pub fn should_log_statistics() -> bool {
    STATISTIC_OPTIONS.get().is_some()
}
