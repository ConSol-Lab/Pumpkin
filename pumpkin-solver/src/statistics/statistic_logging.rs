//! Responsible for behaviour related to logging statistics with a specific pre-fix and closing
//! lines.

use std::fmt::Display;
use std::io::Write;
use std::sync::OnceLock;

use convert_case::Case;
use convert_case::Casing;
use log::debug;

/// The options for statistic logging containing the statistic prefix, the (optional) line which is
/// printed after the statistics, and the (optional) casing of the statistics.
#[derive(Debug)]
pub struct StatisticOptions<'a> {
    // What is printed before a statistic is printed, the statistics will be printed in the
    // form `{PREFIX} {VALUE}={NAME}`
    statistic_prefix: &'a str,
    // A closing line which is printed after all of the statistics have been printed
    after_statistics: Option<&'a str>,
    // The casing of the name of the statistic
    statistics_casing: Option<Case>,
}

static STATISTIC_OPTIONS: OnceLock<StatisticOptions> = OnceLock::new();

/// Configures the logging of the statistics.
///
/// It specifies the (optional) prefix and a closing line (postfix) which
/// can be printed after all of the statistics have been logged. Statistics will only be printed
/// if `log_statistics` is true.
pub fn configure_statistic_logging(
    prefix: &'static str,
    after: Option<&'static str>,
    casing: Option<Case>,
) {
    let _ = STATISTIC_OPTIONS.get_or_init(|| StatisticOptions {
        statistic_prefix: prefix,
        after_statistics: after,
        statistics_casing: casing,
    });
}

/// Logs the provided statistic with name `name` and value `value`. At the moment it will log in
/// the format `STATISTIC_PREFIX NAME=VALUE`.
pub fn write_statistic(writer: &mut Box<dyn Write>, name: impl Display, value: impl Display) {
    if let Some(statistic_options) = STATISTIC_OPTIONS.get() {
        let name = if let Some(casing) = &statistic_options.statistics_casing {
            name.to_string().to_case(*casing)
        } else {
            name.to_string()
        };
        if let Err(e) = write!(writer, "{} {name}={value}\n", statistic_options.statistic_prefix) {
            debug!("Could not write statistic: {e}")
        };
    }
}

/// Logs the postfix of the statistics (if it has been set).
///
/// Certain formats (e.g. the [MiniZinc](https://www.minizinc.org/doc-2.7.6/en/fzn-spec.html#statistics-output)
/// output format) require that a block of statistics is followed by a closing line; this
/// function outputs this closing line **if** it is configued.
pub fn write_statistic_postfix(writer: &mut Box<dyn Write>) {
    if let Some(statistic_options) = STATISTIC_OPTIONS.get() {
        if let Some(post_fix) = statistic_options.after_statistics {
            if let Err(e) = write!(writer, "{post_fix}\n") {
                debug!("Could not write statistic: {e}");
            }
        }
    }
}

/// Returns whether or not statistics should be logged by determining whether the
/// [`StatisticOptions`] have been configured.
pub fn should_log_statistics() -> bool {
    STATISTIC_OPTIONS.get().is_some()
}
