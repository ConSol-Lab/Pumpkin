//! Responsible for behaviour related to logging statistics with a specific pre-fix and closing
//! lines.

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io::Write;
use std::io::stdout;
use std::sync::OnceLock;
use std::sync::RwLock;

use convert_case::Case;
use convert_case::Casing;

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
    // The writer to which the statistics are written
    statistics_writer: Box<dyn Write + Send + Sync>,
}

impl Debug for StatisticOptions<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StatisticOptions")
            .field("statistic_prefix", &self.statistic_prefix)
            .field("after_statistics", &self.after_statistics)
            .field("statistics_casing", &self.statistics_casing)
            .field("statistics_writer", &"<Writer>")
            .finish()
    }
}

static STATISTIC_OPTIONS: OnceLock<RwLock<StatisticOptions>> = OnceLock::new();

/// Configures the logging of the statistics.
///
/// It specifies the (optional) prefix and a closing line (postfix) which
/// can be written to the writer after all of the statistics have been logged.
/// It also specifies the writer to be used for writing statistics. In case no writer is specified,
/// stdout will be used. Statistics will only be written if `log_statistics` is true.
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
            let _ = writeln!(
                statistic_options.statistics_writer,
                "{prefix} {name}={value}"
            );
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
                let _ = writeln!(statistic_options.statistics_writer, "{post_fix}");
            }
        }
    }
}

/// Returns whether or not statistics should be logged by determining whether the
/// [`StatisticOptions`] have been configured.
pub fn should_log_statistics() -> bool {
    STATISTIC_OPTIONS.get().is_some()
}
