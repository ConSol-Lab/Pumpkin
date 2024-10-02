//! Responsible for behaviour related to logging statistics with a specific pre-fix and closing
//! lines.

pub(crate) mod statistic_logger {
    use std::fmt::Display;
    use std::sync::OnceLock;

    use convert_case::Case;
    use convert_case::Casing;

    static STATISTIC_PREFIX: OnceLock<&str> = OnceLock::new();
    static AFTER_STATISTICS: OnceLock<Option<&str>> = OnceLock::new();
    static LOG_STATISTICS: OnceLock<bool> = OnceLock::new();
    static STATISTICS_CASING: OnceLock<Option<Case>> = OnceLock::new();

    /// Configures the logging of the statistics.
    ///
    /// It specifies the (optional) prefix and a closing line (postfix) which
    /// can be printed after all of the statistics have been logged. Statistics will only be printed
    /// if `log_statistics` is true.
    pub fn configure(
        log_statistics: bool,
        prefix: &'static str,
        after: Option<&'static str>,
        casing: Option<Case>,
    ) {
        let _ = LOG_STATISTICS.get_or_init(|| log_statistics);
        if log_statistics {
            let _ = STATISTIC_PREFIX.get_or_init(|| prefix);
            let _ = AFTER_STATISTICS.get_or_init(|| after);
            let _ = STATISTICS_CASING.get_or_init(|| casing);
        }
    }

    /// Logs the provided statistic with name `name` and value `value`. At the moment it will log in
    /// the format `STATISTIC_PREFIX NAME=VALUE`.
    pub fn log_statistic(name: impl Display, value: impl Display) {
        if *LOG_STATISTICS.get().unwrap_or(&false) {
            let name = if let Some(Some(casing)) = STATISTICS_CASING.get() {
                name.to_string().to_case(*casing)
            } else {
                name.to_string()
            };
            println!(
                "{} {name}={value}",
                STATISTIC_PREFIX.get().expect(
                    "If the verbosity is set to true then the statistic should also be initialized"
                )
            )
        }
    }

    /// Logs the postfix of the statistics (if it has been set).
    ///
    /// Certain formats (e.g. the [MiniZinc](https://www.minizinc.org/doc-2.7.6/en/fzn-spec.html#statistics-output)
    /// output format) require that a block of statistics is followed by a closing line; this
    /// function outputs this closing line **if** it is configued.
    pub fn log_statistic_postfix() {
        if *LOG_STATISTICS.get().unwrap_or(&false) {
            if let Some(post_fix) = AFTER_STATISTICS.get().expect(
                    "If the verbosity is set to true then the statistic-postfix should also be initialized"
                ) {
                println!("{post_fix}")
            }
        }
    }
}
