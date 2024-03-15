//! Responsible for behaviour related to logging statistics with a specific pre-fix and closing
//! lines.

pub mod statistic_logger {
    use std::cell::OnceCell;
    use std::fmt::Display;
    use std::sync::Mutex;

    static STATISTIC_PREFIX: Mutex<OnceCell<&str>> = Mutex::new(OnceCell::new());
    static AFTER_STATISTICS: Mutex<OnceCell<Option<&str>>> = Mutex::new(OnceCell::new());

    /// Configures the statistic logger to use a certain prefix and (an optional) closing line which
    /// can be printed after all of the statistics have been logged.
    pub fn configure(prefix: &'static str, after: Option<&'static str>) {
        let _ = STATISTIC_PREFIX.lock().unwrap().get_or_init(|| prefix);
        let _ = AFTER_STATISTICS.lock().unwrap().get_or_init(|| after);
    }

    /// Logs the provided statistic with name `name` and value `value`. At the moment it will log in
    /// the format `STATISTIC_PREFIX NAME=VALUE`.
    pub fn log_statistic(name: impl Display, value: impl Display) {
        println!(
            "{} {name}={value}",
            STATISTIC_PREFIX.lock().unwrap().get().unwrap()
        )
    }

    /// Certain formats (e.g. the [MiniZinc](https://www.minizinc.org/doc-2.7.6/en/fzn-spec.html#statistics-output)
    /// output format) require that a block of statistics is followed by a closing line; this
    /// function outputs this closing line **if** it is configued.
    pub fn log_statistic_postfix() {
        if let Some(post_fix) = AFTER_STATISTICS.lock().unwrap().get().unwrap() {
            println!("{post_fix}")
        }
    }
}
