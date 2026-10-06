use std::io::Write;

use log::Level;
use log::LevelFilter;
use log::info;

use crate::core::convert_case::Case;
use crate::core::statistics::configure_statistic_logging;
use crate::file_format::FileFormat;

/// Configures the logger (and statistic logging) with the output conventions of the given file
/// format.
pub fn configure_logging(
    file_format: FileFormat,
    verbose: bool,
    log_statistics: bool,
    omit_timestamp: bool,
    omit_call_site: bool,
) -> std::io::Result<()> {
    match file_format {
        FileFormat::CnfDimacsPLine | FileFormat::WcnfDimacsPLine => {
            configure_logging_sat(verbose, log_statistics, omit_timestamp, omit_call_site)
        }
        FileFormat::FlatZinc => configure_logging_minizinc(verbose, log_statistics),
    }
}

/// Configures the logger for when the file format of the instance is unknown.
pub fn configure_logging_unknown() -> std::io::Result<()> {
    // A logger may already be installed, e.g. when instances are read repeatedly; keep it.
    let _ = env_logger::Builder::new()
        .format(move |buf, record| writeln!(buf, "{}", record.args()))
        .filter_level(LevelFilter::Trace)
        .target(env_logger::Target::Stdout)
        .try_init();
    Ok(())
}

fn configure_logging_minizinc(verbose: bool, log_statistics: bool) -> std::io::Result<()> {
    if log_statistics {
        configure_statistic_logging(
            "%%%mzn-stat:",
            Some("%%%mzn-stat-end"),
            Some(Case::Camel),
            None,
        );
    }
    let level_filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    // A logger may already be installed, e.g. when instances are read repeatedly; keep it.
    let _ = env_logger::Builder::new()
        .format(move |buf, record| {
            write!(buf, "% ")?;

            writeln!(buf, "{}", record.args())
        })
        .filter_level(level_filter)
        .target(env_logger::Target::Stdout)
        .try_init();
    info!("Logging successfully configured");
    Ok(())
}

fn configure_logging_sat(
    verbose: bool,
    log_statistics: bool,
    omit_timestamp: bool,
    omit_call_site: bool,
) -> std::io::Result<()> {
    if log_statistics {
        configure_statistic_logging("c STAT", None, None, None);
    }
    let level_filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    // A logger may already be installed, e.g. when instances are read repeatedly; keep it.
    let _ = env_logger::Builder::new()
        .format(move |buf, record| {
            write!(buf, "c ")?;
            if record.level() != Level::Info && !omit_timestamp {
                write!(buf, "{} ", buf.timestamp())?;
            }
            write!(buf, "{} ", record.level())?;
            if record.level() != Level::Info && !omit_call_site {
                write!(
                    buf,
                    "[{}:{}] ",
                    record.file().unwrap_or("unknown"),
                    record.line().unwrap_or(0)
                )?;
            }
            writeln!(buf, "{}", record.args())
        })
        .filter_level(level_filter)
        .target(env_logger::Target::Stdout)
        .try_init();
    info!("Logging successfully configured");
    Ok(())
}
