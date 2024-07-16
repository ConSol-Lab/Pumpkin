mod file_format;
mod flatzinc;
mod maxsat;
mod result;

use std::fmt::Debug;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use file_format::FileFormat;
use log::error;
use log::info;
use log::warn;
use log::Level;
use log::LevelFilter;
use pumpkin_lib::options::RestartOptions;
use pumpkin_lib::options::SequenceGeneratorType;
use pumpkin_lib::options::SolverOptions;
use pumpkin_lib::Solver;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use result::PumpkinError;
use result::PumpkinResult;

use crate::flatzinc::FlatZincOptions;
use crate::maxsat::wcnf_problem;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The instance to solve. The file should have one of the following extensions:
    ///  * '.cnf' for SAT instances, given in the DIMACS format,
    ///  * '.wcnf' for MaxSAT instances, given in the WDIMACS format.
    instance_path: PathBuf,

    /// The output path for the proof file.
    ///
    /// When solving a DIMACS instance, a DRAT proof is logged. In case of a FlatZinc model, a DRCP
    /// proof is logged.
    #[arg(long)]
    proof: Option<PathBuf>,

    /// The number of high lbd learned clauses that are kept in the database.
    /// Learned clauses are kept based on the tiered system introduced by Chanseok Oh
    #[arg(long = "learning-max-num-clauses", default_value_t = 4000)]
    learning_max_num_clauses: u64,

    /// Learned clauses with this threshold LBD or lower are kept permanently
    /// Learned clauses are kept based on the tiered system introduced by Chanseok Oh
    #[arg(long = "learning-lbd-threshold", default_value_t = 5)]
    learning_lbd_threshold: u32,

    /// Decides whether learned clauses are minimised as a post-processing step after computing the
    /// 1uip Minimisation is done according to the idea proposed by Van Gelder
    #[arg(
        long = "learning-minimise",
        value_parser = learned_clause_minimisation_parser,
        default_value_t = true.into()
    )]
    learning_clause_minimisation: CliArg<bool>,

    /// Decides the sequence based on which the restarts are performed.
    /// To be used in combination with "restarts-base-interval"
    #[arg(
        long = "restart-sequence",
        value_parser = sequence_generator_parser,
        default_value_t = SequenceGeneratorType::Constant.into()
    )]
    restart_sequence_generator_type: CliArg<SequenceGeneratorType>,

    /// The base interval length is used as a multiplier to the restart sequence.
    /// For example, constant restarts with base interval 100 means a retart is triggered every 100
    /// conflicts.
    #[arg(long = "restart-base-interval", default_value_t = 50)]
    restart_base_interval: u64,

    #[arg(long = "restart-min-initial-conflicts", default_value_t = 10000)]
    restarts_min_num_conflicts_before_first_restart: u64,

    /// Used to determine if a restart should be forced (part of Glucose restarts).
    /// The state is "bad" if the current LBD value is much greater than the global LBD average
    /// A greater/lower value for lbd-coef means a less/more frequent restart policy
    #[arg(long = "restart-lbd-coef", default_value_t = 1.25)]
    restart_lbd_coef: f64,

    /// Used to determine if a restart should be blocked (part of Glucose restarts).
    /// To be used in combination with "restarts-num-assigned-window".
    /// A restart is blocked if the number of assigned propositional variables is must greater than
    /// the average number of assigned variables in the recent past A greater/lower value for
    /// num-assigned-coef means fewer/more blocked restarts
    #[arg(long = "restart-num-assigned-coef", default_value_t = 1.4)]
    restart_num_assigned_coef: f64,

    /// Used to determine the length of the recent past that should be considered when deciding on
    /// blocking restarts (part of Glucose restarts). The solver considers the last
    /// num-assigned-window conflicts as the reference point for the number of assigned variables
    #[arg(long = "restart-num-assigned-window", default_value_t = 5000)]
    restart_num_assigned_window: u64,

    /// The coefficient in the geometric sequence x_i = x_{i-1} * geometric-coef, x_1 =
    /// "restarts-base-interval" Used only if "restarts-sequence-generator" is assigned to
    /// "geometric".
    #[arg(long = "restart-geometric-coef")]
    restart_geometric_coef: Option<f64>,

    /// The time budget for the solver, given in milliseconds.
    #[arg(short = 't', long = "time-limit")]
    time_limit: Option<u64>,

    /// The random seed to use for the PRNG. This influences the initial order of the variables.
    #[arg(short = 'r', long = "random-seed", default_value_t = 42)]
    random_seed: u64,

    /// Enables log message output from the solver
    #[arg(short = 'v', long = "verbose", default_value_t = false)]
    verbose: bool,

    /// Enables logging of statistics from the solver
    #[arg(short = 's', long = "log-statistics", default_value_t = false)]
    log_statistics: bool,

    /// Instructs the solver to perform free search when solving a MiniZinc model; this flag
    /// indicates that it is allowed to ignore the search annotations specified in the model.
    /// See the [MiniZinc specification](https://www.minizinc.org/doc-2.6.3/en/fzn-spec.html#cmdoption-f)
    /// for more information.
    #[arg(short = 'f', long = "free-search", default_value_t = false)]
    free_search: bool,

    /// Instructs the solver to report all solutions in the case of satisfaction problems,
    /// or print intermediate solutions of increasing quality in the case of optimisation
    /// problems.
    ///
    /// See the [MiniZinc specification](https://www.minizinc.org/doc-2.8.2/en/fzn-spec.html#cmdoption-a)
    /// for more information.
    #[arg(short = 'a', long = "all-solutions", default_value_t = false)]
    all_solutions: bool,

    /// If `--verbose` is enabled removes the timestamp information from the log messages
    #[arg(long = "omit-timestamp", default_value_t = false)]
    omit_timestamp: bool,

    /// If `--verbose` is enabled removes the call site information from the log messages.
    /// Call site is the file and line in it that originated the message.
    #[arg(long = "omit-call-site", default_value_t = false)]
    omit_call_site: bool,

    /// Determines whether to allow the cumulative propagator(s) to create holes in the domain
    #[arg(long = "cumulative-allow-holes", default_value_t = false)]
    cumulative_allow_holes: bool,

    /// Verify the reported solution is consistent with the instance, and, if applicable, verify
    /// that it evaluates to the reported objective value.
    #[arg(long = "verify", default_value_t = false)]
    verify_solution: bool,
}

fn configure_logging(
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

fn configure_logging_unknown() -> std::io::Result<()> {
    env_logger::Builder::new()
        .format(move |buf, record| writeln!(buf, "{}", record.args()))
        .filter_level(LevelFilter::Trace)
        .target(env_logger::Target::Stdout)
        .init();
    Ok(())
}

fn configure_logging_minizinc(verbose: bool, log_statistics: bool) -> std::io::Result<()> {
    pumpkin_lib::statistics::configure(log_statistics, "%%%mzn-stat:", Some("%%%mzn-stat-end"));
    let level_filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    env_logger::Builder::new()
        .format(move |buf, record| {
            write!(buf, "% ")?;

            writeln!(buf, "{}", record.args())
        })
        .filter_level(level_filter)
        .target(env_logger::Target::Stdout)
        .init();
    info!("Logging successfully configured");
    Ok(())
}

fn configure_logging_sat(
    verbose: bool,
    log_statistics: bool,
    omit_timestamp: bool,
    omit_call_site: bool,
) -> std::io::Result<()> {
    pumpkin_lib::statistics::configure(log_statistics, "c STAT", None);
    let level_filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    env_logger::Builder::new()
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
        .init();
    info!("Logging successfully configured");
    Ok(())
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => {
            error!("Execution failed, error: {}", e);
            std::process::exit(1);
        }
    }
}

fn run() -> PumpkinResult<()> {
    pumpkin_lib::print_pumpkin_assert_warning_message!();

    let args = Args::parse();

    let file_format = match args.instance_path.extension().and_then(|ext| ext.to_str()) {
        Some("cnf") => FileFormat::CnfDimacsPLine,
        Some("wcnf") => FileFormat::WcnfDimacsPLine,
        Some("fzn") => FileFormat::FlatZinc,
        _ => {
            configure_logging_unknown()?;
            return Err(PumpkinError::invalid_instance(args.instance_path.display()));
        }
    };

    configure_logging(
        file_format,
        args.verbose,
        args.log_statistics,
        args.omit_timestamp,
        args.omit_call_site,
    )?;

    // todo: disabled proof logging
    // let proof_log = if let Some(path_buf) = args.proof {
    //     match file_format {
    //         FileFormat::CnfDimacsPLine => ProofLog::dimacs(&path_buf)?,
    //         FileFormat::WcnfDimacsPLine => {
    //             return Err(PumpkinError::ProofGenerationNotSupported("wcnf".to_owned()))
    //         }
    //         FileFormat::MaxSAT2022 => {
    //             return Err(PumpkinError::ProofGenerationNotSupported(
    //                 "maxsat".to_owned(),
    //             ))
    //         }
    //         FileFormat::FlatZinc => ProofLog::cp(&path_buf, Format::Text)?,
    //     }
    // } else {
    //     ProofLog::default()
    // };

    let solver_options = SolverOptions {
        restart_options: RestartOptions {
            sequence_generator_type: args.restart_sequence_generator_type.inner,
            base_interval: args.restart_base_interval,
            min_num_conflicts_before_first_restart: args
                .restarts_min_num_conflicts_before_first_restart,
            lbd_coef: args.restart_lbd_coef,
            num_assigned_coef: args.restart_num_assigned_coef,
            num_assigned_window: args.restart_num_assigned_window,
            geometric_coef: args.restart_geometric_coef,
        },
        learning_clause_minimisation: args.learning_clause_minimisation.inner,
        random_generator: SmallRng::seed_from_u64(args.random_seed),
    };

    let time_limit = args.time_limit.map(Duration::from_millis);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::invalid_instance(args.instance_path.display()))?;
    let _verify_outcome = args.verify_solution;

    match file_format {
        FileFormat::CnfDimacsPLine => cnf_problem(solver_options, time_limit, instance_path)?,
        FileFormat::WcnfDimacsPLine => wcnf_problem(solver_options, time_limit, instance_path)?,
        FileFormat::FlatZinc => flatzinc::solve(
            Solver::with_options(solver_options),
            instance_path,
            time_limit,
            FlatZincOptions {
                free_search: args.free_search,
                all_solutions: args.all_solutions,
                cumulative_allow_holes: args.cumulative_allow_holes,
            },
        )?,
    }

    Ok(())
}

fn cnf_problem(
    solver_options: SolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
) -> Result<(), PumpkinError> {
    // todo: parsers were removed?
    todo!()
    // let instance_file = File::open(instance_path)?;
    // let mut solver = parse_cnf::<SolverDimacsSink>(
    //     instance_file,
    //     SolverArgs::new(learning_options, solver_options),
    // )?;

    // let mut termination =
    //     TimeBudget::starting_now(time_limit.unwrap_or(Duration::from_secs(u64::MAX)));
    // let mut brancher = solver.default_brancher_over_all_propositional_variables();
    // match solver.satisfy(&mut brancher, &mut termination) {
    //     SatisfactionResult::Satisfiable(solution) => {
    //         println!("s SATISFIABLE");
    //         let num_propositional_variables = solution.num_propositional_variables();
    //         println!(
    //             "v {}",
    //             stringify_solution(&solution, num_propositional_variables, true)
    //         );
    //     }
    //     SatisfactionResult::Unsatisfiable => {
    //         if solver.conclude_proof_unsat().is_err() {
    //             warn!("Failed to log solver conclusion");
    //         };

    //         println!("s UNSATISFIABLE");
    //     }
    //     SatisfactionResult::Unknown => {
    //         println!("s UNKNOWN");
    //     }
    // };

    // Ok(())
}

fn learned_clause_minimisation_parser(s: &str) -> Result<CliArg<bool>, String> {
    if s == "1" || s.to_lowercase() == "true" {
        Ok(true.into())
    } else if s == "0" || s.to_lowercase() == "false" {
        Ok(false.into())
    } else {
        Err(format!(
            "'{s}' is not valid input for the learned clause minimisation parameter."
        ))
    }
}

fn sequence_generator_parser(s: &str) -> Result<CliArg<SequenceGeneratorType>, String> {
    match s {
        "constant" => Ok(SequenceGeneratorType::Constant.into()),
        "geometric" => Ok(SequenceGeneratorType::Geometric.into()),
        "luby" => Ok(SequenceGeneratorType::Luby.into()),
        value => Err(format!("'{value}' is not a valid sequence generator.")),
    }
}

#[derive(Debug, Clone)]
struct CliArg<T> {
    inner: T,
}

impl<T> From<T> for CliArg<T> {
    fn from(value: T) -> Self {
        CliArg { inner: value }
    }
}

impl std::fmt::Display for CliArg<SequenceGeneratorType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.inner, f)
    }
}

impl std::fmt::Display for CliArg<bool> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.inner, f)
    }
}
