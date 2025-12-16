mod file_format;
mod flatzinc;
mod maxsat;
mod os_signal_termination;
mod parsers;
mod result;

use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use clap::ValueEnum;
use file_format::FileFormat;
use log::Level;
use log::LevelFilter;
use log::error;
use log::info;
use log::warn;
use maxsat::PseudoBooleanEncoding;
use parsers::dimacs::SolverArgs;
use parsers::dimacs::SolverDimacsSink;
use parsers::dimacs::parse_cnf;
use pumpkin_propagators::propagators::CumulativeExplanationType;
use pumpkin_propagators::propagators::CumulativeOptions;
use pumpkin_propagators::propagators::CumulativePropagationMethod;
use pumpkin_solver::Solver;
use pumpkin_solver::convert_case::Case;
use pumpkin_solver::optimisation::OptimisationStrategy;
use pumpkin_solver::options::*;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::pumpkin_assert_simple;
use pumpkin_solver::rand::SeedableRng;
use pumpkin_solver::rand::rngs::SmallRng;
use pumpkin_solver::results::ProblemSolution;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::results::SolutionReference;
use pumpkin_solver::statistics::configure_statistic_logging;
use pumpkin_solver::termination::TimeBudget;
use result::PumpkinError;
use result::PumpkinResult;

use crate::flatzinc::FlatZincOptions;
use crate::maxsat::wcnf_problem;

#[derive(Debug, Parser)]
#[command(
    help_template = "\
{before-help}{name} {version}
Authors: {author}
About: {about}

{usage-heading}\n{tab}{usage}

{all-args}{after-help}
",
    author,
    version,
    about,
    arg_required_else_help = true
)]
struct Args {
    /// The instance to solve. The file should have one of the following extensions:
    ///  - '*.cnf' for SAT instances, given in the DIMACS format,
    ///  - '*.wcnf' for MaxSAT instances, given in the WDIMACS format.
    ///  - '*.fzn' for FlatZinc instances (see <https://docs.minizinc.dev/en/stable/flattening.html>).
    #[clap(verbatim_doc_comment)]
    instance_path: PathBuf,

    /// The output path for the proof file.
    ///
    /// When solving a DIMACS instance, a DRAT proof is logged. In case of a FlatZinc model, a DRCP
    /// proof is logged.
    ///
    /// In case of a DRCP proof, if the file extension ends with `.gz`, a gzipped proof will be
    /// produced.
    #[arg(long, verbatim_doc_comment)]
    proof_path: Option<PathBuf>,

    /// What type of proof to log.
    ///
    /// If the `proof_path` option is not provided, this is ignored.
    #[arg(long, value_enum, default_value_t)]
    proof_type: ProofType,

    /// The number of high-lbd learned nogoods that are kept in the database.
    ///
    /// Learned nogoods are kept based on the tiered system introduced in "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL - Chanseok Oh (2016)".
    ///
    /// Possible values: usize
    #[arg(
        long = "learning-max-num-high-lbd-nogoods",
        default_value_t = 20_000,
        verbatim_doc_comment
    )]
    learning_max_num_high_lbd_nogoods: usize,

    /// The number of mid-lbd learned nogoods that are kept in the database.
    ///
    /// This is based on the variation of the three-tiered system proposed in
    /// "Improving Implementation of SAT Competitions 2017–2019 Winners".
    ///
    /// Possible values: usize
    #[arg(
        long = "learning-max-num-mid-lbd-nogoods",
        default_value_t = 7000,
        verbatim_doc_comment
    )]
    learning_max_num_mid_lbd_nogoods: usize,

    /// The number of low-lbd learned nogoods that are kept in the database.
    ///
    /// This is based on the variation of the three-tiered system proposed in
    /// "Improving Implementation of SAT Competitions 2017–2019 Winners".
    ///
    /// Possible values: usize
    #[arg(
        long = "learning-max-num-low-lbd-nogoods",
        default_value_t = 100_000,
        verbatim_doc_comment
    )]
    learning_max_num_low_lbd_nogoods: usize,

    /// The treshold determining whether a learned nogood is "low" LBD.
    ///
    /// "Low" LBD nogood are kept around for longer since they are of better "quality".
    ///
    /// Learned nogoods are kept based on the tiered system introduced "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL - Chanseok Oh (2016)"
    /// with the variation from "Improving Implementation of SAT Competitions 2017–2019 Winners".
    ///
    /// Possible values: u32
    #[arg(
        long = "learning-low-lbd-threshold",
        default_value_t = 3,
        verbatim_doc_comment
    )]
    learning_low_lbd_threshold: u32,

    /// The treshold determining whether a learned nogood is "low" LBD.
    ///
    /// "High" LBD nogood are kept around for a shorter amount of time since they are of bad
    /// "quality".
    ///
    /// Learned nogoods are kept based on the tiered system introduced "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL - Chanseok Oh (2016)"
    /// with the variation from "Improving Implementation of SAT Competitions 2017–2019 Winners".
    ///
    /// Possible values: u32
    #[arg(
        long = "learning-high-lbd-threshold",
        default_value_t = 7,
        verbatim_doc_comment
    )]
    learning_high_lbd_threshold: u32,

    /// Decides whether learned clauses are minimised as a post-processing step after computing the
    /// 1-UIP Minimisation is done; according to the idea proposed in "Generalized Conflict-Clause
    /// Strengthening for Satisfiability Solvers - Allen van Gelder (2011)".
    ///
    /// If this flag is present then the minimisation is turned off.
    ///
    /// Possible values: bool
    #[arg(long = "no-learning-minimise", verbatim_doc_comment)]
    no_learning_clause_minimisation: bool,

    /// Decides the sequence based on which the restarts are performed.
    ///
    /// - The "constant" approach uses a constant number of conflicts before another restart is
    ///   triggered
    /// - The "geometric" approach uses a geometrically increasing sequence
    /// - The "luby" approach uses a recursive sequence of the form 1, 1, 2, 1, 1, 2, 4, 1, 1, 2,
    ///   1, 1, 2, 4, 8, 1, 1, 2.... (see "Optimal speedup of Las Vegas algorithms - Luby et al.
    ///   (1993)")
    ///
    /// To be used in combination with "--restarts-base-interval".
    #[arg(long, value_enum, default_value_t)]
    restart_sequence_generator_type: SequenceGeneratorType,

    /// The base interval length is used as a multiplier to the restart sequence.
    /// - In the case of the "constant" restart sequence this argument indicates the constant which
    ///   is used to determine when a restart occurs
    /// - For the "geometric" approach this argument indicates the starting value of the sequence
    /// - For the "luby" approach, the sequence is multiplied by this value
    ///
    /// For example, constant restarts with base interval 50 means a restart is triggered every 50
    /// conflicts.
    ///
    /// Possible values: u64
    #[arg(
        long = "restart-base-interval",
        default_value_t = 50,
        verbatim_doc_comment
    )]
    restart_base_interval: u64,

    /// Indicates the minimum number of initial conflicts before the first restart can occur. This
    /// allows the solver to learn some things about the problem before a restart is allowed to
    /// occur.
    ///
    /// Possible values: u64
    #[arg(
        long = "restart-min-initial-conflicts",
        default_value_t = 10000,
        verbatim_doc_comment
    )]
    restart_min_num_conflicts_before_first_restart: u64,

    /// Used to determine if a restart should be forced (see "Refining Restarts Strategies for SAT
    /// and UNSAT - Audemard and Simon (2012)").
    ///
    /// The state is "bad" if the current LBD value is much greater than
    /// the global LBD average. A greater (lower) value for lbd-coef means a less (more) frequent
    /// restart policy. If the long-term average LBD multiplied by this coefficient is lower
    /// than the short-term average LBD then a restart is performed.
    ///
    /// Possible values: f64
    #[arg(
        long = "restart-lbd-coef",
        default_value_t = 1.25,
        verbatim_doc_comment
    )]
    restart_lbd_coef: f64,

    /// Used to determine if a restart should be blocked (see "Refining Restarts Strategies for SAT
    /// and UNSAT - Audemard and Simon (2012)").
    ///
    /// To be used in combination with "--restarts-num-assigned-window".
    ///
    /// A restart is blocked if the number of assigned propositional variables is much greater than
    /// the average number of assigned variables in the recent past. A greater (lower) value for
    /// "--restart-num-assigned-coef" means fewer (more) blocked restarts.
    ///
    /// Possible values: f64
    #[arg(
        long = "restart-num-assigned-coef",
        default_value_t = 1.4,
        verbatim_doc_comment
    )]
    restart_num_assigned_coef: f64,

    /// Used to determine the length of the recent past that should be considered when deciding on
    /// blocking restarts (see "Refining Restarts Strategies for SAT
    /// and UNSAT - Audemard and Simon (2012)").
    ///
    /// The solver considers the last "--restart_num_assigned_window" conflicts as the reference
    /// point for the number of assigned variables.
    ///
    /// Possible values: u64
    #[arg(
        long = "restart-num-assigned-window",
        default_value_t = 5000,
        verbatim_doc_comment
    )]
    restart_num_assigned_window: u64,

    /// The coefficient in the geometric sequence `x_i = x_{i-1} * "--restart-geometric-coef"`
    /// where `x_1 = "--restarts-base-interval"`. Used only if "--restarts-sequence-generator"
    /// is assigned to "geometric".
    ///
    /// Possible values: f64 (Optional)
    #[arg(long = "restart-geometric-coef", verbatim_doc_comment)]
    restart_geometric_coef: Option<f64>,

    /// The time budget for the solver, given in milliseconds.
    ///
    /// Possible values: u64 (Optional)
    #[arg(short = 't', long = "time-limit", verbatim_doc_comment)]
    time_limit: Option<u64>,

    /// The random seed to use for the Pseudo Random Number Generator.
    ///
    /// Randomisation can be used for aspects such as the variable/value generator or initial
    /// ordering of variables.
    ///
    /// Possible values: u64
    #[arg(
        short = 'r',
        long = "random-seed",
        default_value_t = 42,
        verbatim_doc_comment
    )]
    random_seed: u64,

    /// Enables log message output from the solver.
    ///
    /// For printing statistics see the option "--log-statistics", and for printing all solutions
    /// (in case of a satisfaction problem) or printing solutions of increasing quality (in case of
    /// an optimization problem) see the option "--all-solutions".
    ///
    /// Possible values: bool
    #[arg(short = 'v', long = "verbose", verbatim_doc_comment)]
    verbose: bool,

    /// Enables logging of statistics from the solver.
    ///
    /// Possible values: bool
    #[arg(short = 's', long = "log-statistics", verbatim_doc_comment)]
    log_statistics: bool,

    /// Instructs the solver to perform free search when solving a MiniZinc model; this flag
    /// indicates that it is allowed to ignore the search annotations specified in the model.
    ///
    /// See the MiniZinc specification (<https://docs.minizinc.dev/en/stable/fzn-spec.html#cmdoption-f>)
    /// for more information.
    ///
    /// Possible values: bool
    #[arg(short = 'f', long = "free-search", verbatim_doc_comment)]
    free_search: bool,

    /// Instructs the solver to report all solutions in the case of satisfaction problems,
    /// or print intermediate solutions of increasing quality in the case of optimisation
    /// problems.
    ///
    /// See the MiniZinc specification (<https://docs.minizinc.dev/en/stable/fzn-spec.html#cmdoption-a>)
    /// for more information.
    ///
    /// Possible values: bool
    #[arg(short = 'a', long = "all-solutions", verbatim_doc_comment)]
    all_solutions: bool,

    /// If `--verbose` is enabled then this option removes the timestamp information from the log
    /// messages. Note that this option will only take affect in the case of a (W)CNF instance.
    ///
    /// Possible values: bool
    #[arg(long = "omit-timestamp", verbatim_doc_comment)]
    omit_timestamp: bool,

    /// If `--verbose` is enabled then this option removes the call site information from the log
    /// messages. The call site is the file and line from which the message
    /// originated. Note that this option will only take affect in the case of a (W)CNF instance.
    ///
    /// Possible values: bool
    #[arg(long = "omit-call-site", default_value_t = false, verbatim_doc_comment)]
    omit_call_site: bool,

    /// The encoding to use for the upper bound constraint in a MaxSAT optimisation problem.
    ///
    /// - The "generalised-totalizer" value specifies that the solver should use the Generalized
    ///   Totalizer Encoding (see "Generalized totalizer encoding for pseudo-boolean constraints -
    ///   Saurabh et al. (2015)")
    /// - The "cardinality-network" value specifies that the solver should use the Cardinality
    ///   Network Encoding (see "Cardinality networks: a theoretical and empirical study - Asín et
    ///   al. (2011)").
    #[arg(long, value_enum, default_value_t)]
    upper_bound_encoding: PseudoBooleanEncoding,

    /// Determines that no restarts are allowed by the solver.
    ///
    /// Possible values: bool
    #[arg(long = "no-restarts", verbatim_doc_comment)]
    no_restarts: bool,

    /// Determines the conflict resolver.
    #[arg(long, value_enum, default_value_t)]
    conflict_resolver: ConflictResolver,

    /// Determines that the cumulative propagator(s) are allowed to create holes in the domain.
    ///
    /// Possible values: bool
    #[arg(long = "cumulative-allow-holes", verbatim_doc_comment)]
    cumulative_allow_holes: bool,

    /// Determines the type of explanation used by the cumulative propagator(s) to explain
    /// propagations/conflicts.
    #[arg(long, value_enum, default_value_t)]
    cumulative_explanation_type: CumulativeExplanationType,

    /// Determines the type of propagator which is used by the cumulative propagator(s) to
    /// propagate the constraint.
    ///
    /// Currently, the solver only supports variations on time-tabling methods.
    #[arg(long, value_enum, default_value_t)]
    cumulative_propagation_method: CumulativePropagationMethod,

    /// Determines whether a single profiles are used generated when explaining a propagation for
    /// the cumulative constraint.
    ///
    /// Possible values: bool
    #[arg(long = "cumulative-single-profiles")]
    cumulative_single_profiles: bool,

    /// Determines whether incremental backtracking is applied or whether the cumulative
    /// propagators compute the time-table from scratch upon backtracking
    ///
    /// Possible values: bool
    #[arg(long = "cumulative-incremental-backtracking")]
    cumulative_incremental_backtracking: bool,

    /// Determine what type of optimisation strategy is used by the solver
    #[arg(long = "optimisation-strategy", value_enum, default_value_t)]
    optimisation_strategy: OptimisationStrategy,

    /// The amount of memory (in MB) that is preallocated for storing nogoods.
    #[arg(long = "memory-preallocated", default_value_t = 1000)]
    memory_preallocated: usize,
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
    if log_statistics {
        configure_statistic_logging("c STAT", None, None, None);
    }
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
            error!("Execution failed, error: {e}");
            std::process::exit(1);
        }
    }
}

fn run() -> PumpkinResult<()> {
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

    if pumpkin_solver::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION
        >= pumpkin_solver::asserts::PUMPKIN_ASSERT_MODERATE
    {
        warn!(
            "Potential performance degradation: the Pumpkin assert level is set to {}, meaning many debug asserts are active which may result in performance degradation.",
            pumpkin_solver::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION
        );
    };

    let proof_log = if let Some(path_buf) = args.proof_path.as_ref() {
        match file_format {
            FileFormat::CnfDimacsPLine => ProofLog::dimacs(path_buf)?,
            FileFormat::WcnfDimacsPLine => {
                return Err(PumpkinError::ProofGenerationNotSupported("wcnf".to_owned()));
            }
            FileFormat::FlatZinc => ProofLog::cp(path_buf, args.proof_type == ProofType::Full)?,
        }
    } else {
        ProofLog::default()
    };

    let restart_options = RestartOptions {
        sequence_generator_type: args.restart_sequence_generator_type,
        base_interval: args.restart_base_interval,
        min_num_conflicts_before_first_restart: args.restart_min_num_conflicts_before_first_restart,
        lbd_coef: args.restart_lbd_coef,
        num_assigned_coef: args.restart_num_assigned_coef,
        num_assigned_window: args.restart_num_assigned_window,
        geometric_coef: args.restart_geometric_coef,
        no_restarts: args.no_restarts,
    };
    let learning_options = LearningOptions {
        max_activity: 1e20,
        activity_decay_factor: 0.99,
        max_num_high_lbd_nogoods: args.learning_max_num_high_lbd_nogoods,
        max_num_mid_lbd_nogoods: args.learning_max_num_mid_lbd_nogoods,
        max_num_low_lbd_nogoods: args.learning_max_num_low_lbd_nogoods,
        lbd_threshold_low: args.learning_low_lbd_threshold,
        lbd_threshold_high: args.learning_high_lbd_threshold,
        activity_bump_increment: 1.0,
    };

    let solver_options = SolverOptions {
        // 1 MB is 1_000_000 bytes
        memory_preallocated: args.memory_preallocated,
        restart_options,
        learning_clause_minimisation: if args.proof_type == ProofType::Full {
            warn!("Recursive minimisation is disabled when logging the full proof.");
            false
        } else {
            !args.no_learning_clause_minimisation
        },
        random_generator: SmallRng::seed_from_u64(args.random_seed),
        proof_log,
        conflict_resolver: args.conflict_resolver,
        learning_options,
    };

    let time_limit = args.time_limit.map(Duration::from_millis);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::invalid_instance(args.instance_path.display()))?;

    match file_format {
        FileFormat::CnfDimacsPLine => cnf_problem(solver_options, time_limit, instance_path)?,
        FileFormat::WcnfDimacsPLine => wcnf_problem(
            solver_options,
            time_limit,
            instance_path,
            args.upper_bound_encoding,
        )?,
        FileFormat::FlatZinc => flatzinc::solve(
            Solver::with_options(solver_options),
            instance_path,
            time_limit,
            FlatZincOptions {
                free_search: args.free_search,
                all_solutions: args.all_solutions,
                cumulative_options: CumulativeOptions::new(
                    args.cumulative_allow_holes,
                    args.cumulative_explanation_type,
                    !args.cumulative_single_profiles,
                    args.cumulative_propagation_method,
                    args.cumulative_incremental_backtracking,
                ),
                optimisation_strategy: args.optimisation_strategy,
                proof_type: args.proof_path.map(|_| args.proof_type),
                verbose: args.verbose,
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
    let instance_file = File::open(instance_path)?;
    let mut solver =
        parse_cnf::<SolverDimacsSink>(instance_file, SolverArgs::new(solver_options))?.solver;

    let mut termination =
        TimeBudget::starting_now(time_limit.unwrap_or(Duration::from_secs(u64::MAX)));
    let mut brancher = solver.default_brancher();
    match solver.satisfy(&mut brancher, &mut termination) {
        SatisfactionResult::Satisfiable(satisfiable) => {
            satisfiable
                .solver()
                .log_statistics(Some(satisfiable.brancher()), true);
            println!("s SATISFIABLE");
            println!(
                "v {}",
                stringify_solution(
                    satisfiable.solution(),
                    satisfiable.solution().num_domains(),
                    true
                )
            );
        }
        SatisfactionResult::Unsatisfiable(solver, brancher) => {
            solver.log_statistics(Some(brancher), true);

            println!("s UNSATISFIABLE");
        }
        SatisfactionResult::Unknown(solver, brancher) => {
            solver.log_statistics(Some(brancher), true);
            println!("s UNKNOWN");
        }
    }

    Ok(())
}

fn stringify_solution(
    solution: SolutionReference,
    number_of_variables: usize,
    terminate_with_zero: bool,
) -> String {
    solution
        .get_domains()
        .take(number_of_variables)
        .map(|domain_id| {
            let value = solution.get_integer_value(domain_id);
            pumpkin_assert_simple!((0..=1).contains(&value));
            if value == 1 {
                format!("{} ", domain_id.id())
            } else {
                format!("-{} ", domain_id.id())
            }
        })
        .chain(if terminate_with_zero {
            std::iter::once(String::from("0"))
        } else {
            std::iter::once(String::new())
        })
        .collect::<String>()
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum ProofType {
    /// Log only the proof scaffold.
    #[default]
    Scaffold,
    /// Log the full proof with hints.
    Full,
}
