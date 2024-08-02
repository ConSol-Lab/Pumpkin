mod file_format;
mod flatzinc;
mod maxsat;
mod parsers;
mod result;

use std::fmt::Debug;
use std::fs::File;
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
use parsers::dimacs::parse_cnf;
use parsers::dimacs::SolverArgs;
use parsers::dimacs::SolverDimacsSink;
use pumpkin_lib::encodings::PseudoBooleanEncoding;
use pumpkin_lib::options::*;
use pumpkin_lib::proof::Format;
use pumpkin_lib::proof::ProofLog;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::results::Solution;
use pumpkin_lib::termination::TimeBudget;
use pumpkin_lib::variables::PropositionalVariable;
use pumpkin_lib::Solver;
use rand::rngs::SmallRng;
use rand::SeedableRng;
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
    #[arg(long, verbatim_doc_comment)]
    proof_path: Option<PathBuf>,

    /// The number of high lbd learned clauses that are kept in the database.
    /// Learned clauses are kept based on the tiered system introduced in "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL - Chanseok Oh (2016)".
    ///
    /// Possible values: u64
    #[arg(
        long = "learning-max-num-clauses",
        default_value_t = 4000,
        verbatim_doc_comment
    )]
    learning_max_num_clauses: u64,

    /// Learned clauses with this threshold LBD or lower are kept permanently
    /// Learned clauses are kept based on the tiered system introduced "Improving
    /// SAT Solvers by Exploiting Empirical Characteristics of CDCL - Chanseok Oh (2016)".
    ///
    /// Possible values: u32
    #[arg(
        long = "learning-lbd-threshold",
        default_value_t = 5,
        verbatim_doc_comment
    )]
    learning_lbd_threshold: u32,

    /// Decides which clauses will be removed when cleaning up the learned clauses. Can either be
    /// based on the LBD of a clause (the number of different decision levels) or on the activity
    /// of a clause (how often it is used in conflict analysis).
    ///
    /// Possible values: ["lbd", "activity"]
    #[arg(
        short = 'l',
        long = "learning-sorting-strategy",
        value_parser = learned_clause_sorting_strategy_parser,
        default_value_t = LearnedClauseSortingStrategy::Activity, verbatim_doc_comment
    )]
    learning_sorting_strategy: LearnedClauseSortingStrategy,

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
    /// - The "constant" approach uses a constant number of conflicts before another restart is
    ///   triggered
    /// - The "geometric" approach uses a geometrically increasing sequence
    /// - The "luby" approach uses a recursive sequence of the form 1, 1, 2, 1, 1, 2, 4, 1, 1, 2,
    ///   1, 1, 2, 4, 8, 1, 1, 2.... (see "Optimal speedup of Las Vegas algorithms - Luby et al.
    ///   (1993)")
    ///
    /// To be used in combination with "--restarts-base-interval".
    ///
    /// Possible values: ["constant", "geometric", "luby"]
    #[arg(
        long = "restart-sequence",
        value_parser = sequence_generator_parser,
        default_value_t = SequenceGeneratorType::Constant, verbatim_doc_comment
    )]
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
    /// The "gte" value specifies that the solver should use the Generalized Totalizer Encoding
    /// (see "Generalized totalizer encoding for pseudo-boolean constraints - Saurabh et al.
    /// (2015)"), and the "cne" value specifies that the solver should use the Cardinality Network
    /// Encoding (see "Cardinality networks: a theoretical and empirical study - Asín et al.
    /// (2011)").
    ///
    /// Possible values: ["gte", "cne"]
    #[arg(
        long = "upper-bound-encoding",
        value_parser = upper_bound_encoding_parser,
        default_value_t = PseudoBooleanEncoding::GeneralizedTotalizer, verbatim_doc_comment
    )]
    upper_bound_encoding: PseudoBooleanEncoding,

    /// Determines whether to allow the cumulative propagator(s) to create holes in the domain.
    ///
    /// If this option is set to false then only the lower- and upper-bounds are updated.
    ///
    /// Possible values: bool
    #[arg(
        long = "cumulative-allow-holes",
        default_value_t = false,
        verbatim_doc_comment
    )]
    cumulative_allow_holes: bool,

    /// Determines the type of explanation used by the cumulative propagator(s) to explain
    /// propagations/conflicts.
    ///
    /// Possible values: ["naive", "big-step", "pointwise"]
    #[arg(long = "cumulative-explanation-type", value_parser = cumulative_explanation_type_parser, default_value_t = CumulativeExplanationType::default())]
    cumulative_explanation_type: CumulativeExplanationType,

    /// Determines whether a sequence of profiles is generated when explaining a propagation for
    /// the cumulative constraint.
    ///
    /// Possible values: bool
    #[arg(long = "cumulative-generate-sequence")]
    cumulative_generate_sequence: bool,
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

    let learning_options = LearningOptions {
        num_high_lbd_learned_clauses_max: args.learning_max_num_clauses,
        high_lbd_learned_clause_sorting_strategy: args.learning_sorting_strategy,
        lbd_threshold: args.learning_lbd_threshold,
        ..Default::default()
    };

    let proof_log = if let Some(path_buf) = args.proof_path {
        match file_format {
            FileFormat::CnfDimacsPLine => ProofLog::dimacs(&path_buf)?,
            FileFormat::WcnfDimacsPLine => {
                return Err(PumpkinError::ProofGenerationNotSupported("wcnf".to_owned()))
            }
            FileFormat::FlatZinc => ProofLog::cp(&path_buf, Format::Text)?,
        }
    } else {
        ProofLog::default()
    };

    let solver_options = SolverOptions {
        restart_options: RestartOptions {
            sequence_generator_type: args.restart_sequence_generator_type,
            base_interval: args.restart_base_interval,
            min_num_conflicts_before_first_restart: args
                .restart_min_num_conflicts_before_first_restart,
            lbd_coef: args.restart_lbd_coef,
            num_assigned_coef: args.restart_num_assigned_coef,
            num_assigned_window: args.restart_num_assigned_window,
            geometric_coef: args.restart_geometric_coef,
        },
        proof_log,
        learning_clause_minimisation: !args.no_learning_clause_minimisation,
        random_generator: SmallRng::seed_from_u64(args.random_seed),
    };

    let time_limit = args.time_limit.map(Duration::from_millis);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::invalid_instance(args.instance_path.display()))?;

    match file_format {
        FileFormat::CnfDimacsPLine => {
            cnf_problem(learning_options, solver_options, time_limit, instance_path)?
        }
        FileFormat::WcnfDimacsPLine => wcnf_problem(
            learning_options,
            solver_options,
            time_limit,
            instance_path,
            args.upper_bound_encoding,
        )?,
        FileFormat::FlatZinc => flatzinc::solve(
            Solver::with_options(learning_options, solver_options),
            instance_path,
            time_limit,
            FlatZincOptions {
                free_search: args.free_search,
                all_solutions: args.all_solutions,
                cumulative_options: CumulativeOptions::new(
                    args.cumulative_allow_holes,
                    args.cumulative_explanation_type,
                    args.cumulative_generate_sequence,
                ),
            },
        )?,
    }

    Ok(())
}

fn cnf_problem(
    learning_options: LearningOptions,
    solver_options: SolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let mut solver = parse_cnf::<SolverDimacsSink>(
        instance_file,
        SolverArgs::new(learning_options, solver_options),
    )?;

    let mut termination =
        TimeBudget::starting_now(time_limit.unwrap_or(Duration::from_secs(u64::MAX)));
    let mut brancher = solver.default_brancher_over_all_propositional_variables();
    match solver.satisfy(&mut brancher, &mut termination) {
        SatisfactionResult::Satisfiable(solution) => {
            solver.log_statistics();
            println!("s SATISFIABLE");
            let num_propositional_variables = solution.num_propositional_variables();
            println!(
                "v {}",
                stringify_solution(&solution, num_propositional_variables, true)
            );
        }
        SatisfactionResult::Unsatisfiable => {
            solver.log_statistics();
            if solver.conclude_proof_unsat().is_err() {
                warn!("Failed to log solver conclusion");
            };

            println!("s UNSATISFIABLE");
        }
        SatisfactionResult::Unknown => {
            solver.log_statistics();
            println!("s UNKNOWN");
        }
    };

    Ok(())
}

fn stringify_solution(
    solution: &Solution,
    num_variables: usize,
    terminate_with_zero: bool,
) -> String {
    (1..num_variables)
        .map(|index| PropositionalVariable::new(index.try_into().unwrap()))
        .map(|var| {
            if solution.get_propositional_variable_value(var) {
                format!("{} ", var.get_index())
            } else {
                format!("-{} ", var.get_index())
            }
        })
        .chain(if terminate_with_zero {
            std::iter::once(String::from("0"))
        } else {
            std::iter::once(String::new())
        })
        .collect::<String>()
}

fn learned_clause_sorting_strategy_parser(s: &str) -> Result<LearnedClauseSortingStrategy, String> {
    match s {
        "lbd" => Ok(LearnedClauseSortingStrategy::Lbd),
        "activity" => Ok(LearnedClauseSortingStrategy::Activity),
        value => Err(format!(
            "'{value}' is not a valid learned clause sorting strategy. Possible values: ['lbd', 'activity']"
        )),
    }
}

fn upper_bound_encoding_parser(s: &str) -> Result<PseudoBooleanEncoding, String> {
    match s {
        "gte" => Ok(PseudoBooleanEncoding::GeneralizedTotalizer),
        "cne" => Ok(PseudoBooleanEncoding::CardinalityNetwork),
        value => Err(format!(
            "'{value}' is not a valid upper bound encoding. Possible values: ['gte', 'cne']."
        )),
    }
}

fn sequence_generator_parser(s: &str) -> Result<SequenceGeneratorType, String> {
    match s {
        "constant" => Ok(SequenceGeneratorType::Constant),
        "geometric" => Ok(SequenceGeneratorType::Geometric),
        "luby" => Ok(SequenceGeneratorType::Luby),
        value => Err(format!("'{value}' is not a valid sequence generator. Possible values: ['constant', 'geometric', 'luby'].")),
    }
}

fn cumulative_explanation_type_parser(s: &str) -> Result<CumulativeExplanationType, String> {
    match s {
        "naive" => Ok(CumulativeExplanationType::Naive),
        "big-step" => Ok(CumulativeExplanationType::BigStep),
        "pointwise" => Ok(CumulativeExplanationType::PointWise),
        value => Err(format!(
            "'{value}' is not a valid cumulative explanation type. Possible values: ['naive', 'big-step', 'pointwise']"
        )),
    }
}
