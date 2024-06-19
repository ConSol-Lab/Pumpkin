mod checker;
mod flatzinc;
mod parsers;
mod result;

use std::fmt::Debug;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use log::error;
use log::info;
use log::warn;
use log::Level;
use log::LevelFilter;
use parsers::dimacs::parse_cnf;
use parsers::dimacs::parse_wcnf;
use parsers::dimacs::CSPSolverArgs;
use parsers::dimacs::SolverDimacsSink;
use parsers::dimacs::WcnfInstance;
use pumpkin_lib::basic_types::sequence_generators::SequenceGeneratorType;
use pumpkin_lib::basic_types::signal_handling::signal_handler;
use pumpkin_lib::basic_types::statistic_logging::statistic_logger;
use pumpkin_lib::basic_types::*;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::encoders::PseudoBooleanEncoding;
use pumpkin_lib::engine::termination::time_budget::TimeBudget;
use pumpkin_lib::engine::variables::PropositionalVariable;
use pumpkin_lib::engine::RestartOptions;
use pumpkin_lib::engine::*;
use pumpkin_lib::optimisation::LinearSearch;
use pumpkin_lib::optimisation::OptimisationResult;
use pumpkin_lib::optimisation::OptimisationSolver;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use result::PumpkinError;
use result::PumpkinResult;

use crate::flatzinc::FlatZincOptions;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The instance to solve. The file should have one of the following extensions:
    ///  * '.cnf' for SAT instances, given in the DIMACS format,
    ///  * '.wcnf' for MaxSAT instances, given in the WDIMACS format.
    instance_path: PathBuf,

    /// The output path for the DRAT certificate file. By default does not output any
    /// certifying information.
    #[arg(long)]
    certificate_path: Option<PathBuf>,

    /// The number of high lbd learned clauses that are kept in the database.
    /// Learned clauses are kept based on the tiered system introduced by Chanseok Oh
    #[arg(long = "learning-max-num-clauses", default_value_t = 4000)]
    learning_max_num_clauses: u64,

    /// Learned clauses with this threshold LBD or lower are kept permanently
    /// Learned clauses are kept based on the tiered system introduced by Chanseok Oh
    #[arg(long = "learning-lbd-threshold", default_value_t = 5)]
    learning_lbd_threshold: u32,

    /// Decides which clauses will be removed when cleaning up the learned clauses.
    #[arg(
        short = 'l',
        long = "learning-sorting-strategy",
        value_parser = learned_clause_sorting_strategy_parser,
        default_value_t = LearnedClauseSortingStrategy::Activity.into()
    )]
    learning_sorting_strategy: CliArg<LearnedClauseSortingStrategy>,

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

    /// The encoding to use for the upper bound constraint in an optimisation problem.
    #[arg(
        long = "upper-bound-encoding",
        value_parser = upper_bound_encoding_parser,
        default_value_t = PseudoBooleanEncoding::GTE.into()
    )]
    upper_bound_encoding: CliArg<PseudoBooleanEncoding>,

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
        FileFormat::CnfDimacsPLine | FileFormat::WcnfDimacsPLine | FileFormat::MaxSAT2022 => {
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
    statistic_logger::configure(log_statistics, "%%%mzn-stat:", Some("%%%mzn-stat-end"));
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
    statistic_logger::configure(log_statistics, "c STAT", None);
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

    // Register the handling of signals (for example CTRL+C)
    signal_handler::register_signals()?;

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
        high_lbd_learned_clause_sorting_strategy: args.learning_sorting_strategy.inner,
        lbd_threshold: args.learning_lbd_threshold,
        ..Default::default()
    };

    let certificate_file = if let Some(path_buf) = args.certificate_path {
        Some(
            OpenOptions::new()
                .create(true)
                .truncate(true)
                .read(true)
                .write(true)
                .open(path_buf.as_path())?,
        )
    } else {
        None
    };

    let solver_options = SatisfactionSolverOptions {
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
        certificate_file,
        learning_clause_minimisation: args.learning_clause_minimisation.inner,
        random_generator: SmallRng::seed_from_u64(args.random_seed),
    };

    let time_limit = args.time_limit.map(Duration::from_millis);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::invalid_instance(args.instance_path.display()))?;
    let verify_outcome = args.verify_solution;

    match file_format {
        FileFormat::CnfDimacsPLine => cnf_problem(
            learning_options,
            solver_options,
            time_limit,
            instance_path,
            verify_outcome,
        )?,
        FileFormat::WcnfDimacsPLine => wcnf_problem(
            learning_options,
            solver_options,
            time_limit,
            instance_path,
            args.upper_bound_encoding.inner,
            verify_outcome,
        )?,
        FileFormat::MaxSAT2022 => todo!(),
        FileFormat::FlatZinc => flatzinc::solve(
            ConstraintSatisfactionSolver::new(learning_options, solver_options),
            instance_path,
            time_limit,
            FlatZincOptions {
                free_search: args.free_search,
                all_solutions: args.all_solutions,
            },
        )?,
    }

    Ok(())
}

fn wcnf_problem(
    learning_options: LearningOptions,
    solver_options: SatisfactionSolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    upper_bound_encoding: PseudoBooleanEncoding,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let WcnfInstance {
        formula: csp_solver,
        objective: objective_function,
        last_instance_variable,
    } = parse_wcnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(learning_options, solver_options),
    )?;

    let brancher =
        IndependentVariableValueBrancher::default_over_all_propositional_variables(&csp_solver);

    let mut solver = OptimisationSolver::new(
        csp_solver,
        objective_function,
        LinearSearch::new(upper_bound_encoding),
    );

    let mut termination = time_limit.map(TimeBudget::starting_now);

    let result = match solver.solve(&mut termination, brancher) {
        OptimisationResult::Optimal {
            solution,
            objective_value,
        } => {
            println!("s OPTIMAL");
            println!(
                "v {}",
                stringify_solution(&solution, last_instance_variable + 1, false)
            );
            Some((solution, objective_value))
        }
        OptimisationResult::Satisfiable {
            best_solution,
            objective_value,
        } => {
            println!("s SATISFIABLE");
            println!(
                "v {}",
                stringify_solution(&best_solution, last_instance_variable + 1, false)
            );
            Some((best_solution, objective_value))
        }
        OptimisationResult::Infeasible => {
            println!("s UNSATISFIABLE");
            None
        }
        OptimisationResult::Unknown => {
            println!("s UNKNOWN");
            None
        }
    };

    if verify {
        if let Some((_solution, _objective)) = result {
            // checker::verify_wcnf_solution(instance_path, &solution, objective)?;
        }
    }

    Ok(())
}

fn cnf_problem(
    learning_options: LearningOptions,
    solver_options: SatisfactionSolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let mut csp_solver = parse_cnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(learning_options, solver_options),
    )?;

    let mut termination = time_limit.map(TimeBudget::starting_now);
    let mut brancher =
        IndependentVariableValueBrancher::default_over_all_propositional_variables(&csp_solver);
    let solution = match csp_solver.solve(&mut termination, &mut brancher) {
        CSPSolverExecutionFlag::Feasible => {
            #[allow(deprecated)]
            let solution = csp_solver.get_solution_reference();

            println!("s SATISFIABLE");
            let num_propositional_variables = solution.num_propositional_variables();
            println!(
                "v {}",
                stringify_solution(&solution.into(), num_propositional_variables, true)
            );

            Some(solution)
        }
        CSPSolverExecutionFlag::Infeasible => {
            println!("s UNSATISFIABLE");
            None
        }
        CSPSolverExecutionFlag::Timeout => {
            println!("s UNKNOWN");
            None
        }
    };

    if verify {
        if let Some(_solution) = solution {
            // checker::verify_cnf_solution(instance_path, &solution)?;
        }
    }

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
                format!("{} ", var.index())
            } else {
                format!("-{} ", var.index())
            }
        })
        .chain(if terminate_with_zero {
            std::iter::once(String::from("0"))
        } else {
            std::iter::once(String::new())
        })
        .collect::<String>()
}

fn learned_clause_sorting_strategy_parser(
    s: &str,
) -> Result<CliArg<LearnedClauseSortingStrategy>, String> {
    match s {
        "lbd" => Ok(LearnedClauseSortingStrategy::Lbd.into()),
        "activity" => Ok(LearnedClauseSortingStrategy::Activity.into()),
        value => Err(format!(
            "'{value}' is not a valid learned clause sorting strategy"
        )),
    }
}

fn upper_bound_encoding_parser(s: &str) -> Result<CliArg<PseudoBooleanEncoding>, String> {
    match s {
        "gte" => Ok(PseudoBooleanEncoding::GTE.into()),
        "cne" => Ok(PseudoBooleanEncoding::CNE.into()),
        value => Err(format!("'{value}' is not a valid upper bound encoding.")),
    }
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

impl std::fmt::Display for CliArg<LearnedClauseSortingStrategy> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self.inner, f)
    }
}

impl std::fmt::Display for CliArg<PseudoBooleanEncoding> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.inner, f)
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
