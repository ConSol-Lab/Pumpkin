mod checker;
mod parsers;
mod result;

use clap::Parser;
use log::{error, info, warn, LevelFilter};
use parsers::dimacs::{parse_cnf, parse_wcnf, CSPSolverArgs, SolverDimacsSink};
use pumpkin_lib::basic_types::sequence_generators::SequenceGeneratorType;
use pumpkin_lib::encoders::PseudoBooleanEncoding;
use pumpkin_lib::optimisation::{LinearSearch, OptimisationResult, OptimisationSolver};
use std::fmt::Debug;
use std::fs::{File, OpenOptions};
use std::path::Path;
use std::time::Duration;
use std::{io::Write, path::PathBuf};

use pumpkin_lib::basic_types::*;
use pumpkin_lib::engine::*;

use result::{PumpkinError, PumpkinResult};

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
    /// Learned clauses are kept based on the tied system introduced by Chanseok Oh
    #[arg(long = "learning-clause-threshold", default_value_t = 4000)]
    threshold_learned_clauses: u64,

    #[arg(long = "learning-lbd-threshold", default_value_t = 5)]
    lbd_threshold: u32,

    /// Decides which clauses will be removed when cleaning up the learned clauses.
    #[arg(short = 'l', long = "learning-sorting-strategy", value_parser = learned_clause_sorting_strategy_parser, default_value_t = LearnedClauseSortingStrategy::Activity.into())]
    learned_clause_sorting_strategy: CliArg<LearnedClauseSortingStrategy>,

    /// Decides the sequence based on which the restarts are performed.
    /// To be used in combination with "restarts-base-interval"
    #[arg(long = "restart-sequence-generator", value_parser = sequence_generator_parser, default_value_t = SequenceGeneratorType::Constant.into())]
    restart_sequence_generator_type: CliArg<SequenceGeneratorType>,

    /// The base interval length is used as a multiplier to the restart sequence.
    /// For example, constant restarts with base interval 100 means a retart is triggered every 100 conflicts.
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
    /// A restart is blocked if the number of assigned propositional variables is must greater than the average number of assigned variables in the recent past
    /// A greater/lower value for num-assigned-coef means fewer/more blocked restarts
    #[arg(long = "restart-num-assigned-coef", default_value_t = 1.4)]
    restart_num_assigned_coef: f64,

    /// Used to determine the length of the recent past that should be considered when deciding on blocking restarts (part of Glucose restarts).
    /// The solver considers the last num-assigned-window conflicts as the reference point for the number of assigned variables
    #[arg(long = "restart-num-assigned-window", default_value_t = 5000)]
    restart_num_assigned_window: u64,

    /// The coefficient in the geometric sequence x_i = x_{i-1} * geometric-coef, x_1 = "restarts-base-interval"
    /// Used only if "restarts-sequence-generator" is assigned to "geometric".
    #[arg(long = "restart-geometric-coef")]
    restart_geometric_coef: Option<f64>,

    /// The time budget for the solver, given in seconds.
    #[arg(short = 't', long = "time-limit")]
    time_limit: Option<u64>,

    /// The random seed to use for the PRNG. This influences the initial order of the variables.
    #[arg(long = "random-seed", default_value_t = -2)]
    random_seed: i64,

    /// Enables log message output from the solver
    #[arg(short = 'v', long = "verbose", default_value_t = false)]
    verbose: bool,

    /// If `--verbose` is enabled removes the timestamp information from the log messages
    #[arg(long = "omit-timestamp", default_value_t = false)]
    omit_timestamp: bool,

    /// If `--verbose` is enabled removes the call site information from the log messages.
    /// Call site is the file and line in it that originated the message.
    #[arg(long = "omit-call-site", default_value_t = false)]
    omit_call_site: bool,

    /// The encoding to use for the upper bound constraint in an optimisation problem.
    #[arg(long = "upper-bound-encoding", value_parser = upper_bound_encoding_parser, default_value_t = PseudoBooleanEncoding::GTE.into())]
    upper_bound_encoding: CliArg<PseudoBooleanEncoding>,

    /// Verify the reported solution is consistent with the instance, and, if applicable, verify
    /// that it evaluates to the reported objective value.
    #[arg(long = "verify", default_value_t = false)]
    verify_solution: bool,
}

fn configure_logging(
    verbose: bool,
    omit_timestamp: bool,
    omit_call_site: bool,
) -> std::io::Result<()> {
    let level_filter = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    env_logger::Builder::new()
        .format(move |buf, record| {
            write!(buf, "c ")?;
            if !omit_timestamp {
                write!(buf, "{} ", buf.timestamp())?;
            }
            write!(buf, "{} ", record.level())?;
            if !omit_call_site {
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

    configure_logging(args.verbose, args.omit_timestamp, args.omit_call_site)?;

    let file_format = match args.instance_path.extension().and_then(|ext| ext.to_str()) {
        Some("cnf") => FileFormat::CnfDimacsPLine,
        Some("wcnf") => FileFormat::WcnfDimacsPLine,
        _ => return Err(PumpkinError::InvalidInstanceFile),
    };

    let sat_options = SatOptions {
        num_high_lbd_learned_clauses_max: args.threshold_learned_clauses,
        high_lbd_learned_clause_sorting_strategy: args.learned_clause_sorting_strategy.inner,
        lbd_threshold: args.lbd_threshold,
        ..Default::default()
    };

    let certificate_file = if let Some(path_buf) = args.certificate_path {
        Some(
            OpenOptions::new()
                .create(true)
                .read(true)
                .write(true)
                .open(path_buf.as_path())?,
        )
    } else {
        None
    };

    let solver_options = SatisfactionSolverOptions {
        restart_sequence_generator_type: args.restart_sequence_generator_type.inner,
        restart_base_interval: args.restart_base_interval,
        restart_min_num_conflicts_before_first_restart: args
            .restarts_min_num_conflicts_before_first_restart,
        restart_lbd_coef: args.restart_lbd_coef,
        restart_num_assigned_coef: args.restart_num_assigned_coef,
        restart_num_assigned_window: args.restart_num_assigned_window,
        restart_geometric_coef: args.restart_geometric_coef,
        certificate_file,
    };

    let time_limit = args.time_limit.map(Duration::from_secs);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::InvalidInstanceFile)?;
    let verify_outcome = args.verify_solution;

    match file_format {
        FileFormat::CnfDimacsPLine => cnf_problem(
            sat_options,
            solver_options,
            time_limit,
            instance_path,
            verify_outcome,
        ),
        FileFormat::WcnfDimacsPLine => wcnf_problem(
            sat_options,
            solver_options,
            time_limit,
            instance_path,
            args.upper_bound_encoding.inner,
            verify_outcome,
        ),
        FileFormat::MaxSAT2022 => todo!(),
    }
}

fn wcnf_problem(
    sat_options: SatOptions,
    solver_options: SatisfactionSolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    upper_bound_encoding: PseudoBooleanEncoding,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let (csp_solver, objective_function) = parse_wcnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(sat_options, solver_options),
    )?;

    let mut solver = OptimisationSolver::new(
        csp_solver,
        objective_function,
        LinearSearch::new(upper_bound_encoding),
    );

    let result = match solver.solve(time_limit) {
        OptimisationResult::Optimal {
            solution,
            objective_value,
        } => {
            println!("s OPTIMAL");
            println!("v {}", stringify_solution(&solution));
            Some((solution, objective_value))
        }
        OptimisationResult::Satisfiable {
            best_solution,
            objective_value,
        } => {
            println!("s SATISFIABLE");
            println!("v {}", stringify_solution(&best_solution));
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
    sat_options: SatOptions,
    solver_options: SatisfactionSolverOptions,
    time_limit: Option<Duration>,
    instance_path: impl AsRef<Path>,
    verify: bool,
) -> Result<(), PumpkinError> {
    let instance_file = File::open(instance_path)?;
    let mut csp_solver = parse_cnf::<SolverDimacsSink>(
        instance_file,
        CSPSolverArgs::new(sat_options, solver_options),
    )?;

    let solution = match csp_solver.solve(time_limit_in_secs(time_limit)) {
        CSPSolverExecutionFlag::Feasible => {
            let solution = Solution::new(
                csp_solver.get_propositional_assignments(),
                csp_solver.get_integer_assignments(),
            );

            println!("s SATISFIABLE");
            println!("v {}", stringify_solution(&solution));

            Some(solution)
        }
        CSPSolverExecutionFlag::Infeasible => {
            println!("s UNSATISFIABLE");
            None
        }
        CSPSolverExecutionFlag::InfeasibleUnderAssumptions => {
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

fn time_limit_in_secs(time_limit: Option<Duration>) -> i64 {
    time_limit
        .map(|limit| limit.as_secs() as i64)
        .unwrap_or(i64::MAX)
}

fn stringify_solution(solution: &Solution) -> String {
    (1..solution.num_propositional_variables())
        .map(|index| PropositionalVariable::new(index.try_into().unwrap()))
        .map(|var| {
            if solution[var] {
                format!("{} ", var.index())
            } else {
                format!("-{} ", var.index())
            }
        })
        .chain(std::iter::once("0".to_string()))
        .collect::<String>()
}

fn learned_clause_sorting_strategy_parser(
    s: &str,
) -> Result<CliArg<LearnedClauseSortingStrategy>, String> {
    match s {
        "lbd" => Ok(LearnedClauseSortingStrategy::LBD.into()),
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
