mod checker;
mod parsers;

use clap::Parser;
use log::{error, info, warn, LevelFilter};
use parsers::dimacs::{parse_cnf, parse_wcnf};
use pumpkin_lib::encoders::PseudoBooleanEncoding;
use pumpkin_lib::optimisation::{LinearSearch, OptimisationResult, OptimisationSolver};
use std::fs::OpenOptions;
use std::time::Duration;
use std::{io::Write, path::PathBuf};

use pumpkin_lib::basic_types::*;
use pumpkin_lib::engine::*;
use pumpkin_lib::result::{PumpkinError, PumpkinResult};

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

    /// The number of learned clauses that can be added to the clause database before clause
    /// deletion is triggered. This number could be exceeded temporarily but occasionally the
    /// solver will delete learned clauses.
    #[arg(long = "threshold-learned-clauses", default_value_t = 4000)]
    threshold_learned_clauses: u64,

    /// Decides which clauses will be removed when cleaning up the learned clauses.
    #[arg(short = 'l', long = "learned-clause-sorting-strategy", value_parser = learned_clause_sorting_strategy_parser, default_value_t = LearnedClauseSortingStrategy::Lbd.into())]
    learned_clause_sorting_strategy: CliArg<LearnedClauseSortingStrategy>,

    /// The number of conflicts before a restart is triggered. This is a fixed-length restart
    /// strategy.
    #[arg(long = "conflicts-per-restart", default_value_t = 4000)]
    conflicts_per_restart: i64,

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
        Err(e) => error!("Execution failed, error: {}", e),
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

    let sat_options = SATDataStructuresInternalParameters {
        num_learned_clauses_max: args.threshold_learned_clauses,
        learned_clause_sorting_strategy: args.learned_clause_sorting_strategy.inner,
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
        conflicts_per_restart: args.conflicts_per_restart,
        certificate_file,
    };

    let solver = ConstraintSatisfactionSolver::new(sat_options, solver_options);
    let time_limit = args.time_limit.map(Duration::from_secs);
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::InvalidInstanceFile)?;
    let verify_outcome = args.verify_solution;

    match file_format {
        FileFormat::CnfDimacsPLine => {
            cnf_problem(solver, time_limit, instance_path, verify_outcome)
        }
        FileFormat::WcnfDimacsPLine => wcnf_problem(
            solver,
            time_limit,
            instance_path,
            args.upper_bound_encoding.inner,
            verify_outcome,
        ),
        FileFormat::MaxSAT2022 => todo!(),
    }
}

fn wcnf_problem(
    mut csp_solver: ConstraintSatisfactionSolver,
    time_limit: Option<Duration>,
    instance_path: &str,
    upper_bound_encoding: PseudoBooleanEncoding,
    verify: bool,
) -> Result<(), PumpkinError> {
    let objective_function = parse_wcnf(instance_path, &mut csp_solver)?;
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
        if let Some((solution, objective)) = result {
            checker::verify_wcnf_solution(instance_path, &solution, objective)?;
        }
    }

    Ok(())
}

fn cnf_problem(
    mut csp_solver: ConstraintSatisfactionSolver,
    time_limit: Option<Duration>,
    instance_path: &str,
    verify: bool,
) -> Result<(), PumpkinError> {
    parse_cnf(instance_path, &mut csp_solver)?;

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
        if let Some(solution) = solution {
            checker::verify_cnf_solution(instance_path, &solution)?;
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
    (0..solution.num_propositional_variables())
        .map(|index| PropositionalVariable::new(index.try_into().unwrap()))
        .map(|var| {
            if solution[var] {
                format!("{} ", var.index())
            } else {
                format!("-{} ", var.index())
            }
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
        match self.inner {
            LearnedClauseSortingStrategy::Lbd => write!(f, "lbd"),
            LearnedClauseSortingStrategy::Activity => write!(f, "activity"),
        }
    }
}

impl std::fmt::Display for CliArg<PseudoBooleanEncoding> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner {
            PseudoBooleanEncoding::GTE => write!(f, "gte"),
            PseudoBooleanEncoding::CNE => write!(f, "cne"),
        }
    }
}
