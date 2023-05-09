#![allow(dead_code)] //turn off dead code warnings for now, later to be removed

mod basic_types;
mod encoders;
mod engine;
mod propagators;
mod pumpkin_asserts;
mod result;

use basic_types::*;
use clap::Parser;
use engine::*;
use log::{error, info, warn, LevelFilter};
use std::fs::OpenOptions;
use std::time::Duration;
use std::{io::Write, path::PathBuf};

use crate::result::PumpkinError;
use result::PumpkinResult;

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
    #[arg(short = 'l', long = "learned-clause-sorting-strategy", default_value_t = LearnedClauseSortingStrategy::Lbd, value_parser = learned_clause_sorting_strategy_parser)]
    learned_clause_sorting_strategy: LearnedClauseSortingStrategy,

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
    #[arg(long = "upper-bound-encoding", default_value_t = UpperBoundEncoding::GTE, value_parser = upper_bound_encoding_parser)]
    upper_bound_encoding: UpperBoundEncoding,
}

fn debug_check_feasibility_and_objective_value(
    file_location: &str,
    file_format: FileFormat,
    solution: &Solution,
    reported_objective_value: u64,
) -> PumpkinResult<()> {
    let mut instance = Instance::new();
    instance.read_file(file_location, file_format)?;

    if instance.are_hard_clauses_violated(solution) {
        return Err(PumpkinError::InconsistentSolution);
    }

    let recomputed_objective_value = instance.compute_soft_clause_violation(solution);
    match recomputed_objective_value.cmp(&reported_objective_value) {
        std::cmp::Ordering::Less => warn!("Reported objective value is greater than the actual cost. In older versions this was fine, but not sure in new version."),
        std::cmp::Ordering::Equal => {} //this is okay
        std::cmp::Ordering::Greater => {
            return Err(PumpkinError::InconsistentObjective);
        }
    }

    info!("No critical issues found after checking the solution.");
    info!("Objective cost: {}", recomputed_objective_value);
    Ok(())
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
    pumpkin_asserts::print_pumpkin_assert_warning_message!();

    let args = Args::parse();

    configure_logging(args.verbose, args.omit_timestamp, args.omit_call_site)?;

    let file_format = match args.instance_path.extension().and_then(|ext| ext.to_str()) {
        Some("cnf") => FileFormat::CnfDimacsPLine,
        Some("wcnf") => FileFormat::WcnfDimacsPLine,
        _ => return Err(PumpkinError::InvalidInstanceFile),
    };

    let sat_options = SATDataStructuresInternalParameters {
        num_learned_clauses_max: args.threshold_learned_clauses,
        learned_clause_sorting_strategy: args.learned_clause_sorting_strategy,
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

    let mut pumpkin = Pumpkin::new(
        sat_options,
        solver_options,
        args.upper_bound_encoding,
        args.time_limit.map(Duration::from_secs),
    );
    let path = args
        .instance_path
        .to_str()
        .ok_or(PumpkinError::InvalidInstanceFile)?;
    pumpkin.read_file(path, file_format)?;
    pumpkin.reset_variable_selection(args.random_seed);

    let pumpkin_output = pumpkin.solve();

    match pumpkin_output {
        PumpkinExecutionFlag::Feasible {
            ref feasible_solution,
            objective_value,
        } => {
            println!("s SATISFIABLE");
            println!("v {}", stringify_solution(feasible_solution));
            debug_check_feasibility_and_objective_value(
                path,
                file_format,
                feasible_solution,
                objective_value,
            )?;
        }
        PumpkinExecutionFlag::Optimal {
            ref optimal_solution,
            objective_value,
        } => {
            println!("s OPTIMAL");
            println!("o {}", objective_value);
            println!("v {}", stringify_solution(optimal_solution));
            debug_check_feasibility_and_objective_value(
                path,
                file_format,
                optimal_solution,
                objective_value,
            )?;
        }
        PumpkinExecutionFlag::Infeasible => println!("s UNSATISFIABLE"),
        PumpkinExecutionFlag::Timeout => println!("s UNKNOWN"),
    }
    Ok(())
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

fn learned_clause_sorting_strategy_parser(s: &str) -> Result<LearnedClauseSortingStrategy, String> {
    match s {
        "lbd" => Ok(LearnedClauseSortingStrategy::Lbd),
        "activity" => Ok(LearnedClauseSortingStrategy::Activity),
        value => Err(format!(
            "'{value}' is not a valid learned clause sorting strategy"
        )),
    }
}

fn upper_bound_encoding_parser(s: &str) -> Result<UpperBoundEncoding, String> {
    match s {
        "gte" => Ok(UpperBoundEncoding::GTE),
        "cne" => Ok(UpperBoundEncoding::CNE),
        value => Err(format!("'{value}' is not a valid upper bound encoding.")),
    }
}

impl std::fmt::Display for LearnedClauseSortingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LearnedClauseSortingStrategy::Lbd => write!(f, "lbd"),
            LearnedClauseSortingStrategy::Activity => write!(f, "activity"),
        }
    }
}

impl std::fmt::Display for UpperBoundEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UpperBoundEncoding::GTE => write!(f, "gte"),
            UpperBoundEncoding::CNE => write!(f, "cne"),
        }
    }
}
