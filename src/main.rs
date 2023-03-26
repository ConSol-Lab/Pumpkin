#![allow(dead_code)] //turn off dead code warnings for now, later to be removed

mod arguments;
mod basic_types;
mod encoders;
mod engine;
mod propagators;
mod pumpkin_asserts;
mod result;

use basic_types::*;
use engine::*;
use log::{error, info, warn, LevelFilter};

use crate::result::PumpkinError::{
    FileReadingError, InconsistentObjective, InconsistentSolution, MissingFileError,
};
use crate::result::PumpkinResult;
use std::io::Write;

fn debug_check_feasibility_and_objective_value(
    file_location: &str,
    file_format: FileFormat,
    solution: &Solution,
    reported_objective_value: u64,
) -> PumpkinResult<()> {
    let mut instance = Instance::new();
    instance.read_file(file_location, file_format)?;

    if instance.are_hard_clauses_violated(solution) {
        return Err(InconsistentSolution);
    }

    let recomputed_objective_value = instance.compute_soft_clause_violation(solution);
    match recomputed_objective_value.cmp(&reported_objective_value) {
        std::cmp::Ordering::Less => warn!("Reported objective value is greater than the actual cost. In older versions this was fine, but not sure in new version."),
        std::cmp::Ordering::Equal => {} //this is okay
        std::cmp::Ordering::Greater => {
            return Err(InconsistentObjective);
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

    let mut argument_handler = Pumpkin::create_argument_handler();

    //argument_handler.set_string_argument("file-location", "instances\\v100c500.cnf");
    argument_handler.set_string_argument("file-location", "instances\\BrazilInstance1.xml.wcnf");
    //argument_handler.set_string_argument("file-location", "instances\\ItalyInstance1.xml.wcnf");

    //argument_handler.print_if_empty_arguments_and_exit();

    argument_handler.print_help_summary_if_needed_and_exit();

    argument_handler.parse_command_line_arguments();

    //argument_handler.print_argument_values();
    argument_handler.print_arguments_different_from_default();

    configure_logging(
        argument_handler.get_bool_argument("verbose"),
        argument_handler.get_bool_argument("omit-timestamp"),
        argument_handler.get_bool_argument("omit-call-site"),
    )?;

    let file_location = argument_handler.get_string_argument("file-location");

    info!("File location: {file_location}");

    if file_location.is_empty() {
        return Err(MissingFileError);
    }

    let file_format = if file_location.ends_with(".cnf") {
        FileFormat::CnfDimacsPLine
    } else if file_location.ends_with(".wcnf") {
        FileFormat::WcnfDimacsPLine
    } else {
        return Err(MissingFileError);
    };

    let mut pumpkin = Pumpkin::new(&argument_handler);
    if let Err(e) = pumpkin.read_file(file_location.as_str(), file_format) {
        return Err(FileReadingError(e, file_location));
    }
    pumpkin.reset_variable_selection(argument_handler.get_integer_argument("random-seed"));

    let pumpkin_output = pumpkin.solve();

    match pumpkin_output {
        PumpkinExecutionFlag::Feasible {
            ref feasible_solution,
            objective_value,
        } => {
            println!("s SATISFIABLE");
            println!("v {}", stringify_solution(feasible_solution));
            debug_check_feasibility_and_objective_value(
                file_location.as_str(),
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
                file_location.as_str(),
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
