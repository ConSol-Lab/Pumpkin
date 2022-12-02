#![allow(dead_code)] //turn off dead code warnings for now, later to be removed

mod arguments;
mod basic_types;
mod encoders;
mod engine;
mod propagators;
mod pumpkin_asserts;

use basic_types::*;
use engine::*;

fn debug_check_feasibility_and_objective_value(
    file_location: &str,
    file_format: FileFormat,
    solution: &Solution,
    reported_objective_value: u64,
) {
    let mut instance = Instance::new();
    instance.read_file(file_location, file_format);

    if instance.are_hard_clauses_violated(solution) {
        panic!("Solution not ok: Hard clauses violated!");
    }

    let recomputed_objective_value = instance.compute_soft_clause_violation(solution);
    match recomputed_objective_value.cmp(&reported_objective_value) {
        std::cmp::Ordering::Less => println!("Reported objective value is greater than the actual cost. In older versions this was fine, but not sure in new version."),
        std::cmp::Ordering::Equal => {} //this is okay
        std::cmp::Ordering::Greater => {
            panic!("Solution not ok: reported objective value is lower than the actual value!")
        }
    }

    println!("No critical issues found after checking the solution.");
    println!("Objective cost: {}", recomputed_objective_value);
}

fn main() {
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

    let file_location = argument_handler.get_string_argument("file-location");

    println!("File location: {file_location}");

    if file_location.is_empty() {
        println!("No file location given. Aborting.");
        std::process::abort();
    }

    let file_format = if file_location.ends_with(".cnf") {
        FileFormat::CnfDimacsPLine
    } else if file_location.ends_with(".wcnf") {
        FileFormat::WcnfDimacsPLine
    } else {
        panic!("Unknown file format!")
    };

    let mut pumpkin = Pumpkin::new(&argument_handler);
    pumpkin.read_file(file_location.as_str(), file_format);
    pumpkin.reset_variable_selection(argument_handler.get_integer_argument("random-seed"));

    let pumpkin_output = pumpkin.solve();

    print!("Pumpkin output: ");

    match pumpkin_output {
        PumpkinExecutionFlag::Optimal {
            optimal_solution,
            objective_value,
        } => {
            println!("Optimal");
            debug_check_feasibility_and_objective_value(
                file_location.as_str(),
                file_format,
                &optimal_solution,
                objective_value,
            );
        }
        PumpkinExecutionFlag::Feasible {
            feasible_solution,
            objective_value,
        } => {
            println!("Feasible");
            debug_check_feasibility_and_objective_value(
                file_location.as_str(),
                file_format,
                &feasible_solution,
                objective_value,
            );
        }
        PumpkinExecutionFlag::Infeasible => println!("Infeasible"),
        PumpkinExecutionFlag::Timeout => println!("Timeout"),
    }
}
