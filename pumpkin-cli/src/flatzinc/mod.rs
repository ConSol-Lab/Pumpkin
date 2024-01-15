mod compiler;
mod error;
mod instance;
mod parser;

use std::fmt::Write;
use std::fs::File;
use std::path::Path;

pub use error::*;
use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use self::instance::FlatZincInstance;
use self::instance::VariableMap;
use crate::flatzinc::instance::OutputVariable;
use crate::flatzinc::instance::Variable;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

pub fn solve(
    mut solver: ConstraintSatisfactionSolver,
    instance: impl AsRef<Path>,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let instance = parser::parse(instance)?;
    let variable_map = compiler::compile(&instance, &mut solver)?;

    match solver.solve(i64::MAX) {
        CSPSolverExecutionFlag::Feasible => print_solution(&solver, &instance, &variable_map),
        CSPSolverExecutionFlag::Infeasible => println!("{MSG_UNSATISFIABLE}"),
        CSPSolverExecutionFlag::Timeout => println!("{MSG_UNKNOWN}"),
    }

    Ok(())
}

fn print_solution(
    solver: &ConstraintSatisfactionSolver,
    instance: &FlatZincInstance,
    variable_map: &VariableMap,
) {
    for id in instance.iter_output_variables() {
        match id {
            OutputVariable::Variable(id) => {
                let variable = variable_map.resolve(&id).expect("existing variable");

                match variable {
                    Variable::Integer(domain_id) => {
                        let value = solver.get_integer_assignments().get_lower_bound(domain_id);
                        println!("{id} = {value};");
                    }
                    Variable::Bool(literal) => {
                        let value = solver
                            .get_propositional_assignments()
                            .is_literal_assigned_true(literal);
                        println!("{id} = {value};");
                    }
                }
            }
            OutputVariable::VariableArray(id) => {
                let mut buf = String::new();
                let value_ids = instance
                    .resolve_variable_array(&id)
                    .expect("existing variable array");

                for (idx, var_id) in value_ids.iter().enumerate() {
                    let var = variable_map.resolve(var_id).expect("existing variable");
                    match var {
                        Variable::Integer(domain_id) => {
                            let value = solver.get_integer_assignments().get_lower_bound(domain_id);
                            write!(buf, "{value}").unwrap();
                        }
                        Variable::Bool(literal) => {
                            let value = solver
                                .get_propositional_assignments()
                                .is_literal_assigned_true(literal);
                            write!(buf, "{value}").unwrap();
                        }
                    }

                    if idx < value_ids.len() - 1 {
                        write!(buf, ", ").unwrap();
                    }
                }

                println!("{id} = array1d(1..{}, [{buf}]);", value_ids.len());
            }
        }
    }

    println!("----------");
}
