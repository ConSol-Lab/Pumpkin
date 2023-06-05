use std::cmp::Ordering;

use pumpkin_lib::{
    basic_types::{FileFormat, Instance, Solution},
    result::{PumpkinError, PumpkinResult},
};

pub fn verify_cnf_solution(file_location: &str, solution: &Solution) -> PumpkinResult<()> {
    let mut instance = Instance::default();
    instance.read_file(file_location, FileFormat::CnfDimacsPLine)?;

    if instance.are_hard_clauses_violated(solution) {
        Err(PumpkinError::InconsistentSolution)
    } else {
        Ok(())
    }
}

pub fn verify_wcnf_solution(
    file_location: &str,
    solution: &Solution,
    reported_objective_value: u64,
) -> PumpkinResult<()> {
    let mut instance = Instance::default();
    instance.read_file(file_location, FileFormat::WcnfDimacsPLine)?;

    if instance.are_hard_clauses_violated(solution) {
        return Err(PumpkinError::InconsistentSolution);
    }

    let recomputed_objective_value = instance.compute_soft_clause_violation(solution);
    match recomputed_objective_value.cmp(&reported_objective_value) {
        Ordering::Greater | Ordering::Less => return Err(PumpkinError::InconsistentObjective),
        Ordering::Equal => {}
    }

    Ok(())
}
