#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
mod cumulative_checker_tests;
mod cumulative_conflict_tests;
mod cumulative_propagation_tests;

use std::rc::Rc;

use implementation::propagators::cumulative::CumulativeConstructor;
use implementation::propagators::cumulative::Task;
use pumpkin_checking::AtomicConstraint;
use pumpkin_core::TestSolver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorId;
use pumpkin_core::variables::DomainId;

use crate::CheckerError;
use crate::Propagator;
use crate::propagators::ProofTestRunner;
use crate::propagators::model::Cumulative;
use crate::propagators::model::Fact;
use crate::propagators::model::Model;

pub fn set_up_cumulative_state(
    task_info: &[((i32, i32), u32, u32)],
    capacity: u32,
    conflict_detection_only: bool,
) -> (TestSolver, Result<PropagatorId, Conflict>, Vec<DomainId>) {
    let mut solver = TestSolver::default();

    let mut start_times = Vec::default();
    let mut tasks = Vec::default();

    for ((lb, ub), duration, resource_usage) in task_info {
        let start_time = solver.new_variable(*lb, *ub);
        start_times.push(start_time);
        tasks.push(Task {
            start_time,
            duration: *duration,
            resource_usage: *resource_usage,
        })
    }
    let constraint_tag = solver.new_constraint_tag();

    let result = solver.new_propagator(CumulativeConstructor {
        tasks: tasks.into(),
        capacity,
        constraint_tag,
        conflict_detection_only,
    });
    (solver, result, start_times)
}

pub(crate) fn add_cumulative_propagator(
    solver: &mut TestSolver,
    variables: &HashMap<Rc<str>, DomainId>,
    cumulative: &Cumulative,
    conflict_detection_only: bool,
) -> Result<PropagatorId, Conflict> {
    let constraint_tag = solver.new_constraint_tag();
    let tasks = cumulative
        .tasks
        .iter()
        .map(|task| {
            let var = match &task.start_time.0 {
                fzn_rs::VariableExpr::Identifier(ident) => *variables.get(ident).unwrap(),
                fzn_rs::VariableExpr::Constant(constant) => {
                    solver.new_variable(*constant, *constant)
                }
            };
            Task {
                start_time: var,
                duration: task.duration.try_into().unwrap(),
                resource_usage: task.resource_usage.try_into().unwrap(),
            }
        })
        .collect::<Vec<_>>()
        .into();
    solver.new_propagator(CumulativeConstructor {
        tasks,
        capacity: cumulative.capacity.try_into().unwrap(),
        constraint_tag,
        conflict_detection_only,
    })
}

pub(crate) fn recreate_propagation_cumulative<'a>(
    instance: &'a str,
    cumulative: &Cumulative,
    fact: &Fact,
    model: &Model,
) -> Result<(), CheckerError<'a>> {
    assert!(fact.consequent.is_some());
    let (mut solver, variables) = ProofTestRunner::create_solver_for_fact(fact, model);

    let result = add_cumulative_propagator(&mut solver, &variables, cumulative, false);

    let var = variables
        .get(&fact.consequent.as_ref().unwrap().identifier())
        .expect("Expected variable to exist");
    let consequent = ProofTestRunner::atomic_to_predicate(fact.consequent.as_ref().unwrap(), var);

    if result.is_ok() && solver.is_predicate_satisfied(consequent) {
        // We have been able to reproduce the conflict
        Ok(())
    } else {
        Err(CheckerError::PropagationCouldNotBeReproduced {
            fact: fact.clone(),
            instance,
            propagator: Propagator::Cumulative,
        })
    }
}

pub(crate) fn recreate_conflict_cumulative<'a>(
    instance: &'a str,
    cumulative: &Cumulative,
    fact: &Fact,
    model: &Model,
) -> Result<(), CheckerError<'a>> {
    assert!(fact.consequent.is_none());
    let (mut solver, variables) = ProofTestRunner::create_solver_for_fact(fact, model);

    let result = add_cumulative_propagator(&mut solver, &variables, cumulative, true);

    if result.is_err() {
        // We have been able to reproduce the conflict
        Ok(())
    } else {
        Err(CheckerError::ConflictCouldNotBeReproduced {
            fact: fact.clone(),
            instance,
            propagator: Propagator::Cumulative,
        })
    }
}
