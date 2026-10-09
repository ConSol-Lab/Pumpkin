use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::state::State;

use super::DisjunctiveDescription;
use crate::disjunctive::ArgDisjunctiveTask;
use crate::disjunctive::DisjunctiveConstructor;
use crate::fixed_domains;

#[test]
fn propagator_propagates_lower_bound() {
    let mut state = State::default();
    let c = state.new_interval_variable(4, 26, None);
    let d = state.new_interval_variable(13, 13, None);
    let e = state.new_interval_variable(5, 10, None);
    let f = state.new_interval_variable(5, 10, None);

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(DisjunctiveConstructor::new(
        [
            ArgDisjunctiveTask {
                start_time: c,
                processing_time: 4,
            },
            ArgDisjunctiveTask {
                start_time: d,
                processing_time: 5,
            },
            ArgDisjunctiveTask {
                start_time: e,
                processing_time: 3,
            },
            ArgDisjunctiveTask {
                start_time: f,
                processing_time: 3,
            },
        ],
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    assert_eq!(state.lower_bound(c), 18);
}

#[test]
fn disjunctive_is_violated_by_overlapping_tasks() {
    let (variables, domains) = fixed_domains(&[0, 2]);
    let description = DisjunctiveDescription {
        tasks: vec![
            ArgDisjunctiveTask {
                start_time: variables[0],
                processing_time: 3,
            },
            ArgDisjunctiveTask {
                start_time: variables[1],
                processing_time: 1,
            },
        ],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn disjunctive_allows_a_task_to_start_when_another_ends() {
    let (variables, domains) = fixed_domains(&[0, 3]);
    let description = DisjunctiveDescription {
        tasks: vec![
            ArgDisjunctiveTask {
                start_time: variables[0],
                processing_time: 3,
            },
            ArgDisjunctiveTask {
                start_time: variables[1],
                processing_time: 1,
            },
        ],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}
