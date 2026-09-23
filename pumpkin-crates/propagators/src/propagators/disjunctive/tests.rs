use pumpkin_core::state::State;

use super::disjunctive_task::ArgDisjunctiveTask;
use crate::disjunctive::DisjunctiveConstructor;

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
