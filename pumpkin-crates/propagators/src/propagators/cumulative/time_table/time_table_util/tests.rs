use std::rc::Rc;

use pumpkin_core::propagation::LocalId;
use pumpkin_core::state::State;

use super::find_profiles_which_propagate_lower_bound;
use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::propagators::cumulative::time_table::time_table_util::find_profiles_which_propagate_upper_bound;

#[test]
fn test_finding_last_index_lower_bound() {
    let mut state = State::default();
    let x = state.new_interval_variable(0, 10, None);
    let y = state.new_interval_variable(5, 5, None);
    let z = state.new_interval_variable(8, 8, None);

    let time_table = [
        &ResourceProfile {
            start: 5,
            end: 6,
            profile_tasks: vec![Rc::new(Task {
                start_variable: y,
                processing_time: 2,
                resource_usage: 1,
                id: LocalId::from(1),
            })],
            height: 1,
        },
        &ResourceProfile {
            start: 8,
            end: 8,
            profile_tasks: vec![Rc::new(Task {
                start_variable: z,
                processing_time: 1,
                resource_usage: 1,
                id: LocalId::from(2),
            })],
            height: 1,
        },
    ];

    let mut profile_buffer = vec![];
    find_profiles_which_propagate_lower_bound(
        0,
        &time_table,
        state.get_domains(),
        &Rc::new(Task {
            start_variable: x,
            processing_time: 6,
            resource_usage: 1,
            id: LocalId::from(0),
        }),
        1,
        &mut profile_buffer,
    );
    assert_eq!(profile_buffer.len(), 2);
}

#[test]
fn test_finding_last_index_upper_bound() {
    let mut state = State::default();

    let x = state.new_interval_variable(7, 7, None);
    let y = state.new_interval_variable(5, 5, None);
    let z = state.new_interval_variable(8, 8, None);

    let time_table = [
        &ResourceProfile {
            start: 5,
            end: 6,
            profile_tasks: vec![Rc::new(Task {
                start_variable: y,
                processing_time: 2,
                resource_usage: 1,
                id: LocalId::from(1),
            })],
            height: 1,
        },
        &ResourceProfile {
            start: 8,
            end: 8,
            profile_tasks: vec![Rc::new(Task {
                start_variable: z,
                processing_time: 1,
                resource_usage: 1,
                id: LocalId::from(2),
            })],
            height: 1,
        },
    ];

    let mut profile_buffer = vec![];
    find_profiles_which_propagate_upper_bound(
        1,
        &time_table,
        state.get_domains(),
        &Rc::new(Task {
            start_variable: x,
            processing_time: 6,
            resource_usage: 1,
            id: LocalId::from(0),
        }),
        1,
        &mut profile_buffer,
    );
    assert_eq!(profile_buffer.len(), 2);
}
