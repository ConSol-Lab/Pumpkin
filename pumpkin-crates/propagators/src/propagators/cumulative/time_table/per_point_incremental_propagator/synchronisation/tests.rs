use std::rc::Rc;

use pumpkin_core::propagation::LocalId;
use pumpkin_core::state::State;

use super::find_synchronised_conflict;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::PerPointTimeTableType;

#[test]
fn test_correct_conflict_returned() {
    let mut state = State::default();

    let x0 = state.new_interval_variable(0, 10, None);
    let x1 = state.new_interval_variable(0, 10, None);
    let x2 = state.new_interval_variable(0, 10, None);

    let tasks = vec![
        Task {
            start_variable: x0,
            processing_time: 2,
            resource_usage: 2,
            id: LocalId::from(0),
        },
        Task {
            start_variable: x1,
            processing_time: 2,
            resource_usage: 2,
            id: LocalId::from(1),
        },
        Task {
            start_variable: x2,
            processing_time: 2,
            resource_usage: 1,
            id: LocalId::from(2),
        },
    ];

    let parameters = CumulativeParameters::new(tasks, 1, CumulativePropagatorOptions::default());

    let mut time_table = PerPointTimeTableType::default();
    let _ = time_table.insert(
        3,
        ResourceProfile {
            start: 3,
            end: 3,
            profile_tasks: vec![Rc::clone(&parameters.tasks[1])],
            height: 2,
        },
    );
    let _ = time_table.insert(
        4,
        ResourceProfile {
            start: 4,
            end: 4,
            profile_tasks: vec![
                Rc::clone(&parameters.tasks[0]),
                Rc::clone(&parameters.tasks[2]),
            ],
            height: 3,
        },
    );

    let result = find_synchronised_conflict(&mut time_table, &parameters);
    assert!(matches!(result, Some(4)));
}
