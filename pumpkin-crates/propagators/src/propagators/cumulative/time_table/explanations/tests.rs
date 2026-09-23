use std::rc::Rc;

use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::state::State;

use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::cumulative::time_table::explanations::get_minimal_profile;

#[test]
fn test_minimal_conflict_returned() {
    let mut state = State::default();

    let profile = ResourceProfile {
        start: 5,
        end: 10,
        profile_tasks: vec![
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 5,
                id: LocalId::from(0),
            }),
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(1),
            }),
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 2,
                id: LocalId::from(2),
            }),
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 4,
                id: LocalId::from(3),
            }),
        ],
        height: 12,
    };

    let minimal_profile = get_minimal_profile(
        &profile,
        |_| [Predicate::trivially_true(), Predicate::trivially_true()],
        9,
        Some(1),
    );

    assert_eq!(minimal_profile.count() / 2, 2);
}

#[test]
fn test_does_not_remove_both() {
    let mut state = State::default();

    let profile = ResourceProfile {
        start: 5,
        end: 10,
        profile_tasks: vec![
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(1),
            }),
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 1,
                id: LocalId::from(2),
            }),
            Rc::new(Task {
                start_variable: state.new_interval_variable(5, 5, None),
                processing_time: 6,
                resource_usage: 4,
                id: LocalId::from(3),
            }),
        ],
        height: 6,
    };

    let minimal_profile = get_minimal_profile(
        &profile,
        |_| [Predicate::trivially_true(), Predicate::trivially_true()],
        4,
        None,
    );

    assert_eq!(minimal_profile.count() / 2, 2);
}
