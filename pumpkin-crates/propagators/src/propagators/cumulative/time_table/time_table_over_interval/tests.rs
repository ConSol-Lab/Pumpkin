use pumpkin_core::TestSolver;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::State;

use crate::StateExt;
use crate::cumulative::ArgTask;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::CumulativeExplanationType;
use crate::cumulative::time_table::TimeTableOverIntervalPropagator;

#[allow(
    deprecated,
    reason = "TestSolver is deprecated but still used in these tests"
)]
#[test]
fn propagator_propagates_from_profile() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 3,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions::default(),
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 1, 1);
    state.assert_bounds(s2, 5, 8);
}

#[test]
fn propagator_detects_conflict() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 1, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 4,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions {
            explanation_type: CumulativeExplanationType::Naive,
            ..Default::default()
        },
        constraint_tag,
    ));

    let Conflict::Propagator(x) = state.propagate_to_fixed_point().unwrap_err() else {
        panic!("an explicit conflict should have been detected");
    };

    let expected = [
        predicate!(s1 <= 1),
        predicate!(s1 >= 1),
        predicate!(s2 >= 1),
        predicate!(s2 <= 1),
    ];

    assert!(expected.iter().all(|y| {
        x.conjunction
            .iter()
            .collect::<Vec<&Predicate>>()
            .contains(&y)
    }));

    assert!(x.conjunction.iter().all(|y| expected.contains(y)));
}

#[test]
fn propagator_propagates_nothing() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(0, 6, None);
    let s2 = state.new_interval_variable(0, 6, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 3,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions::default(),
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 0, 6);
    state.assert_bounds(s2, 0, 6);
}

#[test]
fn propagator_propagates_example_4_3_schutt() {
    let mut state = State::default();
    let f = state.new_interval_variable(0, 14, None);
    let e = state.new_interval_variable(2, 4, None);
    let d = state.new_interval_variable(0, 2, None);
    let c = state.new_interval_variable(8, 9, None);
    let b = state.new_interval_variable(2, 3, None);
    let a = state.new_interval_variable(0, 1, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: a,
                processing_time: 2,
                resource_usage: 1,
            },
            ArgTask {
                start_time: b,
                processing_time: 6,
                resource_usage: 2,
            },
            ArgTask {
                start_time: c,
                processing_time: 2,
                resource_usage: 4,
            },
            ArgTask {
                start_time: d,
                processing_time: 2,
                resource_usage: 2,
            },
            ArgTask {
                start_time: e,
                processing_time: 5,
                resource_usage: 2,
            },
            ArgTask {
                start_time: f,
                processing_time: 6,
                resource_usage: 2,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        5,
        CumulativePropagatorOptions::default(),
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    assert_eq!(state.lower_bound(f), 10);
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn propagator_propagates_after_assignment() {
    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(0, 6);
    let s2 = solver.new_variable(6, 10);
    let constraint_tag = solver.new_constraint_tag();

    let propagator = solver
        .new_propagator(TimeTableOverIntervalPropagator::new(
            &[
                ArgTask {
                    start_time: s1,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2,
                    processing_time: 3,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            1,
            CumulativePropagatorOptions::default(),
            constraint_tag,
        ))
        .expect("No conflict");
    solver.assert_bounds(s1, 0, 6);
    solver.assert_bounds(s2, 6, 10);
    let notification_status = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
    assert!(match notification_status {
        EnqueueDecision::Enqueue => true,
        EnqueueDecision::Skip => false,
    });

    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    solver.assert_bounds(s1, 5, 6);
    solver.assert_bounds(s2, 7, 10);
}

#[test]
fn propagator_propagates_end_time() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(6, 6, None);
    let s2 = state.new_interval_variable(1, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 3,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions {
            explanation_type: CumulativeExplanationType::Naive,
            ..Default::default()
        },
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 6, 6);
    state.assert_bounds(s2, 1, 3);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate!(s2 <= 3),
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([s2 <= 8] & [s1 >= 6] & [s1 <= 6]), reason);
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn propagator_propagates_example_4_3_schutt_multiple_profiles() {
    let mut solver = TestSolver::default();
    let f = solver.new_variable(0, 14);
    let e = solver.new_variable(0, 4);
    let d = solver.new_variable(0, 2);
    let c = solver.new_variable(8, 9);
    let b2 = solver.new_variable(5, 5);
    let b1 = solver.new_variable(3, 3);
    let a = solver.new_variable(0, 1);

    let constraint_tag = solver.new_constraint_tag();

    let propagator = solver
        .new_propagator(TimeTableOverIntervalPropagator::new(
            &[
                ArgTask {
                    start_time: a,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: b1,
                    processing_time: 2,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: b2,
                    processing_time: 3,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: c,
                    processing_time: 2,
                    resource_usage: 4,
                },
                ArgTask {
                    start_time: d,
                    processing_time: 2,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: e,
                    processing_time: 4,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: f,
                    processing_time: 6,
                    resource_usage: 2,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            5,
            CumulativePropagatorOptions::default(),
            constraint_tag,
        ))
        .expect("No conflict");
    solver.assert_bounds(a, 0, 1);
    solver.assert_bounds(c, 8, 9);
    solver.assert_bounds(d, 0, 2);
    solver.assert_bounds(e, 0, 4);
    solver.assert_bounds(f, 0, 14);

    let notification_status = solver.increase_lower_bound_and_notify(propagator, 4, e, 3);
    assert!(match notification_status {
        EnqueueDecision::Enqueue => true,
        EnqueueDecision::Skip => false,
    });
    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    assert_eq!(solver.lower_bound(f), 10);
}

#[test]
fn propagator_propagates_from_profile_reason() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 3,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions {
            explanation_type: CumulativeExplanationType::Naive,
            ..Default::default()
        },
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 1, 1);
    state.assert_bounds(s2, 5, 8);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate!(s2 >= 5),
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([s2 >= 1] & [s1 >= 1] & [s1 <= 1]), reason);
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn propagator_propagates_example_4_3_schutt_after_update() {
    let mut solver = TestSolver::default();
    let f = solver.new_variable(0, 14);
    let e = solver.new_variable(0, 4);
    let d = solver.new_variable(0, 2);
    let c = solver.new_variable(8, 9);
    let b = solver.new_variable(2, 3);
    let a = solver.new_variable(0, 1);
    let constraint_tag = solver.new_constraint_tag();

    let propagator = solver
        .new_propagator(TimeTableOverIntervalPropagator::new(
            &[
                ArgTask {
                    start_time: a,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: b,
                    processing_time: 6,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: c,
                    processing_time: 2,
                    resource_usage: 4,
                },
                ArgTask {
                    start_time: d,
                    processing_time: 2,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: e,
                    processing_time: 4,
                    resource_usage: 2,
                },
                ArgTask {
                    start_time: f,
                    processing_time: 6,
                    resource_usage: 2,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            5,
            CumulativePropagatorOptions::default(),
            constraint_tag,
        ))
        .expect("No conflict");
    solver.assert_bounds(a, 0, 1);
    solver.assert_bounds(b, 2, 3);
    solver.assert_bounds(c, 8, 9);
    solver.assert_bounds(d, 0, 2);
    solver.assert_bounds(e, 0, 4);
    solver.assert_bounds(f, 0, 14);

    let notification_status = solver.increase_lower_bound_and_notify(propagator, 3, e, 3);
    assert!(match notification_status {
        EnqueueDecision::Enqueue => true,
        EnqueueDecision::Skip => false,
    });
    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    assert_eq!(solver.lower_bound(f), 10);
}

#[test]
fn propagator_propagates_generic_bounds() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(3, 3, None);
    let s2 = state.new_interval_variable(5, 5, None);
    let s3 = state.new_interval_variable(1, 15, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 2,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 2,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s3,
                processing_time: 4,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions {
            explanation_type: CumulativeExplanationType::Naive,
            ..Default::default()
        },
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 3, 3);
    state.assert_bounds(s2, 5, 5);
    state.assert_bounds(s3, 7, 15);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate!(s3 >= 7),
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 5]), reason);
}

#[test]
fn propagator_propagates_with_holes() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(4, 4, None);
    let s2 = state.new_interval_variable(0, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
        &[
            ArgTask {
                start_time: s1,
                processing_time: 4,
                resource_usage: 1,
            },
            ArgTask {
                start_time: s2,
                processing_time: 3,
                resource_usage: 1,
            },
        ]
        .into_iter()
        .collect::<Vec<_>>(),
        1,
        CumulativePropagatorOptions {
            explanation_type: CumulativeExplanationType::Naive,
            allow_holes_in_domain: true,
            ..Default::default()
        },
        constraint_tag,
    ));
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 4, 4);
    state.assert_bounds(s2, 0, 8);

    for removed in 2..8 {
        assert!(!state.contains(s2, removed));
        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate!(s2 != removed),
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason: PropositionalConjunction = reason_buffer.into();
        assert_eq!(conjunction!([s1 <= 4] & [s1 >= 4]), reason);
    }
}
