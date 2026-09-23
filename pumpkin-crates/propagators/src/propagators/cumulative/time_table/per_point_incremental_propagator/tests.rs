use pumpkin_core::TestSolver;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

use crate::StateExt;
use crate::cumulative::ArgTask;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::CumulativeExplanationType;
use crate::cumulative::time_table::TimeTablePerPointIncrementalPropagator;
use crate::cumulative::time_table::TimeTablePerPointPropagator;

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

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
    let Conflict::Propagator(x) = state.propagate_to_fixed_point().unwrap_err() else {
        panic!("an explicit conflict should have been detected");
    };

    let expected = [
        predicate!(s1 <= 1),
        predicate!(s1 >= 1),
        predicate!(s2 <= 1),
        predicate!(s2 >= 1),
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

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
            ),
        )
        .expect("No conflict");
    assert_eq!(solver.lower_bound(s2), 6);
    assert_eq!(solver.upper_bound(s2), 10);
    assert_eq!(solver.lower_bound(s1), 0);
    assert_eq!(solver.upper_bound(s1), 6);
    let notification_status = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
    assert!(match notification_status {
        EnqueueDecision::Enqueue => true,
        EnqueueDecision::Skip => false,
    });

    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    assert_eq!(solver.lower_bound(s2), 7);
    assert_eq!(solver.upper_bound(s2), 10);
    assert_eq!(solver.lower_bound(s1), 5);
    assert_eq!(solver.upper_bound(s1), 6);
}

#[test]
fn propagator_propagates_end_time() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(6, 6, None);
    let s2 = state.new_interval_variable(1, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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
    assert_eq!(conjunction!([s2 <= 5] & [s1 >= 6] & [s1 <= 6]), reason);
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
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
            ),
        )
        .expect("No conflict");
    assert_eq!(solver.lower_bound(a), 0);
    assert_eq!(solver.upper_bound(a), 1);
    assert_eq!(solver.lower_bound(b), 2);
    assert_eq!(solver.upper_bound(b), 3);
    assert_eq!(solver.lower_bound(c), 8);
    assert_eq!(solver.upper_bound(c), 9);
    assert_eq!(solver.lower_bound(d), 0);
    assert_eq!(solver.upper_bound(d), 2);
    assert_eq!(solver.lower_bound(e), 0);
    assert_eq!(solver.upper_bound(e), 4);
    assert_eq!(solver.lower_bound(f), 0);
    assert_eq!(solver.upper_bound(f), 14);

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
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
            ),
        )
        .expect("No conflict");
    assert_eq!(solver.lower_bound(a), 0);
    assert_eq!(solver.upper_bound(a), 1);
    assert_eq!(solver.lower_bound(c), 8);
    assert_eq!(solver.upper_bound(c), 9);
    assert_eq!(solver.lower_bound(d), 0);
    assert_eq!(solver.upper_bound(d), 2);
    assert_eq!(solver.lower_bound(e), 0);
    assert_eq!(solver.upper_bound(e), 4);
    assert_eq!(solver.lower_bound(f), 0);
    assert_eq!(solver.upper_bound(f), 14);

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

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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
    assert_eq!(
        conjunction!([s2 >= 4] & [s1 >= 1] & [s1 <= 1]), /* Note that this not
                                                          * the most general
                                                          * explanation, if s2
                                                          * could have started at
                                                          * 0 then it would still
                                                          * have
                                                          * overlapped with the
                                                          * current interval */
        reason
    );
}

#[test]
fn propagator_propagates_generic_bounds() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(3, 3, None);
    let s2 = state.new_interval_variable(5, 5, None);
    let s3 = state.new_interval_variable(1, 15, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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
    assert_eq!(
        conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 6]), /* Note that s3 would
                                                          * have been able to
                                                          * propagate
                                                          * this bound even if it
                                                          * started at time 0 */
        reason
    );
}

#[test]
fn propagator_propagates_with_holes() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(4, 4, None);
    let s2 = state.new_interval_variable(0, 8, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(
        TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
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
        ),
    );
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

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn synchronisation_leads_to_same_conflict_explanation() {
    let mut solver_scratch = TestSolver::default();
    let s1_scratch = solver_scratch.new_variable(5, 5);
    let s2_scratch = solver_scratch.new_variable(1, 10);
    let s3_scratch = solver_scratch.new_variable(1, 10);
    let constraint_tag = solver_scratch.new_constraint_tag();
    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s3_scratch,
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
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
    let result_scratch = solver_scratch.propagate(propagator_scratch);

    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(5, 5);
    let s2 = solver.new_variable(1, 10);
    let s3 = solver.new_variable(1, 10);
    let constraint_tag = solver.new_constraint_tag();
    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                &[
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 4,
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
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
    let result = solver.propagate(propagator);
    assert!(
        {
            if let Err(Conflict::Propagator(conflict)) = &result {
                if let Err(Conflict::Propagator(explanation_scratch)) = &result_scratch {
                    conflict.conjunction.iter().collect::<Vec<_>>()
                        == explanation_scratch.conjunction.iter().collect::<Vec<_>>()
                } else {
                    false
                }
            } else {
                false
            }
        },
        "The results are different than expected - Expected: {result_scratch:?} but was: {result:?}"
    );
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn synchronisation_leads_to_same_conflict_after_propagating() {
    let mut solver_scratch = TestSolver::default();
    let s1_scratch = solver_scratch.new_variable(5, 5);
    let s2_scratch = solver_scratch.new_variable(1, 10);
    let s3_scratch = solver_scratch.new_variable(1, 10);
    let constraint_tag = solver_scratch.new_constraint_tag();
    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s3_scratch,
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
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);

    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(5, 5);
    let s2 = solver.new_variable(1, 10);
    let s3 = solver.new_variable(1, 10);
    let constraint_tag = solver.new_constraint_tag();
    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                &[
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 4,
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
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
    let result = solver.propagate(propagator);
    assert!(result.is_err());
    let result_scratch = solver_scratch.propagate(propagator_scratch);
    assert!(result_scratch.is_err());
    assert!({
        if let Err(Conflict::Propagator(explanation)) = &result {
            if let Err(Conflict::Propagator(explanation_scratch)) = &result_scratch {
                explanation.conjunction.iter().collect::<Vec<_>>()
                    == explanation_scratch.conjunction.iter().collect::<Vec<_>>()
            } else {
                false
            }
        } else {
            false
        }
    });
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn no_synchronisation_leads_to_different_conflict_explanation() {
    let mut solver_scratch = TestSolver::default();
    let s1_scratch = solver_scratch.new_variable(5, 5);
    let s2_scratch = solver_scratch.new_variable(1, 10);
    let s3_scratch = solver_scratch.new_variable(1, 10);
    let constraint_tag = solver_scratch.new_constraint_tag();
    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s3_scratch,
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
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);

    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(5, 5);
    let s2 = solver.new_variable(1, 10);
    let s3 = solver.new_variable(1, 10);
    let constraint_tag = solver.new_constraint_tag();
    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                &[
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 4,
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
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
    let result = solver.propagate(propagator);
    let result_scratch = solver_scratch.propagate(propagator_scratch);
    assert!({
        if let Err(Conflict::Propagator(explanation)) = &result {
            if let Err(Conflict::Propagator(explanation_scratch)) = &result_scratch {
                explanation.conjunction.iter().collect::<Vec<_>>()
                    != explanation_scratch.conjunction.iter().collect::<Vec<_>>()
            } else {
                false
            }
        } else {
            false
        }
    });
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn synchronisation_leads_to_same_explanation() {
    let mut solver_scratch = TestSolver::default();
    let s1_scratch = solver_scratch.new_variable(1, 6);
    let s2_scratch = solver_scratch.new_variable(1, 6);
    let s3_scratch = solver_scratch.new_variable(5, 11);
    let constraint_tag = solver_scratch.new_constraint_tag();
    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s3_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            2,
            CumulativePropagatorOptions {
                explanation_type: CumulativeExplanationType::Naive,
                ..Default::default()
            },
            constraint_tag,
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 5);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s1_scratch, 5);

    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(1, 6);
    let s2 = solver.new_variable(1, 6);
    let s3 = solver.new_variable(5, 11);
    let constraint_tag = solver.new_constraint_tag();
    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                &[
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 4,
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
                2,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 5);
    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    let _ = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
    let result_scratch = solver_scratch.propagate(propagator_scratch);
    assert!(result_scratch.is_ok());
    assert_eq!(solver_scratch.lower_bound(s3_scratch), 7);
    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    assert_eq!(solver.lower_bound(s3), 7);
    let reason_scratch = solver_scratch.get_reason_int(predicate!(s3_scratch >= 7));
    let reason = solver.get_reason_int(predicate!(s3 >= 7));
    assert_eq!(
        reason_scratch.iter().collect::<Vec<_>>(),
        reason.iter().collect::<Vec<_>>()
    );
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn no_synchronisation_leads_to_different_explanation() {
    let mut solver_scratch = TestSolver::default();
    let s1_scratch = solver_scratch.new_variable(1, 6);
    let s2_scratch = solver_scratch.new_variable(1, 6);
    let s3_scratch = solver_scratch.new_variable(5, 11);
    let constraint_tag = solver_scratch.new_constraint_tag();
    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 2,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s3_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect::<Vec<_>>(),
            2,
            CumulativePropagatorOptions {
                explanation_type: CumulativeExplanationType::Naive,
                ..Default::default()
            },
            constraint_tag,
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 5);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s1_scratch, 5);
    let mut solver = TestSolver::default();
    let s1 = solver.new_variable(1, 6);
    let s2 = solver.new_variable(1, 6);
    let s3 = solver.new_variable(5, 11);
    let constraint_tag = solver.new_constraint_tag();
    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                &[
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 4,
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
                2,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 5);
    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    let _ = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);

    let result_scratch = solver_scratch.propagate(propagator_scratch);
    assert!(result_scratch.is_ok());
    assert_eq!(solver_scratch.lower_bound(s3_scratch), 7);

    let result = solver.propagate(propagator);
    assert!(result.is_ok());
    assert_eq!(solver.lower_bound(s3), 7);

    let reason_scratch = solver_scratch.get_reason_int(predicate!(s3_scratch >= 7));
    let reason = solver.get_reason_int(predicate!(s3 >= 7));
    assert_ne!(
        reason_scratch.iter().collect::<Vec<_>>(),
        reason.iter().collect::<Vec<_>>()
    );
}

#[test]
#[allow(
    deprecated,
    reason = "Uses TestSolver for incremental notification assertions"
)]
fn synchronisation_leads_to_same_conflict() {
    let mut solver_scratch = TestSolver::default();
    let s0_scratch = solver_scratch.new_variable(1, 11);
    let s1_scratch = solver_scratch.new_variable(1, 5);
    let s2_scratch = solver_scratch.new_variable(1, 5);
    let constraint_tag = solver_scratch.new_constraint_tag();

    let propagator_scratch = solver_scratch
        .new_propagator(TimeTablePerPointPropagator::new(
            &[
                ArgTask {
                    start_time: s0_scratch,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s1_scratch,
                    processing_time: 1,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2_scratch,
                    processing_time: 1,
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
        ))
        .expect("No conflict");
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s2_scratch, 5);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s1_scratch, 5);
    let _ = solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);
    let _ = solver_scratch.decrease_upper_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);

    let mut solver = TestSolver::default();
    let s0 = solver.new_variable(1, 11);
    let s1 = solver.new_variable(1, 5);
    let s2 = solver.new_variable(1, 5);

    let propagator = solver
        .new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                &[
                    ArgTask {
                        start_time: s0,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s1,
                        processing_time: 1,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 1,
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
            ),
        )
        .expect("No conflict");
    let _ = solver.increase_lower_bound_and_notify(propagator, 2, s2, 5);
    let _ = solver.increase_lower_bound_and_notify(propagator, 1, s1, 5);
    let _ = solver.increase_lower_bound_and_notify(propagator, 0, s0, 5);
    let _ = solver.decrease_upper_bound_and_notify(propagator, 0, s0, 5);

    let result_scratch = solver_scratch.propagate(propagator_scratch);
    assert!(result_scratch.is_err());
    let result = solver.propagate(propagator);
    assert!(result.is_err());
    if let (
        Err(Conflict::Propagator(explanation)),
        Err(Conflict::Propagator(explanation_scratch)),
    ) = (result, result_scratch)
    {
        assert_eq!(
            explanation.conjunction.iter().collect::<Vec<_>>(),
            explanation_scratch.conjunction.iter().collect::<Vec<_>>()
        );
    } else {
        panic!("Incorrect result")
    }
}
