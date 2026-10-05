use crate::Comparison;
use crate::ConflictCheck;
use crate::InferenceChecker;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::CheckerTask;
use crate::checkers::TimeTableChecker;
use crate::checkers::test_state;
use crate::test_atomic;

#[test]
fn conflict() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::Equal,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::Equal,
            value: 1,
        },
    ];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 1,
                processing_time: 1,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 1,
                processing_time: 1,
            },
        ]
        .into(),
        capacity: 1,
    };

    assert_eq!(
        checker.check(state, &premises, None),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn hole_in_domain() {
    let premises = [TestAtomic {
        name: "x1",
        comparison: Comparison::Equal,
        value: 6,
    }];

    let consequent = Some(TestAtomic {
        name: "x2",
        comparison: Comparison::NotEqual,
        value: 2,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn lower_bound_chain() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::Equal,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::Equal,
            value: 6,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x3",
        comparison: Comparison::GreaterEqual,
        value: 16,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 3,
                processing_time: 10,
            },
            CheckerTask {
                start_time: "x3",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn upper_bound_chain() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::Equal,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::Equal,
            value: 6,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::LessEqual,
            value: 15,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x3",
        comparison: Comparison::LessEqual,
        value: -4,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 3,
                processing_time: 10,
            },
            CheckerTask {
                start_time: "x3",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn hole_in_domain_not_accepted() {
    let premises = [TestAtomic {
        name: "x1",
        comparison: Comparison::Equal,
        value: 6,
    }];

    let consequent = Some(TestAtomic {
        name: "x2",
        comparison: Comparison::NotEqual,
        value: 1,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn lower_bound_chain_not_accepted() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::Equal,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::Equal,
            value: 8,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x3",
        comparison: Comparison::GreaterEqual,
        value: 16,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 3,
                processing_time: 10,
            },
            CheckerTask {
                start_time: "x3",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn upper_bound_chain_not_accepted() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::Equal,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::Equal,
            value: 8,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::LessEqual,
            value: 15,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x3",
        comparison: Comparison::LessEqual,
        value: -4,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x1",
                resource_usage: 3,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 3,
                processing_time: 10,
            },
            CheckerTask {
                start_time: "x3",
                resource_usage: 2,
                processing_time: 5,
            },
        ]
        .into(),
        capacity: 4,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn simple_test() {
    let premises = [
        TestAtomic {
            name: "x3",
            comparison: Comparison::GreaterEqual,
            value: 5,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::LessEqual,
            value: 6,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::LessEqual,
            value: 7,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x2",
        comparison: Comparison::LessEqual,
        value: 4,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x3",
                resource_usage: 1,
                processing_time: 3,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 2,
                processing_time: 2,
            },
        ]
        .into(),
        capacity: 2,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn test_holes_in_domain() {
    let premises = [
        TestAtomic {
            name: "x3",
            comparison: Comparison::GreaterEqual,
            value: 1,
        },
        TestAtomic {
            name: "x3",
            comparison: Comparison::LessEqual,
            value: 3,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 4,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 4,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::GreaterEqual,
            value: 2,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x2",
        comparison: Comparison::GreaterEqual,
        value: 5,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "x3",
                resource_usage: 1,
                processing_time: 3,
            },
            CheckerTask {
                start_time: "x2",
                resource_usage: 2,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "x1",
                resource_usage: 1,
                processing_time: 1,
            },
        ]
        .into(),
        capacity: 2,
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

/// Task `a` is fixed to start at 0 and task `b` can start in `[b_lower, 5]`; both take 2 and use
/// 1 of a capacity of 1.
fn two_tasks(b_lower: i32) -> (TimeTableChecker<&'static str>, VariableState<TestAtomic>) {
    let checker = TimeTableChecker {
        tasks: vec![
            CheckerTask {
                start_time: "a",
                resource_usage: 1,
                processing_time: 2,
            },
            CheckerTask {
                start_time: "b",
                resource_usage: 1,
                processing_time: 2,
            },
        ]
        .into(),
        capacity: 1,
    };
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 0]),
        test_atomic!([b >= b_lower]),
        test_atomic!([b <= 5]),
    ]);
    (checker, state)
}

#[test]
fn retention_fails_when_a_task_can_still_start_in_a_full_time_table() {
    let (checker, state) = two_tasks(0);

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_no_task_can_start_in_a_full_time_table() {
    let (checker, state) = two_tasks(2);

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_fails_when_the_mandatory_parts_exceed_the_capacity() {
    let (checker, _) = two_tasks(0);
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 0]),
        test_atomic!([b >= 1]),
        test_atomic!([b <= 1]),
    ]);

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}
