use crate::Comparison;
use crate::ConflictCheck;
use crate::InferenceChecker;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::DisjunctiveCheckerTask;
use crate::checkers::DisjunctiveEdgeFindingChecker;
use crate::checkers::test_state;
use crate::test_atomic;

#[test]
fn test_simple_propagation() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 7,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::GreaterEqual,
            value: 5,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::LessEqual,
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
        value: 8,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = DisjunctiveEdgeFindingChecker {
        tasks: vec![
            DisjunctiveCheckerTask {
                start_time: "x1",
                processing_time: 2,
            },
            DisjunctiveCheckerTask {
                start_time: "x2",
                processing_time: 3,
            },
            DisjunctiveCheckerTask {
                start_time: "x3",
                processing_time: 5,
            },
        ]
        .into(),
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn test_conflict() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::LessEqual,
            value: 1,
        },
    ];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = DisjunctiveEdgeFindingChecker {
        tasks: vec![
            DisjunctiveCheckerTask {
                start_time: "x1",
                processing_time: 2,
            },
            DisjunctiveCheckerTask {
                start_time: "x2",
                processing_time: 3,
            },
        ]
        .into(),
    };

    assert_eq!(
        checker.check(state, &premises, None),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn test_simple_propagation_not_accepted() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 7,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::GreaterEqual,
            value: 5,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::LessEqual,
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
        value: 9,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = DisjunctiveEdgeFindingChecker {
        tasks: vec![
            DisjunctiveCheckerTask {
                start_time: "x1",
                processing_time: 2,
            },
            DisjunctiveCheckerTask {
                start_time: "x2",
                processing_time: 3,
            },
            DisjunctiveCheckerTask {
                start_time: "x3",
                processing_time: 5,
            },
        ]
        .into(),
    };

    assert_eq!(
        checker.check(state, &premises, consequent.as_ref()),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn test_conflict_not_accepted() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x1",
            comparison: Comparison::LessEqual,
            value: 1,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::GreaterEqual,
            value: 0,
        },
        TestAtomic {
            name: "x2",
            comparison: Comparison::LessEqual,
            value: 2,
        },
    ];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = DisjunctiveEdgeFindingChecker {
        tasks: vec![
            DisjunctiveCheckerTask {
                start_time: "x1",
                processing_time: 2,
            },
            DisjunctiveCheckerTask {
                start_time: "x2",
                processing_time: 3,
            },
        ]
        .into(),
    };

    assert_eq!(
        checker.check(state, &premises, None),
        ConflictCheck::NoConflictDetected
    );
}

/// Task `a` is fixed to start at 0 and task `b` can start in `[b_lower, 5]`; both take 2.
fn two_tasks(
    b_lower: i32,
) -> (
    DisjunctiveEdgeFindingChecker<&'static str>,
    VariableState<TestAtomic>,
) {
    let checker = DisjunctiveEdgeFindingChecker {
        tasks: vec![
            DisjunctiveCheckerTask {
                start_time: "a",
                processing_time: 2,
            },
            DisjunctiveCheckerTask {
                start_time: "b",
                processing_time: 2,
            },
        ]
        .into(),
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
fn retention_fails_when_a_task_can_still_overlap_a_task_it_must_follow() {
    let (checker, state) = two_tasks(0);

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_a_task_starts_after_the_task_it_must_follow() {
    let (checker, state) = two_tasks(2);

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}
