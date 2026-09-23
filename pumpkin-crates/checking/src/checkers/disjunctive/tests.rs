use crate::Comparison;
use crate::InferenceChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::DisjunctiveCheckerTask;
use crate::checkers::DisjunctiveEdgeFindingChecker;

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

    assert!(checker.check(state, &premises, consequent.as_ref()));
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

    assert!(checker.check(state, &premises, None));
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

    assert!(!checker.check(state, &premises, consequent.as_ref()));
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

    assert!(!checker.check(state, &premises, None));
}
