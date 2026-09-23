use crate::Comparison;
use crate::InferenceChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::CheckerTask;
use crate::checkers::TimeTableChecker;

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

    assert!(checker.check(state, &premises, None));
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

    assert!(checker.check(state, &premises, consequent.as_ref()));
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

    assert!(checker.check(state, &premises, consequent.as_ref()));
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

    assert!(checker.check(state, &premises, consequent.as_ref()));
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

    assert!(!checker.check(state, &premises, consequent.as_ref()));
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

    assert!(!checker.check(state, &premises, consequent.as_ref()));
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

    assert!(!checker.check(state, &premises, consequent.as_ref()));
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

    assert!(checker.check(state, &premises, consequent.as_ref()));
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

    assert!(checker.check(state, &premises, consequent.as_ref()));
}
