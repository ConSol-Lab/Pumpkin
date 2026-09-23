use crate::Comparison;
use crate::InferenceChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::IntegerMultiplicationChecker;

#[test]
fn checker_detects_a_pure_conflict_with_no_consequent() {
    // `consequent: None` is how the checker is invoked for a propagator-reported conflict
    // that isn't a single propagated predicate (see `VariableState::prepare_for_conflict_check`
    // and `State::check_conflict`). The checker must not just reject these outright: it needs
    // to confirm the premises alone are already contradictory.

    let premises = [
        TestAtomic {
            name: "a",
            comparison: Comparison::Equal,
            value: 3,
        },
        TestAtomic {
            name: "b",
            comparison: Comparison::Equal,
            value: 4,
        },
        TestAtomic {
            name: "c",
            comparison: Comparison::Equal,
            value: 10,
        },
    ];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = IntegerMultiplicationChecker {
        a: "a",
        b: "b",
        c: "c",
    };

    // 3 * 4 = 12 != 10, so this is a genuine conflict.
    assert!(checker.check(state, &premises, None));
}

#[test]
fn checker_does_not_report_a_conflict_for_consistent_premises_with_no_consequent() {
    let premises = [TestAtomic {
        name: "a",
        comparison: Comparison::Equal,
        value: 3,
    }];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = IntegerMultiplicationChecker {
        a: "a",
        b: "b",
        c: "c",
    };

    // `b` and `c` are unconstrained, so `a = 3` alone can't be a conflict.
    assert!(!checker.check(state, &premises, None));
}
