use crate::Comparison;
use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::TestAtomic;
use crate::VariableState;
use crate::checkers::IntegerMultiplicationChecker;
use crate::checkers::test_state;
use crate::test_atomic;

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
    assert_eq!(
        checker.check(&state, &premises, None),
        ConflictCheck::ConflictDetected
    );
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
    assert_eq!(
        checker.check(&state, &premises, None),
        ConflictCheck::NoConflictDetected
    );
}

const RETENTION_CHECKER: IntegerMultiplicationChecker<&str, &str, &str> =
    IntegerMultiplicationChecker {
        a: "a",
        b: "b",
        c: "c",
    };

#[test]
fn retention_fails_when_the_product_is_not_propagated_to_c() {
    let state = test_state([
        test_atomic!([a >= 2]),
        test_atomic!([a <= 3]),
        test_atomic!([b >= 4]),
        test_atomic!([b <= 5]),
        test_atomic!([c >= 0]),
        test_atomic!([c <= 100]),
    ]);

    assert_eq!(
        RETENTION_CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_the_quotient_is_not_propagated_to_a() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 10]),
        test_atomic!([b >= 2]),
        test_atomic!([b <= 2]),
        test_atomic!([c >= 6]),
        test_atomic!([c <= 6]),
    ]);

    assert_eq!(
        RETENTION_CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_the_bounds_agree() {
    let state = test_state([
        test_atomic!([a >= 2]),
        test_atomic!([a <= 2]),
        test_atomic!([b >= 3]),
        test_atomic!([b <= 3]),
        test_atomic!([c >= 6]),
        test_atomic!([c <= 6]),
    ]);

    assert_eq!(
        RETENTION_CHECKER.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}
