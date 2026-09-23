use crate::RetentionChecker;
use crate::checkers::BinaryEqualsChecker;
use crate::checkers::test_state;
use crate::test_atomic;

const CHECKER: BinaryEqualsChecker<&str, &str> = BinaryEqualsChecker { lhs: "a", rhs: "b" };

#[test]
fn retention_fails_when_the_bounds_differ() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 5]),
        test_atomic!([b >= 3]),
        test_atomic!([b <= 8]),
    ]);

    assert!(!CHECKER.check_retention(&state));
}

#[test]
fn retention_fails_when_a_hole_is_not_shared() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 5]),
        test_atomic!([a != 2]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 5]),
    ]);

    assert!(!CHECKER.check_retention(&state));
}

#[test]
fn retention_holds_when_removed_values_leave_equal_domains() {
    let state = test_state([
        test_atomic!([a >= 1]),
        test_atomic!([a <= 3]),
        test_atomic!([a != 1]),
        test_atomic!([a != 2]),
        test_atomic!([b == 3]),
    ]);

    assert!(CHECKER.check_retention(&state));
}
