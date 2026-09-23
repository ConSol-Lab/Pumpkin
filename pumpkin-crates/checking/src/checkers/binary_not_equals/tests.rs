use crate::RetentionChecker;
use crate::checkers::BinaryNotEqualsChecker;
use crate::checkers::test_state;
use crate::test_atomic;

const CHECKER: BinaryNotEqualsChecker<&str, &str> = BinaryNotEqualsChecker { lhs: "a", rhs: "b" };

#[test]
fn retention_fails_when_the_fixed_value_is_present_in_the_other_domain() {
    let state = test_state([
        test_atomic!([a == 3]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 5]),
    ]);

    assert!(!CHECKER.check_retention(&state));
}

#[test]
fn retention_holds_with_both_sides_unfixed() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 5]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 5]),
    ]);

    assert!(CHECKER.check_retention(&state));
}
