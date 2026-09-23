use crate::RetentionChecker;
use crate::checkers::LinearLessOrEqualChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn checker() -> LinearLessOrEqualChecker<&'static str> {
    LinearLessOrEqualChecker::new(["x", "y"].into(), 7)
}

#[test]
fn retention_fails_when_an_upper_bound_can_be_lowered() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 5]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
    ]);

    assert!(!checker().check_retention(&state));
}

#[test]
fn retention_fails_when_the_lower_bounds_exceed_the_bound() {
    let state = test_state([
        test_atomic!([x >= 4]),
        test_atomic!([x <= 5]),
        test_atomic!([y >= 4]),
        test_atomic!([y <= 10]),
    ]);

    assert!(!checker().check_retention(&state));
}
