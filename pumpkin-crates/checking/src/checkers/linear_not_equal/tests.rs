use crate::RetentionChecker;
use crate::checkers::LinearNotEqualChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn checker() -> LinearNotEqualChecker<&'static str> {
    LinearNotEqualChecker {
        terms: ["x", "y"].into(),
        bound: 5,
    }
}

#[test]
fn retention_fails_when_the_forbidden_value_is_present() {
    let state = test_state([
        test_atomic!([x == 3]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
    ]);

    assert!(!checker().check_retention(&state));
}

#[test]
fn retention_holds_when_the_forbidden_value_is_absent() {
    let state = test_state([
        test_atomic!([x == 3]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
        test_atomic!([y != 2]),
    ]);

    assert!(checker().check_retention(&state));
}

#[test]
fn retention_holds_with_two_unfixed_terms() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 5]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
    ]);

    assert!(checker().check_retention(&state));
}

#[test]
fn retention_fails_when_the_fixed_terms_sum_to_the_bound() {
    let state = test_state([test_atomic!([x == 3]), test_atomic!([y == 2])]);

    assert!(!checker().check_retention(&state));
}
