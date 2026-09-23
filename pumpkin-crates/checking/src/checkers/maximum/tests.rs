use crate::RetentionChecker;
use crate::checkers::MaximumChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn checker() -> MaximumChecker<&'static str, &'static str> {
    MaximumChecker {
        array: ["a", "b"].into(),
        rhs: "rhs",
    }
}

#[test]
fn retention_fails_when_the_lower_bound_of_the_maximum_is_below_the_greatest_lower_bound() {
    let state = test_state([
        test_atomic!([a >= 3]),
        test_atomic!([a <= 10]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 5]),
        test_atomic!([rhs >= 0]),
        test_atomic!([rhs <= 10]),
    ]);

    assert!(!checker().check_retention(&state));
}

#[test]
fn retention_fails_when_an_element_exceeds_the_upper_bound_of_the_maximum() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 10]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 5]),
        test_atomic!([rhs >= 0]),
        test_atomic!([rhs <= 8]),
    ]);

    assert!(!checker().check_retention(&state));
}

#[test]
fn retention_fails_when_the_sole_candidate_does_not_attain_the_lower_bound_of_the_maximum() {
    let state = test_state([
        test_atomic!([a >= 0]),
        test_atomic!([a <= 8]),
        test_atomic!([b >= 0]),
        test_atomic!([b <= 3]),
        test_atomic!([rhs >= 5]),
        test_atomic!([rhs <= 8]),
    ]);

    assert!(!checker().check_retention(&state));
}
