use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::checkers::ElementChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn checker() -> ElementChecker<&'static str, &'static str, &'static str> {
    ElementChecker::new(["x0", "x1"].into(), "index", "rhs")
}

#[test]
fn retention_fails_when_the_index_exceeds_the_array() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 3]),
        test_atomic!([x1 >= 2]),
        test_atomic!([x1 <= 4]),
        test_atomic!([index >= 0]),
        test_atomic!([index <= 5]),
        test_atomic!([rhs >= 1]),
        test_atomic!([rhs <= 4]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_the_rhs_is_wider_than_the_selectable_elements() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 3]),
        test_atomic!([x1 >= 2]),
        test_atomic!([x1 <= 4]),
        test_atomic!([index >= 0]),
        test_atomic!([index <= 1]),
        test_atomic!([rhs >= 0]),
        test_atomic!([rhs <= 4]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_a_selectable_element_misses_the_rhs() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 2]),
        test_atomic!([x1 >= 3]),
        test_atomic!([x1 <= 4]),
        test_atomic!([index >= 0]),
        test_atomic!([index <= 1]),
        test_atomic!([rhs >= 3]),
        test_atomic!([rhs <= 4]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_every_selectable_element_meets_the_rhs() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 3]),
        test_atomic!([x1 >= 2]),
        test_atomic!([x1 <= 4]),
        test_atomic!([index >= 0]),
        test_atomic!([index <= 1]),
        test_atomic!([rhs >= 1]),
        test_atomic!([rhs <= 4]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}
