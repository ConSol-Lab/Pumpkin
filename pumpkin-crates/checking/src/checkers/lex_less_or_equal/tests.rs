use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::checkers::LexLessOrEqualChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn checker() -> LexLessOrEqualChecker<&'static str> {
    LexLessOrEqualChecker {
        x: ["x0", "x1"].into(),
        y: ["y0", "y1"].into(),
    }
}

#[test]
fn a_greater_lower_bound_after_equal_bounds_is_a_conflict() {
    let state = test_state([
        test_atomic!([x0 >= 2]),
        test_atomic!([y0 <= 2]),
        test_atomic!([x1 >= 4]),
        test_atomic!([y1 <= 3]),
    ]);

    assert_eq!(
        checker().check(&state, &[], None),
        ConflictCheck::ConflictDetected
    );
}

#[test]
fn a_smaller_lower_bound_at_the_first_position_is_no_conflict() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([y0 <= 2]),
        test_atomic!([x1 >= 4]),
        test_atomic!([y1 <= 3]),
    ]);

    assert_eq!(
        checker().check(&state, &[], None),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn equal_vectors_are_no_conflict() {
    let state = test_state([
        test_atomic!([x0 >= 2]),
        test_atomic!([y0 <= 2]),
        test_atomic!([x1 >= 3]),
        test_atomic!([y1 <= 3]),
    ]);

    assert_eq!(
        checker().check(&state, &[], None),
        ConflictCheck::NoConflictDetected
    );
}

#[test]
fn retention_fails_when_the_first_position_of_x_can_be_lowered() {
    let state = test_state([
        test_atomic!([x0 >= 2]),
        test_atomic!([x0 <= 5]),
        test_atomic!([y0 >= 0]),
        test_atomic!([y0 <= 3]),
        test_atomic!([x1 >= 0]),
        test_atomic!([x1 <= 9]),
        test_atomic!([y1 >= 0]),
        test_atomic!([y1 <= 9]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_an_infeasible_tail_is_not_made_strict() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 3]),
        test_atomic!([y0 >= 1]),
        test_atomic!([y0 <= 3]),
        test_atomic!([x1 >= 4]),
        test_atomic!([x1 <= 5]),
        test_atomic!([y1 >= 0]),
        test_atomic!([y1 <= 3]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_at_the_fixpoint_after_a_fixed_equal_prefix() {
    let state = test_state([
        test_atomic!([x0 == 2]),
        test_atomic!([y0 == 2]),
        test_atomic!([x1 >= 3]),
        test_atomic!([x1 <= 4]),
        test_atomic!([y1 >= 3]),
        test_atomic!([y1 <= 4]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_holds_at_a_strict_fixpoint() {
    let state = test_state([
        test_atomic!([x0 >= 1]),
        test_atomic!([x0 <= 2]),
        test_atomic!([y0 >= 2]),
        test_atomic!([y0 <= 3]),
        test_atomic!([x1 >= 4]),
        test_atomic!([x1 <= 5]),
        test_atomic!([y1 >= 0]),
        test_atomic!([y1 <= 3]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}
