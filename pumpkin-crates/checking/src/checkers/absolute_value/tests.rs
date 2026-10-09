use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::checkers::AbsoluteValueChecker;
use crate::checkers::test_state;
use crate::test_atomic;

const CHECKER: AbsoluteValueChecker<&str, &str> = AbsoluteValueChecker {
    signed: "signed",
    absolute: "absolute",
};

#[test]
fn retention_fails_when_the_upper_bound_of_absolute_exceeds_the_greatest_absolute_value() {
    let state = test_state([
        test_atomic!([signed >= -3]),
        test_atomic!([signed <= 5]),
        test_atomic!([absolute >= 0]),
        test_atomic!([absolute <= 10]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_the_lower_bound_of_absolute_is_below_the_least_absolute_value() {
    let state = test_state([
        test_atomic!([signed >= 2]),
        test_atomic!([signed <= 5]),
        test_atomic!([absolute >= 0]),
        test_atomic!([absolute <= 5]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_fails_when_a_sign_fixed_signed_reaches_below_the_lower_bound_of_absolute() {
    let state = test_state([
        test_atomic!([signed >= -5]),
        test_atomic!([signed <= -1]),
        test_atomic!([absolute >= 3]),
        test_atomic!([absolute <= 5]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}
