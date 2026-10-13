use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::checkers::IntegerDivisionChecker;
use crate::checkers::test_state;
use crate::test_atomic;

const CHECKER: IntegerDivisionChecker<&str, &str, &str> = IntegerDivisionChecker {
    numerator: "numerator",
    denominator: "denominator",
    rhs: "rhs",
};

#[test]
fn retention_fails_when_the_rhs_exceeds_the_quotient() {
    let state = test_state([
        test_atomic!([numerator >= 10]),
        test_atomic!([numerator <= 10]),
        test_atomic!([denominator >= 2]),
        test_atomic!([denominator <= 2]),
        test_atomic!([rhs >= 0]),
        test_atomic!([rhs <= 10]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_the_rhs_is_the_quotient() {
    let state = test_state([
        test_atomic!([numerator >= 10]),
        test_atomic!([numerator <= 10]),
        test_atomic!([denominator >= 2]),
        test_atomic!([denominator <= 2]),
        test_atomic!([rhs >= 5]),
        test_atomic!([rhs <= 5]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_holds_for_a_negative_denominator() {
    let state = test_state([
        test_atomic!([numerator >= 10]),
        test_atomic!([numerator <= 10]),
        test_atomic!([denominator >= -2]),
        test_atomic!([denominator <= -2]),
        test_atomic!([rhs >= -5]),
        test_atomic!([rhs <= -5]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_holds_while_the_sign_of_the_denominator_is_open() {
    let state = test_state([
        test_atomic!([numerator >= 10]),
        test_atomic!([numerator <= 10]),
        test_atomic!([denominator >= -2]),
        test_atomic!([denominator <= 2]),
        test_atomic!([rhs >= -100]),
        test_atomic!([rhs <= 100]),
    ]);

    assert_eq!(
        CHECKER.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}
