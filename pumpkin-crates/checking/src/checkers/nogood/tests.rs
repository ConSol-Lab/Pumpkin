use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::TestAtomic;
use crate::checkers::ExtendedNogoodChecker;
use crate::checkers::NogoodChecker;
use crate::checkers::test_state;
use crate::test_atomic;

fn nogood() -> Box<[TestAtomic]> {
    [
        test_atomic!([x >= 3]),
        test_atomic!([x <= 5]),
        test_atomic!([y == 1]),
    ]
    .into()
}

#[test]
fn a_nogood_with_multiple_untrue_predicates_is_consistent() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 5]),
        test_atomic!([y >= 1]),
        test_atomic!([y <= 5]),
    ]);
    let checker = NogoodChecker {
        nogood: [test_atomic!([x >= 4]), test_atomic!([y <= 2])].into(),
    };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn a_nogood_with_one_untrue_predicates_and_no_false_predicates_is_inconsistent() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 5]),
        test_atomic!([y >= 1]),
        test_atomic!([y <= 5]),
    ]);
    let checker = NogoodChecker {
        nogood: [test_atomic!([x >= 4]), test_atomic!([y <= 5])].into(),
    };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn a_nogood_with_any_false_predicates_is_consistent() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 3]),
        test_atomic!([y >= 1]),
        test_atomic!([y <= 5]),
    ]);
    let checker = NogoodChecker {
        nogood: [test_atomic!([x >= 4]), test_atomic!([y <= 2])].into(),
    };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn a_free_variable_with_allowed_values_is_not_consistent_under_extended_propagation() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 10]),
        test_atomic!([y == 1]),
    ]);

    let extended = ExtendedNogoodChecker { nogood: nogood() };
    assert_eq!(
        extended.check_retention(&state),
        RetentionCheck::PropagationMissed
    );

    // Unit propagation cannot fire with two atomic constraints over `x` unassigned.
    let unit = NogoodChecker { nogood: nogood() };
    assert_eq!(
        unit.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn a_free_variable_without_allowed_values_is_consistent() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 10]),
        test_atomic!([x != 3]),
        test_atomic!([x != 4]),
        test_atomic!([x != 5]),
        test_atomic!([y == 1]),
    ]);
    let checker = ExtendedNogoodChecker { nogood: nogood() };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn two_free_variables_are_consistent() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 10]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 5]),
    ]);
    let checker = ExtendedNogoodChecker { nogood: nogood() };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn a_nogood_that_holds_is_not_consistent() {
    let state = test_state([test_atomic!([x == 4]), test_atomic!([y == 1])]);
    let checker = ExtendedNogoodChecker { nogood: nogood() };

    assert_eq!(
        checker.check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}
