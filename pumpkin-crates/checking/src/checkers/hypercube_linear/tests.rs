use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::TestAtomic;
use crate::checkers::HypercubeLinearChecker;
use crate::checkers::test_state;
use crate::test_atomic;

/// `[x >= 1] -> y <= 5`.
fn checker() -> HypercubeLinearChecker<TestAtomic, &'static str> {
    HypercubeLinearChecker {
        hypercube: vec![test_atomic!([x >= 1])],
        terms: vec!["y"],
        bound: 5,
    }
}

#[test]
fn retention_fails_when_the_linear_is_not_propagated_under_a_true_hypercube() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 3]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}

#[test]
fn retention_holds_when_the_linear_is_propagated_under_a_true_hypercube() {
    let state = test_state([
        test_atomic!([x >= 1]),
        test_atomic!([x <= 3]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 5]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_holds_when_the_hypercube_is_false() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 0]),
        test_atomic!([y >= 0]),
        test_atomic!([y <= 10]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::NothingToPropagate
    );
}

#[test]
fn retention_fails_when_a_violated_linear_does_not_falsify_the_last_predicate() {
    let state = test_state([
        test_atomic!([x >= 0]),
        test_atomic!([x <= 3]),
        test_atomic!([y >= 6]),
        test_atomic!([y <= 10]),
    ]);

    assert_eq!(
        checker().check_retention(&state),
        RetentionCheck::PropagationMissed
    );
}
