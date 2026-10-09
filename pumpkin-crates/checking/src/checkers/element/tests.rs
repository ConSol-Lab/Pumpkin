use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::checkers::ElementChecker;
use crate::test_atomic;

fn checker() -> ElementChecker<&'static str, &'static str, &'static str> {
    ElementChecker::new(["x0", "x1"].into(), "index", "rhs")
}

#[test]
fn an_index_that_selects_no_element_is_a_conflict() {
    // To check the inference [index >= 0], the checker assumes its negation, index <= -1, under
    // which the index selects no element of the array.
    let consequent = test_atomic!([index >= 0]);
    let state = crate::VariableState::prepare_for_conflict_check([], Some(consequent))
        .expect("no conflicting atomics");

    assert_eq!(
        checker().check(&state, &[], Some(&consequent)),
        ConflictCheck::ConflictDetected
    );
}
