use super::ElementChecker;
use crate::Comparison;
use crate::ConflictChecker;
use crate::TestAtomic;
use crate::VariableState;

#[test]
fn an_index_outside_the_array_is_a_conflict() {
    // To check the inference [index >= 0], the checker assumes its negation, index <= -1, under
    // which the index selects no element of the array.
    let consequent = TestAtomic {
        name: "index",
        comparison: Comparison::GreaterEqual,
        value: 0,
    };
    let state = VariableState::prepare_for_conflict_check([], Some(consequent))
        .expect("no conflicting atomics");

    let checker = ElementChecker::new(Box::from(["x0", "x1"]), "index", "rhs");

    assert!(checker.check(state, &[], Some(&consequent)));
}
