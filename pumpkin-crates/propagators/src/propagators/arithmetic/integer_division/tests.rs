use pumpkin_checking::InferenceChecker;
use pumpkin_checking::checkers::IntegerDivisionChecker;
use pumpkin_core::state::State;

use super::*;

#[test]
fn detects_conflicts() {
    let mut state = State::default();
    let numerator = state.new_interval_variable(1, 1, None);
    let denominator = state.new_interval_variable(2, 2, None);
    let rhs = state.new_interval_variable(2, 2, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(DivisionArgs {
        numerator,
        denominator,
        rhs,
        constraint_tag,
    });

    let _ = state.propagate_to_fixed_point().unwrap_err();
}

#[test]
fn checker_does_not_report_false_conflict_for_tight_but_valid_quotient() {
    use pumpkin_checking::Comparison;
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;

    let premises = [
        TestAtomic {
            name: "numerator",
            comparison: Comparison::Equal,
            value: 7,
        },
        TestAtomic {
            name: "denominator",
            comparison: Comparison::GreaterEqual,
            value: 2,
        },
        TestAtomic {
            name: "denominator",
            comparison: Comparison::LessEqual,
            value: 3,
        },
        TestAtomic {
            name: "rhs",
            comparison: Comparison::Equal,
            value: 3,
        },
    ];

    let state =
        VariableState::prepare_for_conflict_check(premises, None).expect("no conflicting atomics");

    let checker = IntegerDivisionChecker {
        numerator: "numerator",
        denominator: "denominator",
        rhs: "rhs",
    };

    // div_floor(7, 2) = 3 is the max corner, so the true upper bound is 3 (matching rhs); a
    // buggy `.min()` over the floor-corners instead yields 2, which would wrongly conflict.
    assert!(!checker.check(state, &premises, None));
}
