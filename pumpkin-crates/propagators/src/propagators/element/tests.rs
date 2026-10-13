use pumpkin_checking::ConflictChecker;
use pumpkin_checking::TestAtomic;
use pumpkin_checking::VariableState;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::state::State;

use super::ElementArgs;
use super::ElementChecker;
use super::ElementDescription;
use crate::StateExt;
use crate::fixed_domains;

#[test]
fn elements_from_array_with_disjoint_domains_to_rhs_are_filtered_from_index() {
    let mut state = State::default();

    let x_0 = state.new_interval_variable(4, 6, None);
    let x_1 = state.new_interval_variable(2, 3, None);
    let x_2 = state.new_interval_variable(7, 9, None);
    let x_3 = state.new_interval_variable(14, 15, None);

    let index = state.new_interval_variable(0, 3, None);
    let rhs = state.new_interval_variable(6, 9, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(ElementArgs {
        constraint_description: ElementDescription {
            array: vec![x_0, x_1, x_2, x_3].into(),
            index,
            rhs,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(index, 0, 2);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![index != 3],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([x_3 >= 10] & [rhs <= 9]), reason);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![index != 1],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([x_1 <= 5] & [rhs >= 6]), reason);
}

#[test]
fn bounds_of_rhs_are_min_and_max_of_lower_and_upper_in_array() {
    let mut state = State::default();

    let x_0 = state.new_interval_variable(3, 10, None);
    let x_1 = state.new_interval_variable(2, 3, None);
    let x_2 = state.new_interval_variable(7, 9, None);
    let x_3 = state.new_interval_variable(14, 15, None);

    let index = state.new_interval_variable(0, 3, None);
    let rhs = state.new_interval_variable(0, 20, None);
    let constraint_tag = state.new_constraint_tag();

    let _ = state.add_propagator(ElementArgs {
        constraint_description: ElementDescription {
            array: vec![x_0, x_1, x_2, x_3].into(),
            index,
            rhs,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(rhs, 2, 15);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs >= 2],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(
        conjunction!([x_0 >= 2] & [x_1 >= 2] & [x_2 >= 2] & [x_3 >= 2]),
        reason
    );

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs <= 15],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(
        conjunction!([x_0 <= 15] & [x_1 <= 15] & [x_2 <= 15] & [x_3 <= 15]),
        reason
    );
}

#[test]
fn fixed_index_propagates_bounds_on_element() {
    let mut state = State::default();

    let x_0 = state.new_interval_variable(3, 10, None);
    let x_1 = state.new_interval_variable(0, 15, None);
    let x_2 = state.new_interval_variable(7, 9, None);
    let x_3 = state.new_interval_variable(14, 15, None);
    let constraint_tag = state.new_constraint_tag();

    let index = state.new_interval_variable(1, 1, None);
    let rhs = state.new_interval_variable(6, 9, None);

    let _ = state.add_propagator(ElementArgs {
        constraint_description: ElementDescription {
            array: vec![x_0, x_1, x_2, x_3].into(),
            index,
            rhs,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(x_1, 6, 9);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![x_1 >= 6],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([index == 1] & [rhs >= 6]), reason);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![x_1 <= 9],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(conjunction!([index == 1] & [rhs <= 9]), reason);
}

#[test]
fn index_hole_propagates_bounds_on_rhs() {
    let mut state = State::default();

    let x_0 = state.new_interval_variable(3, 10, None);
    let x_1 = state.new_interval_variable(0, 15, None);
    let x_2 = state.new_interval_variable(7, 9, None);
    let x_3 = state.new_interval_variable(14, 15, None);
    let constraint_tag = state.new_constraint_tag();

    let index = state.new_interval_variable(0, 3, None);
    let _ = state
        .post(predicate![index != 1])
        .expect("Value can be removed");

    let rhs = state.new_interval_variable(-10, 30, None);

    let _ = state.add_propagator(ElementArgs {
        constraint_description: ElementDescription {
            array: vec![x_0, x_1, x_2, x_3].into(),
            index,
            rhs,
        },
        constraint_tag,
    });
    state.propagate_to_fixed_point().expect("no empty domains");

    state.assert_bounds(rhs, 3, 15);

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs >= 3],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(
        conjunction!([x_0 >= 3] & [x_2 >= 3] & [x_3 >= 3] & [index != 1]),
        reason
    );

    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(
        predicate![rhs <= 15],
        &mut reason_buffer,
        CurrentNogood::empty(),
    );
    let reason: PropositionalConjunction = reason_buffer.into();
    assert_eq!(
        conjunction!([x_0 <= 15] & [x_2 <= 15] & [x_3 <= 15] & [index != 1]),
        reason
    );
}

#[test]
fn holes_outside_union_bounds_are_ignored() {
    let premises = [
        TestAtomic {
            name: "x1",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 4,
        },
        TestAtomic {
            name: "x2",
            comparison: pumpkin_checking::Comparison::NotEqual,
            value: 2,
        },
    ];

    let consequent = Some(TestAtomic {
        name: "x4",
        comparison: pumpkin_checking::Comparison::NotEqual,
        value: 2,
    });
    let state = VariableState::prepare_for_conflict_check(premises, consequent)
        .expect("no conflicting atomics");

    let checker = ElementChecker::new(vec!["x1", "x2"].into(), "x3", "x4");

    assert!(checker.check(state, &premises, consequent.as_ref()));
}

#[test]
fn element_with_an_index_outside_the_array_is_a_violation() {
    let (variables, domains) = fixed_domains(&[1, 2, 2, 2]);
    let description = ElementDescription {
        array: Box::from([variables[0], variables[1]]),
        index: variables[2],
        rhs: variables[3],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn element_selects_by_an_index_starting_at_zero() {
    let (variables, domains) = fixed_domains(&[5, 7, 1, 7]);
    let description = ElementDescription {
        array: Box::from([variables[0], variables[1]]),
        index: variables[2],
        rhs: variables[3],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}
