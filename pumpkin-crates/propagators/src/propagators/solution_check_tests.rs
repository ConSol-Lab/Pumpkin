//! Tests of [`ConstraintDescription::check_solution`] for the descriptions of this crate.

use pumpkin_checking::VariableState;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

use super::arithmetic::AbsoluteValueDescription;
use super::arithmetic::BinaryEqualsDescription;
use super::arithmetic::BinaryNotEqualsDescription;
use super::arithmetic::DivisionDescription;
use super::arithmetic::IntegerMultiplicationDescription;
use super::arithmetic::LinearLessOrEqualDescription;
use super::arithmetic::LinearNotEqualDescription;
use super::arithmetic::MaximumDescription;
use super::cumulative::ArgTask;
use super::cumulative::time_table::CumulativeDescription;
use super::disjunctive::ArgDisjunctiveTask;
use super::disjunctive::DisjunctiveDescription;
use super::element::ElementDescription;

/// Creates a variable for each value, and the domains in which each variable is fixed to its
/// value.
fn fixed(values: &[i32]) -> (Vec<DomainId>, VariableState<Predicate>) {
    let mut state = State::default();
    let variables = values
        .iter()
        .map(|_| state.new_interval_variable(-100, 100, None))
        .collect::<Vec<_>>();
    let domains = VariableState::prepare_for_conflict_check(
        variables
            .iter()
            .zip(values)
            .map(|(&variable, &value)| predicate![variable == value]),
        None,
    )
    .expect("the values are consistent");

    (variables, domains)
}

#[test]
fn division_truncates_towards_zero() {
    let (variables, domains) = fixed(&[-7, 2, -3]);
    let description = DivisionDescription {
        numerator: variables[0],
        denominator: variables[1],
        rhs: variables[2],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn division_by_zero_is_a_violation() {
    let (variables, domains) = fixed(&[0, 0, 0]);
    let description = DivisionDescription {
        numerator: variables[0],
        denominator: variables[1],
        rhs: variables[2],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn element_with_an_index_outside_the_array_is_a_violation() {
    let (variables, domains) = fixed(&[1, 2, 2, 2]);
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
    let (variables, domains) = fixed(&[5, 7, 1, 7]);
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

#[test]
fn cumulative_is_violated_by_an_overload_at_a_start_time() {
    let (variables, domains) = fixed(&[0, 2]);
    let description = CumulativeDescription {
        tasks: Box::from([
            ArgTask {
                start_time: variables[0],
                processing_time: 3,
                resource_usage: 2,
            },
            ArgTask {
                start_time: variables[1],
                processing_time: 3,
                resource_usage: 2,
            },
        ]),
        capacity: 3,
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn cumulative_allows_a_task_to_start_when_another_ends() {
    let (variables, domains) = fixed(&[0, 3]);
    let description = CumulativeDescription {
        tasks: Box::from([
            ArgTask {
                start_time: variables[0],
                processing_time: 3,
                resource_usage: 2,
            },
            ArgTask {
                start_time: variables[1],
                processing_time: 3,
                resource_usage: 2,
            },
        ]),
        capacity: 3,
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn disjunctive_is_violated_by_overlapping_tasks() {
    let (variables, domains) = fixed(&[0, 2]);
    let description = DisjunctiveDescription {
        tasks: vec![
            ArgDisjunctiveTask {
                start_time: variables[0],
                processing_time: 3,
            },
            ArgDisjunctiveTask {
                start_time: variables[1],
                processing_time: 1,
            },
        ],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn disjunctive_allows_a_task_to_start_when_another_ends() {
    let (variables, domains) = fixed(&[0, 3]);
    let description = DisjunctiveDescription {
        tasks: vec![
            ArgDisjunctiveTask {
                start_time: variables[0],
                processing_time: 3,
            },
            ArgDisjunctiveTask {
                start_time: variables[1],
                processing_time: 1,
            },
        ],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn maximum_has_to_equal_an_element() {
    let (variables, domains) = fixed(&[1, 3, 2]);
    let description = MaximumDescription {
        array: Box::from([variables[0], variables[1]]),
        rhs: variables[2],
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn a_disequality_over_an_unfixed_variable_is_unknown() {
    let mut state = State::default();
    let fixed_variable = state.new_interval_variable(-100, 100, None);
    let unfixed_variable = state.new_interval_variable(-100, 100, None);
    let domains =
        VariableState::prepare_for_conflict_check([predicate![fixed_variable == 1]], None)
            .expect("the predicate is consistent");
    let description = LinearNotEqualDescription {
        terms: [fixed_variable, unfixed_variable].into(),
        rhs: 0,
    };

    assert_eq!(description.check_solution(&domains), SolutionCheck::Unknown);
}

#[test]
fn a_linear_inequality_is_decided_by_the_bounds() {
    let mut state = State::default();
    let x = state.new_interval_variable(-100, 100, None);
    let y = state.new_interval_variable(-100, 100, None);
    let domains = VariableState::prepare_for_conflict_check(
        [
            predicate![x >= 0],
            predicate![x <= 3],
            predicate![y >= 1],
            predicate![y <= 2],
        ],
        None,
    )
    .expect("the predicates are consistent");

    let check = |bound| {
        LinearLessOrEqualDescription {
            terms: Box::from([x, y]),
            bound,
        }
        .check_solution(&domains)
    };

    assert_eq!(check(5), SolutionCheck::ConstraintSatisfied);
    assert_eq!(check(0), SolutionCheck::ConstraintViolated);
    assert_eq!(check(3), SolutionCheck::Unknown);
}

#[test]
fn absolute_value_compares_the_magnitude() {
    let (variables, domains) = fixed(&[-4, 4, 3]);

    let satisfied = AbsoluteValueDescription {
        signed: variables[0],
        absolute: variables[1],
    };
    let violated = AbsoluteValueDescription {
        signed: variables[0],
        absolute: variables[2],
    };

    assert_eq!(
        satisfied.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        violated.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn binary_equality_and_disequality_compare_the_values() {
    let (variables, domains) = fixed(&[2, 2, 3]);

    assert_eq!(
        BinaryEqualsDescription {
            a: variables[0],
            b: variables[1],
        }
        .check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        BinaryEqualsDescription {
            a: variables[0],
            b: variables[2],
        }
        .check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
    assert_eq!(
        BinaryNotEqualsDescription {
            a: variables[0],
            b: variables[2],
        }
        .check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        BinaryNotEqualsDescription {
            a: variables[0],
            b: variables[1],
        }
        .check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn multiplication_compares_the_product() {
    let (variables, domains) = fixed(&[-3, 4, -12, 12]);

    let satisfied = IntegerMultiplicationDescription {
        a: variables[0],
        b: variables[1],
        c: variables[2],
    };
    let violated = IntegerMultiplicationDescription {
        a: variables[0],
        b: variables[1],
        c: variables[3],
    };

    assert_eq!(
        satisfied.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        violated.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn a_linear_disequality_over_fixed_variables_is_decided() {
    let (variables, domains) = fixed(&[1, 2]);
    let check = |rhs| {
        LinearNotEqualDescription {
            terms: [variables[0], variables[1]].into(),
            rhs,
        }
        .check_solution(&domains)
    };

    assert_eq!(check(4), SolutionCheck::ConstraintSatisfied);
    assert_eq!(check(3), SolutionCheck::ConstraintViolated);
}
