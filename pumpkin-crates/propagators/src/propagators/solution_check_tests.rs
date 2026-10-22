//! Tests of [`ConstraintDescription::check_solution`] for the descriptions of this crate.

use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::predicate;
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
use super::lexicographic::LexLessOrEqualDescription;

/// Creates a variable for each value, in a state in which each variable is fixed to its value.
fn fixed(values: &[i32]) -> (Vec<DomainId>, State) {
    let mut state = State::default();
    let variables = values
        .iter()
        .map(|_| state.new_interval_variable(-100, 100, None))
        .collect::<Vec<_>>();
    for (&variable, &value) in variables.iter().zip(values) {
        let _ = state
            .post(predicate![variable == value])
            .expect("the value is in the domain");
    }

    (variables, state)
}

fn solution(state: &State) -> CompleteSolution<'_> {
    state.complete_solution().expect("every variable is fixed")
}

#[test]
fn division_truncates_towards_zero() {
    let (variables, state) = fixed(&[-7, 2, -3]);
    let description = DivisionDescription {
        numerator: variables[0],
        denominator: variables[1],
        rhs: variables[2],
    };

    assert_eq!(
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn division_by_zero_is_a_violation() {
    let (variables, state) = fixed(&[0, 0, 0]);
    let description = DivisionDescription {
        numerator: variables[0],
        denominator: variables[1],
        rhs: variables[2],
    };

    assert_eq!(
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn element_with_an_index_outside_the_array_is_a_violation() {
    let (variables, state) = fixed(&[1, 2, 2, 2]);
    let description = ElementDescription {
        array: Box::from([variables[0], variables[1]]),
        index: variables[2],
        rhs: variables[3],
    };

    assert_eq!(
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn element_selects_by_an_index_starting_at_zero() {
    let (variables, state) = fixed(&[5, 7, 1, 7]);
    let description = ElementDescription {
        array: Box::from([variables[0], variables[1]]),
        index: variables[2],
        rhs: variables[3],
    };

    assert_eq!(
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn cumulative_is_violated_by_an_overload_at_a_start_time() {
    let (variables, state) = fixed(&[0, 2]);
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
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn cumulative_allows_a_task_to_start_when_another_ends() {
    let (variables, state) = fixed(&[0, 3]);
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
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn disjunctive_is_violated_by_overlapping_tasks() {
    let (variables, state) = fixed(&[0, 2]);
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
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn disjunctive_allows_a_task_to_start_when_another_ends() {
    let (variables, state) = fixed(&[0, 3]);
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
        description.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn maximum_has_to_equal_the_largest_element() {
    let (variables, state) = fixed(&[1, 3, 2, 3]);
    let maximum = |rhs| MaximumDescription {
        array: Box::from([variables[0], variables[1]]),
        rhs,
    };

    assert_eq!(
        maximum(variables[2]).check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
    assert_eq!(
        maximum(variables[3]).check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
}

#[test]
fn a_linear_inequality_compares_the_sum_with_the_bound() {
    let (variables, state) = fixed(&[3, 2]);
    let check = |bound| {
        LinearLessOrEqualDescription {
            terms: Box::from([variables[0], variables[1]]),
            bound,
        }
        .check_solution(&solution(&state))
    };

    assert_eq!(check(5), SolutionCheck::ConstraintSatisfied);
    assert_eq!(check(4), SolutionCheck::ConstraintViolated);
}

#[test]
fn absolute_value_compares_the_magnitude() {
    let (variables, state) = fixed(&[-4, 4, 3]);

    let satisfied = AbsoluteValueDescription {
        signed: variables[0],
        absolute: variables[1],
    };
    let violated = AbsoluteValueDescription {
        signed: variables[0],
        absolute: variables[2],
    };

    assert_eq!(
        satisfied.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        violated.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn binary_equality_and_disequality_compare_the_values() {
    let (variables, state) = fixed(&[2, 2, 3]);

    assert_eq!(
        BinaryEqualsDescription {
            a: variables[0],
            b: variables[1],
        }
        .check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        BinaryEqualsDescription {
            a: variables[0],
            b: variables[2],
        }
        .check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
    assert_eq!(
        BinaryNotEqualsDescription {
            a: variables[0],
            b: variables[2],
        }
        .check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        BinaryNotEqualsDescription {
            a: variables[0],
            b: variables[1],
        }
        .check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn multiplication_compares_the_product() {
    let (variables, state) = fixed(&[-3, 4, -12, 12]);

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
        satisfied.check_solution(&solution(&state)),
        SolutionCheck::ConstraintSatisfied
    );
    assert_eq!(
        violated.check_solution(&solution(&state)),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn a_linear_disequality_over_fixed_variables_is_decided() {
    let (variables, state) = fixed(&[1, 2]);
    let check = |rhs| {
        LinearNotEqualDescription {
            terms: [variables[0], variables[1]].into(),
            rhs,
        }
        .check_solution(&solution(&state))
    };

    assert_eq!(check(4), SolutionCheck::ConstraintSatisfied);
    assert_eq!(check(3), SolutionCheck::ConstraintViolated);
}

#[test]
fn lex_less_or_equal_compares_at_the_first_difference() {
    let (variables, state) = fixed(&[1, 5, 2, 0, 1, 5]);
    let check = |x: [usize; 2], y: [usize; 2]| {
        LexLessOrEqualDescription {
            x: x.map(|index| variables[index]).into(),
            y: y.map(|index| variables[index]).into(),
        }
        .check_solution(&solution(&state))
    };

    // [1, 5] <=lex [2, 0]: the first position decides.
    assert_eq!(check([0, 1], [2, 3]), SolutionCheck::ConstraintSatisfied);
    // [2, 0] <=lex [1, 5] is violated at the first position.
    assert_eq!(check([2, 3], [0, 1]), SolutionCheck::ConstraintViolated);
    // [1, 5] <=lex [1, 5]: equal vectors satisfy the constraint.
    assert_eq!(check([0, 1], [4, 5]), SolutionCheck::ConstraintSatisfied);
}
