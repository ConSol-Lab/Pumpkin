//! Fuzzing propagators that are built by Rust code. The checkers are only active when they are
//! compiled in, as in the CI test command; otherwise only crashes are found.
#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use pumpkin_core::rand::RngExt;
use pumpkin_core::rand::rngs::SmallRng;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_fuzzer::driver::Configuration;
use pumpkin_fuzzer::fuzz_propagator;
use pumpkin_fuzzer::fuzz_propagator_with_parameters;
use pumpkin_solver::Solver;
use pumpkin_solver::propagators::cumulative::options::CumulativeOptions;

#[test]
fn linear_less_or_equal_built_in_rust() {
    let configuration = Configuration {
        cases: 10,
        ..Configuration::default()
    };

    let outcome = fuzz_propagator(
        "linear_less_or_equal",
        50,
        &configuration,
        build_linear_less_or_equal,
    );

    outcome.assert_no_failures();
}

#[test]
fn cumulative_built_in_rust_with_its_parameters() {
    let configuration = Configuration {
        cases: 10,
        // Settings without known defects.
        parameter_filters: vec!["TimeTablePerPoint,".to_owned(), "Pointwise".to_owned()],
        ..Configuration::default()
    };

    let outcome =
        fuzz_propagator_with_parameters("cumulative", 30, &configuration, build_cumulative);

    outcome.assert_no_failures();
}

/// The regression test that a failure report for [`build_cumulative`] gives, for the known defect
/// N7.
#[test]
#[ignore = "N7: the naive explanation of a single profile of the cumulative is unsound"]
fn cumulative_regression() {
    pumpkin_fuzzer::replay_propagator_with_parameters(
        build_cumulative,
        86,
        "CumulativeOptions { propagation_method: TimeTablePerPoint, propagator_options: \
         CumulativePropagatorOptions { allow_holes_in_domain: true, explanation_type: Naive, \
         generate_sequence: false, incremental_backtracking: true } }",
        "s1 == 3\n",
    );
}

fn build_linear_less_or_equal(rng: &mut SmallRng, solver: &mut Solver) {
    let terms = (0..rng.random_range(1..4))
        .map(|index| {
            let lower_bound = rng.random_range(-3..3);
            let upper_bound = lower_bound + rng.random_range(0..5);
            let variable =
                solver.new_named_bounded_integer(lower_bound, upper_bound, format!("x{index}"));
            let coefficient = if rng.random_bool(0.5) {
                rng.random_range(1..4)
            } else {
                -rng.random_range(1..4)
            };
            variable.scaled(coefficient)
        })
        .collect::<Vec<_>>();
    let constraint_tag = solver.new_constraint_tag();
    solver
        .add_constraint(pumpkin_solver::less_than_or_equals(
            terms,
            rng.random_range(-4..6),
            constraint_tag,
        ))
        .post();
}

fn build_cumulative(rng: &mut SmallRng, solver: &mut Solver, options: CumulativeOptions) {
    let tasks = rng.random_range(2..5);
    let start_times = (0..tasks)
        .map(|index| {
            let earliest_start = rng.random_range(0..4);
            solver.new_named_bounded_integer(
                earliest_start,
                earliest_start + rng.random_range(0..6),
                format!("s{index}"),
            )
        })
        .collect::<Vec<_>>();
    let durations = (0..tasks)
        .map(|_| rng.random_range(1..4))
        .collect::<Vec<_>>();
    let resource_requirements = (0..tasks)
        .map(|_| rng.random_range(1..4))
        .collect::<Vec<_>>();
    let constraint_tag = solver.new_constraint_tag();
    solver
        .add_constraint(pumpkin_solver::cumulative_with_options(
            start_times,
            durations,
            resource_requirements,
            rng.random_range(3..6),
            options,
            constraint_tag,
        ))
        .post();
}
