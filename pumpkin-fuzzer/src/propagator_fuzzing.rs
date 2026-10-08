//! Fuzzing a propagator from Rust code, for propagators that cannot be built from FlatZinc. A build
//! function creates the variables and posts the propagator in a [`Solver`], drawing what it needs,
//! such as the domains, from a random generator; every example has its own seed.
//!
//! ```ignore
//! let outcome = pumpkin_fuzzer::fuzz_propagator("my_constraint", 200, &configuration, |rng, solver| {
//!     let x = solver.new_named_bounded_integer(0, rng.random_range(0..5), "x");
//!     let y = solver.new_named_bounded_integer(0, 5, "y");
//!     let constraint_tag = solver.new_constraint_tag();
//!     let _ = solver.add_propagator(MyConstructor::new(x, y, constraint_tag));
//! });
//! outcome.assert_no_failures();
//! ```

use std::rc::Rc;

use pumpkin_core::propagation::PropagatorParameters;
use pumpkin_core::rand::rngs::SmallRng;
use pumpkin_solver::Solver;

use crate::driver::Configuration;
use crate::driver::ExampleOutcome;
use crate::driver::Failure;
use crate::driver::Statistics;
use crate::driver::fuzz_example;
use crate::driver::replay_example;
use crate::example::BuildFunction;
use crate::example::Builder;
use crate::example::Example;
use crate::report;

/// What fuzzing a propagator gave.
#[derive(Debug, Default)]
pub struct PropagatorFuzzing {
    pub statistics: Statistics,
    /// The number of examples in which propagation at the root found a conflict.
    pub infeasible_at_root: usize,
    pub failures: Vec<Failure>,
}

impl PropagatorFuzzing {
    /// Panics with the description of every group of failures, if there are any, or when no case
    /// was run, since then nothing was tested.
    pub fn assert_no_failures(&self) {
        assert!(
            self.statistics.cases > 0,
            "no case was run: every example is infeasible at the root"
        );
        if self.failures.is_empty() {
            return;
        }

        let groups = report::group(self.failures.clone());
        let descriptions = groups
            .iter()
            .enumerate()
            .map(|(number, group)| {
                report::describe(group, number + 1, None)
                    .expect("a description without files cannot fail")
            })
            .collect::<Vec<_>>();
        panic!(
            "{} failing examples in {} groups:\n\n{}",
            self.failures.len(),
            groups.len(),
            descriptions.join("\n")
        );
    }
}

/// Fuzzes the propagator that `build` posts, on `examples` examples of `configuration.cases`
/// cases each. `name` names the propagator in reports.
pub fn fuzz_propagator(
    name: &str,
    examples: usize,
    configuration: &Configuration,
    build: impl Fn(&mut SmallRng, &mut Solver) + 'static,
) -> PropagatorFuzzing {
    let build = Rc::new(move |rng: &mut SmallRng, solver: &mut Solver, _: &str| build(rng, solver));
    fuzz_builder(name, examples, configuration, vec![], build)
}

/// [`fuzz_propagator`] for a propagator with [`PropagatorParameters`]: every case draws a legal
/// setting, which `build` passes to the propagator.
pub fn fuzz_propagator_with_parameters<Parameters: PropagatorParameters + 'static>(
    name: &str,
    examples: usize,
    configuration: &Configuration,
    build: impl Fn(&mut SmallRng, &mut Solver, Parameters) + 'static,
) -> PropagatorFuzzing {
    let setting_names = Parameters::all_legal()
        .iter()
        .map(|parameters| format!("{parameters:?}"))
        .collect();
    fuzz_builder(
        name,
        examples,
        configuration,
        setting_names,
        with_parameters(build),
    )
}

/// Replays `moves`, one per line, on the example that `build` posts with `seed`, as a failure
/// report of [`fuzz_propagator`] gives them, and panics like the solver does if a checker rejects
/// what the solver does.
pub fn replay_propagator(
    build: impl Fn(&mut SmallRng, &mut Solver) + 'static,
    seed: u64,
    moves: &str,
) {
    let build = Rc::new(move |rng: &mut SmallRng, solver: &mut Solver, _: &str| build(rng, solver));
    replay_example(&builder_example(seed, vec![], build), None, moves);
}

/// [`replay_propagator`] with the setting of the parameters named `parameters`, as a failure report
/// of [`fuzz_propagator_with_parameters`] gives it.
pub fn replay_propagator_with_parameters<Parameters: PropagatorParameters + 'static>(
    build: impl Fn(&mut SmallRng, &mut Solver, Parameters) + 'static,
    seed: u64,
    parameters: &str,
    moves: &str,
) {
    let setting_names = Parameters::all_legal()
        .iter()
        .map(|parameters| format!("{parameters:?}"))
        .collect();
    let example = builder_example(seed, setting_names, with_parameters(build));
    replay_example(&example, Some(parameters), moves);
}

/// The build function that finds the setting by its name and passes it to `build`.
fn with_parameters<Parameters: PropagatorParameters + 'static>(
    build: impl Fn(&mut SmallRng, &mut Solver, Parameters) + 'static,
) -> BuildFunction {
    Rc::new(move |rng: &mut SmallRng, solver: &mut Solver, name: &str| {
        let parameters = Parameters::all_legal()
            .into_iter()
            .find(|parameters| format!("{parameters:?}") == name)
            .unwrap_or_else(|| panic!("'{name}' is not a legal setting of the parameters"));
        build(rng, solver, parameters);
    })
}

fn builder_example(seed: u64, setting_names: Vec<String>, build: BuildFunction) -> Example {
    Example {
        origin: "replay".to_owned(),
        constraint_name: String::new(),
        source: String::new(),
        occurrences: 1,
        builder: Some(Builder {
            seed,
            setting_names,
            build,
        }),
    }
}

fn fuzz_builder(
    name: &str,
    examples: usize,
    configuration: &Configuration,
    setting_names: Vec<String>,
    build: BuildFunction,
) -> PropagatorFuzzing {
    let mut outcome = PropagatorFuzzing::default();

    for index in 0..examples {
        let seed = configuration
            .seed
            .wrapping_mul(0x2545_F491_4F6C_DD1D)
            .wrapping_add(index as u64);
        let mut example = builder_example(seed, setting_names.clone(), Rc::clone(&build));
        example.origin = format!("example {index} of '{name}'");
        example.constraint_name = name.to_owned();

        match fuzz_example(&example, configuration, index) {
            ExampleOutcome::Fuzzed(statistics) => outcome.statistics.add(&statistics),
            ExampleOutcome::InfeasibleAtRoot => outcome.infeasible_at_root += 1,
            ExampleOutcome::Rejected(reason) => {
                panic!("the example {index} of '{name}' was rejected: {reason}")
            }
            ExampleOutcome::Failed(statistics, failure) => {
                outcome.statistics.add(&statistics);
                outcome.failures.push(*failure);
            }
        }
    }

    outcome
}
