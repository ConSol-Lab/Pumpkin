use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use clap::Parser;
use pumpkin_core::rand::SeedableRng;
use pumpkin_core::rand::rngs::SmallRng;
use pumpkin_fuzzer::driver::Configuration;
use pumpkin_fuzzer::driver::ExampleOutcome;
use pumpkin_fuzzer::driver::Statistics;
use pumpkin_fuzzer::driver::install_panic_hook;
use pumpkin_fuzzer::example::Example;
use pumpkin_fuzzer::extraction;
use pumpkin_fuzzer::generation;
use pumpkin_fuzzer::logging;
use pumpkin_fuzzer::report;

/// Fuzzes the propagators of Pumpkin against the checkers of their rules.
///
/// Run it with the checkers compiled in and with unwinding panics:
/// `cargo run -p pumpkin-fuzzer --profile fuzz --features checks -- <PATHS>`.
#[derive(Debug, Parser)]
struct Arguments {
    /// FlatZinc files, or directories that are searched for them; every constraint becomes an
    /// example.
    paths: Vec<PathBuf>,

    /// The number of random examples to generate.
    #[arg(long, default_value_t = 0)]
    random: usize,

    /// The number of mutants to make of every extracted example.
    #[arg(long, default_value_t = 0)]
    mutants: usize,

    /// Fuzz only these constraints, by FlatZinc name. Repeatable.
    #[arg(long)]
    constraint: Vec<String>,

    /// Draw only the settings of the parameters of a propagator, such as the explanation type of
    /// the cumulative, whose `Debug` text contains this text. Repeatable: a setting has to contain
    /// every text. With `--replay`, it selects the setting, as a failure report gives it.
    #[arg(long)]
    parameters: Vec<String>,

    /// The number of cases per example; each starts from a freshly compiled state.
    #[arg(long, default_value_t = 20)]
    cases: usize,

    /// The number of moves per case.
    #[arg(long, default_value_t = 40)]
    steps: usize,

    #[arg(long, default_value_t = 0)]
    seed: u64,

    /// Stop starting new examples after this many seconds.
    #[arg(long)]
    time_limit: Option<u64>,

    /// The largest number of replays spent on shrinking one failure.
    #[arg(long, default_value_t = 300)]
    shrink_budget: usize,

    /// Where the reproducers of the failures are written.
    #[arg(long, default_value = "target/fuzzer")]
    out: PathBuf,

    /// Print the descriptions of at most this many failure groups.
    #[arg(long, default_value_t = 20)]
    max_reported: usize,

    /// List the constraints that random examples are generated for, and stop.
    #[arg(long)]
    list_constraints: bool,

    /// Replay the moves in `--moves` on this FlatZinc instance, and stop.
    #[arg(long, requires = "moves")]
    replay: Option<PathBuf>,

    #[arg(long)]
    moves: Option<PathBuf>,
}

fn main() {
    let arguments = Arguments::parse();
    logging::init();

    if arguments.list_constraints {
        for name in generation::constraint_names() {
            println!("{name}");
        }
        return;
    }

    if let (Some(instance), Some(moves)) = (&arguments.replay, &arguments.moves) {
        let source = std::fs::read_to_string(instance).expect("the instance can be read");
        let moves = std::fs::read_to_string(moves).expect("the moves can be read");
        match arguments.parameters.as_slice() {
            [] => pumpkin_fuzzer::replay(&source, &moves),
            [parameters] => pumpkin_fuzzer::replay_with_parameters(&source, parameters, &moves),
            _ => panic!("a replay takes one setting of the parameters"),
        }
        println!("The replay did not fail.");
        return;
    }

    print_active_checks();

    let examples = collect_examples(&arguments);
    println!("Fuzzing {} examples.", examples.len());

    install_panic_hook();
    let configuration = Configuration {
        cases: arguments.cases,
        steps: arguments.steps,
        seed: arguments.seed,
        shrink_budget: arguments.shrink_budget,
        parameter_filters: arguments.parameters.clone(),
    };

    let start = Instant::now();
    let time_limit = arguments.time_limit.map(Duration::from_secs);
    let mut statistics = Statistics::default();
    let mut failures = vec![];
    let mut fuzzed = 0;
    let mut infeasible_at_root = 0;
    let mut rejected = std::collections::BTreeMap::<String, usize>::new();

    for (index, example) in examples.iter().enumerate() {
        if time_limit.is_some_and(|limit| start.elapsed() > limit) {
            println!("The time limit was reached after {index} examples.");
            break;
        }

        match pumpkin_fuzzer::driver::fuzz_example(example, &configuration, index) {
            ExampleOutcome::Fuzzed(example_statistics) => {
                fuzzed += 1;
                statistics.add(&example_statistics);
            }
            ExampleOutcome::Rejected(reason) => {
                let reason = reason.lines().next().unwrap_or_default().to_owned();
                *rejected
                    .entry(format!("{}: {reason}", example.constraint_name))
                    .or_default() += 1;
            }
            ExampleOutcome::InfeasibleAtRoot => infeasible_at_root += 1,
            ExampleOutcome::Failed(example_statistics, failure) => {
                fuzzed += 1;
                statistics.add(&example_statistics);
                failures.push(*failure);
            }
        }
    }

    let failing_examples = failures.len();
    let groups = report::group(failures);
    for (number, group) in groups.iter().enumerate().take(arguments.max_reported) {
        match report::describe(group, number + 1, &arguments.out) {
            Ok(description) => println!("{description}"),
            Err(error) => println!(
                "Failure {}: the reproducer could not be written: {error}",
                number + 1
            ),
        }
    }

    println!("=== Summary after {:.1} s", start.elapsed().as_secs_f64());
    println!(
        "{fuzzed} examples fuzzed, {infeasible_at_root} infeasible at the root, {} rejected.",
        rejected.values().sum::<usize>()
    );
    println!(
        "{} cases, {} decisions, {} conflicts, {} full assignments.",
        statistics.cases, statistics.decisions, statistics.conflicts, statistics.solutions
    );
    println!(
        "{failing_examples} failing examples in {} groups:",
        groups.len()
    );
    for (number, group) in groups.iter().enumerate() {
        println!(
            "  {}. {:?} on '{}' ({} examples)",
            number + 1,
            group.representative.oracle,
            group.representative.example.constraint_name,
            group.count
        );
    }
    if !rejected.is_empty() {
        println!("Rejected examples:");
        for (reason, count) in &rejected {
            println!("  {count} x {reason}");
        }
    }
}

fn print_active_checks() {
    let checks = [
        ("check-inferences", cfg!(feature = "check-inferences")),
        (
            "check-inferences-proof",
            cfg!(feature = "check-inferences-proof"),
        ),
        ("check-retention", cfg!(feature = "check-retention")),
        ("check-retention-all", cfg!(feature = "check-retention-all")),
        ("check-solutions", cfg!(feature = "check-solutions")),
    ]
    .into_iter()
    .filter(|&(_, is_active)| is_active)
    .map(|(name, _)| name)
    .collect::<Vec<_>>();

    if checks.is_empty() {
        println!(
            "No checkers are compiled in, so only crashes are found. Enable them with \
             `--features checks`."
        );
    } else {
        println!("Active checks: {}.", checks.join(", "));
    }
}

fn collect_examples(arguments: &Arguments) -> Vec<Example> {
    let mut examples = vec![];

    let instances = extraction::find_instances(&arguments.paths).expect("the paths can be read");
    for instance in &instances {
        let source = std::fs::read_to_string(instance).expect("the instance can be read");
        examples.extend(extraction::extract(
            &instance.display().to_string(),
            &source,
        ));
    }
    examples.retain(|example| {
        arguments.constraint.is_empty() || arguments.constraint.contains(&example.constraint_name)
    });
    let extracted = examples.len();
    let mut examples = extraction::deduplicate(examples);
    if !instances.is_empty() {
        println!(
            "{extracted} constraints in {} instances, {} distinct up to naming.",
            instances.len(),
            examples.len()
        );
    }

    let mut rng = SmallRng::seed_from_u64(arguments.seed);
    let mut mutants = vec![];
    for example in &examples {
        for index in 0..arguments.mutants {
            if let Some(mutant) = generation::mutate(
                &mut rng,
                example,
                format!("mutant {index} of {}", example.origin),
            ) {
                mutants.push(mutant);
            }
        }
    }
    examples.extend(mutants);

    for index in 0..arguments.random {
        if let Some(example) = generation::random_example(
            &mut rng,
            &arguments.constraint,
            format!("random example {index} with seed {}", arguments.seed),
        ) {
            examples.push(example);
        }
    }

    extraction::deduplicate(examples)
}
