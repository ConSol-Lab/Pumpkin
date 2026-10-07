//! Compiles an example into a solver state and drives it with random decisions and backtracks. The
//! runtime checkers of the solver are the only oracles: they panic when a propagation, a fixpoint
//! or a solution is wrong, and the driver catches the panic.

use std::fmt::Display;
use std::fmt::Write;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::sync::Mutex;

use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateConstructor;
use pumpkin_core::rand::RngExt;
use pumpkin_core::rand::SeedableRng;
use pumpkin_core::rand::rngs::SmallRng;
use pumpkin_core::state::CurrentNogood;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;
use pumpkin_solver::Solver;
use pumpkin_solver::flatzinc::CompilationOptions;
use pumpkin_solver::flatzinc::parse_and_compile;

use crate::example::Example;
use crate::logging;

/// A step of a case: a decision on a variable, or a backtrack to an earlier checkpoint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Move {
    Decide {
        /// The FlatZinc name of the variable, or `#k` for the `k`-th variable without a name.
        variable: String,
        kind: DecisionKind,
        value: i32,
    },
    Backtrack {
        checkpoint: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DecisionKind {
    LowerBound,
    UpperBound,
    NotEqual,
    Equal,
}

impl DecisionKind {
    fn operator(self) -> &'static str {
        match self {
            DecisionKind::LowerBound => ">=",
            DecisionKind::UpperBound => "<=",
            DecisionKind::NotEqual => "!=",
            DecisionKind::Equal => "==",
        }
    }

    fn predicate(self, domain: DomainId, value: i32) -> Predicate {
        match self {
            DecisionKind::LowerBound => domain.lower_bound_predicate(value),
            DecisionKind::UpperBound => domain.upper_bound_predicate(value),
            DecisionKind::NotEqual => domain.disequality_predicate(value),
            DecisionKind::Equal => domain.equality_predicate(value),
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Move::Decide {
                variable,
                kind,
                value,
            } => write!(f, "{variable} {} {value}", kind.operator()),
            Move::Backtrack { checkpoint } => write!(f, "backtrack {checkpoint}"),
        }
    }
}

impl Move {
    /// Parses the notation of [`Display`]: `x >= 3`, `x <= 3`, `x != 3`, `x == 3` or
    /// `backtrack 1`.
    pub fn parse(text: &str) -> Option<Move> {
        let words = text.split_whitespace().collect::<Vec<_>>();
        match words.as_slice() {
            ["backtrack", checkpoint] => Some(Move::Backtrack {
                checkpoint: checkpoint.parse().ok()?,
            }),
            [variable, operator, value] => {
                let kind = match *operator {
                    ">=" => DecisionKind::LowerBound,
                    "<=" => DecisionKind::UpperBound,
                    "!=" => DecisionKind::NotEqual,
                    "==" => DecisionKind::Equal,
                    _ => return None,
                };
                Some(Move::Decide {
                    variable: (*variable).to_owned(),
                    kind,
                    value: value.parse().ok()?,
                })
            }
            _ => None,
        }
    }
}

/// The checker, or other part of the solver, that rejected what the solver did.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Oracle {
    /// The conflict checker of the rule rejected an inference.
    InferenceChecker,
    /// An inference is invalid in the solver state, independent of its rule: a premise is not true
    /// or became true after the propagation.
    InferenceTiming,
    /// An inference has a rule without a conflict checker.
    MissingChecker,
    /// A retention checker found that a propagator that reported a fixpoint was not at one.
    RetentionAfterCall,
    /// A retention checker found that a propagator was not called after its domains changed.
    RetentionAtFixpoint,
    /// The solution checker found that a full assignment violates a constraint.
    SolutionChecker,
    /// The solver panicked outside the checkers.
    Crash,
}

impl Oracle {
    fn classify(message: &str) -> Oracle {
        if message.contains("is invalid in the solver state") {
            Oracle::InferenceTiming
        } else if message.contains("missing checker for inference code") {
            Oracle::MissingChecker
        } else if message.contains("checker for inference code") {
            Oracle::InferenceChecker
        } else if message.contains("did not enqueue itself again after it was called") {
            Oracle::RetentionAfterCall
        } else if message.contains("Propagation reached a fixed point, but the retention checker") {
            Oracle::RetentionAtFixpoint
        } else if message.contains("The solver reported a solution") {
            Oracle::SolutionChecker
        } else {
            Oracle::Crash
        }
    }
}

/// A case that one of the oracles rejected.
#[derive(Clone, Debug)]
pub struct Failure {
    pub oracle: Oracle,
    /// The message of the panic, with the location it was raised at.
    pub message: String,
    /// What the solver logged at the error level during the failing move, such as what a
    /// retention checker expected.
    pub logs: Vec<String>,
    pub example: Example,
    /// The moves up to and including the one that failed.
    pub moves: Vec<Move>,
    /// The number of moves before shrinking.
    pub moves_before_shrinking: usize,
    /// The domains before the move that failed.
    pub domains_before: String,
    /// Whether replaying the shrunk moves on a fresh state fails in the same way.
    pub is_reproducible: bool,
}

impl Failure {
    /// Identifies failures that have the same cause, for grouping: the oracle, the constraint and
    /// the message without numbers, which name variables and values.
    pub fn group_key(&self) -> String {
        format!(
            "{:?} {} {}",
            self.oracle,
            self.example.constraint_name,
            message_shape(&self.message)
        )
    }
}

fn message_shape(message: &str) -> String {
    let first_line = message.lines().next().unwrap_or_default();
    first_line
        .chars()
        .filter(|character| !character.is_ascii_digit())
        .take(240)
        .collect()
}

#[derive(Clone, Copy, Debug)]
pub struct Configuration {
    pub cases: usize,
    pub steps: usize,
    pub seed: u64,
    /// The largest number of replays spent on shrinking one failure.
    pub shrink_budget: usize,
}

/// What fuzzing one example gave.
#[derive(Debug)]
pub enum ExampleOutcome {
    Fuzzed(Statistics),
    /// The compiler rejected the example, or does not support its constraint.
    Rejected(String),
    /// Propagation at the root found a conflict, so there is nothing to decide.
    InfeasibleAtRoot,
    Failed(Statistics, Box<Failure>),
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Statistics {
    pub cases: usize,
    pub decisions: usize,
    pub conflicts: usize,
    pub solutions: usize,
}

impl Statistics {
    pub fn add(&mut self, other: &Statistics) {
        self.cases += other.cases;
        self.decisions += other.decisions;
        self.conflicts += other.conflicts;
        self.solutions += other.solutions;
    }
}

static PANIC: Mutex<Option<String>> = Mutex::new(None);

/// Records the message of a panic instead of printing it, so that the report can quote it.
pub fn install_panic_hook() {
    std::panic::set_hook(Box::new(|info| {
        let location = info
            .location()
            .map(|location| format!(" (at {location})"))
            .unwrap_or_default();
        let payload = info
            .payload()
            .downcast_ref::<&str>()
            .map(|message| (*message).to_owned())
            .or_else(|| info.payload().downcast_ref::<String>().cloned())
            .unwrap_or_else(|| "a panic without a message".to_owned());
        if let Ok(mut panic) = PANIC.lock() {
            *panic = Some(format!("{payload}{location}"));
        }
    }));
}

fn take_panic_message() -> String {
    PANIC
        .lock()
        .ok()
        .and_then(|mut panic| panic.take())
        .unwrap_or_else(|| "a panic without a message".to_owned())
}

/// The solver state with the example compiled into it.
struct Session {
    state: State,
    /// Every domain with its label: the FlatZinc name, or `#k` for the `k`-th unnamed domain.
    domains: Vec<(String, DomainId)>,
    is_in_conflict: bool,
    statistics: Statistics,
}

enum Opening {
    Ready(Box<Session>),
    Rejected(String),
    InfeasibleAtRoot,
}

impl Session {
    fn open(example: &Example) -> Opening {
        let mut solver = Solver::default();
        if let Err(error) = parse_and_compile(
            &mut solver,
            example.source.as_bytes(),
            CompilationOptions::default(),
        ) {
            return Opening::Rejected(format!("compilation failed: {error}"));
        }

        let state = solver.into_state();
        let mut unnamed = 0;
        let domains = state
            .get_domain_ids()
            .map(|domain| {
                let label = match state.variable_name(domain) {
                    Some(name) => name.to_owned(),
                    None => {
                        unnamed += 1;
                        format!("#{}", unnamed - 1)
                    }
                };
                (label, domain)
            })
            .collect::<Vec<_>>();

        let mut session = Session {
            state,
            domains,
            is_in_conflict: false,
            statistics: Statistics::default(),
        };

        let start = session.state.trail().len();
        let result = session.state.propagate_to_fixed_point();
        session.explain_trail_from(start);
        if result.is_err() {
            return Opening::InfeasibleAtRoot;
        }

        Opening::Ready(Box::new(session))
    }

    fn domain(&self, label: &str) -> Option<DomainId> {
        self.domains
            .iter()
            .find(|(name, _)| name == label)
            .map(|&(_, domain)| domain)
    }

    fn is_fixed(&self, domain: DomainId) -> bool {
        self.state.lower_bound(domain) == self.state.upper_bound(domain)
    }

    fn apply(&mut self, applied_move: &Move) {
        match applied_move {
            Move::Decide {
                variable,
                kind,
                value,
            } => {
                if self.is_in_conflict {
                    return;
                }
                let Some(domain) = self.domain(variable) else {
                    return;
                };
                let predicate = kind.predicate(domain, *value);
                if self.state.truth_value(predicate).is_some() {
                    return;
                }

                self.statistics.decisions += 1;
                self.state.new_checkpoint();
                let start = self.state.trail().len();
                let _ = self
                    .state
                    .post(predicate)
                    .expect("a predicate that is not false can be posted");
                self.propagate_from(start);
            }

            Move::Backtrack { checkpoint } => {
                let current = self.state.get_checkpoint();
                let mut checkpoint = (*checkpoint).min(current);
                if self.is_in_conflict {
                    checkpoint = checkpoint.min(current.saturating_sub(1));
                }
                let _ = self.state.restore_to(checkpoint);
                self.is_in_conflict = false;

                let start = self.state.trail().len();
                self.propagate_from(start);
            }
        }
    }

    fn propagate_from(&mut self, start: usize) {
        let result = self.state.propagate_to_fixed_point();
        self.explain_trail_from(start);

        if result.is_err() {
            self.is_in_conflict = true;
            self.statistics.conflicts += 1;
        } else if self
            .domains
            .iter()
            .all(|&(_, domain)| self.is_fixed(domain))
        {
            self.statistics.solutions += 1;
            #[cfg(feature = "check-solutions")]
            self.state.check_solution();
        }
    }

    /// Computes the reason of every propagation on the trail from `start`, as conflict analysis
    /// would, so that the inference checkers also see the lazy reasons.
    fn explain_trail_from(&mut self, start: usize) {
        let predicates = self.state.trail().skip(start).collect::<Vec<_>>();
        let mut reason = vec![];
        for predicate in predicates {
            reason.clear();
            let _ =
                self.state
                    .get_propagation_reason(predicate, &mut reason, CurrentNogood::empty());
        }
    }

    fn random_move(&self, rng: &mut SmallRng) -> Option<Move> {
        let checkpoint = self.state.get_checkpoint();
        let unfixed = self
            .domains
            .iter()
            .filter(|&&(_, domain)| !self.is_fixed(domain))
            .collect::<Vec<_>>();

        if self.is_in_conflict || unfixed.is_empty() || (checkpoint > 0 && rng.random_bool(0.15)) {
            if checkpoint == 0 {
                return None;
            }
            let upper = if self.is_in_conflict || unfixed.is_empty() {
                checkpoint - 1
            } else {
                checkpoint
            };
            return Some(Move::Backtrack {
                checkpoint: rng.random_range(0..=upper),
            });
        }

        let (label, domain) = unfixed[rng.random_range(0..unfixed.len())];
        let lower_bound = self.state.lower_bound(*domain);
        let upper_bound = self.state.upper_bound(*domain);
        let values = (lower_bound..=upper_bound)
            .take(64)
            .filter(|&value| self.state.contains(*domain, value))
            .collect::<Vec<_>>();

        let (kind, value) = match rng.random_range(0..4) {
            0 => (
                DecisionKind::LowerBound,
                rng.random_range(lower_bound + 1..=upper_bound),
            ),
            1 => (
                DecisionKind::UpperBound,
                rng.random_range(lower_bound..upper_bound),
            ),
            2 => (
                DecisionKind::NotEqual,
                values[rng.random_range(0..values.len())],
            ),
            _ => (
                DecisionKind::Equal,
                values[rng.random_range(0..values.len())],
            ),
        };

        Some(Move::Decide {
            variable: label.clone(),
            kind,
            value,
        })
    }

    fn describe_domains(&self) -> String {
        let mut description = String::new();
        for (label, domain) in &self.domains {
            let lower_bound = self.state.lower_bound(*domain);
            let upper_bound = self.state.upper_bound(*domain);
            if label.starts_with('#') && lower_bound == upper_bound {
                continue;
            }
            let holes = (lower_bound..=upper_bound)
                .take(256)
                .filter(|&value| !self.state.contains(*domain, value))
                .map(|value| value.to_string())
                .collect::<Vec<_>>();
            write!(description, "  {label}: {lower_bound}..{upper_bound}")
                .expect("writing to a string succeeds");
            if !holes.is_empty() {
                write!(description, " \\ {{{}}}", holes.join(", "))
                    .expect("writing to a string succeeds");
            }
            description.push('\n');
        }
        description
    }
}

/// Fuzzes `example` with `configuration.cases` cases, each on a fresh state, and stops at the
/// first failure, which it shrinks.
pub fn fuzz_example(
    example: &Example,
    configuration: &Configuration,
    example_index: usize,
) -> ExampleOutcome {
    let mut statistics = Statistics::default();

    for case in 0..configuration.cases {
        let seed = configuration
            .seed
            .wrapping_mul(0x9E37_79B9_7F4A_7C15)
            .wrapping_add((example_index as u64) << 20)
            .wrapping_add(case as u64);
        let mut rng = SmallRng::seed_from_u64(seed);
        let mut moves = vec![];

        let _ = logging::take();
        let outcome = catch_unwind(AssertUnwindSafe(|| {
            let mut session = match Session::open(example) {
                Opening::Ready(session) => session,
                Opening::Rejected(reason) => return Err(ExampleOutcome::Rejected(reason)),
                Opening::InfeasibleAtRoot => return Err(ExampleOutcome::InfeasibleAtRoot),
            };
            for _ in 0..configuration.steps {
                let Some(next_move) = session.random_move(&mut rng) else {
                    break;
                };
                moves.push(next_move.clone());
                session.apply(&next_move);
            }
            Ok(session.statistics)
        }));

        match outcome {
            Ok(Ok(case_statistics)) => {
                statistics.add(&case_statistics);
                statistics.cases += 1;
            }
            Ok(Err(outcome)) => return outcome,
            Err(_) => {
                let message = take_panic_message();
                if moves.is_empty() && is_unsupported(&message) {
                    return ExampleOutcome::Rejected(message);
                }
                let failure = shrink(example, moves, message, configuration.shrink_budget);
                return ExampleOutcome::Failed(statistics, Box::new(failure));
            }
        }
    }

    ExampleOutcome::Fuzzed(statistics)
}

/// Whether the compiler panicked because it does not support a constraint, which is not a defect
/// that the fuzzer looks for.
fn is_unsupported(message: &str) -> bool {
    message.contains("unsupported constraint") || message.contains("is not implemented yet")
}

/// What replaying moves on a fresh state gave.
struct Replay {
    /// The message of the panic and the number of moves applied, including the failing one.
    panic: Option<(String, usize)>,
    domains_before_last: String,
    logs: Vec<String>,
}

fn replay_moves(example: &Example, moves: &[Move]) -> Replay {
    let mut applied = 0;
    let mut domains_before_last = String::new();
    let _ = logging::take();

    let outcome = catch_unwind(AssertUnwindSafe(|| {
        let Opening::Ready(mut session) = Session::open(example) else {
            return;
        };
        for next_move in moves {
            domains_before_last = session.describe_domains();
            applied += 1;
            session.apply(next_move);
        }
    }));

    let logs = logging::take();
    Replay {
        panic: outcome.err().map(|_| (take_panic_message(), applied)),
        domains_before_last,
        logs,
    }
}

/// Removes moves while the failure keeps occurring with the same oracle and message shape.
fn shrink(example: &Example, moves: Vec<Move>, message: String, budget: usize) -> Failure {
    let moves_before_shrinking = moves.len();
    let oracle = Oracle::classify(&message);
    let shape = message_shape(&message);
    let mut replays = 0;

    let fails_the_same = |candidate: &[Move], replays: &mut usize| -> Option<usize> {
        *replays += 1;
        let replay = replay_moves(example, candidate);
        replay.panic.and_then(|(message, applied)| {
            (Oracle::classify(&message) == oracle && message_shape(&message) == shape)
                .then_some(applied)
        })
    };

    let mut moves = moves;
    let is_reproducible = match fails_the_same(&moves, &mut replays) {
        Some(applied) => {
            moves.truncate(applied);
            true
        }
        None => false,
    };

    if is_reproducible {
        let mut chunk = moves.len().div_ceil(2).max(1);
        loop {
            let mut start = 0;
            while start < moves.len() && replays < budget {
                let end = (start + chunk).min(moves.len());
                let candidate = moves[..start]
                    .iter()
                    .chain(&moves[end..])
                    .cloned()
                    .collect::<Vec<_>>();
                match fails_the_same(&candidate, &mut replays) {
                    Some(applied) => {
                        moves = candidate;
                        moves.truncate(applied);
                    }
                    None => start += chunk,
                }
            }
            if chunk == 1 || replays >= budget {
                break;
            }
            chunk = chunk.div_ceil(2);
        }
    }

    let replay = replay_moves(example, &moves);
    let message = replay
        .panic
        .as_ref()
        .map(|(message, _)| message.clone())
        .unwrap_or(message);

    Failure {
        oracle: Oracle::classify(&message),
        message,
        logs: replay
            .logs
            .into_iter()
            .filter(|line| line.starts_with("[ERROR]") || line.starts_with("[WARN]"))
            .collect(),
        example: example.clone(),
        moves,
        moves_before_shrinking,
        domains_before: replay.domains_before_last,
        is_reproducible,
    }
}

/// Replays `moves`, one per line in the notation of [`Move`], on the FlatZinc instance `source`,
/// and panics like the solver does if a checker rejects what the solver does. Meant for regression
/// tests that the fuzzer writes.
pub fn replay(source: &str, moves: &str) {
    let example = Example {
        origin: "replay".to_owned(),
        constraint_name: String::new(),
        source: source.to_owned(),
        occurrences: 1,
    };
    let moves = moves
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| Move::parse(line).unwrap_or_else(|| panic!("'{line}' is not a move")))
        .collect::<Vec<_>>();

    let Opening::Ready(mut session) = Session::open(&example) else {
        return;
    };
    for next_move in &moves {
        session.apply(next_move);
    }
}
