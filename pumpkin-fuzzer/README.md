# pumpkin-fuzzer

Randomised testing of Pumpkin propagators. The checkers of a rule are the ground truth; the
propagator is what is tested.

```bash
cargo run -p pumpkin-fuzzer --profile fuzz --features checks -- \
    pumpkin-solver/tests/mzn_constraints pumpkin-solver/tests/mzn_search \
    pumpkin-solver/tests/mzn_optimization pumpkin-solver/tests/mzn_infeasible \
    --mutants 5 --random 3000
```

## Examples

An example is a FlatZinc instance with one constraint, compiled by the solver's own FlatZinc
compiler, so every constraint the solver can compile can be fuzzed without further code. A
propagator without a FlatZinc name is fuzzed from Rust code (see the last section).

- **Extracted**: every constraint of the given FlatZinc files, with the declarations it refers to.
  Constraints equal up to the names of variables are collapsed.
- **Random** (`--random N`): generated from the signatures in `generation.rs`, which list the
  argument kinds of each constraint. `--list-constraints` lists them.
- **Mutants** (`--mutants N` per extracted example): one domain narrowed, given a hole or fixed, or
  one constant changed while keeping its sign.

`--constraint NAME` restricts all three to some constraints.

## Cases

Each case starts from a freshly compiled state and makes random moves: a decision (a bound, a
removal or an assignment on an unfixed variable) followed by propagation to a fixpoint, or a
backtrack to an earlier checkpoint. After each propagation the reason of every new trail entry is
computed, as conflict analysis would, so lazy explanations are checked as well.

## Oracles

Only the runtime checkers of the solver, compiled in through the features of this crate (`checks`
enables `check-inferences`, `check-retention` and `check-solutions`; `check-inferences-proof` and
`check-retention-all` are available separately):

- the conflict checkers, on every inference and every reported conflict;
- the retention checkers, after every call of a propagator and at every fixpoint;
- the solution checkers, whenever every variable is fixed;
- the rule self-test, whenever every variable is fixed and the solution checkers accept the
  assignment: no conflict checker may report that assignment as a conflict, since a solution is no
  conflict of a sound rule. It needs `check-solutions` and `check-inferences`.

Any other panic is reported as a crash. The `fuzz` profile is `release` with unwinding panics,
because the checkers report by panicking.

## Parameters

A propagator whose behaviour depends on settings that are not part of its constraint, such as the
explanation type of the cumulative, implements `PropagatorParameters`: `all()` lists every setting,
and `is_legal` rejects the combinations it does not support (only needed when there are any). Each
case draws one legal setting. `--parameters TEXT` keeps only the settings whose `Debug` text
contains `TEXT`; given more than once, a setting has to contain every text:

```bash
cargo run -p pumpkin-fuzzer --profile fuzz --features checks -- --constraint pumpkin_cumulative \
    --random 500 --parameters "explanation_type: Naive" --parameters "generate_sequence: false"
```

A failure report names the setting, and its replay command and regression test pass it on.

## Failures

A failing case is shrunk by removing moves while the same oracle fails with the same message.
Failures with the same cause are grouped. Each group names the oracle, quotes the checker, and
gives the example with its origin, the moves, the domains before the failing move, a command that
replays it, and a regression test that calls `pumpkin_fuzzer::replay`. The instance and the moves
are also written to `--out` (default `target/fuzzer`).

Cases are independent and seeded (`--seed`), so a failure is reproduced by its moves alone.

## Fuzzing a new propagator

The propagator needs a rule with a conflict checker and a retention checker, and a constraint
description with `check_solution`; these are the oracles. Then:

1. **From FlatZinc.** Add an arm for the constraint to `post_constraints.rs` of the FlatZinc
   compiler in `pumpkin-solver`. Examples are then extracted from every FlatZinc file that uses it.
2. **Random examples.** Add a line to `SIGNATURES` in `generation.rs` with the argument kinds of the
   constraint (documented there), and fuzz it with `--constraint NAME --random N`.
3. **Parameters.** If the propagator has settings, implement `PropagatorParameters` for them, and
   add an arm to `settings` in `parameters.rs` that sets them in the `CompilationOptions`.
4. Run the fuzzer on the constraint alone first; every failure group comes with a regression test.

A propagator that cannot be built from FlatZinc is fuzzed from Rust code instead, in a test of this
crate. A build function creates the variables and posts the propagator, drawing what it needs from
the random generator; each example has its own seed:

```rust
fn build(rng: &mut SmallRng, solver: &mut Solver, options: MyOptions) {
    let x = solver.new_named_bounded_integer(0, rng.random_range(0..5), "x");
    let y = solver.new_named_bounded_integer(0, 5, "y");
    let constraint_tag = solver.new_constraint_tag();
    let _ = solver.add_propagator(MyConstructor { x, y, options, constraint_tag });
}

#[test]
fn my_propagator() {
    let configuration = Configuration { cases: 10, ..Configuration::default() };
    pumpkin_fuzzer::fuzz_propagator_with_parameters("my_propagator", 100, &configuration, build)
        .assert_no_failures();
}
```

`fuzz_propagator` takes a build function without parameters. A failure report gives a regression
test that calls `replay_propagator` with the build function, the seed and the moves. See
`tests/propagator_fuzzing.rs`.

## Mutation testing

[cargo-mutants](https://mutants.rs) checks whether the oracles detect defects in one propagator and
its checkers: it changes their code one mutation at a time and runs a fuzzer test on each. A mutant
that survives is a weak checker, a gap in the examples, or a change without effect. Run it in a
separate worktree, since `--in-place` edits the sources:

```bash
cargo mutants --in-place --baseline skip --timeout 300 \
    -f 'pumpkin-crates/propagators/src/propagators/arithmetic/linear_less_or_equal/propagator.rs' \
    -f 'pumpkin-crates/checking/src/checkers/linear_less_or_equal/*.rs' \
    --test-package pumpkin-fuzzer --profile fuzz --features pumpkin-fuzzer/checks \
    -- --test propagator_fuzzing -- linear_less_or_equal_built_in_rust
```

`--baseline skip` is needed because cargo-mutants 27 builds the baseline with the mutated packages
instead of `--test-package`, which lack the `checks` feature; run the test once unmutated instead.
