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
compiler, so every constraint the solver can compile can be fuzzed without further code.

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
- the solution checkers, whenever every variable is fixed.

Any other panic is reported as a crash. The `fuzz` profile is `release` with unwinding panics,
because the checkers report by panicking.

## Failures

A failing case is shrunk by removing moves while the same oracle fails with the same message.
Failures with the same cause are grouped. Each group names the oracle, quotes the checker, and
gives the example with its origin, the moves, the domains before the failing move, a command that
replays it, and a regression test that calls `pumpkin_fuzzer::replay`. The instance and the moves
are also written to `--out` (default `target/fuzzer`).

Cases are independent and seeded (`--seed`), so a failure is reproduced by its moves alone.
