# Integration branch: checkers and croc proof logging

**Unreviewed and unstable.** This branch exists to try the two lines of work together; nothing
on it has been reviewed, and it is not meant to be merged as it is.

## What is in it

- `features/checkers-15-ci-checks`: constraint descriptions, conflict rules with conflict and
  retention checkers, the rule store, `PropagatorWithEvents`, `PUMPKIN_CHECK_RULES`, the
  propagator fuzzer, the CI checks, and the FlatZinc compiler in the `pumpkin-solver` library.
- `upstream/main` up to `9e1bd00a`, with the tests of the watcher fix moved into
  `propagators/nogoods/tests.rs` and the fix gated on `inference-checkers`.
- The croc stack (`pr/01` to `pr/13`): croc proof logging (`--proof-format croc`,
  `--proof-type blocks`, scaffolds headed by a preamble), the view pass, the proof processor
  reading croc scaffolds (deduction databases, `--root-domains` with the header `m lenient`,
  `--omit-premises`), the `lex_less_or_equal` propagator and constraint, opt-in symmetry
  breaking (lex-leader, value symmetry), dominance breaking and dual fixing.
- Two steps on top of the checkers stack, `features/checkers-16-scope-item-derive` and
  `features/checkers-17-complete-solution`:
  - `#[derive(ScopeItem)]`, from the new crate `pumpkin-core-derive`, replaces the
    `scoped_struct!` macro: the struct keeps its own definition and gets the derive.
  - The trait `DomainView` is renamed `DomainContext` (file `domain_context.rs`).
  - `ConstraintDescription::check_solution` takes a `CompleteSolution`, an assignment in which
    every domain is fixed (from `State::complete_solution`), and returns `ConstraintSatisfied` or
    `ConstraintViolated`; `SolutionCheck::Unknown` is gone. Under `check-solutions`, a reported
    solution with a domain that is not fixed fails with that domain. The `lex_less_or_equal`
    description and the processor's deduction descriptions follow these changes.

## How the two were joined

- The FlatZinc compiler keeps the lead's move into the library. `CompilationOptions` holds the
  compile options of both sides. The logged proof is one option, `proof_logging`, of the enum
  `ProofLogging`: `Disabled` (the default), `Drcp { proof_type }` or
  `Croc { proof_type, applied_preamble }`. `Disabled` and `Croc` run the view pass and allow the
  breaking passes; `Drcp` skips them and refuses `var b = a`. A library caller with default
  options, such as the fuzzer, takes the `Disabled` path. `is_logging_full_proof` is a method
  over `proof_logging`. The solver maps `--proof-path`, `--proof-format` and `--proof-type` to
  the enum and refuses `--proof-format croc --proof-type full`.
  `parse_and_compile` returns `FlatZincInstance`, which now also carries `view_statistics`.
- The generalised variable and value selectors are re-applied in `pumpkin-crates/branching`.
- `lex_less_or_equal` has the per-propagator folder, a constraint description, a conflict and a
  retention checker in `pumpkin-checking`, and a fuzzer test built in Rust.
- The processor's deduction propagators and databases use a processor rule, `DeductionRule`
  (`pumpkin-proof-processor/src/deduction_rule.rs`): the conflict checker of a nogood under the
  name `nogood`, and no retention check, since the processor splits conflict detection from
  propagation and deactivates deductions while trimming.
- A block proof takes the inner rule of the solver's `half_reified(R)` rule names, since it
  states the reification of a constraint itself.

## Tests

The CI command (`cargo test --release --no-fail-fast` with `pumpkin-solver/check-inferences`,
`pumpkin-core/check-retention`, `check-solutions`, `check-deductions`, and `RUSTFLAGS=-Dwarnings`),
with `CROC_CHECK` naming `croc_check.exe` of fzn-drcp-check, passes except for two tests that
also fail on the checkers branch merged with `upstream/main`, without the croc stack:

- `wcnf_test::riskmap`: the solution check of a nogood over unfixed variables (the MaxSAT fix
  the CI commit mentions is not on the branch).
- `process_scaffolds::ttp_n10_k3`: the retention checker of `binary_equals` on scaled views
  (`10 * x9 = x973 + 9`). The processor now compiles the model with the solver's compiler, so
  it posts this propagator; the solver of the checkers branch fails the same way on this model.

`cargo +nightly fmt --check`, `cargo +nightly clippy --all-targets` and `cargo doc --no-deps`
are clean. The fuzzer campaign of its README finds 50 failure groups, none of a kind that the
same campaign on the checkers branch does not find (60 groups there). The WASM, Python and
`cargo deny` jobs were not run.

## Known weak spots

- `ghoulomb_3_5_11` (optimisation and infeasible tests) can time out under load.
- The view pass is very slow on one challenge instance (2025 `work-task-variation`, seed 10):
  compilation does not finish, and the time limit and SIGTERM are not honoured during
  compilation.
- An unbounded `var int` under croc logging takes the solver's default bounds, which the croc
  checker rejects.
- The deduction databases of the processor take the first constraint tag for their own rule,
  under which they make no inferences, so that the identifiers of the proof do not shift.
