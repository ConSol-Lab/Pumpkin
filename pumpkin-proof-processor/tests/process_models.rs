#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

//! The solver writes a scaffold, headed by its preamble, for a model with `--proof-format croc`,
//! and the processor, given only the model and the scaffold, turns it into a proof in the block
//! format, as resolution chains and as refutations, and each also for the checker's lenient reading
//! within the root domains (`--root-domains`), which the proof's header `m lenient` names. When the
//! environment variable `CROC_CHECK` names the fzn-croc-check binary, the proofs are checked, in
//! the reading their header names.

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
#[cfg(unix)]
use std::process::Stdio;
use std::sync::OnceLock;

macro_rules! process_model {
    ($name:ident, $model:literal, $conclusion:expr $(, $flag:literal)*) => {
        #[test]
        fn $name() {
            run_pipeline(stringify!($name), $model, $conclusion, &[$($flag),*]);
        }
    };
}

// Sparse domains and an all-different, unsatisfiable.
process_model!(sparse_all_different, "sparse_all_different", "c UNSAT");

// A table with its decomposition line, a reified inequality, a clause and an element, minimised.
process_model!(
    table_reified_element,
    "table_reified_element",
    "c LOWER_BOUND 1"
);
process_model!(
    table_reified_element_declared,
    "table_reified_element",
    "c LOWER_BOUND 1",
    "--declare-replacements",
    "--literal-predicates"
);

// Multiplication, absolute value and an unbounded integer, unsatisfiable.
process_model!(times_abs_unbounded, "times_abs_unbounded", "c UNSAT");

// Views of every kind (a definition, a two-term equality, bool2int, a parameter), a fixed
// variable and two booleans that reify comparisons with a constant, maximised.
process_model!(
    views_literals_constants,
    "views_literals_constants",
    "c UPPER_BOUND 28"
);
process_model!(
    views_literals_constants_declared,
    "views_literals_constants",
    "c UPPER_BOUND 28",
    "--declare-replacements",
    "--literal-predicates"
);
process_model!(
    views_literals_constants_literals,
    "views_literals_constants",
    "c UPPER_BOUND 28",
    "--literal-predicates"
);

// An objective that the view pass makes a view of another variable.
process_model!(objective_view, "objective_view", "c UPPER_BOUND 28");
// Two definitions under declared replacements: the head numbers both before the first
// declaration, as the checker requires.
process_model!(
    objective_view_declared,
    "objective_view",
    "c UPPER_BOUND 28",
    "--declare-replacements",
    "--literal-predicates"
);

// The symmetry and dominance breaking steps, which the preamble states with the constraints the
// solver posts for them.
process_model!(
    symmetry_star,
    "symmetry_star",
    "c UPPER_BOUND 5",
    "--symmetry-breaking"
);
process_model!(
    symmetry_star_declared,
    "symmetry_star",
    "c UPPER_BOUND 5",
    "--symmetry-breaking",
    "--declare-replacements",
    "--literal-predicates"
);
process_model!(
    dominance_knapsack,
    "dominance_knapsack",
    "c UPPER_BOUND 24",
    "--dominance-breaking"
);
process_model!(
    dual_fixing_knapsack,
    "dual_fixing_knapsack",
    "c UPPER_BOUND 24",
    "--dual-fixing"
);
process_model!(
    value_symmetry_colouring,
    "value_symmetry_colouring",
    "c UNSAT",
    "--value-symmetry-breaking"
);

// The breaking steps whose constraint no kept inference cites, left out on request.
#[test]
fn symmetry_star_dropping_unused_steps() {
    let name = "symmetry_star_dropping_unused_steps";
    let (model_path, scaffold_path) = solve(name, "symmetry_star", &["--symmetry-breaking"]);
    for derivation in ["resolution", "refutation"] {
        let _ = process_and_check(
            name,
            &model_path,
            &scaffold_path,
            derivation,
            "c UPPER_BOUND 5",
            &["--drop-unused-breaking-steps"],
        );
    }
}

// Some of the dominance steps, and the dual fixing step, whose `w` line the proof then lacks.
#[test]
fn dominance_knapsack_dropping_unused_steps() {
    let name = "dominance_knapsack_dropping_unused_steps";
    let (model_path, scaffold_path) = solve(name, "dominance_knapsack", &["--dominance-breaking"]);
    let left_out = regex::Regex::new(r"Leaving out [1-9][0-9]* of [0-9]+ held steps").unwrap();
    for derivation in ["resolution", "refutation"] {
        let (log, _) = process_and_check(
            name,
            &model_path,
            &scaffold_path,
            derivation,
            "c UPPER_BOUND 24",
            &["--drop-unused-breaking-steps"],
        );
        assert!(left_out.is_match(&log), "no step is left out:\n{log}");
    }
}

#[test]
fn dual_fixing_knapsack_dropping_unused_steps() {
    let name = "dual_fixing_knapsack_dropping_unused_steps";
    let (model_path, scaffold_path) = solve(name, "dual_fixing_knapsack", &["--dual-fixing"]);
    for derivation in ["resolution", "refutation"] {
        let (_, proof) = process_and_check(
            name,
            &model_path,
            &scaffold_path,
            derivation,
            "c UPPER_BOUND 24",
            &["--drop-unused-breaking-steps"],
        );
        assert!(
            !proof.lines().any(|line| line.starts_with("w ")),
            "the dual fixing step is not left out"
        );
    }
}

// The scaffold read from a pipe while the solver writes it.
#[cfg(unix)]
#[test]
fn objective_view_through_a_pipe() {
    let model_path = format!(
        "{}/../pumpkin-solver/tests/croc_models/objective_view.fzn",
        env!("CARGO_MANIFEST_DIR")
    );
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("process_models");
    std::fs::create_dir_all(&directory).expect("can create the output directory");
    let pipe_path = directory.join("objective_view_through_a_pipe.scaffold");
    let _ = std::fs::remove_file(&pipe_path);
    let made = Command::new("mkfifo").arg(&pipe_path).status().unwrap();
    assert!(made.success(), "mkfifo makes the pipe");

    let mut solver_process = Command::new(solver())
        .arg("--proof-path")
        .arg(&pipe_path)
        .args(["--proof-format", "croc"])
        .arg(&model_path)
        .stdout(Stdio::null())
        .spawn()
        .unwrap();

    let _ = process_and_check(
        "objective_view_through_a_pipe",
        &model_path,
        &pipe_path,
        "refutation",
        "c UPPER_BOUND 28",
        &[],
    );

    assert!(solver_process.wait().unwrap().success());
    std::fs::remove_file(&pipe_path).expect("the pipe is removed");
}

/// The solver binary, built once for all tests.
fn solver() -> &'static Path {
    static SOLVER: OnceLock<PathBuf> = OnceLock::new();
    SOLVER.get_or_init(|| {
        escargot::CargoBuild::new()
            .package("pumpkin-solver")
            .bin("pumpkin-solver")
            .current_target()
            .current_release()
            .run()
            .expect("the solver builds")
            .path()
            .to_path_buf()
    })
}

fn run_pipeline(name: &str, model: &str, conclusion: &str, solver_flags: &[&str]) {
    let (model_path, scaffold_path) = solve(name, model, solver_flags);
    for derivation in ["resolution", "refutation"] {
        let readings: &[&[&str]] = if derivation == "refutation" {
            &[
                &[],
                &["--root-domains"],
                &["--root-domains", "--omit-premises"],
            ]
        } else {
            &[&[], &["--root-domains"]]
        };
        for &processor_flags in readings {
            let _ = process_and_check(
                name,
                &model_path,
                &scaffold_path,
                derivation,
                conclusion,
                processor_flags,
            );
        }
    }
}

/// Solve the model with croc proof logging; returns the paths of the model and the scaffold.
fn solve(name: &str, model: &str, solver_flags: &[&str]) -> (String, PathBuf) {
    let model_path = format!(
        "{}/../pumpkin-solver/tests/croc_models/{model}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("process_models");
    std::fs::create_dir_all(&directory).expect("can create the output directory");
    let scaffold_path = directory.join(format!("{name}.scaffold.drcp"));

    let solver_output = Command::new(solver())
        .arg("--proof-path")
        .arg(&scaffold_path)
        .arg("--proof-format")
        .arg("croc")
        .args(solver_flags)
        .arg(&model_path)
        .output()
        .unwrap();
    assert!(solver_output.status.success());
    (model_path, scaffold_path)
}

/// Process the scaffold into a proof in the block format that derives each nogood as
/// `derivation` says, and check that the proof ends with the conclusion and, when a checker is
/// given, that it checks. Returns what the processor printed and the proof.
fn process_and_check(
    name: &str,
    model_path: &str,
    scaffold_path: &Path,
    derivation: &str,
    conclusion: &str,
    processor_flags: &[&str],
) -> (String, String) {
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("process_models");
    let root_domains = processor_flags.contains(&"--root-domains");
    let omit_premises = processor_flags.contains(&"--omit-premises");
    let reading = match (root_domains, omit_premises) {
        (true, true) => ".omit_premises",
        (true, false) => ".root_domains",
        _ => "",
    };
    let proof_path = directory.join(format!("{name}.{derivation}{reading}.croc"));

    let processor_output = Command::new(env!("CARGO_BIN_EXE_pumpkin-proof-processor"))
        .args(["--format", "blocks", "--proof-encoding", "text"])
        .args(["--nogood-derivation", derivation])
        .args(processor_flags)
        .arg(model_path)
        .arg(scaffold_path)
        .arg(&proof_path)
        .output()
        .unwrap();
    assert!(
        processor_output.status.success(),
        "{}{}",
        String::from_utf8_lossy(&processor_output.stdout),
        String::from_utf8_lossy(&processor_output.stderr)
    );

    let proof = std::fs::read_to_string(&proof_path).expect("the proof is written as text");
    assert!(
        proof.lines().any(|line| line == conclusion),
        "the {derivation} proof does not conclude {conclusion}"
    );

    let header = proof.lines().next().filter(|line| line.starts_with("m "));
    let expected = root_domains.then_some("m lenient");
    assert_eq!(
        header, expected,
        "the {derivation} proof states the wrong reading"
    );
    check_premise_free_lines(&proof, omit_premises);

    check_if_checker_given(model_path, &proof_path);
    (
        String::from_utf8_lossy(&processor_output.stdout).into_owned(),
        proof,
    )
}

/// A proof states consequents alone, in `>` lines, only when premises are omitted, and then only
/// for the rules the lenient reading propagates from the current domains (`nogood`,
/// `linear_bounds`, `element`, and their reified forms), and only in refutations.
fn check_premise_free_lines(proof: &str, omit_premises: bool) {
    let mut labels = std::collections::HashMap::new();
    let mut block = ' ';
    for line in proof.lines() {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if !line.starts_with(' ') {
            block = line.chars().next().unwrap_or(' ');
            if let ["l", number, name] = fields.as_slice() {
                let _ = labels.insert((*number).to_owned(), (*name).to_owned());
            }
            continue;
        }
        if fields.first() != Some(&">") {
            continue;
        }
        assert!(
            omit_premises,
            "a proof with every premise states a `>` line: {line}"
        );
        assert_eq!(block, 'g', "a `>` line outside a refutation: {line}");
        let [_, consequent, constraint, label, ..] = fields.as_slice() else {
            panic!("a `>` line without its fields: {line}");
        };
        assert!(
            consequent
                .parse::<i32>()
                .is_ok_and(|reference| reference != 0)
        );
        assert!(constraint.starts_with("c:"), "{line}");
        let rule = &labels[label.strip_prefix("l:").expect("a label follows")];
        let unreified = rule
            .strip_prefix("reified(")
            .or_else(|| rule.strip_prefix("half_reified("))
            .and_then(|inner| inner.strip_suffix(')'))
            .unwrap_or(rule);
        assert!(
            ["nogood", "linear_bounds", "element"].contains(&unreified),
            "a `>` line by the rule {rule}"
        );
    }
}

fn check_if_checker_given(model_path: &str, proof_path: &Path) {
    let Some(checker) = std::env::var_os("CROC_CHECK") else {
        return;
    };

    // The checker takes the reading from the proof's header.
    let output = Command::new(checker)
        .arg(model_path)
        .arg(proof_path)
        .output()
        .expect("CROC_CHECK names a binary that runs");
    let verdict = String::from_utf8_lossy(&output.stdout);
    assert!(
        verdict.lines().any(|line| line == "VERIFIED"),
        "the checker does not verify {}:\n{verdict}",
        proof_path.display()
    );
}
