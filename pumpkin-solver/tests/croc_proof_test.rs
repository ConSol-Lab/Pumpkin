#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

//! The solver writes croc proofs, whole in the block proof type and as a scaffold headed by its
//! preamble. When the environment variable `CROC_CHECK` names the fzn-croc-check binary, the
//! whole proofs are checked.

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

macro_rules! croc_proofs {
    ($name:ident, $model:literal, $conclusion:literal $(, $flag:literal)*) => {
        #[test]
        fn $name() {
            check_block_proofs(stringify!($name), $model, $conclusion, &[$($flag),*]);
        }
    };
}

// Sparse domains and an all-different, unsatisfiable.
croc_proofs!(sparse_all_different, "sparse_all_different", "c UNSAT");

// A table, a reified inequality, a clause and an element, minimised.
croc_proofs!(
    table_reified_element,
    "table_reified_element",
    "c LOWER_BOUND 1"
);

// Equalities, reifications, a set and a minimum, maximised.
croc_proofs!(
    equalities_and_reifications,
    "equalities_and_reifications",
    "c UPPER_BOUND 4"
);

// Multiplication, absolute value and an unbounded integer, unsatisfiable.
croc_proofs!(times_abs_unbounded, "times_abs_unbounded", "c UNSAT");

// Views, a fixed variable and two booleans that reify comparisons with a constant, maximised.
croc_proofs!(
    views_literals_constants,
    "views_literals_constants",
    "c UPPER_BOUND 28"
);
croc_proofs!(
    views_literals_constants_declared,
    "views_literals_constants",
    "c UPPER_BOUND 28",
    "--declare-replacements",
    "--literal-predicates"
);

// An objective that the view pass makes a view of another variable.
croc_proofs!(objective_view, "objective_view", "c UPPER_BOUND 28");

#[test]
fn a_scaffold_states_its_preamble_before_the_nogoods() {
    let model_path = model_path("views_literals_constants");
    let scaffold_path = output_path("views_literals_constants.scaffold");
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--proof-path")
        .arg(&scaffold_path)
        .args(["--proof-format", "croc"])
        .arg(&model_path)
        .output()
        .unwrap();
    assert!(output.status.success());

    let scaffold = std::fs::read_to_string(&scaffold_path).expect("the scaffold is text");
    let marker = scaffold
        .lines()
        .position(|line| line == "nogoods")
        .expect("the scaffold has the marker line");
    let preamble: Vec<&str> = scaffold.lines().take(marker).collect();
    assert!(preamble.iter().any(|line| line.starts_with("view ")));
    assert!(preamble.contains(&"proof"));
    std::fs::remove_file(&scaffold_path).expect("the scaffold is removed");
}

#[test]
fn a_drcp_proof_skips_the_view_options() {
    let model_path = model_path("table_reified_element");
    let proof_path = output_path("table_reified_element.drcp");
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--proof-path")
        .arg(&proof_path)
        .args(["--declare-replacements", "--literal-predicates"])
        .arg(&model_path)
        .output()
        .unwrap();
    assert!(output.status.success());
    let _ = std::fs::remove_file(&proof_path);
}

/// Solve the model with a croc proof of the block proof type for each nogood derivation and
/// encoding, and check that each proof concludes as expected and, when a checker is given, that
/// it checks.
fn check_block_proofs(name: &str, model: &str, conclusion: &str, flags: &[&str]) {
    let model_path = model_path(model);
    for derivation in ["resolution", "refutation"] {
        for encoding in ["text", "binary"] {
            let proof_path = output_path(&format!("{name}.{derivation}.{encoding}.croc"));
            let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
                .arg("--proof-path")
                .arg(&proof_path)
                .args(["--proof-format", "croc", "--proof-type", "blocks"])
                .args(["--nogood-derivation", derivation])
                .args(["--proof-encoding", encoding])
                .args(flags)
                .arg(&model_path)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );

            if encoding == "text" {
                let proof = std::fs::read_to_string(&proof_path).expect("the proof is text");
                assert!(
                    proof.lines().any(|line| line == conclusion),
                    "the {derivation} proof does not conclude {conclusion}"
                );
            }
            check_if_checker_given(&model_path, &proof_path);
            std::fs::remove_file(&proof_path).expect("the proof is removed");
        }
    }
}

fn check_if_checker_given(model_path: &Path, proof_path: &Path) {
    let Some(checker) = std::env::var_os("CROC_CHECK") else {
        return;
    };

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

fn model_path(model: &str) -> PathBuf {
    PathBuf::from(format!(
        "{}/tests/croc_models/{model}.fzn",
        env!("CARGO_MANIFEST_DIR")
    ))
}

fn output_path(file_name: &str) -> PathBuf {
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("croc_proofs");
    std::fs::create_dir_all(&directory).expect("can create the output directory");
    directory.join(file_name)
}

#[test]
fn a_variable_without_declared_bounds_is_warned_about() {
    let scaffold_path = output_path("unbounded.scaffold");
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--proof-path")
        .arg(&scaffold_path)
        .args(["--proof-format", "croc"])
        .arg(model_path("unbounded"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let log = String::from_utf8_lossy(&output.stdout);
    assert!(log.contains("'z' has no declared bounds"), "{log}");
    std::fs::remove_file(&scaffold_path).expect("the scaffold is removed");
}
