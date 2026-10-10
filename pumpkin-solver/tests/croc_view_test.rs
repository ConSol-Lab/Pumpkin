#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

//! The view pass under croc proof logging: each kind of view the compiler makes is stated in the
//! preamble of the scaffold, and the whole proofs check. When the environment variable
//! `CROC_CHECK` names the fzn-croc-check binary, the whole proofs are checked.

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn a_definition_and_an_equality_become_views() {
    let preamble = preamble_of("views_core", "views_literals_constants", &[]);
    assert!(
        preamble.contains(&"view x y 2 1".to_owned()),
        "{preamble:?}"
    );
    assert!(
        preamble.contains(&"view z y 2 1".to_owned()),
        "{preamble:?}"
    );
    check_block_proofs(
        "views_core",
        "views_literals_constants",
        "c UPPER_BOUND 28",
        &[],
    );
}

#[test]
fn a_drcp_proof_skips_the_view_pass() {
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--proof-path")
        .arg(output_path("int_lin_eq_drcp.drcp"))
        .arg(model_path("times_abs_unbounded"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let log = String::from_utf8_lossy(&output.stdout);
    assert!(log.contains("view pass is skipped"), "{log}");
    let _ = std::fs::remove_file(output_path("int_lin_eq_drcp.drcp"));
}

#[test]
fn an_equality_whose_coefficients_divide_neither_way_introduces_a_parameter() {
    let preamble = preamble_of("parameter", "views_literals_constants", &[]);
    assert!(
        preamble.iter().any(|line| line.starts_with("parameter ")),
        "{preamble:?}"
    );
    assert!(preamble.iter().any(|line| line.starts_with("view u ")));
    assert!(preamble.iter().any(|line| line.starts_with("view v ")));
}

#[test]
fn declared_replacements_replace_a_fixed_variable_by_its_value() {
    let preamble = preamble_of(
        "declared",
        "views_literals_constants",
        &["--declare-replacements"],
    );
    assert!(
        preamble.contains(&"constant k 3".to_owned()),
        "{preamble:?}"
    );
    check_block_proofs(
        "views_declared",
        "views_literals_constants",
        "c UPPER_BOUND 28",
        &["--declare-replacements"],
    );
}

/// The lines of the preamble of the scaffold the solver writes for the model, before the marker.
fn preamble_of(name: &str, model: &str, flags: &[&str]) -> Vec<String> {
    let scaffold_path = output_path(&format!("{name}.scaffold"));
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--proof-path")
        .arg(&scaffold_path)
        .args(["--proof-format", "croc"])
        .args(flags)
        .arg(model_path(model))
        .output()
        .unwrap();
    assert!(output.status.success());
    let scaffold = std::fs::read_to_string(&scaffold_path).expect("the scaffold is text");
    std::fs::remove_file(&scaffold_path).expect("the scaffold is removed");
    scaffold
        .lines()
        .take_while(|line| *line != "nogoods")
        .map(str::to_owned)
        .collect()
}

/// Solve the model with a croc proof of the block proof type for each nogood derivation, and
/// check that each proof concludes as expected and, when a checker is given, that it checks.
fn check_block_proofs(name: &str, model: &str, conclusion: &str, flags: &[&str]) {
    for derivation in ["resolution", "refutation"] {
        let proof_path = output_path(&format!("{name}.{derivation}.croc"));
        let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
            .arg("--proof-path")
            .arg(&proof_path)
            .args(["--proof-format", "croc", "--proof-type", "blocks"])
            .args([
                "--nogood-derivation",
                derivation,
                "--proof-encoding",
                "text",
            ])
            .args(flags)
            .arg(model_path(model))
            .output()
            .unwrap();
        assert!(output.status.success());
        let proof = std::fs::read_to_string(&proof_path).expect("the proof is text");
        assert!(
            proof.lines().any(|line| line == conclusion),
            "the {derivation} proof does not conclude {conclusion}"
        );
        check_if_checker_given(&model_path(model), &proof_path);
        std::fs::remove_file(&proof_path).expect("the proof is removed");
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
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("croc_views");
    std::fs::create_dir_all(&directory).expect("can create the output directory");
    directory.join(file_name)
}
