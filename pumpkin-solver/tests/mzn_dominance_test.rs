#![cfg(test)]

//! Integration tests for `--dominance-breaking`.
//!
//! The optimisation tests run each instance with the flag and compare the printed solutions to
//! the snapshot. The snapshots were produced with the flag enabled, and their optimal values were
//! checked against runs without it.

mod helpers;

use std::path::PathBuf;
use std::process::Command;

use helpers::TestType;
use helpers::run_mzn_test_with_options;

macro_rules! mzn_dominance_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let output = run_mzn_test_with_options::<false>(
                stringify!($name),
                "mzn_dominance",
                false,
                TestType::Optimality,
                vec!["--dominance-breaking".to_owned()],
                "dom",
            );
            assert!(output.ends_with("==========\n"));
        }
    };
}

mzn_dominance_test!(knapsack_small);
mzn_dominance_test!(identical_items);

/// Dominance breaking and symmetry breaking order tied variables in opposite directions, so the
/// solver must refuse the combination.
#[test]
fn combination_with_symmetry_breaking_is_refused() {
    let instance = format!(
        "{}/tests/mzn_dominance/knapsack_small.fzn",
        env!("CARGO_MANIFEST_DIR")
    );
    let output = Command::new(PathBuf::from(env!("CARGO_BIN_EXE_pumpkin-solver")))
        .arg("--dominance-breaking")
        .arg("--symmetry-breaking")
        .arg(&instance)
        .output()
        .expect("failed to run solver");

    assert!(!output.status.success());
    // The solver reports errors on stdout as a FlatZinc comment line.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("dominance breaking cannot be combined with symmetry breaking"),
        "expected a dominance-breaking error, got: {stdout}"
    );
}
