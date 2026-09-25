#![cfg(test)]

//! Integration tests for `--symmetry-breaking`.
//!
//! The optimisation tests run each instance with the flag and compare the printed solutions to
//! the snapshot. The snapshots were produced with the flag enabled, since symmetry breaking may
//! change which of several equivalent solutions is printed, but never the optimal value.

mod helpers;

use std::path::PathBuf;
use std::process::Command;

use helpers::TestType;
use helpers::run_mzn_test_with_options;

macro_rules! mzn_symmetry_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let output = run_mzn_test_with_options::<false>(
                stringify!($name),
                "mzn_symmetry",
                false,
                TestType::Optimality,
                vec!["--symmetry-breaking".to_owned()],
                "sym",
            );
            assert!(output.ends_with("==========\n"));
        }
    };
}

mzn_symmetry_test!(interchangeable_pair);
mzn_symmetry_test!(objective_breaks_symmetry);
mzn_symmetry_test!(three_way);

/// Enumerating all solutions of a satisfaction problem is incompatible with symmetry breaking,
/// so the solver must refuse rather than silently drop solutions.
#[test]
fn enumeration_is_refused() {
    let instance = format!(
        "{}/tests/mzn_symmetry/enumerate_refused.fzn",
        env!("CARGO_MANIFEST_DIR")
    );
    let output = Command::new(PathBuf::from(env!("CARGO_BIN_EXE_pumpkin-solver")))
        .arg("--symmetry-breaking")
        .arg("-a")
        .arg(&instance)
        .output()
        .expect("failed to run solver");

    assert!(!output.status.success());
    // The solver reports errors on stdout as a FlatZinc comment line.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("symmetry breaking cannot be combined"),
        "expected a symmetry-breaking error, got: {stdout}"
    );
}
