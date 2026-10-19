#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

//! With `--full-solution-path`, the solver writes the final solution with the value of every
//! FlatZinc variable, also of those the compiler removes as unused, and solves as it does
//! without it. When the environment variable `CROC_CHECK` names the fzn-croc-check binary, the
//! written solution is checked against the model.

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn unused_variables_take_their_declared_lower_bound() {
    let model = model_path("full_solution_models/unused_variables");
    let solution = write_full_solution("unused_variables", &model);

    let values: Vec<&str> = solution.lines().collect();
    for expected in [
        "x = 3;",
        "y = 2;",
        "b = true;",
        "unused = 3;",
        "unused_sparse = 4;",
        "unused_b = false;",
        "alias = 2;",
    ] {
        assert!(values.contains(&expected), "{expected} not in\n{solution}");
    }
    assert_eq!(values.last(), Some(&"----------"));
}

#[test]
fn views_literals_and_constants_have_values() {
    let model = model_path("croc_models/views_literals_constants");
    let solution = write_full_solution("views_literals_constants", &model);
    for name in [
        "x", "y", "z", "b", "i", "r1", "r2", "u", "v", "k", "p", "q", "obj",
    ] {
        assert!(
            solution
                .lines()
                .any(|line| line.starts_with(&format!("{name} = "))),
            "no value of {name} in\n{solution}"
        );
    }
}

#[test]
fn the_search_is_the_same_with_the_full_solution() {
    for model in [
        "croc_models/views_literals_constants",
        "croc_models/equalities_and_reifications",
        "croc_models/table_reified_element",
    ] {
        let model = model_path(model);
        let plain = solve(&model, &[]);
        let full_path = output_path("same_search.sol");
        let with_full = solve(
            &model,
            &[
                "--full-solution-path",
                full_path.to_str().expect("a UTF-8 path"),
            ],
        );
        assert_eq!(plain, with_full, "{}", model.display());
    }
}

/// The solutions and the search statistics the solver prints, without the timings.
fn solve(model: &Path, flags: &[&str]) -> Vec<String> {
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("-s")
        .args(flags)
        .arg(model)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|line| !line.contains("Time") && !line.contains("time"))
        .map(str::to_owned)
        .collect()
}

fn write_full_solution(name: &str, model: &Path) -> String {
    let solution_path = output_path(&format!("{name}.sol"));
    let output = Command::new(env!("CARGO_BIN_EXE_pumpkin-solver"))
        .arg("--full-solution-path")
        .arg(&solution_path)
        .arg(model)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    check_if_checker_given(model, &solution_path);
    std::fs::read_to_string(&solution_path).expect("the solution is written")
}

fn check_if_checker_given(model: &Path, solution: &Path) {
    let Some(checker) = std::env::var_os("CROC_CHECK") else {
        return;
    };
    let output = Command::new(checker)
        .arg("--solution")
        .arg(model)
        .arg(solution)
        .output()
        .expect("CROC_CHECK names a binary that runs");
    let verdict = String::from_utf8_lossy(&output.stdout);
    assert!(
        verdict.lines().any(|line| line == "SATISFIED"),
        "the checker does not accept {}:\n{verdict}",
        solution.display()
    );
}

fn model_path(model: &str) -> PathBuf {
    PathBuf::from(format!("{}/tests/{model}.fzn", env!("CARGO_MANIFEST_DIR")))
}

fn output_path(file_name: &str) -> PathBuf {
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("full_solutions");
    std::fs::create_dir_all(&directory).expect("can create the output directory");
    directory.join(file_name)
}
