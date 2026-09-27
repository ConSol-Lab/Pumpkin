//! Crate to run integration tests for the solver.
#![allow(
    dead_code,
    reason = "is used in integration tests but unable to find a way to silence these warnings"
)]

pub(crate) mod flatzinc;

use std::collections::BTreeMap;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;
use std::time::Duration;

use flatzinc::Solutions;
use flatzinc::Value;
use wait_timeout::ChildExt;

#[derive(Debug)]
pub(crate) struct Files {
    pub(crate) instance_file: PathBuf,
    pub(crate) proof_file: PathBuf,
    pub(crate) log_file: PathBuf,
    pub(crate) err_file: PathBuf,
}

impl Files {
    pub(crate) fn cleanup(self) -> std::io::Result<()> {
        std::fs::remove_file(self.log_file)?;
        std::fs::remove_file(self.err_file)?;

        if self.proof_file.is_file() {
            std::fs::remove_file(self.proof_file)?;
        }

        Ok(())
    }
}

pub(crate) fn run_solver(instance_path: impl AsRef<Path>, with_proof: bool) -> Files {
    run_solver_with_options(instance_path, with_proof, std::iter::empty(), None)
}

pub(crate) fn run_solver_with_options(
    instance_path: impl AsRef<Path>,
    with_proof: bool,
    args: impl IntoIterator<Item = String>,
    prefix: Option<&str>,
) -> Files {
    let args = args.into_iter().collect::<Vec<_>>();

    const TEST_TIMEOUT: Duration = Duration::from_secs(60);

    let instance_path = instance_path.as_ref();

    let solver = PathBuf::from(env!("CARGO_BIN_EXE_pumpkin-solver"));

    let add_extension = |extension: &str| -> PathBuf {
        if let Some(prefix) = prefix.filter(|s| !s.is_empty()) {
            instance_path.with_extension(format!("{prefix}.{extension}"))
        } else {
            instance_path.with_extension(extension)
        }
    };

    let log_file_path = add_extension("log");
    let err_file_path = add_extension("err");
    let proof_file_path = add_extension("drcp");

    let mut command = Command::new(solver);

    if with_proof {
        let _ = command
            .arg("--proof-path")
            .arg(&proof_file_path)
            .arg("--proof-type")
            .arg("full");
    }

    for arg in args {
        let _ = command.arg(arg);
    }

    let mut child = command
        .arg(instance_path)
        .stdout(
            File::create(&log_file_path).expect("Failed to create log file for {instance_name}."),
        )
        .stderr(
            File::create(&err_file_path).expect("Failed to create error file for {instance_name}."),
        )
        .stdin(Stdio::null())
        .spawn()
        .expect("Failed to run solver.");

    match child.wait_timeout(TEST_TIMEOUT) {
        Ok(None) => {
            child.kill().unwrap();
            let _ = child.wait().unwrap();
            panic!("Solver took more than {} seconds", TEST_TIMEOUT.as_secs())
        }
        Ok(Some(status)) if status.success() => {}
        Ok(Some(e)) => panic!(
            "error solving instance {e}\n{:?}",
            std::fs::read_to_string(err_file_path)
        ),
        Err(e) => panic!("error starting solver: {e}"),
    }

    Files {
        instance_file: instance_path.to_path_buf(),
        log_file: log_file_path,
        proof_file: proof_file_path,
        err_file: err_file_path,
    }
}

pub(crate) fn get_executable(path: impl AsRef<Path>) -> PathBuf {
    if cfg!(windows) {
        path.as_ref().with_extension("exe")
    } else {
        path.as_ref().to_path_buf()
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum CheckerOutput {
    Panic,
    Acceptable,
}

pub(crate) trait Checker {
    fn executable_name(&self) -> &'static str;

    fn prepare_command(&self, cmd: &mut Command, files: &Files);

    fn parse_checker_output(&self, output: &Output) -> CheckerOutput;

    fn after_checking_action(&self, files: Files, _output: &Output) {
        files.cleanup().unwrap()
    }
}

pub(crate) fn run_solution_checker(files: Files, checker: impl Checker) {
    let checker_exe = get_executable(format!("{}/{}", env!("OUT_DIR"), checker.executable_name()));

    let mut command = Command::new(checker_exe);
    let _ = command
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .stderr(Stdio::piped());

    checker.prepare_command(&mut command, &files);

    let output = command.output().unwrap_or_else(|_| {
        panic!(
            "Failed to run solution checker: {}",
            checker.executable_name()
        )
    });

    match checker.parse_checker_output(&output) {
        CheckerOutput::Panic => {
            println!("{}", std::str::from_utf8(&output.stdout).unwrap());

            panic!(
                "Failed to verify solution file. Checker exited with code {}",
                output.status
            );
        }
        CheckerOutput::Acceptable => checker.after_checking_action(files, &output),
    }
}

pub(crate) fn verify_proof(files: Files, checker_output: &Output) -> std::io::Result<()> {
    if checker_output.status.code().unwrap() == 0 {
        return Ok(());
    }

    let drat_trim = get_executable(format!("{}/drat-trim", env!("OUT_DIR")));

    let output = Command::new(drat_trim)
        .stdout(Stdio::piped())
        .arg(&files.instance_file)
        .arg(&files.proof_file)
        .output()
        .expect("Failed to run drat-trim");

    if !output.status.success() {
        println!("{}", std::str::from_utf8(&output.stdout).unwrap());
        panic!("drat-trim reported an error");
    }

    files.cleanup()
}

pub(crate) fn run_mzn_test<const ORDERED: bool>(
    instance_name: &str,
    folder_name: &str,
    with_proof: bool,
    test_type: TestType,
) -> String {
    run_mzn_test_with_options::<ORDERED>(
        instance_name,
        folder_name,
        with_proof,
        test_type,
        vec![],
        "",
    )
}

pub(crate) fn check_statistic_equality(
    instance_name: &str,
    folder_name: &str,
    mut options_first: Vec<String>,
    mut options_second: Vec<String>,
    prefix_first: &str,
    prefix_second: &str,
) {
    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    options_first.push("-sa".to_owned());
    options_second.push("-sa".to_owned());

    let files_first = run_solver_with_options(
        instance_path.clone(),
        false,
        options_first,
        Some(prefix_first),
    );

    let files_second =
        run_solver_with_options(instance_path, false, options_second, Some(prefix_second));

    let output_first =
        std::fs::read_to_string(files_first.log_file).expect("Failed to read solver output");
    let output_second =
        std::fs::read_to_string(files_second.log_file).expect("Failed to read solver output");

    let filtered_output_first = output_first
        .lines()
        .filter(|line| {
            line.starts_with("%%%mzn-stat")
                && !line.contains("Time")
                && !line.contains("propagations")
        })
        .collect::<Vec<&str>>();
    let filtered_output_second = output_second
        .lines()
        .filter(|line| {
            line.starts_with("%%%mzn-stat")
                && !line.contains("Time")
                && !line.contains("propagations")
        })
        .collect::<Vec<&str>>();
    assert_eq!(
        filtered_output_first,
        filtered_output_second,
        "Lines first differ at:\n{:?}",
        {
            assert_eq!(
                filtered_output_first.len(),
                filtered_output_second.len(),
                "The output length was not the same"
            );
            filtered_output_first
                .iter()
                .zip(filtered_output_second.iter())
                .find(|(a, b)| a != b)
                .unwrap()
        }
    )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TestType {
    Unsatisfiable,
    SolutionEnumeration,
    Optimality,
}

pub(crate) fn run_mzn_test_with_options<const ORDERED: bool>(
    instance_name: &str,
    folder_name: &str,
    with_proof: bool,
    test_type: TestType,
    mut options: Vec<String>,
    prefix: &str,
) -> String {
    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let snapshot_path = format!(
        "{}/tests/{folder_name}/{instance_name}.expected",
        env!("CARGO_MANIFEST_DIR")
    );

    if matches!(
        test_type,
        TestType::SolutionEnumeration | TestType::Optimality
    ) {
        // Both for optimisation and enumeration do we want to log all encountered solutions.
        options.push("-a".to_owned());
    }

    let files = run_solver_with_options(&instance_path, with_proof, options, Some(prefix));

    let output = std::fs::read_to_string(files.log_file).expect("Failed to read solver output");

    if test_type == TestType::Unsatisfiable {
        assert!(output.contains("=====UNSATISFIABLE====="));
    } else {
        let expected_file =
            std::fs::read_to_string(snapshot_path).expect("Failed to read expected solution file.");

        let actual_solutions = output
            .parse::<Solutions<ORDERED>>()
            .expect("Valid solution");

        let expected_solutions = expected_file
            .parse::<Solutions<ORDERED>>()
            .expect("Valid solution");

        if test_type == TestType::Optimality {
            check_optimisation_solutions(
                Path::new(&instance_path),
                &actual_solutions,
                &expected_solutions,
            );
            return output;
        }

        assert_eq!(
            actual_solutions,
            expected_solutions,
            "Did not find the elements {:?} in the expected solution and the expected solution contained {:?} while the actual solution did not.",
            actual_solutions
                .assignments
                .iter()
                .filter(|solution| !expected_solutions.assignments.contains(solution))
                .collect::<Vec<_>>(),
            expected_solutions
                .assignments
                .iter()
                .filter(|solution| !actual_solutions.assignments.contains(solution))
                .collect::<Vec<_>>()
        );
    }

    output
}

/// Checks the solutions reported by an optimisation run: every one is a solution of the model, and
/// the last one has the objective value of the last expected solution, which is the optimum.
///
/// Which solutions are found depends on the search, so different solutions with the same objective
/// value are accepted.
fn check_optimisation_solutions<const ORDERED: bool>(
    instance_path: &Path,
    actual: &Solutions<ORDERED>,
    expected: &Solutions<ORDERED>,
) {
    // The part after the last separator of solutions is parsed as a solution as well, but it is
    // not one.
    fn reported<const ORDERED: bool>(
        solutions: &Solutions<ORDERED>,
    ) -> Vec<&BTreeMap<String, Value>> {
        let num_solutions = solutions.assignments.len().saturating_sub(1);
        solutions.assignments[..num_solutions].iter().collect()
    }
    let actual = reported(actual);
    let expected = reported(expected);

    let objectives = actual
        .iter()
        .enumerate()
        .map(|(index, solution)| {
            solution_objective(instance_path, solution, &format!("actual{index}")).unwrap_or_else(
                || panic!("the reported solution {solution:?} is not a solution of the model"),
            )
        })
        .collect::<Vec<_>>();

    let expected_solution = expected.last().expect("the expected output has a solution");
    let expected_objective = solution_objective(instance_path, expected_solution, "expected")
        .unwrap_or_else(|| {
            panic!("the expected solution {expected_solution:?} is not a solution of the model")
        });

    assert_eq!(
        objectives.last(),
        Some(&expected_objective),
        "the last reported solution does not have the optimal objective value"
    );
}

/// The objective value of `solution` for the model at `instance_path`, or `None` if it is not a
/// solution of the model.
///
/// The output variables are fixed to their values in the solution, and the solver solves the
/// resulting model; the objective value is the optimum of that model.
fn solution_objective(
    instance_path: &Path,
    solution: &BTreeMap<String, Value>,
    tag: &str,
) -> Option<i64> {
    let model = std::fs::read_to_string(instance_path).expect("Failed to read the instance.");
    let fixed_model = fix_output_variables(&model, solution)?;

    let name = instance_path
        .file_stem()
        .expect("the instance has a name")
        .to_string_lossy();
    let fixed_model_path =
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}.{tag}.fzn"));
    std::fs::write(&fixed_model_path, fixed_model).expect("Failed to write the fixed model.");

    let files = run_solver_with_options(&fixed_model_path, false, ["-s".to_owned()], None);
    let output = std::fs::read_to_string(&files.log_file).expect("Failed to read solver output");
    files
        .cleanup()
        .expect("Failed to remove the files of the fixed model.");
    std::fs::remove_file(&fixed_model_path).expect("Failed to remove the fixed model.");

    if output.contains("=====UNSATISFIABLE=====") {
        return None;
    }

    let objective = output
        .lines()
        .filter_map(|line| line.strip_prefix("%%%mzn-stat: objective="))
        .next_back()
        .expect("the solver reports the objective value")
        .parse::<i64>()
        .expect("the objective value is an integer");
    Some(objective)
}

/// Adds constraints to the FlatZinc `model` that fix its output variables to their values in
/// `solution`. Returns `None` if an output array contains a constant that differs from its value in
/// the solution.
fn fix_output_variables(model: &str, solution: &BTreeMap<String, Value>) -> Option<String> {
    let mut constraints = vec![];

    for line in model.lines() {
        let Some((declaration, annotations)) = line.split_once("::") else {
            continue;
        };
        if !annotations.contains("output_var") && !annotations.contains("output_array") {
            continue;
        }

        let name = declaration
            .rsplit(':')
            .next()
            .expect("a declaration names its variable")
            .trim();
        let Some(value) = solution.get(name) else {
            continue;
        };

        // The elements of an output array, which are variables or constants.
        let elements = || {
            line.rsplit_once('=')
                .and_then(|(_, right)| right.split_once('['))
                .and_then(|(_, right)| right.split_once(']'))
                .map(|(elements, _)| elements)
                .expect("an output array lists its elements")
                .split(',')
                .map(str::trim)
                .collect::<Vec<_>>()
        };

        match value {
            Value::Int(value) => constraints.push(format!("constraint int_eq({name}, {value});")),
            Value::Bool(value) => constraints.push(format!("constraint bool_eq({name}, {value});")),
            Value::IntArray(values) => {
                for (element, value) in elements().into_iter().zip(values) {
                    match element.parse::<i32>() {
                        Ok(constant) if constant != *value => return None,
                        Ok(_) => {}
                        Err(_) => {
                            constraints.push(format!("constraint int_eq({element}, {value});"))
                        }
                    }
                }
            }
            Value::BoolArray(values) => {
                for (element, value) in elements().into_iter().zip(values) {
                    match element.parse::<bool>() {
                        Ok(constant) if constant != *value => return None,
                        Ok(_) => {}
                        Err(_) => {
                            constraints.push(format!("constraint bool_eq({element}, {value});"))
                        }
                    }
                }
            }
        }
    }

    // The constraints are added before the solve item, which is the last item of the model.
    let lines = model.lines().collect::<Vec<_>>();
    let solve_item = lines
        .iter()
        .position(|line| line.trim_start().starts_with("solve"))
        .expect("the model has a solve item");

    let mut fixed_model = lines[..solve_item].join("\n");
    for constraint in constraints {
        fixed_model.push('\n');
        fixed_model.push_str(&constraint);
    }
    fixed_model.push('\n');
    fixed_model.push_str(&lines[solve_item..].join("\n"));
    fixed_model.push('\n');
    Some(fixed_model)
}

pub(crate) fn check_mzn_proof(instance_name: &str, folder_name: &str) {
    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let proof_path = format!(
        "{}/tests/{folder_name}/{instance_name}.drcp",
        env!("CARGO_MANIFEST_DIR")
    );

    pumpkin_checker::run_checker(instance_path, proof_path).expect("proof should be valid");
}
