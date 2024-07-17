//! Crate to run integration tests for the solver.
#![allow(dead_code)]

pub(crate) mod flatzinc;

use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;
use std::time::Duration;

use flatzinc::Solutions;
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
    run_solver_with_options(instance_path, with_proof, std::iter::empty())
}

pub(crate) fn run_solver_with_options<'a>(
    instance_path: impl AsRef<Path>,
    with_proof: bool,
    args: impl IntoIterator<Item = &'a str>,
) -> Files {
    const TEST_TIMEOUT: Duration = Duration::from_secs(60);

    let instance_path = instance_path.as_ref();

    let solver = PathBuf::from(env!("CARGO_BIN_EXE_pumpkin-cli"));

    let log_file_path = instance_path.with_extension("log");
    let err_file_path = instance_path.with_extension("err");
    let proof_file_path = instance_path.with_extension("proof");

    let mut command = Command::new(solver);

    if with_proof {
        let _ = command.arg("--proof").arg(&proof_file_path);
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
        Ok(None) => panic!("solver took more than {} seconds", TEST_TIMEOUT.as_secs()),
        Ok(Some(status)) if status.success() => {}
        Ok(Some(_)) => panic!("error solving instance"),
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
    fn executable_name() -> &'static str;

    fn prepare_command(cmd: &mut Command, files: &Files);

    fn parse_checker_output(output: &Output) -> CheckerOutput;

    fn after_checking_action(files: Files, _output: &Output) {
        files.cleanup().unwrap()
    }
}

pub(crate) fn run_solution_checker<Check: Checker>(files: Files) {
    let checker_exe = get_executable(format!("{}/{}", env!("OUT_DIR"), Check::executable_name()));

    let mut command = Command::new(checker_exe);
    let _ = command
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .stderr(Stdio::piped());

    Check::prepare_command(&mut command, &files);

    let output = command.output().unwrap_or_else(|_| {
        panic!(
            "Failed to run solution checker: {}",
            Check::executable_name()
        )
    });

    match Check::parse_checker_output(&output) {
        CheckerOutput::Panic => {
            println!("{}", std::str::from_utf8(&output.stdout).unwrap());

            panic!(
                "Failed to verify solution file. Checker exited with code {}",
                output.status
            );
        }
        CheckerOutput::Acceptable => Check::after_checking_action(files, &output),
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

pub(crate) fn run_mzn_test<const ORDERED: bool>(instance_name: &str, folder_name: &str) {
    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let snapshot_path = format!(
        "{}/tests/{folder_name}/{instance_name}.expected",
        env!("CARGO_MANIFEST_DIR")
    );

    let files = run_solver_with_options(instance_path, false, ["-a"]);

    let output = std::fs::read_to_string(files.log_file).expect("Failed to read solver output");

    let expected_file =
        std::fs::read_to_string(snapshot_path).expect("Failed to read expected solution file.");

    let actual_solutions = output
        .parse::<Solutions<ORDERED>>()
        .expect("Valid solution");

    let expected_solutions = expected_file
        .parse::<Solutions<ORDERED>>()
        .expect("Valid solution");

    assert_eq!(actual_solutions, expected_solutions, "Did not find the elements {:?} in the expected solution and the expected solution contained {:?} while the actual solution did not.", actual_solutions.assignments.iter().filter(|solution| !expected_solutions.assignments.contains(solution)).collect::<Vec<_>>(), expected_solutions.assignments.iter().filter(|solution| !actual_solutions.assignments.contains(solution)).collect::<Vec<_>>());
}
