//! Crate to run integration tests for the solver.

use std::{
    fs::File,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::Duration,
};

use wait_timeout::ChildExt;

pub struct Files {
    pub proof_file: PathBuf,
    pub log_file: PathBuf,
    pub err_file: PathBuf,
}
impl Files {
    pub fn cleanup(self) -> std::io::Result<()> {
        std::fs::remove_file(self.log_file)?;
        std::fs::remove_file(self.proof_file)?;
        std::fs::remove_file(self.err_file)?;

        Ok(())
    }
}

pub fn run_solver(instance_path: impl AsRef<Path>) -> Files {
    const TEST_TIMEOUT: Duration = Duration::from_secs(60);

    let instance_path = instance_path.as_ref();

    let solver = get_executable(format!(
        "{}/../target/release/pumpkin-cli",
        env!("CARGO_MANIFEST_DIR")
    ));

    let log_file_path = instance_path.with_extension("log");
    let err_file_path = instance_path.with_extension("err");
    let proof_file_path = instance_path.with_extension("proof");

    let mut child = Command::new(solver)
        .arg("--certificate-path")
        .arg(&proof_file_path)
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
        log_file: log_file_path,
        proof_file: proof_file_path,
        err_file: err_file_path,
    }
}

pub fn ensure_release_binary_built() {
    let working_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../pumpkin-cli");

    let compile_status = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .arg("-q")
        .current_dir(working_dir)
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to execute command to compile solver.");

    assert!(compile_status.success(), "Failed to compile solver.");
}

pub fn get_executable(path: impl AsRef<Path>) -> PathBuf {
    if cfg!(windows) {
        path.as_ref().with_extension("exe")
    } else {
        path.as_ref().to_path_buf()
    }
}

pub fn verify_proof(instance_path: String, files: Files) -> std::io::Result<()> {
    let drat_trim = get_executable(format!("{}/drat-trim", env!("OUT_DIR")));

    let status = Command::new(drat_trim)
        .stdout(Stdio::null())
        .arg(instance_path)
        .arg(&files.proof_file)
        .status()
        .expect("Failed to run drat-trim");

    assert!(status.success(), "drat-trim reported an error");

    files.cleanup()
}
