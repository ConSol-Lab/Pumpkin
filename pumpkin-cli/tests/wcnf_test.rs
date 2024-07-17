#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
use std::process::Command;
use std::process::Output;

mod helpers;

use helpers::run_solution_checker;
use helpers::run_solver;
use helpers::Checker;
use helpers::CheckerOutput;
use helpers::Files;

macro_rules! test_wcnf_instance {
    ($name:ident, $optimal_objective:literal) => {
        #[test]
        fn $name() {
            run_wcnf_test(stringify!($name), $optimal_objective);
        }
    };
}

test_wcnf_instance!(simple, 1);
test_wcnf_instance!(karate, 4);
test_wcnf_instance!(riskmap, 9);
test_wcnf_instance!(johnson8_2_4, 24);
test_wcnf_instance!(johnson8_4_4, 56);
test_wcnf_instance!(normalized_g2x2, 2);
test_wcnf_instance!(normalized_g9x3, 7);
test_wcnf_instance!(normalized_g9x9, 20);
test_wcnf_instance!(ram_k3_n9, 1);

struct MaxSATChecker {
    expected_objective: u64,
}

impl Checker for MaxSATChecker {
    fn executable_name(&self) -> &'static str {
        "maxsat-checker"
    }

    fn prepare_command(&self, cmd: &mut Command, files: &Files) {
        let _ = cmd.arg("-o").arg(format!("{}", self.expected_objective));
        let _ = cmd.arg(&files.instance_file);
        let _ = cmd.arg(&files.log_file);
    }

    fn parse_checker_output(&self, output: &Output) -> CheckerOutput {
        if output.status.success() {
            CheckerOutput::Acceptable
        } else {
            CheckerOutput::Panic
        }
    }
}

fn run_wcnf_test(instance_name: &str, expected_objective: u64) {
    let instance_path = format!(
        "{}/tests/wcnf/{instance_name}.wcnf",
        env!("CARGO_MANIFEST_DIR")
    );
    let files = run_solver(instance_path, false);

    run_solution_checker(files, MaxSATChecker { expected_objective });
}
