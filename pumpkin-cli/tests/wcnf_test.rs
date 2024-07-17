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
    ($name:ident) => {
        #[test]
        fn $name() {
            run_wcnf_test(stringify!($name));
        }
    };
}

test_wcnf_instance!(simple);
test_wcnf_instance!(karate);
test_wcnf_instance!(riskmap);
test_wcnf_instance!(johnson8_2_4);
test_wcnf_instance!(johnson8_4_4);
test_wcnf_instance!(normalized_g2x2);
test_wcnf_instance!(normalized_g9x3);
test_wcnf_instance!(normalized_g9x9);
test_wcnf_instance!(ram_k3_n9);

struct MaxSATChecker;

impl Checker for MaxSATChecker {
    fn executable_name() -> &'static str {
        "maxsat-checker"
    }

    fn prepare_command(cmd: &mut Command, files: &Files) {
        let _ = cmd.arg(&files.instance_file);
        let _ = cmd.arg(&files.log_file);
    }

    fn parse_checker_output(output: &Output) -> CheckerOutput {
        if output.status.success() {
            CheckerOutput::Acceptable
        } else {
            CheckerOutput::Panic
        }
    }
}

fn run_wcnf_test(instance_name: &str) {
    let instance_path = format!(
        "{}/tests/wcnf/{instance_name}.wcnf",
        env!("CARGO_MANIFEST_DIR")
    );
    let files = run_solver(instance_path, false);

    run_solution_checker::<MaxSATChecker>(files);
}
