use std::process::{Command, Output};

use integration_tests::{
    ensure_release_binary_built, run_solution_checker, run_solver, Checker, CheckerOutput, Files,
};

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
        cmd.arg(&files.instance_file);
        cmd.arg(&files.log_file);
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
    ensure_release_binary_built();

    let instance_path = format!(
        "{}/tests/wcnf/{instance_name}.wcnf",
        env!("CARGO_MANIFEST_DIR")
    );
    let files = run_solver(instance_path);

    run_solution_checker::<MaxSATChecker>(files);
}
