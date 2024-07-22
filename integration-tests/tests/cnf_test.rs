#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
use std::process::Command;
use std::process::Output;

use integration_tests::ensure_release_binary_built;
use integration_tests::run_solution_checker;
use integration_tests::run_solver;
use integration_tests::Checker;
use integration_tests::CheckerOutput;
use integration_tests::Files;

macro_rules! test_cnf_instance {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_cnf_test(stringify!($name));
        }
    };
}

// test_cnf_instance!(add128);
test_cnf_instance!(add16);
test_cnf_instance!(add32);
test_cnf_instance!(add4);
test_cnf_instance!(add64);
test_cnf_instance!(add8);
test_cnf_instance!(block0);
test_cnf_instance!(elimclash);
test_cnf_instance!(elimredundant);
test_cnf_instance!(empty);
test_cnf_instance!(factor2708413neg);
test_cnf_instance!(factor2708413pos);
test_cnf_instance!(full1);
test_cnf_instance!(full2);
test_cnf_instance!(full3);
test_cnf_instance!(full4);
test_cnf_instance!(full5);
test_cnf_instance!(full6);
test_cnf_instance!(full7);
test_cnf_instance!(ph2);
test_cnf_instance!(ph3);
test_cnf_instance!(ph4);
test_cnf_instance!(ph5);
test_cnf_instance!(ph6);
test_cnf_instance!(prime121);
test_cnf_instance!(prime1369);
test_cnf_instance!(prime1681);
test_cnf_instance!(prime169);
test_cnf_instance!(prime1849);
test_cnf_instance!(prime2209);
test_cnf_instance!(prime25);
test_cnf_instance!(prime289);
test_cnf_instance!(prime361);
test_cnf_instance!(prime4);
// test_cnf_instance!(prime4294967297);
test_cnf_instance!(prime49);
test_cnf_instance!(prime529);
// test_cnf_instance!(prime65537);
test_cnf_instance!(prime841);
test_cnf_instance!(prime9);
test_cnf_instance!(prime961);
test_cnf_instance!(regr000);
test_cnf_instance!(sat0);
test_cnf_instance!(sat1);
test_cnf_instance!(sat10);
test_cnf_instance!(sat11);
test_cnf_instance!(sat12);
test_cnf_instance!(sat13);
test_cnf_instance!(sat2);
test_cnf_instance!(sat3);
test_cnf_instance!(sat4);
test_cnf_instance!(sat5);
test_cnf_instance!(sat6);
test_cnf_instance!(sat7);
test_cnf_instance!(sat8);
test_cnf_instance!(sat9);
test_cnf_instance!(sqrt10201);
test_cnf_instance!(sqrt1042441);
test_cnf_instance!(sqrt10609);
test_cnf_instance!(sqrt11449);
test_cnf_instance!(sqrt11881);
test_cnf_instance!(sqrt12769);
test_cnf_instance!(sqrt16129);
test_cnf_instance!(sqrt259081);
test_cnf_instance!(sqrt2809);
test_cnf_instance!(sqrt3481);
test_cnf_instance!(sqrt3721);
test_cnf_instance!(sqrt4489);
test_cnf_instance!(sqrt5041);
test_cnf_instance!(sqrt5329);
test_cnf_instance!(sqrt6241);
test_cnf_instance!(sqrt63001);
test_cnf_instance!(sqrt6889);
test_cnf_instance!(sqrt7921);
test_cnf_instance!(sqrt9409);
test_cnf_instance!(sub0);
test_cnf_instance!(trivially_false);
test_cnf_instance!(unit0);
test_cnf_instance!(unit1);
test_cnf_instance!(unit2);
test_cnf_instance!(unit3);
test_cnf_instance!(unit4);
test_cnf_instance!(unit5);
test_cnf_instance!(unit6);
test_cnf_instance!(unit7);

struct CnfChecker;

impl Checker for CnfChecker {
    fn executable_name(&self) -> &'static str {
        "precochk"
    }

    fn prepare_command(&self, cmd: &mut Command, files: &Files) {
        let _ = cmd.arg(&files.instance_file);
        let _ = cmd.arg(&files.log_file);
    }

    fn parse_checker_output(&self, output: &Output) -> CheckerOutput {
        let code = output.status.code().unwrap_or(1);

        if code == 0 || code == 20 {
            CheckerOutput::Acceptable
        } else {
            CheckerOutput::Panic
        }
    }

    fn after_checking_action(&self, _files: Files, _output: &Output) {
        // todo: bring back the proof logging
        // verify_proof(files, output).unwrap()
    }
}

fn run_cnf_test(instance_name: &str) {
    ensure_release_binary_built();

    let instance_path = format!(
        "{}/tests/cnf/{instance_name}.cnf",
        env!("CARGO_MANIFEST_DIR")
    );
    let files = run_solver(instance_path, true);

    run_solution_checker(files, CnfChecker);
}
