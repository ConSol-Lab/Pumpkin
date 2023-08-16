use std::process::{Command, Stdio};

use integration_tests::{ensure_release_binary_built, get_executable, run_solver, verify_proof};

macro_rules! test_cnf_instance {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_cnf_test(stringify!($name));
        }
    };
}

test_cnf_instance!(add128);
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
test_cnf_instance!(prime4294967297);
test_cnf_instance!(prime49);
test_cnf_instance!(prime529);
test_cnf_instance!(prime65537);
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

fn run_cnf_test(instance_name: &str) {
    ensure_release_binary_built();

    let precochk = get_executable(format!("{}/precochk", env!("OUT_DIR")));

    let instance_path = format!(
        "{}/tests/cnf/{instance_name}.cnf",
        env!("CARGO_MANIFEST_DIR")
    );

    let files = run_solver(&instance_path);

    let solution_check = Command::new(precochk)
        .arg(&instance_path)
        .arg(&files.log_file)
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to run precochk")
        .code()
        .expect("precochk exited by signal");

    match solution_check {
        // The formula is satisfiable and the solution is correct.
        0 => files.cleanup().unwrap(),

        // An error occurred while running the solution checker.
        1 => panic!("precochk errored"),

        // The reported solution does not satisfy all clauses in the instance.
        2 => panic!("not all clauses are satisfied"),

        // The formula is unsatisfiable, so we verify the proof.
        20 => verify_proof(instance_path, files).unwrap(),

        code => todo!("unhandled code: {code}"),
    }
}
