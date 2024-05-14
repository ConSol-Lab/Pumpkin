#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
use integration_tests::ensure_release_binary_built;
use integration_tests::run_solver_with_options;

macro_rules! mzn_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_constraint_test(stringify!($name));
        }
    };
}

mzn_test!(int_eq);
mzn_test!(int_eq_reif);
mzn_test!(array_int_maximum);
mzn_test!(array_int_minimum);
mzn_test!(int_min);
mzn_test!(int_max);
mzn_test!(int_lin_ne_reif);

fn run_mzn_constraint_test(instance_name: &str) {
    ensure_release_binary_built();

    let instance_path = format!(
        "{}/tests/mzn_constraints/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let snapshot_path = format!(
        "{}/tests/mzn_constraints/{instance_name}.expected",
        env!("CARGO_MANIFEST_DIR")
    );

    let files = run_solver_with_options(instance_path, ["-a"]);

    let output = std::fs::read_to_string(files.log_file).expect("Failed to read solver output");
    let actual_solutions = output
        .parse::<integration_tests::flatzinc::Solutions>()
        .expect("Valid solution");

    let expected_file =
        std::fs::read_to_string(snapshot_path).expect("Failed to read expected solution file.");
    let expected_solutions = expected_file
        .parse::<integration_tests::flatzinc::Solutions>()
        .expect("Valid solution");

    assert_eq!(actual_solutions, expected_solutions);
}
