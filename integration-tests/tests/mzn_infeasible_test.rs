#![cfg(test)]
use integration_tests::ensure_release_binary_built;
use integration_tests::run_solver_with_options;

macro_rules! mzn_infeasible_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_infeasible_test(stringify!($name), "mzn_infeasible");
        }
    };
}
mzn_infeasible_test!(prop_stress);

pub fn run_mzn_infeasible_test(instance_name: &str, folder_name: &str) {
    ensure_release_binary_built();

    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let files = run_solver_with_options(instance_path, false, ["-a"]);

    let output = std::fs::read_to_string(files.log_file).expect("Failed to read solver output");
    assert_eq!(output, "=====UNSATISFIABLE=====\n");
}
