use std::process::{Command, Stdio};

use integration_tests::{ensure_release_binary_built, get_executable, run_solver};

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

fn run_wcnf_test(instance_name: &str) {
    ensure_release_binary_built();

    let checker = get_executable(format!("{}/maxsat-checker", env!("OUT_DIR")));

    let instance_path = format!(
        "{}/tests/wcnf/{instance_name}.wcnf",
        env!("CARGO_MANIFEST_DIR")
    );

    let files = run_solver(&instance_path);

    let solution_check = Command::new(checker)
        .arg(&instance_path)
        .arg(&files.log_file)
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to run maxsat-checker")
        .code()
        .expect("maxsat-checker exited by signal");

    match solution_check {
        // The formula is satisfiable and the solution is correct.
        0 => files.cleanup().unwrap(),

        // An error occurred running the solution checker.
        1 => panic!("Failed to verify solution file."),

        code => todo!("unhandled code: {code}"),
    }
}
