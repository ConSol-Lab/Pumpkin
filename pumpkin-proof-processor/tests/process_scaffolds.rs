#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
#![allow(deprecated, reason = "the Command::cargo_bin function is fine for us")]

macro_rules! accept_proof {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_processor_on_proof(stringify!($name));
        }
    };
}

accept_proof!(rcpsp_00);

fn run_processor_on_proof(model: &str) {
    let model_path = format!("{}/tests/scaffolds/{model}.fzn", env!("CARGO_MANIFEST_DIR"));
    let scaffold_path = format!(
        "{}/tests/scaffolds/{model}.scaffold.drcp",
        env!("CARGO_MANIFEST_DIR")
    );
    let full_proof_path = format!(
        "{}/tests/scaffolds/{model}.full.drcp",
        env!("CARGO_MANIFEST_DIR")
    );

    let _ = assert_cmd::Command::cargo_bin("pumpkin-proof-processor")
        .expect("could not find executable")
        .arg(&model_path)
        .arg(&scaffold_path)
        .arg(&full_proof_path)
        .assert()
        .success();

    let _ = assert_cmd::Command::cargo_bin("pumpkin-checker")
        .expect("could not find executable")
        .arg(&model_path)
        .arg(&full_proof_path)
        .assert()
        .success();
}
