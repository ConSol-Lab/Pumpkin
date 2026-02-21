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

accept_proof!(root_propagation_unsatisfiable);

accept_proof!(rcpsp_00);

accept_proof!(market_split_u3_01);
accept_proof!(market_split_u3_02);
accept_proof!(market_split_u3_03);
accept_proof!(market_split_u3_04);
accept_proof!(market_split_u3_05);
accept_proof!(market_split_u3_06);
accept_proof!(market_split_u3_07);
accept_proof!(market_split_u3_08);
accept_proof!(market_split_u3_09);
accept_proof!(market_split_u3_10);

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
