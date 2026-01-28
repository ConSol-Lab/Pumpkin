#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
#![allow(deprecated, reason = "the Command::cargo_bin function is fine for us")]

macro_rules! accept_proof {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_checker_on_proof(stringify!($name));
        }
    };
}

accept_proof!(market_split_u3_01);
accept_proof!(market_split_u3_02);
accept_proof!(market_split_u3_03);
accept_proof!(market_split_u3_04);

accept_proof!(rcpsp00);
accept_proof!(rcpsp01);
accept_proof!(rcpsp02);
accept_proof!(rcpsp03);

accept_proof!(sudoku_p0);
accept_proof!(sudoku_p1);
accept_proof!(sudoku_p16);
accept_proof!(sudoku_p17);

fn run_checker_on_proof(model: &str) {
    let model_path = format!("{}/tests/proofs/{model}.fzn", env!("CARGO_MANIFEST_DIR"));
    let proof_path = format!("{}/tests/proofs/{model}.drcp", env!("CARGO_MANIFEST_DIR"));

    let _ = assert_cmd::Command::cargo_bin("pumpkin-checker")
        .expect("could not find executable")
        .arg(model_path)
        .arg(proof_path)
        .assert()
        .success();
}
