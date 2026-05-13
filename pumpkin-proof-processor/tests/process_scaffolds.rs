#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024
#![allow(deprecated, reason = "the Command::cargo_bin function is fine for us")]

use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use drcp_format::Step;
use drcp_format::reader::ProofReader;

macro_rules! accept_proof {
    ($name:ident, no_trimming) => {
        #[test]
        fn $name() {
            run_processor_on_proof(stringify!($name), false);
        }
    };

    ($name:ident) => {
        #[test]
        fn $name() {
            run_processor_on_proof(stringify!($name), true);
        }
    };
}

accept_proof!(root_propagation_unsatisfiable, no_trimming);

accept_proof!(rcpsp_00);

accept_proof!(market_split_u3_01);
accept_proof!(market_split_u3_02, no_trimming);
accept_proof!(market_split_u3_03, no_trimming);
accept_proof!(market_split_u3_04, no_trimming);
accept_proof!(market_split_u3_05, no_trimming);
accept_proof!(market_split_u3_06);
accept_proof!(market_split_u3_07, no_trimming);
accept_proof!(market_split_u3_08);
accept_proof!(market_split_u3_09, no_trimming);
accept_proof!(market_split_u3_10, no_trimming);

fn run_processor_on_proof(model: &str, assert_trimming: bool) {
    let model_path = format!("{}/tests/scaffolds/{model}.fzn", env!("CARGO_MANIFEST_DIR"));
    let scaffold_path = format!(
        "{}/tests/scaffolds/{model}.scaffold.drcp",
        env!("CARGO_MANIFEST_DIR")
    );
    let full_proof_path = format!(
        "{}/tests/scaffolds/{model}.full.drcp",
        env!("CARGO_MANIFEST_DIR")
    );

    let processor_output = escargot::CargoBuild::new()
        .bin("pumpkin-proof-processor")
        .current_target()
        .current_release()
        .run()
        .unwrap()
        .command()
        .arg(&model_path)
        .arg(&scaffold_path)
        .arg(&full_proof_path)
        .output()
        .unwrap();
    assert!(processor_output.status.success());

    if assert_trimming {
        let num_nogoods_in_scaffold = count_nogoods(&scaffold_path);
        let num_nogoods_in_full_proof = count_nogoods(&full_proof_path);
        assert!(
            num_nogoods_in_scaffold > num_nogoods_in_full_proof,
            "number of nogoods in scaffold ({num_nogoods_in_scaffold}) should be larger than number of nogoods in full proof ({num_nogoods_in_full_proof})"
        );
    }

    let checker_status = escargot::CargoBuild::new()
        .package("pumpkin-checker")
        .bin("pumpkin-checker")
        .current_target()
        .current_release()
        .run()
        .unwrap()
        .command()
        .arg(&model_path)
        .arg(&full_proof_path)
        .output()
        .unwrap();
    assert!(checker_status.status.success());
}

fn count_nogoods(proof_path: impl AsRef<Path>) -> usize {
    let proof = File::open(proof_path).expect("failed read scaffold");
    let mut proof_reader = ProofReader::<_, i32>::new(BufReader::new(proof));

    let mut counter = 0;
    while let Some(step) = proof_reader.next_step().expect("failed to read proof") {
        if matches!(step, Step::Deduction(_)) {
            counter += 1
        }
    }

    counter
}
