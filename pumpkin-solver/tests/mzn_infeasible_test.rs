#![cfg(test)]

mod helpers;

use helpers::TestType;
use helpers::run_mzn_test;

macro_rules! mzn_infeasible_test {
    ($name:ident, with_proof: $with_proof:literal) => {
        #[test]
        fn $name() {
            run_mzn_infeasible_test(stringify!($name), "mzn_infeasible", $with_proof);
        }
    };

    ($name:ident) => {
        mzn_infeasible_test!($name, with_proof: false);
    };
}
mzn_infeasible_test!(prop_stress);
mzn_infeasible_test!(connected);

mzn_infeasible_test!(rcpsp_00_unsat, with_proof: true);
mzn_infeasible_test!(rcpsp_01_unsat, with_proof: true);
mzn_infeasible_test!(rcpsp_st27_14, with_proof: true);
mzn_infeasible_test!(rcpsp_bl2006, with_proof: true);
mzn_infeasible_test!(rcpsp_j60_1_6, with_proof: true);

mzn_infeasible_test!(ghoulomb_3_5_11, with_proof: true);

pub fn run_mzn_infeasible_test(instance_name: &str, folder_name: &str, with_proof: bool) {
    let _ = run_mzn_test::<false>(
        instance_name,
        folder_name,
        with_proof,
        TestType::Unsatisfiable,
    );

    if !with_proof {
        return;
    }

    let instance_path = format!(
        "{}/tests/{folder_name}/{instance_name}.fzn",
        env!("CARGO_MANIFEST_DIR")
    );

    let proof_path = format!(
        "{}/tests/{folder_name}/{instance_name}.drcp.gz",
        env!("CARGO_MANIFEST_DIR")
    );

    #[allow(deprecated, reason = "don't know a better way to do it")]
    let mut command = assert_cmd::Command::cargo_bin("pumpkin-checker").unwrap();
    let _ = command.arg(instance_path);
    let _ = command.arg(proof_path);

    let _ = command.assert().success();
}
