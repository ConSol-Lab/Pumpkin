#![cfg(test)]

mod helpers;

use helpers::TestType;
use helpers::run_mzn_test;

macro_rules! mzn_infeasible_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_infeasible_test(stringify!($name), "mzn_infeasible");
        }
    };
}
mzn_infeasible_test!(prop_stress);
mzn_infeasible_test!(connected);

mzn_infeasible_test!(rcpsp_00_unsat);
mzn_infeasible_test!(rcpsp_01_unsat);
mzn_infeasible_test!(rcpsp_st27_14);
mzn_infeasible_test!(rcpsp_bl2006);
mzn_infeasible_test!(rcpsp_j60_1_6);

mzn_infeasible_test!(ghoulomb_3_5_11);

pub fn run_mzn_infeasible_test(instance_name: &str, folder_name: &str) {
    let _ = run_mzn_test::<false>(instance_name, folder_name, TestType::Unsatisfiable);
}
