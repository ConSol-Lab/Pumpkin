#![cfg(test)]

mod helpers;

use helpers::TestType;
use helpers::check_mzn_proof;
use helpers::run_mzn_test;

macro_rules! mzn_optimization_test {
    ($name:ident, with_proof: $with_proof:literal) => {
        #[test]
        fn $name() {
            let output = run_mzn_test::<false>(
                stringify!($name),
                "mzn_optimization",
                $with_proof,
                TestType::Optimality,
            );
            assert!(output.ends_with("==========\n"));

            if $with_proof {
                check_mzn_proof(stringify!($name), "mzn_optimization");
            }
        }
    };

    ($name:ident) => {
        mzn_optimization_test!($name, with_proof: false);
    };
}

mzn_optimization_test!(constant_objective);
mzn_optimization_test!(unfixed_objective);
mzn_optimization_test!(minimise_1);
mzn_optimization_test!(maximise_1);
mzn_optimization_test!(unconstrained_objective_minimise);
mzn_optimization_test!(unconstrained_objective_maximise);

mzn_optimization_test!(rcpsp_00, with_proof: true);
mzn_optimization_test!(rcpsp_01, with_proof: true);
mzn_optimization_test!(rcpsp_st27_14, with_proof: true);
mzn_optimization_test!(rcpsp_bl2006, with_proof: true);
mzn_optimization_test!(rcpsp_j60_1_6, with_proof: true);

#[cfg(not(feature = "check-propagations"))]
mzn_optimization_test!(ghoulomb_3_5_11, with_proof: true);
