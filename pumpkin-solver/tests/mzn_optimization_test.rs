#![cfg(test)]

mod helpers;

use helpers::run_mzn_test;
use helpers::TestType;

macro_rules! mzn_optimization_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let output =
                run_mzn_test::<false>(stringify!($name), "mzn_optimization", TestType::Optimality);
            assert!(output.ends_with("==========\n"));
        }
    };
}

mzn_optimization_test!(constant_objective);
mzn_optimization_test!(unfixed_objective);
mzn_optimization_test!(minimise_1);
mzn_optimization_test!(maximise_1);
mzn_optimization_test!(unconstrained_objective_minimise);
mzn_optimization_test!(unconstrained_objective_maximise);
