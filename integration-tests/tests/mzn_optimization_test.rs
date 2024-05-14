#![cfg(test)]
use integration_tests::run_mzn_test;

macro_rules! mzn_optimization_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<false>(stringify!($name), "mzn_optimization");
        }
    };
}
mzn_optimization_test!(constant_objective);
