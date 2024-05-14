#![cfg(test)]
use integration_tests::run_mzn_test;

macro_rules! mzn_search_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<true>(stringify!($name), "mzn_search");
        }
    };
}

mzn_search_test!(search_over_ints_no_propagators);
