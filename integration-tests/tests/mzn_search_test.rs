#![cfg(test)]
use integration_tests::run_mzn_test;

macro_rules! mzn_search_no_constraints {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<true>(stringify!($name), "mzn_search");
        }
    };
}

macro_rules! mzn_search_with_constraints {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<false>(stringify!($name), "mzn_search");
        }
    };
}

mzn_search_no_constraints!(search_over_ints_no_propagators);
mzn_search_with_constraints!(search_with_constants_in_search);
