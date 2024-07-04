#![cfg(test)]
use integration_tests::run_mzn_test;

macro_rules! mzn_search_ordered {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<true>(stringify!($name), "mzn_search");
        }
    };
}

macro_rules! mzn_search_unordered {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<false>(stringify!($name), "mzn_search");
        }
    };
}

mzn_search_ordered!(search_over_ints_no_propagators);
mzn_search_unordered!(search_with_constants_in_search);
mzn_search_unordered!(search_annotation_does_not_fix_all_variables);
