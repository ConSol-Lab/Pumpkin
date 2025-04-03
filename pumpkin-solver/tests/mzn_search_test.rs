#![cfg(test)]

mod helpers;

use helpers::run_mzn_test;
use helpers::TestType;

macro_rules! mzn_search_ordered {
    ($name:ident) => {
        #[test]
        fn $name() {
            let _ = run_mzn_test::<true>(
                stringify!($name),
                "mzn_search",
                TestType::SolutionEnumeration,
            );
        }
    };
}

macro_rules! mzn_search_unordered {
    ($name:ident) => {
        #[test]
        fn $name() {
            let _ = run_mzn_test::<false>(
                stringify!($name),
                "mzn_search",
                TestType::SolutionEnumeration,
            );
        }
    };
}

mzn_search_ordered!(bool_search_provided_directly);
mzn_search_ordered!(search_over_ints_no_propagators);
mzn_search_ordered!(search_over_bools_no_propagators);
mzn_search_ordered!(seq_search_1);
mzn_search_unordered!(search_with_constants_in_search);
mzn_search_unordered!(search_annotation_does_not_fix_all_variables);
