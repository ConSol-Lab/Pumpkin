#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;
use helpers::check_statistic_equality;
use helpers::run_mzn_test_with_options;
use helpers::TestType;
use pumpkin_macros::cumulative;
use pumpkin_macros::cumulative_synchronised;

macro_rules! mzn_test {
    ($name:ident) => {
        mzn_test!($name, stringify!($name), vec![]);
    };

    ($name:ident, $file:expr, $options:expr) => {
        #[test]
        fn $name() {
            let output = run_mzn_test_with_options::<false>(
                $file,
                "mzn_constraints",
                TestType::SolutionEnumeration,
                $options,
                stringify!($name),
            );
            assert!(output.ends_with("==========\n"));
        }
    };
}

mzn_test!(int_lin_ne);
mzn_test!(int_lin_ne_reif);
mzn_test!(int_lin_le);
mzn_test!(int_lin_le_reif);
mzn_test!(int_lin_eq);
mzn_test!(int_lin_eq_reif);

mzn_test!(int_eq);
mzn_test!(int_eq_reif);
mzn_test!(int_ne);
mzn_test!(int_ne_reif);
mzn_test!(int_le);
mzn_test!(int_le_reif);
mzn_test!(int_lt);
mzn_test!(int_lt_reif);

mzn_test!(int_times);
mzn_test!(int_plus);
mzn_test!(int_abs);
mzn_test!(int_div);
mzn_test!(int_mod);

mzn_test!(array_int_maximum);
mzn_test!(array_int_minimum);
mzn_test!(int_min);
mzn_test!(int_max);

mzn_test!(set_in);
mzn_test!(set_in_reif_interval);
mzn_test!(set_in_reif_sparse);

mzn_test!(bool_xor_reif);
mzn_test!(bool_xor);
mzn_test!(bool_not);

mzn_test!(bool_lin_eq);
mzn_test!(bool_lin_le);
mzn_test!(bool_clause);

cumulative!(time_table_per_point);
cumulative!(time_table_per_point_incremental);
cumulative!(time_table_per_point_incremental_synchronised);
cumulative!(time_table_over_interval);
cumulative!(time_table_over_interval_incremental);
cumulative!(time_table_over_interval_incremental_synchronised);

cumulative_synchronised!(
    time_table_per_point,
    time_table_per_point_incremental_synchronised
);
cumulative_synchronised!(
    time_table_over_interval,
    time_table_over_interval_incremental_synchronised
);

mzn_test!(all_different);
