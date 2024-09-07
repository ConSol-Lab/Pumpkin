#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;

use helpers::run_mzn_test;
use helpers::run_mzn_test_with_options;

macro_rules! mzn_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<false>(stringify!($name), "mzn_constraints");
        }
    };
}

macro_rules! cumulative_test {
    ($name:ident, $file:expr, $propagator: ident) => {
        paste::item! {
            #[test]
            fn [< cumulative_ $name _naive >] () {
                run_mzn_test_with_options::<false>(
                    stringify!($file),
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", stringify!($propagator), "--cumulative-explanation-type", "naive"],
                    Some(&format!("{}_naive", stringify!($name))),
                );
            }
        }

        paste::item! {
            #[test]
            fn [< cumulative_ $name _big_step >] () {
                run_mzn_test_with_options::<false>(
                    stringify!($file),
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", stringify!($propagator), "--cumulative-explanation-type", "big-step"],
                    Some(&format!("{}_big_step", stringify!($name))),
                );
            }
        }


        paste::item! {
            #[test]
            fn [< cumulative_ $name _pointwise >] () {
                run_mzn_test_with_options::<false>(
                    stringify!($file),
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", stringify!($propagator), "--cumulative-explanation-type", "pointwise"],
                    Some(&format!("{}_pointwise", stringify!($name))),
                );
            }
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

cumulative_test!(time_table_per_point, cumulative, TimeTablePerPoint);
cumulative_test!(
    time_table_per_point_incremental,
    cumulative,
    TimeTablePerPointIncremental
);
cumulative_test!(time_table_over_interval, cumulative, TimeTableOverInterval);
cumulative_test!(
    time_table_over_interval_incremental,
    cumulative,
    TimeTableOverIntervalIncremental
);

mzn_test!(all_different);
