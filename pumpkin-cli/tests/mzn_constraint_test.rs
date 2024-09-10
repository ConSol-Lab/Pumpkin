#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;

use helpers::run_mzn_test_with_options;

macro_rules! mzn_test {
    ($name:ident) => {
        mzn_test!($name, stringify!($name), vec![]);
    };

    ($name:ident, $file:expr, $options:expr) => {
        #[test]
        fn $name() {
            run_mzn_test_with_options::<false>(
                $file,
                "mzn_constraints",
                $options,
                stringify!($name),
            );
        }
    };
}

macro_rules! cumulative_test {
    ($propagator:ident) => {
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _naive >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)),"--cumulative-explanation-type", "naive"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _big_step >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "big-step"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _pointwise >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "pointwise"]
            );
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

cumulative_test!(time_table_per_point);
cumulative_test!(time_table_per_point_incremental);
cumulative_test!(time_table_over_interval);
cumulative_test!(time_table_over_interval_incremental);

mzn_test!(all_different);
