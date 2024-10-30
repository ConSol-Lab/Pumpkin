#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;

use helpers::check_statistic_equality;
use helpers::run_mzn_test_with_options;
use test_macros::cumulative;

macro_rules! statistic_equality_cumulative {
    ($first_name:ident, $second_name:ident) => {
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _naive>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "naive"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "naive"],
                    stringify!(< equality_ $first_name _naive>),
                    stringify!(< equality_ $second _naive>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _big_step>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "big-step"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "big-step"],
                    stringify!(< equality_ $first_name _big_step>),
                    stringify!(< equality_ $second _big_step>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _pointwise>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "pointwise"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "pointwise"],
                    stringify!(< equality_ $first_name _pointwise>),
                    stringify!(< equality_ $second _pointwise>),
                );
            }
        }

        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _naive_sequence>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence"],
                    stringify!(< equality_ $first_name _naive_sequence>),
                    stringify!(< equality_ $second _naive_sequence>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _big_step_sequence>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence"],
                    stringify!(< equality_ $first_name _big_step_sequence>),
                    stringify!(< equality_ $second _big_step_sequence>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _pointwise_sequence>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"],
                    stringify!(< equality_ $first_name _pointwise_sequence>),
                    stringify!(< equality_ $second _pointwise_sequence>),
                );
            }
        }

        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _naive_sequence_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence", "--cumulative-incremental-backtracking"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence", "--cumulative-incremental-backtracking"],
                    stringify!(< equality_ $first_name _naive_sequence_incremental_backtracking>),
                    stringify!(< equality_ $second _naive_sequence_incremental_backtracking>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _big_step_sequence_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence", "--cumulative-incremental-backtracking"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence", "--cumulative-incremental-backtracking"],
                    stringify!(< equality_ $first_name _big_step_sequence_incremental_backtracking>),
                    stringify!(< equality_ $second _big_step_sequence_incremental_backtracking>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _pointwise_sequence_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"],
                    stringify!(< equality_ $first_name _pointwise_sequence_incremental_backtracking>),
                    stringify!(< equality_ $second _pointwise_sequence_incremental_backtracking>),
                );
            }
        }

        paste::item! {
           #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _naive_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "naive", "--cumulative-incremental-backtracking"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "naive", "--cumulative-incremental-backtracking"],
                    stringify!(< equality_ $first_name _naive_incremental_backtracking>),
                    stringify!(< equality_ $second _naive_incremental_backtracking>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _big_step_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "big-step", "--cumulative-incremental-backtracking"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "big-step", "--cumulative-incremental-backtracking"],
                    stringify!(< equality_ $first_name _big_step_incremental_backtracking>),
                    stringify!(< equality_ $second _big_step_incremental_backtracking>),
                );
            }
        }
        paste::item! {
            #[test]
            fn [< cumulative_ $first_name _equal_with_ $second_name _pointwise_incremental_backtracking>]() {
                check_statistic_equality(
                    "cumulative",
                    "mzn_constraints",
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($first_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-incremental-backtracking"],
                    vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($second_name)),"--cumulative-explanation-type", "pointwise", "--cumulative-incremental-backtracking"],
                    stringify!(< equality_ $first_name _pointwise_incremental_backtracking>),
                    stringify!(< equality_ $second _pointwise_incremental_backtracking>),
                );
            }
        }

    };
}

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

macro_rules! cumulative {
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

        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _naive_sequence >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _big_step_sequence >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _pointwise_sequence >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"]
            );
        }

        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _naive_sequence_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)),"--cumulative-explanation-type", "naive", "--cumulative-generate-sequence"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _big_step_sequence_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "big-step", "--cumulative-generate-sequence"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _pointwise_sequence_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "pointwise", "--cumulative-generate-sequence"]
            );
        }

        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _naive_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)),"--cumulative-explanation-type", "naive", "--cumulative-incremental-backtracking"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _big_step_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "big-step", "--cumulative-incremental-backtracking"]
            );
        }
        paste::item! {
            mzn_test!(
                [< cumulative_ $propagator _pointwise_incremental_backtracking >],
                "cumulative",
                vec!["--cumulative-propagation-method", &stringcase::kebab_case(stringify!($propagator)), "--cumulative-explanation-type", "pointwise", "--cumulative-incremental-backtracking"]
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

cumulative!(time_table_per_point);
cumulative!(time_table_per_point_incremental);
cumulative!(time_table_per_point_incremental_synchronised);
cumulative!(time_table_over_interval);
cumulative!(time_table_over_interval_incremental);
cumulative!(time_table_over_interval_incremental_synchronised);

statistic_equality_cumulative!(
    time_table_per_point,
    time_table_per_point_incremental_synchronised
);
statistic_equality_cumulative!(
    time_table_over_interval,
    time_table_over_interval_incremental_synchronised
);

mzn_test!(all_different);
