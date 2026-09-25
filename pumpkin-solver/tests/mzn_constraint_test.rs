#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;
use helpers::TestType;
use helpers::check_statistic_equality;
use helpers::run_mzn_test_with_options;
use pumpkin_macros::cumulative;
use pumpkin_macros::cumulative_synchronised;

macro_rules! mzn_test {
    ($name:ident) => {
        mzn_test!($name, stringify!($name), vec![]);
    };

    // Each constraint test runs once per conflict resolver, as `<name>::one_uip` and
    // `<name>::hypercube_linear`. A module is used because `macro_rules!` cannot form new
    // identifiers, so the resolver name cannot be appended to `$name` itself.
    ($name:ident, $file:expr, $options:expr) => {
        mod $name {
            use super::*;

            fn run(resolver_options: &[&str]) {
                let mut actual_options: Vec<String> = vec![];
                actual_options.extend($options);
                actual_options.extend(resolver_options.iter().map(|option| option.to_string()));

                let output = run_mzn_test_with_options::<false>(
                    $file,
                    "mzn_constraints",
                    false,
                    TestType::SolutionEnumeration,
                    actual_options,
                    stringify!($name),
                );
                assert!(output.ends_with("==========\n"));
            }

            #[test]
            fn one_uip() {
                run(&[]);
            }

            #[test]
            fn hypercube_linear() {
                run(&["--conflict-resolver", "hypercube-linear"]);
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
mzn_test!(binary_int_lin_eq);

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
mzn_test!(set_in_set);
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
mzn_test!(table);
mzn_test!(table_reif);

mzn_test!(bool2int);
mzn_test!(bool2int_set);
mzn_test!(bool2int_mix);

mzn_test!(disjunctive_strict);
mzn_test!(unbounded_integer);
