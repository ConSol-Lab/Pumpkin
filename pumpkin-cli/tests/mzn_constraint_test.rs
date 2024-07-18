#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod helpers;

use helpers::run_mzn_test;

macro_rules! mzn_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_test::<false>(stringify!($name), "mzn_constraints");
        }
    };
}

mzn_test!(int_eq);
mzn_test!(int_eq_reif);
mzn_test!(set_in);
mzn_test!(array_int_maximum);
mzn_test!(array_int_minimum);
mzn_test!(int_min);
mzn_test!(int_max);
mzn_test!(int_lin_ne_reif);
mzn_test!(set_in_reif_interval);
mzn_test!(set_in_reif_sparse);
mzn_test!(cumulative);
mzn_test!(int_lin_le);
mzn_test!(int_lin_le_reif);
mzn_test!(int_lin_eq_reif);
mzn_test!(bool_xor_reif);
mzn_test!(bool_xor);
mzn_test!(bool_not);
mzn_test!(int_div);
mzn_test!(int_times);
mzn_test!(int_mod);
mzn_test!(bool_lin_eq);
mzn_test!(bool_lin_le);
