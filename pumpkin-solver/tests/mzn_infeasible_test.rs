#![cfg(test)]

mod helpers;

use helpers::TestType;
use helpers::run_mzn_test;

macro_rules! mzn_infeasible_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_infeasible_test(stringify!($name), "mzn_infeasible");
        }
    };
}
mzn_infeasible_test!(prop_stress);
mzn_infeasible_test!(connected);

pub fn run_mzn_infeasible_test(instance_name: &str, folder_name: &str) {
    let _ = run_mzn_test::<false>(instance_name, folder_name, TestType::Unsatisfiable);
}
