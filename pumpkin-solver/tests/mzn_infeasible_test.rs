#![cfg(test)]

mod helpers;

use helpers::run_mzn_test;
use helpers::TestType;

macro_rules! mzn_infeasible_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_mzn_infeasible_test(stringify!($name), "mzn_infeasible");
        }
    };
}
mzn_infeasible_test!(prop_stress);

pub fn run_mzn_infeasible_test(instance_name: &str, folder_name: &str) {
    let output = run_mzn_test::<false>(instance_name, folder_name, TestType::Unsatisfiable);
    assert!(output.ends_with("=====UNSATISFIABLE=====\n"));
}
