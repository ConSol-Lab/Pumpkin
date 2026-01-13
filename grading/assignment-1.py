import argparse

from common import IMPLEMENTATION_GRADE_CONTRIBUTION, build_test_cases, passed_all_test_cases

TEST_CASES_WITH_PERCENTAGE = {
    "propagators::linear_tests::linear_propagation_tests": 5.0 / 6.0,
    "propagators::linear_tests::linear_conflict_tests": 5.0 / 6.0,
    "propagators::linear_tests::linear_checker_tests": 5.0 / 6.0,
    "propagators::circuit_tests::circuit_propagation_tests": 5.0 / 6.0,
    "propagators::circuit_tests::circuit_conflict_tests": 5.0 / 6.0,
    "propagators::circuit_tests::circuit_checker_tests": 5.0 / 6.0,
}
assert sum(TEST_CASES_WITH_PERCENTAGE.values()) == IMPLEMENTATION_GRADE_CONTRIBUTION

if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="Grader Assignment 1")
    parser.add_argument(
        "--log",
        action="store_true",
        help="When this option is enabled, the names of the failed test cases are logged for debugging purposes",
    )
    args = parser.parse_args()

    print(f"Building test cases...\n")
    build_test_cases()

    result = 0.0
    for test_name, grade_contribution in TEST_CASES_WITH_PERCENTAGE.items():
        if passed_all_test_cases(test_name, args.log):
            result += grade_contribution
    print("----------------------------------------------------------------------------------------")
    print()
    print(f"Final Points: {round(result, 2)} out of {IMPLEMENTATION_GRADE_CONTRIBUTION}")
