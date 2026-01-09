from common import IMPLEMENTATION_GRADE_CONTRIBUTION, check_nightly_installed, passed_all_test_cases

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
    check_nightly_installed()

    result = 0.0
    for test_name, grade_contribution in TEST_CASES_WITH_PERCENTAGE.items():
        if passed_all_test_cases(test_name):
            result += grade_contribution
    print("----------------------------------------------------------------------------------------")
    print()
    print(f"Final Points: {round(result, 2)} out of {IMPLEMENTATION_GRADE_CONTRIBUTION}")
