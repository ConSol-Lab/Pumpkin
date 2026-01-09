import json
import subprocess

OKGREEN = "\033[92m"
FAIL = "\033[91m"
ENDC = "\033[0m"

IMPLEMENTATION_GRADE_CONTRIBUTION = 5.0


def passed_all_test_cases(test_name: str, timeout=60, crate="implementation") -> bool:
    print(f"Running test cases {test_name}...")
    try:
        result = subprocess.run(
            [
                "cargo",
                "+nightly",
                "test",
                "-p",
                crate,
                test_name,
                "--",
                "-Z",
                "unstable-options",
                "--format",
                "json",
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            text=True,
        )
    except subprocess.TimeoutExpired:
        print(f"\t{FAIL}TIMEOUT: {test_name} took longer than {timeout} seconds{ENDC}")
        print()
        return False

    num_matched = 0
    passed_all = False
    total_passed = None
    total_failed = None

    for line in result.stdout.splitlines():
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

    if "type" in msg and msg["type"] == "suite":
        assert "passed" in msg
        assert "failed" in msg

        num_matched += 1

        passed = msg["passed"]
        failed = msg["failed"]

        passed_all = failed == 0

        total_passed = passed
        total_failed = failed

    if num_matched == 0:
        raise Exception(f"No test cases ran for {test_name}")
    elif num_matched > 1:
        raise Exception(
            f"More than 1 test cases ran for {test_name}; this indicates that there are overlapping test cases"
        )

    if passed_all:
        print(f"\t{OKGREEN}Passed {total_passed} / {total_passed + total_failed} test cases for {test_name}{ENDC}")
    else:
        print(f"\t{FAIL}Passed {total_passed} / {total_passed + total_failed} test cases for {test_name}{ENDC}")
    print()

    return passed_all
