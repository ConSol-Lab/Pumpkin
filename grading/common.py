import json
import subprocess

OKGREEN = "\033[92m"
FAIL = "\033[91m"
ENDC = "\033[0m"

IMPLEMENTATION_GRADE_CONTRIBUTION = 5.0


def build_test_cases(crate: str = "implementation"):
    try:
        subprocess.run(
            [
                "cargo",
                "+nightly",
                "build",
                "--tests",
                "-p",
                crate,
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        print(f"{FAIL}Building test cases failed with the following error log:\n{e.stderr}{ENDC}")
        raise e


def passed_all_test_cases(
    test_name: str, log_failed: bool = False, timeout: int = 60, crate: str = "implementation"
) -> bool:
    """Runs the test cases with `test_name` as name and returns whether *all* test cases passed.

    :param test_name: The name of the test cases to run.
    :param log_failed: Whether to log the failed test cases.
    :param timeout: The timeout in seconds for the command (60 seconds by default).
    :param crate: The crate which contains the test cases ('implementation' by default).
    :return: True if all test cases passed and false otherwise.
    """
    print(f"Running test cases {test_name}...")
    try:
        result = subprocess.run(
            [
                "cargo",
                "+nightly",
                "test",
                "-p",
                crate,
                "--features",
                "pumpkin-core/check-propagations",
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

        if "type" in msg and msg["type"] == "suite" and "passed" in msg and "failed" in msg:
            # A test suite has been run, we should retrieve the information

            passed = msg["passed"]
            failed = msg["failed"]

            if passed + failed != 0:
                num_matched += 1

            passed_all = failed == 0

            total_passed = passed
            total_failed = failed
        elif log_failed and "type" in msg and msg["type"] == "test" and "event" in msg and msg["event"] == "failed":
            # A failing test case; since `log_failed` is enabled, we should log it
            assert "name" in msg
            print(f"\t{FAIL}Failed:{ENDC} {msg['name']}")

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
