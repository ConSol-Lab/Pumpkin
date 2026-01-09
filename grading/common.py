import json
import re
import subprocess

OKGREEN = "\033[92m"
FAIL = "\033[91m"
ENDC = "\033[0m"

IMPLEMENTATION_GRADE_CONTRIBUTION = 5.0


def passed_all_test_cases(test_name: str, timeout=60) -> bool:
    print(f"Running test cases {test_name}...")
    try:
        result = subprocess.run(
            ["cargo", "test", "--tests", test_name],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            timeout=timeout,
            text=True,
        )
    except subprocess.TimeoutExpired:
        print(f"\t{FAIL}TIMEOUT: {test_name} took longer than {timeout} seconds{ENDC}")
        print()
        return False

    num_matched = 0
    passed_all = True
    total_passed = None
    total_failed = None

    for line in result.stdout.splitlines():
        if line.startswith("test result"):
            regex = (
                r"test result: ([a-zA-Z]+). ([0-9]+) passed; ([0-9]+) failed; ([0-9]+) ignored; ([0-9]+) measured;"
                r" ([0-9]+)"
                r" filtered out"
            )
            compiled_regex = re.compile(regex)
            result = compiled_regex.match(line)

            if result is not None:
                _status = result.group(1)
                passed = int(result.group(2))
                failed = int(result.group(3))
                _ignored = int(result.group(4))
                _measured = int(result.group(5))

                if passed + failed == 0:
                    continue
                else:
                    num_matched += 1
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
