# Python pytest test suite that compiles every example with --emit-ir and checks output file
import os
import subprocess
import glob
import re
from enum import Enum
from concurrent.futures import ProcessPoolExecutor
from typing import Any, Optional
from pathlib import Path
import time


def color_text(text: Any, rgb: tuple[int, int, int]) -> str:
    text = str(text)
    r, g, b = rgb
    return f"\033[38;2;{r};{g};{b}m{text}\033[0m"


def parse_test_file(text: str):
    SECTION_NAME = re.compile(r"^NAME:\s*(.*)")
    SECTION_DESC = re.compile(r"^DESC:\s*(.*)")
    BEGIN_CODE = "BEGIN CODE"
    END_CODE = "END CODE"
    BEGIN_EXPECT_OUT = "BEGIN EXPECT"
    END_EXPECT_OUT = "END EXPECT"

    BEGIN_EXPECT_ERR = "BEGIN EXPECT ERR"
    END_EXPECT_ERR = "END EXPECT ERR"

    lines = text.splitlines()

    name = None
    desc = None
    code = []
    expect = []
    expect_err = []

    mode = None  # None, "code", "expect", "expect_err"

    for _line in lines:
        line = _line[2:] # skip the leading comment characters
        m = SECTION_NAME.match(line)
        if m:
            name = m.group(1)
            continue

        m = SECTION_DESC.match(line)
        if m:
            desc = m.group(1)
            continue

        if line.strip() == BEGIN_CODE:
            mode = "code"
            continue
        if line.strip() == END_CODE:
            mode = None
            continue

        if line.strip() == BEGIN_EXPECT_OUT:
            mode = "expect"
            continue
        if line.strip() == END_EXPECT_OUT:
            mode = None
            continue
        if line.strip() == BEGIN_EXPECT_ERR:
            mode = "expect_err"
            continue
        if line.strip() == END_EXPECT_ERR:
            mode = None
            continue

        if mode == "code":
            code.append(_line)
        elif mode == "expect":
            expect.append(line.encode("utf-8").decode("unicode_escape"))
        elif mode == "expect_err":
            expect_err.append(line.encode("utf-8").decode("unicode_escape"))

    return {
        "name": name,
        "desc": desc,
        "code": "\n".join(code).rstrip(),
        "expect": "\n".join(expect).rstrip(),
        "expect_err": "\n".join(expect_err).rstrip(),
    }


ROOT = os.path.dirname(os.path.dirname(__file__))
COMP_CANDIDATES = [os.path.join(ROOT, "build", "crag"), os.path.join(ROOT, "crag"), os.path.join(ROOT, "build", "comp"), os.path.join(ROOT, "comp")]


def find_compiler() -> Optional[str]:
    for p in COMP_CANDIDATES:
        if os.path.isfile(p) and os.access(p, os.X_OK):
            return p


class TestResult(Enum):
    PASS = 1
    FAIL = 2
    SKIP = 3


C_RED = (255, 0, 0)
C_GREEN = (0, 255, 0)
C_BLUE = (0, 0, 255)
C_YELLOW = (255, 255, 0)


comp = find_compiler()


def run_unit_test(
    tmp_path: Path, src_path: str
) -> tuple[TestResult, list[str], float]:  # result, output lines, time
    outs: list[str] = []

    def printc(text: Any, color: tuple[int, int, int]) -> None:
        nonlocal outs
        outs.append(color_text(text, color))

    if comp is None or not isinstance(comp, str):
        printc("Compiler not found, skipping test.", C_YELLOW)
        return TestResult.SKIP, outs, 0
    outdir = tmp_path / "out"
    outdir.mkdir(exist_ok=True)
    base = os.path.splitext(os.path.basename(src_path))[0]
    out_base = str(outdir / base)
    test_file_data = parse_test_file(open(src_path).read())
    src_code = test_file_data["code"]
    expected_output = test_file_data["expect"]
    src_file = tmp_path / (base + ".y")
    with open(src_file, "w") as f:
        f.write(src_code)
    cmd = [comp, str(src_file), "-o", out_base]

    start_time = time.time()

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=1)
    except subprocess.TimeoutExpired:
        printc("Compilation timed out.", C_RED)
        return TestResult.FAIL, outs, time.time() - start_time
    if not result.returncode == 0:
        printc("Compilation failed:\n", C_RED)
        printc(f"----------Stdout----------\n{result.stdout.strip()}\n", C_RED)
        printc(
            f"----------Stderr----------\n{result.stderr.strip()}\n--------------------------",
            C_RED,
        )
        return TestResult.FAIL, outs, time.time() - start_time
    # call the generated binary
    bin_file = out_base
    try:
        result = subprocess.run([bin_file], capture_output=True, text=True, timeout=1)
    except subprocess.TimeoutExpired:
        printc("Execution timed out.", C_RED)
        return TestResult.FAIL, outs, time.time() - start_time
    end_time = time.time()
    total_time = end_time - start_time
    if result.returncode != 0:
        if not test_file_data["expect_err"].strip():
            printc(
                f"Execution failed:\nStdout:{result.stdout}\nStderr:\n{result.stderr}",
                C_RED,
            )
            return TestResult.FAIL, outs, time.time() - start_time
        if test_file_data["expect_err"]:
            expected_err = test_file_data["expect_err"]
            actual_err = result.stderr.rstrip()
            if actual_err != expected_err:
                printc("Error output mismatch:", C_RED)
                printc("Expected Stderr:", C_RED)
                printc(expected_err, C_RED)
                printc("Actual Stderr:", C_RED)
                printc(actual_err, C_RED)
                return TestResult.FAIL, outs, total_time
            else:
                return TestResult.PASS, outs, total_time
    elif test_file_data["expect_err"].strip():
        printc("Expected execution to fail but it succeeded.", C_RED)
        return TestResult.FAIL, outs, total_time
    actual_output = result.stdout.rstrip()
    if test_file_data["expect"]:
        if actual_output != expected_output:
            printc("Output mismatch:", C_RED)
            printc("Expected Stdout:", C_RED)
            printc(expected_output, C_RED)
            printc("Actual Stdout:", C_RED)
            printc(actual_output, C_RED)
            return TestResult.FAIL, outs, total_time
    return TestResult.PASS, outs, total_time


def main():
    TEST_FILES = glob.glob(os.path.join(ROOT, "tests", "*.cragtest"))

    passed = 0
    failed = 0
    skipped = 0
    fails = []
    skips = []

    import tempfile

    with tempfile.TemporaryDirectory() as tmpdirname:
        tmp_path = Path(tmpdirname)
        # Create executor once
        with ProcessPoolExecutor() as pool:
            # Submit each test exactly once
            futures = {
                pool.submit(run_unit_test, tmp_path, test_path): test_path
                for test_path in TEST_FILES
            }

            total_time = 0.0
            total_tests = len(futures)
            # Process results
            for future in futures:
                test_path = futures[future]

                result, out, time = future.result()
                if result == TestResult.PASS:
                    print(color_text(".", C_GREEN), end="", flush=True)
                    passed += 1
                elif result == TestResult.FAIL:
                    print(color_text("F", C_RED), end="", flush=True)
                    failed += 1
                    fails.extend([test_path] + out)
                else:
                    print(color_text("S", C_YELLOW), end="", flush=True)
                    skipped += 1
                    skips.extend([test_path] + out)
                total_time += time

        print()

    print(
        f"Passed: {color_text(passed, C_GREEN)}, Failed: {color_text(failed, C_RED)}, Skipped: {color_text(skipped, C_YELLOW)}"
    )

    if skipped > 0:
        print("Skipped details:")
        for output in skips:
            print(output)
    if failed > 0:
        print("Failure details:")
        for output in fails:
            print(output)

    exit(failed)


if __name__ == "__main__":
    main()
