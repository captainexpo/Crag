# Python pytest test suite that compiles every example with --emit-ir and checks output file
import os
import subprocess
import glob
import re
from enum import Enum
from typing import Any, Optional
from pathlib import Path

def color_text(text: Any, rgb: tuple[int, int, int]) -> str:
    text = str(text)
    r, g, b = rgb
    return f"\033[38;2;{r};{g};{b}m{text}\033[0m"

def parse_test_file(text: str):
    SECTION_NAME = re.compile(r"^NAME:\s*(.*)")
    SECTION_DESC = re.compile(r"^DESC:\s*(.*)")
    BEGIN_CODE = "BEGIN CODE"
    END_CODE = "END CODE"
    BEGIN_EXPECT = "BEGIN EXPECT"
    END_EXPECT = "END EXPECT"

    lines = text.splitlines()

    name = None
    desc = None
    code = []
    expect = []

    mode = None  # None, "code", "expect"

    for line in lines:
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

        if line.strip() == BEGIN_EXPECT:
            mode = "expect"
            continue
        if line.strip() == END_EXPECT:
            mode = None
            continue

        if mode == "code":
            code.append(line)
        elif mode == "expect":
            expect.append(line.encode("utf-8").decode("unicode_escape"))

    return {
        "name": name,
        "desc": desc,
        "code": "\n".join(code).rstrip(),
        "expect": "\n".join(expect).rstrip(),
    }

ROOT = os.path.dirname(os.path.dirname(__file__))
COMP_CANDIDATES = [os.path.join(ROOT, "build", "comp"), os.path.join(ROOT, "comp")]

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



def run_unit_test(tmp_path: Path, src_path: str) -> (list[str], TestResult):
    g_outputs: list[str] = []
    def printc(text: Any, color: tuple[int, int, int]) -> None:
        nonlocal g_outputs
        g_outputs.append(color_text(text, color))
    comp = find_compiler()
    if comp is None or not isinstance(comp, str):
        print("Compiler not found, skipping test.")
        return TestResult.SKIP, g_outputs
    if not isinstance(src_path, str):
        print("Invalid source path, skipping test.")
        return TestResult.SKIP
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
    result = subprocess.run(cmd, capture_output=True, text=True)
    if not result.returncode == 0:
        printc("Compilation failed:\n", C_RED)
        printc(f"----------Stdout----------\n{result.stdout.strip()}\n", C_RED)
        printc(f"----------Stderr----------\n{result.stderr.strip()}\n--------------------------", C_RED)
        return TestResult.FAIL,  g_outputs
    # call the generated binary
    bin_file = out_base
    result = subprocess.run([bin_file], capture_output=True, text=True)
    if result.returncode != 0:
        printc(f"Execution failed:\nStdout{result.stdout}\nStderr:\n{result.stderr}", C_RED)
        return TestResult.FAIL, g_outputs
    actual_output = result.stdout.rstrip()
    if actual_output != expected_output:
        printc("Output mismatch:", C_RED)
        printc("Expected Output:", C_RED)
        printc(expected_output, C_RED)
        printc("Actual Output:", C_RED)
        printc(actual_output, C_RED)
        return TestResult.FAIL, g_outputs
    return TestResult.PASS, g_outputs


def main():
    EXAMPLES = []
    EXAMPLES += glob.glob(os.path.join(ROOT, "tests", "*.ytest"))

    total = len(EXAMPLES)
    passed = 0
    failed = 0
    skipped = 0
    outs = []
    import tempfile
    with tempfile.TemporaryDirectory() as tmpdirname:
        tmp_path = os.path.abspath(tmpdirname)
        for example in EXAMPLES:
            result, out = run_unit_test(Path(tmp_path), example)
            if result == TestResult.PASS:
                print(color_text(".", C_GREEN), end="", flush=True)
                passed += 1
            elif result == TestResult.FAIL:
                print(color_text("F", C_RED), end="", flush=True)
                failed += 1
                outs.extend([example] + out)
            else:
                print(color_text("S", C_YELLOW), end="", flush=True)
                skipped += 1
        print()
    print(f"Passed: {color_text(passed, C_GREEN)}, Failed: {color_text(failed, C_RED)}, Skipped: {color_text(skipped, C_YELLOW)}")
    if failed > 0:
        print("Failure details:")
        for output in outs:
            print(output)
    exit(failed)

if __name__ == "__main__":
    main()
