# Python pytest test suite that compiles every example with --emit-ir and checks output file
import os
import subprocess
import glob
import pytest

ROOT = os.path.dirname(os.path.dirname(__file__))
COMP_CANDIDATES = [os.path.join(ROOT, "build", "comp"), os.path.join(ROOT, "comp")]

def find_compiler():
    for p in COMP_CANDIDATES:
        if os.path.isfile(p) and os.access(p, os.X_OK):
            return p
    pytest.skip("compiler binary not found; build the project first (cmake && ninja)")

EXAMPLES = []
# top-level examples
EXAMPLES += glob.glob(os.path.join(ROOT, "examples", "*.y"))
# modules main (tests will compile main which imports other module files)
EXAMPLES += [os.path.join(ROOT, "examples", "modules", "main.y")]

@pytest.mark.parametrize("src_path", EXAMPLES)
def test_compile_example_emit_ir(tmp_path, src_path):
    comp = find_compiler()
    # Narrow types for the type checker: make sure these are plain strings.
    assert comp is not None and isinstance(comp, str)
    assert isinstance(src_path, str)
    outdir = tmp_path / "out"
    outdir.mkdir()
    base = os.path.splitext(os.path.basename(src_path))[0]
    out_base = str(outdir / base)
    # run compiler with emit-ir so clang is not invoked
    proc = subprocess.run([comp, src_path, "-o", out_base, "--emit-ir"],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    assert proc.returncode == 0, f"Compiler failed for {src_path}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    ir_path = out_base + ".tmp.ll"
    assert os.path.isfile(ir_path), f"Expected IR file {ir_path} to exist for {src_path}"