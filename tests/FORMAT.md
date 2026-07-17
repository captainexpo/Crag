# `.cragtest` file format

Sections understood by the test runner:

- `NAME:`
- `DESC:`
- `REQUIRES:` (comma-separated additional files)
- `ARGS:` (pipe-separated argument cases, run one at a time)
- `BEGIN CODE` / `END CODE`
- `BEGIN EXPECT ERR` / `END EXPECT ERR` (only for tests that must fail to compile or panic — see below)

Tests prove themselves at runtime using `std.testing.assert(condition, message)`
(`stdlib/testing.crag`, exposed as `std.testing` via `import "stdlib" as std;`).
`assert` panics and aborts (nonzero exit) on failure, so the runner just checks
the process's exit code — no expected-stdout block to keep in sync by hand.

Minimal example:

```text
//NAME: Hello
//BEGIN CODE

import "stdlib" as std;

fn main() -> i32 {
    let greeting: str = "hi";
    std.testing.assert(greeting == "hi", "greeting should be 'hi'");
    return 0;
}

//END CODE
```

## Tests that expect a compile or runtime error

A small number of tests intentionally fail to compile, or panic before any
assertion can run (out-of-bounds access, division by zero, etc.). For these,
there's no running program left to assert from, so they still declare the
exact expected stderr with `BEGIN EXPECT ERR` / `END EXPECT ERR`:

```text
//NAME: Division by zero
//BEGIN CODE

fn main() -> i32 {
    let a: i32 = 5;
    let b: i32 = 0;
    return a / b;
}

//END CODE
//BEGIN EXPECT ERR
//PANIC: Runtime Panic: Division by Zero
// at line 6, col 12
//END EXPECT ERR
```

If a test has no `EXPECT ERR` block, the runner requires it to exit 0 and
treats any nonzero exit (a failed `assert` or a crash) as a failure.
