# Crag

> [!WARNING]
> The Crag compiler is merely a recreational toy language and is not suitable for production code. Expect frequent breaking changes, incomplete features, spaghetti code, and informal commits. Contributions and feedback are welcome!

Crag is a compiled systems programming language with a custom frontend and an LLVM backend.

This repository contains:

- The Crag compiler (`src/`)
- Platform runtime sources (`runtime/`)
- A small standard library (`stdlib/`)
- Language examples (`examples/`)
- An end-to-end test suite (`tests/`)

## Current status

- Backend: LLVM (`--backend llvm`)
- Runtime safety checks: enabled by default (null pointer, bounds, divide-by-zero)
- Platform runtime directories exist for Linux, macOS, and Windows

## Requirements

- CMake 3.10+
- A C++20 compiler
- LLVM development tools and libraries available through `llvm-config`
  - `llvm-config --cxxflags`
  - `llvm-config --ldflags --libs core support irreader`
- Python 3 (for running tests)

## Build

From the repository root:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

Build outputs:

- Compiler executable: `build/crag`
- Runtime static library: `build/lib/libruntime.a`

## Quick start

Create a file `hello.crag`:

```crag
import "stdlib" as std;

fn main() -> i32 {
	std::cstdio::printf("Hello, Crag!\n");
	return 0;
}
```

Compile and run:

```bash
./build/crag hello.crag -o hello
./hello
```

## Compiler usage

```text
Crag Compiler [options] input
```

### Positional argument

- `input` — source file to compile

### Options

- `-o, --output <name>`: output filename (default: `main`)
- `--emit-ir`: emit LLVM IR instead of object code
- `--runtime-path <path>`: path to runtime static library
- `--backend <name>`: backend to use (currently only `llvm`)
- `-ODebug`: debug optimization profile
- `-ORelease`: release optimization profile
- `--unsafe`: disable runtime safety checks
- `--no-runtime`: do not link the runtime library
- `-h, --help`: help
- `-v, --version`: version

### Build products by output extension

- Output ends with `.o` → emits object file only
- Otherwise → emits object file then links executable

## Standard library and imports

Crag supports relative file imports and shortcut imports.

### Built-in import shortcuts

- `stdlib` → standard umbrella module
- `string`
- `vector`
- `random`
- `libc`

Example:

```crag
import "stdlib" as std;
import "string" as string;
```

### `CRAGSTD` environment variable

Set `CRAGSTD` to the directory containing `std.crag`.

Example expected layout:

```text
$CRAGSTD/std.crag
$CRAGSTD/string.crag
$CRAGSTD/vector.crag
...
```

### Project-local import shortcuts with `.cragrc`

Create `.cragrc` next to your entry module:

```ini
# alias = path
mylib = "./vendor/mylib/main.crag"
game = "./src/game.crag"
```

Then in code:

```crag
import "mylib" as mylib;
import "game" as game;
```

## Language examples

### Functions, loops, and function pointers

```crag
extern fn printf(fmt: *u8, ...) -> i32;

fn twice(x: i32) -> i32 {
	return x * 2;
}

fn main() -> i32 {
	let f: fn(i32) -> i32 = &twice;
	for (let i: i32 = 0; i < 3; i += 1) {
		printf("%d\n", f(i));
	}
	return 0;
}
```

### Structs with methods

```crag
struct Counter {
	value: i32,

	fn inc(self: *Counter) -> void {
		self.value += 1;
	}
}

fn main() -> i32 {
	let c = Counter { value: 0 };
	c.inc();
	return 0;
}
```

### Generics

```crag
struct Pair<T> {
	a: T,
	b: T,
}

fn main() -> i32 {
	let p = Pair::<i32> { a: 1, b: 2 };
	return p.a + p.b;
}
```

### Error unions (`T!E`)

```crag
enum ParseErr(i32) {
	BAD = 1,
}

fn parse(ok: bool) -> i32!ParseErr {
	if (!ok) {
		return! ParseErr:BAD;
	}
	return 42;
}
```

## Runtime safety model

By default, generated programs include checks for:

- null pointer dereference
- out-of-bounds array access
- division by zero

To disable checks:

```bash
./build/crag input.crag --unsafe
```

## Tests

Run all tests:

```bash
./run_tests.sh
```

or:

```bash
python3 tests/run_all.py
```

The runner:

- compiles each `tests/*.cragtest`
- executes produced binaries
- compares stdout/stderr against expectations

### `.cragtest` file format

Sections understood by the test runner:

- `NAME:`
- `DESC:`
- `REQUIRES:` (comma-separated additional files)
- `BEGIN CODE` / `END CODE`
- `BEGIN EXPECT` / `END EXPECT`
- `BEGIN EXPECT ERR` / `END EXPECT ERR`

Minimal example:

```text
//NAME: Hello
//BEGIN CODE
extern fn printf(fmt: *u8, ...) -> i32;
fn main() -> i32 { printf("hi\n"); return 0; }
//END CODE
//BEGIN EXPECT
//hi
//END EXPECT
```

## Repository layout

```text
src/        compiler frontend + backend integration
runtime/    platform panic/runtime sources
stdlib/     standard library modules
examples/   example Crag programs
tests/      integration tests and runner
lib/        generated artifacts (for runtime build steps)
```

## Example programs

- `examples/fibb.crag` — vector + loops
- `examples/strings.crag` — string module usage
- `examples/modules/` — multi-file imports
- `examples/raylib/` — raylib bindings demo

Compile any example:

```bash
./build/crag examples/fibb.crag -o fibb
./fibb
```

