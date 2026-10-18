# Crag

> [!WARNING]
> The Crag compiler is merely a recreational toy language and is not suitable for production code. Contributions and feedback are welcome!

Crag is a systems programming language that is designed to be simple, similarly to C, but with more modern syntax and features, similar to Zig and Rust.

The language was created as a learning project to explore compiler design and implementation, and to create a language that had all the good parts of C, while adding more modern features that improve the developer experience. At this point, it is not intended to be a production-ready language (and probably never will be), but is instead a fun project to work on and learn from.


## Language Features

- Zig/Rust inspired syntax
- C-like semantics
- Manual memory management
- Simple runtime safety checks (e.g. bounds checking, null pointer checks, division by zero)
- Module system with `.` member access and selective imports
- Generics for functions and types
- Structs with methods and static methods
- Error unions (`T!E`) with `try` / `catch`
- Tuple types and tuple literals
- `if` expressions
- Native `str` type (pointer + length)
- Function pointers, `switch`, enums, unions, type aliases and inline assembly
- Small standard library (I/O, strings, `ArrayList`, arenas, math, random, time, testing, libc bindings)
- Built on top of LLVM for code generation and optimization (bloated and slow, but it works)

## Usage

The Crag compiler is a command-line tool that takes a Crag source file and compiles it to an executable. The basic usage is as follows:

```bash
./crag <source_file.crag> -o <output_executable>
```

There are also some additional flags that can be used to control the compilation process:

- `-ODebug` (debug symbols, no optimization), `-ORelease` (no debug symbols, equivalent of -O3 optimization) - Optimization levels (default is `-ODebug`)
- Any linker flags needed to link against other libraries (e.g. `-lm` to link against the math library)
- `--unsafe` - Disable runtime safety checks.
- `--emit-ir` - Emit LLVM IR instead of a native executable
- `--dump-ast [asa|bsa]` - Dump the abstract syntax tree (AST) in either before semantic analysis (BSA) or after semantic analysis (ASA) form. This is useful for debugging and understanding how the compiler processes the source code.

## Syntax Examples

### Hello World

```zig
import "stdlib" as std;

fn main() -> i32 {
    std.io.println("Hello, World!");
    return 0;
}
```

### Fibonacci

```zig
fn fib(n: i32) -> i32 {
    let a: i32 = 0;
    let b: i32 = 1;
    let temp: i32;
    for (let i: i32 = 0; i < n; i++) {
        temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}
```

### Modules and imports

Module members are accessed with `.`. An import can also bring specific symbols or sub-modules directly into scope with a `{ ... }` list, and these lists can be nested:

```zig
import "stdlib" as std { arraylist { ArrayList } };

fn main() -> i32 {
    let list = ArrayList.new::<i64>();   // instead of std.arraylist.ArrayList
    list.append(1);
    std.io.println("appended");
    return 0;
}
```

`pub import` re-exports a module from the importing module.

### Generics

Bare `Foo<T>` is only allowed where a type is expected. In an expression (a struct literal, a function call, a static method call), type arguments use the `::<T>` turbofish:

```zig
struct Vec2<T> {
    x: T,
    y: T,
}

fn id<T>(x: T) -> T {
    return x;
}

fn main() -> i32 {
    let v: Vec2<f64> = Vec2::<f64>{ x: 3.0, y: 4.0 };
    let n = id::<i32>(5);
    return 0;
}
```

### Structs, methods and static methods

Methods take an explicit `self` parameter. Functions marked `static` don't take `self` and are called on the type: `Type.fn()`. On a generic struct, a static method declares its own type parameters.

```zig
struct Node<T> {
    value: T,
    next: *Node<T>,

    static fn new<T>(value: T) -> Node<T> {
        return Node::<T>{ value: value, next: null };
    }

    fn append(self: *Node<T>, node: *Node<T>) -> void {
        let current = self;
        while (current.next != null) {
            current = current.next;
        }
        current.next = node;
    }
}

fn main() -> i32 {
    let head = Node.new::<i32>(1);
    let second = Node.new::<i32>(2);
    head.append(&second);
    return 0;
}
```

### Error unions, `try` and `catch`

`T!E` is either a `T` or an error `E`. `return!` returns an error, `try` passes an error up to the caller, and `catch` supplies a fallback value. `|err|` captures the error value.

```zig
enum MathError(i32) { DivisionByZero = 0 }

fn divide(a: i32, b: i32) -> i32!MathError {
    if (b == 0) {
        return! MathError:DivisionByZero;
    }
    return a / b;
}

fn divide_twice(a: i32, b: i32) -> i32!MathError {
    let once = try divide(a, b);    // passes the error up if divide failed
    let twice = try divide(once, b);
    return twice;
}

fn main() -> i32 {
    let a = divide(10, 2) catch 0;                // 5
    let b = divide_twice(100, 0) catch |err| -1;  // -1
    return 0;
}
```

An error union can also be inspected by hand with `.is_err`, `.ok` and `.err`.

### Tuples and `if` expressions

```zig
fn min_max(a: i32, b: i32) -> (i32, i32) {
    return if (a < b) (a, b) else (b, a);
}

fn main() -> i32 {
    let mm = min_max(7, 3);
    let lo = mm.0;  // 3
    let hi = mm.1;  // 7
    return 0;
}
```

An `if` expression always needs an `else` branch.

### Strings and function pointers

`str` is a native string type that carries its length. It supports `.len`, indexing and `==`, and it can be cast to `*const u8` to pass to C functions.

```zig
import "stdlib" as std;

fn is_even(x: i64) -> bool {
    return x % 2 == 0;
}

fn main() -> i32 {
    let name: str = "crag";
    let first: u8 = name[0];
    let greeting = std.string.concat("Hello, ", name);

    let pred: fn(i64) -> bool = &is_even;
    let list = std.arraylist.ArrayList.new::<i64>();
    list.append(1);
    list.append(4);
    let first_even = list.find(pred) catch -1;   // 4
    return 0;
}
```

## Architecture

### Lexer/Tokenizer

The lexer reads the source code and produces a list of tokens. It handles whitespace, comments, identifiers, keywords, literals, and operators. It can be found in `src/lexer.h`.

### Parser

The parser takes the list of tokens from the lexer and produces an abstract syntax tree (AST). It uses a recursive descent parsing approach, with operator precedence parsing for expressions. The parser can be found in `src/parser.h` and the ast node structs can be found in `src/ast/ast.h`.

### Module resolver

The module resolver runs the lexer and parser and resolves module imports and exports. It also handles the Crag standard library and any user-defined modules. The module resolver can be found in `src/module_resolver.h`

### Semantic Analysis

The semantic analysis phase takes the AST and performs type checking, scope resolution, and other semantic checks. It also builds symbol tables and resolves/instantiates types for generic functions and structs. The semantic analysis can be found in `src/typechecking/typechecker.h` (the typechecker/sema core), `src/typechecking/tables.h` (symbol tables), `src/typechecking/templates.cpp` (template/generic instantiation logic).

### Code Generation

The code generation phase takes the semantically analyzed AST and generates LLVM IR code. It uses the LLVM IR Builder C++ API to create modules, functions, basic blocks, and instructions. The code generation can be found in `src/backends/llvmcodegen.h`.

## Building the Compiler

### Requirements

- CMake 3.30+
- A C++26 compiler
- LLVM 21+
- LLVM development tools and libraries available through `llvm-config`
  - `llvm-config --cxxflags`
  - `llvm-config --ldflags --libs core support irreader`
- Python 3 (optional - for running tests)

### Building

From the repository root:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

Build outputs:

- Compiler executable: `build/crag`
- Runtime static library: `build/lib/libruntime.a`

Alternatively, you can use the provided `Dockerfile` to build the compiler in a containerized environment.

To build and run a terminal with the compiler in a Docker container:

```bash
docker build -t crag .
docker run --rm -it -v $(pwd):/work:Z --entrypoint bash crag
```

Instead of docker, you can also use `podman` if you prefer.


## Building a Crag Program

This example will build a simple Crag program that runs the game `pong`. The souce code is located in the `examples/pong` directory.

```bash
# navigate to the example
cd examples/pong

# make sure raylib is installed
# on fedora: sudo dnf install raylib raylib-devel (this is the linux distribution the Dockerfile uses)

# build the program, it requires linking against libraylib for graphics and libmath
crag ./main.crag -o pong -ORelease -lraylib -lm

# run the program
./pong
```

### Testing

The test suite is located in the `tests` directory. It contains a variety of test cases in a custom format outlined in `tests/FORMAT.md`. The test suite is run by a Python script that compiles and runs each test case, checking the output against the expected output.

To run the test suite, ensure you have Python 3 installed. From the repository root:

```bash
python3 tests/run_all.py
```

Or

```bash
bash run_tests.sh
```
