# Crag

> [!WARNING]
> The Crag compiler is merely a recreational toy language and is not suitable for production code. Expect frequent breaking changes, incomplete features, spaghetti code, and informal commits. Contributions and feedback are welcome!

Crag is a systems programming language that is designed to be simple, similarly to C, but with more modern syntax and features, similar to Zig and Rust.

The language was created as a learning project to explore compiler design and implementation, and to create a language that had all the good parts of C, while adding more modern features that improve the developer experience. At this point, it is not intended to be a production-ready language (and probably never will be), but is instead a fun project to work on and learn from.


## Language Features

- Zig/Rust inspired syntax
- C-like semantics
- Manual memory management
- Simple runtime safety checks (e.g. bounds checking, null pointer checks)
- Simple module system
- Generics for functions, structs, and unions
- Small standard library (currently just a few basic utilities)
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
- `--emit-llvm` - Emit LLVM IR instead of a native executable
- `--dump-ast [asa|bsa]` - Dump the abstract syntax tree (AST) in either before semantic analysis (BSA) or after semantic analysis (ASA) form. This is useful for debugging and understanding how the compiler processes the source code.



## Syntax Examples

Hello World:
```crag
import "stdlib" as std;

fn main() -> i32 {
    // I hope to eventually have a more ergonomic standard library, but for now it needs to borrow from C's standard library for basic functionality.
    std.cstdio.printf("Hello, World!");
    return 0;
}
```

Fibonacci:
```crag
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

Linked list:
```crag
import "stdlib" as std;

struct Node<T> {
    value: T,
    next: *Node::<T>,

    fn append(self: Node::<T>, value: T) -> void {
        if (self.next != null) {
            self.next.append(value);
            return;
        }
        let new_node = Node::<T>{ value: value, next: null };
        let current = self;
        while (current.next != null) {
            current = current.next;
        }
        current.next = &new_node;
    }
}

fn main() -> i32 {
    let head = Node::<i32>{ value: 1, next: null };
    head.append(2);
    head.append(3);
    return 0;
}
```

## Building the Compiler

### Requirements

- CMake 3.10+
- A C++20 compiler
- LLVM development tools and libraries available through `llvm-config`
  - `llvm-config --cxxflags`
  - `llvm-config --ldflags --libs core support irreader`
- Python 3 (optional - for running tests)
From the repository root:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

Build outputs:

- Compiler executable: `build/crag`
- Runtime static library: `build/lib/libruntime.a`


### Testing

To run the test suite, ensure you have Python 3 installed. From the repository root:

```bash
python3 tests/run_all.py
```

Or

```bash
bash run_tests.sh
```
