# Crag — Syntax Reference

This file summarizes the language syntax as implemented by `src/lexer.*` and `src/parser.*`.

## Lexical
- Whitespace: any `isspace()` is ignored between tokens.
- Comments:
  - Single-line: `// comment` until newline
  - Block: `/* comment */`
- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`. The special identifier `_` is a token `UNDERSCORE`.
- Attributes: `@name` (token `ATTRIBUTE`).

### Literals
- Numbers:
  - Hex: `0x1f` (accepted by lexer as NUMBER)
  - Binary: `0b1010`
  - Decimal integers: `123`
  - Floating point: include a `.` (e.g. `1.23`)
- Strings: double-quoted with C-like escapes `"\n\t\\"` (token STRING).
- Chars: single-quoted, single character or escape `'a'`, `'\n'` (token CHAR).
- Boolean: `true`, `false` (keywords)
- Null: `null` (keyword)

## Keywords (non-exhaustive)
fn, return, if, else, while, for, let, const, struct, union, enum, import, extern, pub, break, continue, using, when, type, asm, volatile, switch

## Operators & Delimiters (high level)
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Arithmetic: `+ - * / %`
- Bitwise: `& | ^ << >> ~`
- Logical: `&& || !`
- Assignment: `=` and compound `+= -= *= /= %= <<= >>= &= |= ^=`
- Other: `? :`, `->` (arrow), `=>` (case arrow), `::` (double-colon / qualification), `...` (variadic)
- Delimiters: `(` `)` `{` `}` `[` `]` `,` `:` `.` `;`

## Types
- Primitive names: `u8 u16 u32 u64 usize i8 i16 i32 i64 isize f32 f64 bool void`
- Pointer: `*T` or `*const T` (parser produces `PointerType`).
- Array type: `[<size>]T` for fixed-sized arrays, or `[]T` (written as `[]T` by using `[]` with no size) — parser uses `LBRACKET` `RBRACKET` and an optional size expression. Unsized arrays are represented with `unsized=true`.
- Function pointer type: `fn(T1, T2) -> TRet` (parsed into a `FunctionType` wrapped in a `PointerType`).
- Template instance / qualified type: `Module::Name` or `Type:: <T...>` (the parser supports `DOUBLE_COLON` and `:: < ... >` template args).
- Error union (return-with-error): `T!E` (type `T` with error `E`).

## Declarations
- Function:
  - `fn name<T...>(param: T, ...) -> Ret { /* body */ }`
  - Functions may be `extern` or `pub` and may end with `;` to indicate no body (extern/imported linkage).
  - Attributes appear after the signature as `@attr` tokens (parser records them in `attributes`).
- Variables:
  - `let name: Type = expr;` or `const name: Type = expr;`
  - Type is optional: `let x = 1;` — parser collects `var_type` if `:` is present.
  - `extern let/const x: T;` supported.
- Type aliases: `using Name = Type;` (supports generics after `using Name<T...>`).
- Struct / Union / Enum:
  - `struct Name { field: Type, ... }` (can have methods declared with `fn` inside)
  - `union` similar to struct
  - `enum Name { VAR = <literal>, ... }` (first variant must have explicit value)
- Import: `import "path" as alias;`
- Extern: `extern fn ...` or `extern let/const ...;`
- When blocks: `when <const_expr> { decl... }` or inline `when <const_expr> decl`

## Statements
- Expression statement: an expression terminated by `;` (unless statement form accepts a block)
- Block: `{ stmt; stmt; }`
- If: `if (cond) stmt [else stmt]`
- While: `while (cond) stmt`
- For (C-style): `for (init; cond; increment) stmt` where `init` may be a declaration or expression
- Return: `return expr;` or `return;` ; supports `return !expr` for error returns
- Break / Continue
- Switch: `switch (expr) { caseExpr, ... => stmt, _ => stmt, }` — underscores `_` denote default case; cases use `=>` and are separated by commas.
- Asm: `asm("template", in(...), out(...))` — inline assembly with operands and options
- When (statement form): `when <const_expr> { stmt }` or `when <const_expr> stmt`

## Expressions
- Literals: numbers, strings, chars, true/false/null
- Variable access: `ident`
- Module access / qualification: `mod::name` or `mod::type::<T>`
- Template instantiation: `Name::<T1, T2>` (parser accepts `ID DOUBLE_COLON LT ... GT` form)
- Struct initializer: `TypeName { field: expr, ... }` (also works after module/field access or template instantiation)
- Array literal: `{ e1, e2, ... }` (note: parser uses `{}` for array literals)
- Unary: prefix `* & + - ! ~ ++ --` and postfix `++ --`
- Binary: standard infix operators including `.` `:` (enum access), `::` (qualification), array indexing `[]`, function call `()`
- Field access: `expr.field` (also used for module member access when left side is an imported module identifier)
- Offset access (indexing): `expr[expr]`
- Function / method call: `f(a, b)` or `obj.method(a)`
- Casts: `expr as Type` (normal) and `expr re Type` (re-interpret)

## Notable Parser Behaviors / Constraints
- Array literal syntax uses `{ ... }` (same token as struct initializer start). When the parser sees `{` after an identifier or type it treats as a struct initializer; when it sees `{` directly it parses an array literal.
- Empty array literals (`{}`) require expected type context to infer element type and length.
- `using` creates type aliases and supports generic parameters.
- `fn` generic params are parsed with `<T,...>` immediately after the function name.
- Unary & binary operator precedence and associativity are implemented in `get_precedence()` and the Pratt parser (`parse_nud` / `parse_led`).

## Examples
- Variable
```
let x: i32 = 42;
const s = "hello";
```

- Function
```
fn add(a: i32, b: i32) -> i32 {
  return a + b;
}
```

- Struct and initializer
```
struct S { a: i32, buf: [4]u8 }
let v: S = S { a: 1, buf: {0,1,2,3} };
```

- Array literal and indexing
```
let a: [3]u8 = { 1, 2, 3 };
let b = a[1];
```

- Import / extern
```
import "stdlib" as std;
extern fn printf(fmt: *const u8, ...) -> i32;
```