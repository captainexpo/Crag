# `.cragtest` file format

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

fn main() -> i32 {
    printf("hi\n");
    return 0;
}

//END CODE
//BEGIN EXPECT
//hi
//END EXPECT
```
