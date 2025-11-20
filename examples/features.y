extern fn printf(_s: *const u8, ...) -> i32;

fn for_loops() -> void {
  let i: i32 = 0;
  for (; i < 10; i = i + 1) {
    printf("i = %d\n", i);
  }
}

fn while_loops() -> void {
  let i: i32 = 0;
  while (i < 10) {
    printf("i = %d\n", i);
    i = i + 1;
  }
}

fn other_function(x: i32) -> i32 {
  printf("Called: other_function with x = %x\n", x);
  return 42;
}

fn function_pointers() -> void {
  let f: fn(i32) -> i32 = &other_function;
  let result: i32 = f(25);
  printf("result = %d\n", result);
}

struct Point {
  x: i32,
  y: i32,
}

fn structs() -> void {
  let p: Point = Point { x: 10, y: 20 };
  printf("Point x = %d, y = %d\n", p.x, p.y);
}

fn casting() -> void {
  let i: i32 = 42;
  let f: f32 = i as f32;
  let reinteperet: f32 = i re f32;
  printf("f = %f\n", f);
  printf("reinteperet = 0x%x\n", reinteperet);
}

fn null_pointer() -> void {
  let p: *i32 = null;
  if (p == null) {
    printf("p is null\n");
    printf("p + 1 = %p\n", p as i32 + 1);
  }
}

fn constants() -> void {
  const MAX: i32 = 100;
  printf("MAX = %d\n", MAX);
}

fn constant_pointer() -> void {
  let x: i32 = 10;
  let p: *const i32 = &x;
}


fn main(argc: i32, argv: **u8) -> i32 {
  printf("Hello, World!\n");
  for_loops();
  while_loops();
  function_pointers();
  structs();
  casting();
  null_pointer();
  constants();
  constant_pointer();
  return 0;
}
