extern fn printf(fmt: *const u8, ...) -> i32;

fn pow(base: u64, exp: u64) -> u64 {
  let result: u64 = 1;
  for (let i: u64 = 0; i < exp; i = i + 1) {
    result = result * base;
  }
  return result;
}

fn main(argc: i32, argv: **u8) -> i32 {
  let a: u64 = 0;
  let b: u64 = 1;
  let c: u64 = 0;

  while (c < pow(2, 62)) {
    c = a + b;
    a = b;
    b = c;
    printf("%llu\n", c);
  }
  return 0;
}
