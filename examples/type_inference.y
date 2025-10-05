fn printf(fmt: *const u8, ...) -> i32;

fn main(argc: i32, argv: **u8) -> i32 {
  let x = 1.2 + 3; // Will be inferred as a float 
  printf("%f\n", x);
  return 0;
}
