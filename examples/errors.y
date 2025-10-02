fn printf(fmt: *const u8, ...) -> i32;
fn atoi(s: *const u8) -> i32;

enum MyError(i32) {
  DivisionByZero = 0,
  Unknown = 1,
}

fn myErrorToString(err: MyError) -> *const u8 {
  if (err == MyError::DivisionByZero) {
    return "Division by Zero";
  } else if (err == MyError::Unknown) {
    return "Unknown Error";
  }
  return "Invalid Error Code";
}

fn divide(a: i32, b: i32) -> i32!MyError {
  if (b == 0) {
    return! MyError::DivisionByZero;
  }
  return a / b; 
}

fn main(argc: i32, argv: **u8) -> i32 {
  let result: i32!MyError = divide(10, atoi(argv[1]));
  if (result.is_err) {
    printf("Error occurred: %s\n", myErrorToString(result.err));
    return 1;
  }
  printf("Result: %d\n", result.ok);
  return 0;
}
