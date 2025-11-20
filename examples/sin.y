extern fn printf(format: *const u8, ...) -> i32;

const PI = 3.14159;

fn sin(x: f64) -> f64 {
  let term = x; // First term in the series
  let sum = term; // Initialize sum of series
  let n: f64 = 1;

  while (n as i32 < 10) {
      term = -term * x * x / (2 * n * (2 * n + 1.0));
      sum = sum + term;
      n = n + 1;
  }

  return sum;
}

fn cos(x: f64) -> f64 {
  return sin(PI / 2.0 - x);
}

fn tan(x: f64) -> f64 {
  return sin(x) / cos(x);
}


fn main(argc: i32, argv: *const *const u8) -> i32 {
    printf("Hello, World!\n" as *const u8);
    printf("sin(PI/2): %f\n", sin(PI / 2.0));
    printf("sin(PI/6): %f\n", sin(PI / 6.0));
    printf("sin(PI/4): %f\n", sin(PI / 4.0));
    printf("cos(PI/2): %f\n", cos(PI / 2.0));
    printf("cos(PI/6): %f\n", cos(PI / 6.0));
    printf("cos(PI/4): %f\n", cos(PI / 4.0));
    printf("tan(PI/2): %f\n", tan(PI / 2.0));
    printf("tan(PI/6): %f\n", tan(PI / 6.0));
    printf("tan(PI/4): %f\n", tan(PI / 4.0));
    return 0;
}
