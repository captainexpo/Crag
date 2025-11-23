pub fn pow(a: f32, b: i32 ) -> f32 {
  let result: f32 = 1.0;
  for (let i: i32= 0; i < b; i = i + 1) {
    result = result * a;
  }
  return result;
}

pub fn sqrt(n: f32) -> f32 {
  let x: f32 = n;
  let y: f32 = 1.0;
  let e: f32 = 0.000001;
  while (x - y > e) {
    x = (x + y) / 2.0;
    y = n / x;
  }
  return x;
}
