fn printf(format: *const u8, ...) -> i32;
fn malloc(size: usize) -> *u8;
fn memcpy(dest: *u8, src: *const u8, n: usize) -> *u8;
fn strlen(s: *const u8) -> u32;
fn free(ptr: *u8) -> void;

fn max(a: i32, b: i32) -> i32 {
  if (a > b) {
    return a;
  } else {
    return b;
  }
  return 0;
}
fn min(a: i32, b: i32) -> i32 {
  if (a < b) {
    return a;
  } else {
    return b;
  }
  return 0;
}

struct Str {
  ptr: *u8,
  len: usize,

  fn len(self: *Str) -> usize {
    return self.len;
  }
  fn ptr(self: *Str) -> *u8 {
    return self.ptr;
  }
  fn append(self: *Str, s: *Str) -> void {
    let new_len: usize = self.len + s.len;
    let new_ptr: *u8 = malloc(new_len );
    memcpy(new_ptr, self.ptr, self.len );
    memcpy(new_ptr + self.len as *u8, s.ptr, s.len as usize);
    self.ptr = new_ptr;
    self.len = new_len;
  }
  fn deinit(self: *Str) -> void {
    free(self.ptr);
  }
  fn clear(self: *Str) -> void {
    free(self.ptr);
    self.ptr = malloc(1 as usize);
    self.ptr[0] = 0 re u8;
    self.len = 0 as usize;
  }
  fn compare(self: *Str, s: *Str) -> i32 {
    let min_len: usize = min(self.len as i32, s.len as i32) as usize;  
    for (let i: usize = 0; i < min_len; i = i + 1) {
      if (self.ptr[i] != s.ptr[i]) {
        return (self.ptr[i] - s.ptr[i]) re i32;
      }
    }
    return (self.len - s.len) as i32;
  }
  fn index(self: *Str, i: usize) -> u8 {
    if (i >= self.len) {
      return 0 re u8;
    }
    return self.ptr[i];
  }
}

fn make_str(s: *u8) -> Str {
  const len: usize = strlen(s) as usize;
  let newptr: *u8 = malloc(len);
  memcpy(newptr, s, len);
  s = newptr;
  let str: Str = Str { ptr: s, len: len };
  return str;
}

fn main() -> i32 {
  let s: Str = make_str("Hello,");
  let t: Str = make_str(" World!"); 
  s.append(&t);
  printf("%s\n", s.ptr());
  printf("Length: %d\n", s.len());
  printf("Comparison with 'Hello, World!': %d\n", s.compare(&t));
  s.deinit();
  t.deinit();
  printf("%s", true);
  return 0;
}
