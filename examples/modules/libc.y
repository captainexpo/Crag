pub extern fn printf(fmt: *u8, ...) -> i32;
pub extern fn scanf(fmt: *u8, ...) -> i32;
pub extern fn malloc(size: usize) -> *u8;
pub extern fn free(ptr: *u8) -> void;
pub extern fn realloc(ptr: *u8, size: usize) -> *u8;
pub extern fn memcpy(dest: *u8, src: *u8, n: usize) -> *u8;
pub extern fn memset(s: *u8, c: i32, n: usize) -> *u8;
