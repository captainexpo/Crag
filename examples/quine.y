extern fn printf(s:*u8,...)->i32;const f: *u8="extern fn printf(s:*u8,...)->i32;const f: *u8=%c%s%c;fn main()->i32{printf(f,34,f,34,10);return 0;}%c";fn main()->i32{printf(f,34,f,34,10);return 0;}
