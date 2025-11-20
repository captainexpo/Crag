extern fn printf(fmt: *const u8, ...) -> i32;

const x: u32 = 2;

struct V2 {
    x: f32,
    y: f32,
}

fn printmore(str: *const u8) -> void {
    printf("More: %s\n", str);
}
