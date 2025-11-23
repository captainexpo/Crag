import "libc.y" as libc
import "math.y" as math

pub const x: u32 = 2;

pub struct V2 {
    x: f32,
    y: f32,
}

pub fn printmore(str: *const u8) -> void {
    libc.printf("More: %s\n", str);
}

pub fn printmoreer(stor: *const u8) -> void {
    libc.printf("Even More: %s %f\n", stor, math.pow(stor[0], 2));
}
