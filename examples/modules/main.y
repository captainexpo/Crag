import "math.y" as math

extern fn printf(fmt: *const u8, ...) -> i32;

const x: i32 = 5;

fn main(argc: i32, argv: **u8) -> i32 {
    const x: i32 = 5;
    printf("2^3 = %f\n", math.pow(2.0, 3));

    return 0;
}
