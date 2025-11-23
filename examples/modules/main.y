import "libc.y" as libc
import "math.y" as math
import "printmore.y" as pm

const x: i32 = 5;

fn main(argc: i32, argv: **u8) -> i32 {
    const x: i32 = 5;
    pm.printmore("Hello from main.y");
    libc.printf("2^3 = %f\n", math.pow(2.0, 3));
    pm.printmoreer("Test");
    libc.printf("sqrt(2) = %f\n", math.sqrt(2));

    return 0;
}
