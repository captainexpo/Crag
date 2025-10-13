fn printf(format: *const u8, ...) -> i32;

const PI = 3.14159;
const E = 2.71828;

const TAU = 2 * PI;


fn main(argc: i32, argv: *const *const u8) -> i32 {
    printf("Hello, World!\n" as *const u8);
    printf("PI: %f\n", PI);
    printf("E: %f\n", E);
    printf("TAU: %f\n", TAU);
    return 0;
}

