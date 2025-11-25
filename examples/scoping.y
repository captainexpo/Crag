
extern fn printf(format: *const u8, ...) -> i32;

fn main(argc: i32, argv: *const *const u8) -> i32 {
    let a: i32 = 5;
    {
        let a: i32 = 7;
        printf("Inner a: %d\n", a);
    }
    printf("Outer a: %d\n", a);
    return 0;
}
