fn printf(fmt: *const u8, ...) -> i32;


enum EnumCode(i32) {
    A = 1,
    B, // automatically set to 2
    C = 3,
}

enum OtherEnum(bool) {
    X = true,
    Y = false,
}

fn main(argc: i32, argv: **u8) -> i32 {
    let a: EnumCode = EnumCode::A;
    let b: EnumCode = EnumCode::B;
    let c: EnumCode = EnumCode::C;

    printf("a = %d\n", a as i32);
    printf("b = %d\n", b as i32);
    printf("c = %d\n", c as i32);

    let x: OtherEnum = OtherEnum::X;
    let y: OtherEnum = OtherEnum::Y;

    printf("x = %d\n", x);
    printf("y = %d\n", y);

    return 0;
}
