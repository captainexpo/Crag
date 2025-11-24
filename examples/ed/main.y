import "libc.y" as libc

// extern const stdin: *void;

fn main(argc: i32, argv: **u8) -> i32 {
    // input loop
    let input_buf: *u8 = libc.malloc(1024) as *u8;
    input_buf[0] = 0;
    let a: bool = true;
    while (libc.strcmp(input_buf, "exit\n") != 0)  {
        libc.printf("Enter command: ");
        libc.fgets(input_buf, 1024, libc.__stdinp);
        libc.printf("You entered: %s", input_buf);
    }
    return 0;
}
