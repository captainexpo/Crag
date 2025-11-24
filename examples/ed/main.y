import "libc.y" as libc

// extern const stdin: *void;

fn main(argc: i32, argv: **u8) -> i32 {
    // input loop
    let input_buf: *u8 = libc.malloc(1024) as *u8;
    input_buf[0] = 0;
    while (true)  {
        libc.printf("> ");
        libc.fgets(input_buf, 1024, libc.__stdinp);
        libc.printf("You entered: %s", input_buf);
        if (libc.strcmp(input_buf, "q\n") == 0) break;
    }
    return 0;
}
