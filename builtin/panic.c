extern int write(int fd, const void *buf, unsigned int count);
extern void abort(void);

int __strlen(const char *s) {
  int len = 0;
  while (s[len] != '\0') {
    len++;
  }
  return len;
}

void __panic__(const char *msg, int line, int col) {
  write(2, "PANIC: ", 7);
  write(2, msg, __strlen(msg));
  write(2, " at line ", 9);
  char line_buf[12];
  int line_len = 0;
  int temp_line = line;
  if (temp_line == 0) {
    line_buf[line_len++] = '0';
  } else {
    while (temp_line > 0) {
      line_buf[line_len++] = '0' + (temp_line % 10);
      temp_line /= 10;
    }
  }
  for (int i = line_len - 1; i >= 0; i--) {
    write(2, &line_buf[i], 1);
  }
  write(2, ", col ", 6);
  char col_buf[12];
  int col_len = 0;
  int temp_col = col;
  if (temp_col == 0) {
    col_buf[col_len++] = '0';
  } else {
    while (temp_col > 0) {
      col_buf[col_len++] = '0' + (temp_col % 10);
      temp_col /= 10;
    }
  }
  for (int i = col_len - 1; i >= 0; i--) {
    write(2, &col_buf[i], 1);
  }
  write(2, "\n", 1);
  abort();
}
