extern int write(int fd, const void *buf, unsigned int count);
extern void abort(void);

int __strlen(const char *s) {
  int len = 0;
  while (s[len] != '\0') {
    len++;
  }
  return len;
}
void __printint(int value) {
  char buf[20];
  int len = 0;
  int temp = value;
  if (temp == 0) {
    buf[len++] = '0';
  } else {
    while (temp > 0) {
      buf[len++] = '0' + (temp % 10);
      temp /= 10;
    }
  }
  for (int i = len - 1; i >= 0; i--) {
    write(2, &buf[i], 1);
  }
}
void __panic__(const char *msg, int line, int col) {
  write(2, "PANIC: ", 7);
  write(2, msg, __strlen(msg));
  write(2, " at line ", 9);
  __printint(line);
  write(2, ", col ", 6);
  __printint(col);
  write(2, "\n", 1);
  abort();
}
