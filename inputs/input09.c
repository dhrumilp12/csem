#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i;
  i = 0;
  goto AFTER;
L1:
  i = i + 1;
  if (i < 2) goto L1;
AFTER:
  print("%d\n", i);
  return 0;
}
