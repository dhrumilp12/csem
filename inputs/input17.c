#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int f(int flag) {
  if (flag) return 42;
  return;
}

int main() {
  print("%d %d\n", f(1), f(0));
  return 0;
}
