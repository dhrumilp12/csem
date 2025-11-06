#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int x;
  x = 5;
  print("%d %d\n", -x, ~x);
  return 0;
}
