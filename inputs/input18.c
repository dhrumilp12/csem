#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i; double d;
  i = 3; d = 2.5;
  print("%.1f %d %d %d\n", i + d, i < d, d < i, i == d);
  return 0;
}
