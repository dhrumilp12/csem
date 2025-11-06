#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int a, b;
  a = 6; b = 3;
  print("%d %d %d %d %d\n", a|b, a^b, a&b, a<<1, a>>1);
  return 0;
}
