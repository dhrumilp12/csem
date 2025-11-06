#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int add(int x, int y) {
  return x + y;
}

double mix(double x, int y) {
  if (y < 0) return x - y;
  return x + y;
}

int main() {
  int i;
  double d;
  i = add(7, 5);
  d = mix(2.5, i);
  print("%d %.1f\n", i, d);
  return 0;
}
