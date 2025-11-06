#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  double a[4];
  int i, j;
  i = 1; j = 2;
  a[i + 1] = 3.5;
  a[j - 1] = 2.0;
  print("%.1f %.1f\n", a[2], a[1]);
  return 0;
}
