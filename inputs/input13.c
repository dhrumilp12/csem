#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int a; double d;
  a = 5; d = 2.5;
  a += 3;        /* 8 */
  a <<= 1;       /* 16 */
  d *= 2;        /* 5.0 */
  a |= 3;        /* 19 */
  print("%d %.1f\n", a, d);
  return 0;
}
