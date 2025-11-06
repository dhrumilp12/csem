#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int x;
  x = 1 + 2 * 3 - 4 / 2;       /* 1 + 6 - 2 = 5 */
  x += 1 << 2;                 /* 5 + 4 = 9 */
  x ^= 3 & 6 | 1;              /* 3&6=2; 2|1=3; 9^3=10 */
  print("%d\n", x);
  return 0;
}
