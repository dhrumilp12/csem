#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int x, y;
  x = 3; y = 3;
  if (x < y) { print("lt\n"); }
  else if (x > y) { print("gt\n"); }
  else { print("eq\n"); }
  if (x <= y) print("le\n");
  if (x >= y) print("ge\n");
  if (x != y) print("ne\n");
  if (x == y) print("ee\n");
  return 0;
}
