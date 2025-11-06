#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int x, y, z;
  x = 0; y = 1; z = 0;
  if (y || (z = 1)) { /* z must remain 0 because left is true */ }
  print("%d\n", z);
  return 0;
}
