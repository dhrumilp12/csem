#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i;
  for (i = 0; i < 3; i = i + 1) print("%d ", i);
  print("| ");
  i = 0;
  for (; i < 2; ) { print("%d ", i); i = i + 1; }
  print("\n");
  return 0;
}
