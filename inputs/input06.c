#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i;
  i = 0;
  while (i < 7) {
    i = i + 1;
    if (i == 2) continue;
    if (i == 5) break;
    print("%d ", i);
  }
  print("\n");
  return 0;
}
