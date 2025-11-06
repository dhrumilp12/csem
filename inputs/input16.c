#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i, j;
  for (i = 0; i < 3; i = i + 1) {
    j = 0;
    while (j < 3) {
      if (j == 1) { j = j + 1; continue; }
      print("%d,%d ", i, j);
      j = j + 1;
    }
    print("| ");
  }
  print("\n");
  return 0;
}
