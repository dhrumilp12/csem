#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i;
  i = 0;
  do {
    print("%d ", i);
    i = i + 1;
  } while (i < 3);
  print("\n");
  return 0;
}
