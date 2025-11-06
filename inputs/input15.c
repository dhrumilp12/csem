#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  print("line1\nline2\t\"quoted\"\\backslash\n");
  return 0;
}
