#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int main() {
  int i;
  i = 7;
  &i;           /* exercise BIT_AND lval -> returns lvalue; no effect */
  print("%d\n", i);
  return 0;
}
