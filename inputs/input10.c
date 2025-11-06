#ifdef DEFAULT_CLANG
int print(const char *fmt, ...);
#endif

int side(int *acc) {
  *acc = *acc + 1;
  return *acc;
}

int main() {
  int v[1];
  int t;
  v[0] = 0;
  /* If left is false, right must not execute */
  if (0 && v[0]) print("bad\n");
  /* Make left true then evaluate right */
  t = 1 && v[0];
  print("%d %d\n", t, v[0]);
  return 0;
}
