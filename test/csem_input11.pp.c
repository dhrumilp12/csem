



int main() {
  int x, y, z;
  x = 0; y = 1; z = 0;
  if (y || (z = 1)) { }
  print("%d\n", z);
  return 0;
}
