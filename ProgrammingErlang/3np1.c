#include <stdio.h>

unsigned int cycle(unsigned int n) {
  int c = 1;
  while (n != 1) {
    n = (n % 2) == 0 ? n / 2 : 3 * n + 1;
    c++;
  }
  return c;
}

unsigned int max_cycle(unsigned int from, unsigned int to) {
  unsigned int max = 0;
  unsigned int n;
  for (n = from; n <= to; n++) {
    int c = cycle(n);
    if (max < c) {
      max = c;
    }
  }
  return max;
}

int main(int argc, char** argv) {
  printf("%d\n", max_cycle(1, atoi(argv[1])));
  return 0;
}
