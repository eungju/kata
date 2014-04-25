#include <string.h>
#include <assert.h>

void reverse(char* vector, int start, int end) {
  int n = end - start + 1;
  if (n > 1) {
    int i;
    for (i = 0; i < n / 2; i++) {
      char temp = vector[start + i];
      vector[start + i] = vector[end - i];
      vector[end - i] = temp;
    }
  }
}

void test_reverse_1() {
  char vector[256];
  strcpy(vector, "abcd");
  reverse(vector, 0, 0);
  assert(strcmp("abcd", vector) == 0);
}

void test_reverse_2() {
  char vector[256];
  strcpy(vector, "abcd");
  reverse(vector, 0, 1);
  assert(strcmp("bacd", vector) == 0);
}

void test_reverse_3() {
  char vector[256];
  strcpy(vector, "abcd");
  reverse(vector, 0, 2);
  assert(strcmp("cbad", vector) == 0);
}

void rotate(char* vector, int n, int i) {
  reverse(vector, 0, i - 1);
  reverse(vector, i, n - 1);
  reverse(vector, 0, n - 1);
}

void test_rotate() {
  char vector[256];
  strcpy(vector, "0123456789");
  rotate(vector, 10, 4);
  assert(strcmp("4567890123", vector) == 0);
}

void test_all() {
  test_reverse_1();
  test_reverse_2();
  test_reverse_3();
  test_rotate();
}

int main(int argc, char** argv) {
  test_all();
  return 0;
}
