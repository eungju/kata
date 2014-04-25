#include <stdio.h>

#define FONT_HEIGHT 5
#define FONT_WIDTH 4

const char DIGITS[10][FONT_HEIGHT][FONT_WIDTH+1] = {
  {"*** ",
   "* * ",
   "* * ",
   "* * ",
   "*** "},

  {"  * ",
   "  * ",
   "  * ",
   "  * ",
   "  * "},

  {"*** ",
   "  * ",
   "*** ",
   "*   ",
   "*** "},

  {"*** ",
   "  * ",
   "*** ",
   "  * ",
   "*** "},

  {" ** ",
   "* * ",
   "****",
   "  * ",
   "  * "},

  {"*** ",
   "*   ",
   "*** ",
   "  * ",
   "*** "},

  {"*** ",
   "*   ",
   "*** ",
   "* * ",
   "*** "},

  {"*** ",
   "  * ",
   "  * ",
   "  * ",
   "  * "},

  {"*** ",
   "* * ",
   "*** ",
   "* * ",
   "*** "},

  {"*** ",
   "* * ",
   "*** ",
   "  * ",
   "*** "},
};

void banner_putchar(char c) {
  int h = 0;
  while (h != FONT_HEIGHT) {
    int w = 0;
    while (w != FONT_WIDTH) {
      putchar(DIGITS[c - '0'][h][w]);
      w++;
    }
    putchar('\n');
    h++;
  }
  putchar('\n');
}

void banner_puts(char* message) {
  while (*message != 0) {
    banner_putchar(*message++);
  }
}

int main(int argc, char** argv) {
  int number;
  char message[256];

  scanf("%d", &number);
  sprintf(message, "%d", number);
  banner_puts(message);
}
