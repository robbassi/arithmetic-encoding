#include <stdio.h>
#include <stdlib.h>
#include "bitio.h"

void test_bitio(char *msg);

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: %s STRING\n", argv[0]);
    exit(1);
  }

  test_bitio(argv[1]);
  return 0;
}

void test_bitio(char *msg) {
  struct bit_writer w = bw_init(fopen("/tmp/t.txt", "wb"));
  char *p = msg;

  printf("out: ");
  while (*p) {
    for (uint8_t i = 0x80; i > 0; i >>= 1) {
      printf("%d", (*p & i) ? 1 : 0);
      write_bit(&w, (*p & i) ? 1 : 0);
    }
    p++;
  }
  bw_close(&w);
  printf("\n");

  struct bit_reader r = br_init(fopen("/tmp/t.txt", "rb"));
  w = bw_init(fopen("/tmp/t2.txt", "wb"));
  int8_t b;

  printf("in:  ");
  while ((b = read_bit(&r)) != -1) {
    printf("%d", b);
    write_bit(&w, b);  
  }
  br_close(&r);
  bw_close(&w);
  printf("\n");

  r = br_init(fopen("/tmp/t.txt", "rb"));
  int8_t n = 0;
  int8_t c = 0; 

  while ((b = read_bit(&r)) != -1) {
    c |= b;
    n++;
    if (n % 8 == 0) {
      printf("%c (%u)\n", c, (uint8_t) c);
      c = 0;
      n = 0;
    } else {
      c <<= 1;
    }
  }
  br_close(&r);
  printf("\n");
}
