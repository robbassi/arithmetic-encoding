#ifndef _BITIO_H_
#define _BITIO_H_

#include <stdio.h>
#include <stdint.h>

#define bw_init(f) (struct bit_writer) { 0x80, 0, f }
#define br_init(f) (struct bit_reader) { 0, 0, f }

struct bit_writer {
  uint8_t mask;
  uint8_t value;
  FILE *out;
};

struct bit_reader {
  int8_t size;
  int8_t buff;
  FILE *in;
};

void write_bit(struct bit_writer *, uint8_t);
int8_t read_bit(struct bit_reader *);
void bw_flush(struct bit_writer *);
void bw_close(struct bit_writer *);
void br_close(struct bit_reader *);

#endif /* _BITIO_H_ */
