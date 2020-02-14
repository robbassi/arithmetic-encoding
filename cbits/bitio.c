#include "bitio.h"

void write_bit(struct bit_writer *bw, uint8_t b) {
  if (b == 1) {
    bw->value |= bw->mask;
  }
  bw->mask >>= 1;
  if (!bw->mask) {
    fputc(bw->value, bw->out);
    bw->value = 0;
    bw->mask = 0x80;
  }
}

int8_t read_bit(struct bit_reader *br) {
  int8_t b;

  if (br->size == 0) {
    br->buff = fgetc(br->in);
    br->size = 0x08;
    
    if (feof(br->in) != 0) {
      return -1;
    }
  }

  b = !!(br->buff & 0x80);
  br->buff <<= 1;
  br->size--;
  return b;
}

void bw_flush(struct bit_writer *bw) {
  if (bw->mask != 0x80) {
    fputc(bw->value, bw->out);
    bw->mask = 0x80;
  }
  fflush(bw->out);
}

void bw_close(struct bit_writer *bw) {
  bw_flush(bw);
  fclose(bw->out);
}

void br_close(struct bit_reader *br) {
  fclose(br->in);
}

