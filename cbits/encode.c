#include <stdlib.h>
#include "encode.h"
#include "log.h"

static void encode_symbol(struct encoder *enc, int16_t sym);
static void write_bits(struct bit_writer *bw, uint8_t b, int *pending);

struct encoder *new_encoder(struct model *m, FILE *in, FILE *out) {
  struct encoder *e = malloc(sizeof(struct encoder));
  struct bit_writer bw = bw_init(out);
  
  e->model = m;
  e->range = 0;
  e->low = 0;
  e->high = m->limits->max;
  e->pending = 0;
  e->in = in;
  e->out = bw;

  return e;
}

void encode(struct encoder *enc) {
  int16_t sym;

  write_model(enc->model, enc->out.out);
  while ((sym = fgetc(enc->in)) != EOF) {
    encode_symbol(enc, sym);
  }
  encode_symbol(enc, EOM);

  enc->pending++;
  if (enc->low < enc->model->limits->fourth) {
    write_bits(&enc->out, 0, &enc->pending);
  } else {
    write_bits(&enc->out, 1, &enc->pending);
  }

  bw_flush(&enc->out);  
  log_("encode | complete\n");
}

static void encode_symbol(struct encoder *enc, int16_t sym) {
  struct prob p = enc->model->probs[sym];
  enc->range = enc->high - enc->low + 1;
  enc->high = enc->low + (enc->range * p.upper / enc->model->denom) - 1;
  enc->low = enc->low + (enc->range * p.lower / enc->model->denom);

  log_("encode | c: '%c', enc->low: %lu, enc->high: %lu, enc->range:%lu\n", 
       sym, enc->low, enc->high, enc->range);

  for (;;) {
    if (enc->high < enc->model->limits->half) {
      write_bits(&enc->out, 0, &enc->pending);
    } else if (enc->low >= enc->model->limits->half) {
      write_bits(&enc->out, 1, &enc->pending);
    } else if (enc->low >= enc->model->limits->fourth && 
               enc->high < enc->model->limits->third) {
      enc->pending++;
      enc->low -= enc->model->limits->fourth;
      enc->high -= enc->model->limits->fourth;
    } else {
      break;
    }

    enc->low <<= 1;
    enc->high <<= 1;
    enc->high |= 1;
    enc->low &= enc->model->limits->max;
    enc->high &= enc->model->limits->max;
  }
}

void write_bits(struct bit_writer *bw, uint8_t b, int *pending) {
  write_bit(bw, b);
  for (int i = 0; i < *pending; i++) {
    write_bit(bw, !b);
  }
  *pending = 0;
}
