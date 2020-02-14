#include <stdlib.h>
#include <stdint.h>
#include "decode.h"
#include "log.h"

#define next_bit(dec,b) (((b = read_bit(&dec->in)) == -1) ? 0 : b)

static int16_t decode_symbol(struct decoder *d);
static struct model *decode_model(struct decoder *d);

struct decoder *new_decoder(FILE *in, FILE *out) {
  struct decoder *d = malloc(sizeof(struct decoder));
  
  d->model = NULL;
  d->range = 0;
  d->low = 0;
  d->high = 0;
  d->value = 0;
  d->in = br_init(in);
  d->out = out;
  
  return d;
}

void decode(struct decoder *dec) {
  int8_t b;
  int16_t sym;

  log_("decode | start\n");

  dec->model = read_model(dec->in.in);
  dec->high = dec->model->limits->max;

  for (int i = 0; i < dec->model->limits->cbits; i++) {
    dec->value <<= 1;
    dec->value |= next_bit(dec, b);
  }

  for (;;) {
    sym = decode_symbol(dec);
    if (sym == EOM)
      break;
    fputc(sym, dec->out);
 }
}

static int16_t decode_symbol(struct decoder *dec) {
  struct prob p;
  int16_t sym;
  int8_t b; 
  code_t cum;  

  dec->range = dec->high - dec->low + 1;
  cum = ((dec->value - dec->low + 1) * dec->model->denom - 1) / dec->range;

  for (sym = 0; sym < MAX_SYM; sym++) {
    p = dec->model->probs[sym];

    if (cum < p.upper)
      break;
  }
 
  // terminal symbol
  if (sym == EOM) return sym;

  dec->high = dec->low + (dec->range * p.upper / dec->model->denom) - 1;
  dec->low = dec->low + (dec->range * p.lower / dec->model->denom);

  log_("decode | c: %c, dec->low: %u, dec->high: %u, dec->range: %u, dec->value: %u\n", 
       sym, dec->low, dec->high, dec->range, cum);

  for (;;) {
    if (dec->high < dec->model->limits->half) {
      // nothing
    } else if (dec->low >= dec->model->limits->half) {
      dec->value -= dec->model->limits->half;
    } else if (dec->low >= dec->model->limits->fourth && 
               dec->high < dec->model->limits->third) {
      dec->value -= dec->model->limits->fourth;
      dec->low -= dec->model->limits->fourth;
      dec->high -= dec->model->limits->fourth;
    } else {
      break;
    }

    dec->low <<= 1;
    dec->high <<= 1;
    dec->high |= 1;
    dec->low &= dec->model->limits->max;
    dec->high &= dec->model->limits->max;
    dec->value <<= 1;
    dec->value |= next_bit(dec, b); 
  }
  
  return sym;
}
