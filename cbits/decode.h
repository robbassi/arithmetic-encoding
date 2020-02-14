#ifndef _DECODE_H_
#define _DECODE_H_

#include <stdio.h>
#include "model.h"
#include "bitio.h"

struct decoder {
  struct model *model;
  code_t range, low, high, value;
  struct bit_reader in;
  FILE *out;
};

struct decoder *new_decoder(FILE *in, FILE *out);
void decode(struct decoder *d);

#endif /* _DECODE_H_ */
