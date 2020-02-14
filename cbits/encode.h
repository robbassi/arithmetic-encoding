#ifndef _ENCODE_H_
#define _ENCODE_H_

#include <stdio.h>
#include "model.h"
#include "bitio.h"

struct encoder {
  struct model *model;
  code_t range, low, high;
  int pending;
  FILE *in;
  struct bit_writer out;
};

struct encoder *new_encoder(struct model *m, FILE *in, FILE *out);
void encode(struct encoder *e);

#endif /* _ENCODE_H_ */
