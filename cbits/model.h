#ifndef _MODEL_H_
#define _MODEL_H_

#include <stdio.h>
#include <stdint.h>

#define MAX_SYM 257
#define EOM (MAX_SYM-1)

typedef uint32_t code_t;

struct prob {
  uint32_t lower, upper;
};

struct limits {
  uint8_t bits, cbits, fbits;
  code_t max, third, half, 
             fourth, max_freq;
};

struct model {
  struct limits *limits;
  struct prob *probs;
  uint32_t denom;
};

struct limits *compute_limits();
struct model *compute_model(FILE *in);
void write_model(struct model *m, FILE *out);
struct model *read_model(FILE *in);
void print_model(struct model *);

#endif /* _MODEL_H_ */
