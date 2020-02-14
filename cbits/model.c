#include "model.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define MAX_BITS (sizeof(code_t)*8-1)
#define CODE_BITS (MAX_BITS/2)
#define FREQ_BITS (CODE_BITS-2)

void compute_probs(struct model *model, code_t freq[MAX_SYM]);

struct limits *compute_limits() {
  struct limits *l = malloc(sizeof(struct limits));
  l->bits = MAX_BITS;
  l->cbits = CODE_BITS;
  l->fbits = FREQ_BITS;

  /* Make sure we don't use more bits than we have. */
  if (l->cbits + l->fbits > l->bits) {
    l->fbits = l->bits - l->cbits;
  
    if (l->fbits < 1) {
      // abort!
    }
  }

  l->max = (code_t)(1 << l->cbits) - 1;
  l->fourth = l->max / 4 + 1;
  l->half = l->fourth * 2;
  l->third = l->fourth * 3;
  l->max_freq = (code_t)(1 << l->fbits);

  //printf("max: %lu, third: %lu, half: %lu, fourth: %lu\n",
  //       l->max, l->third, l->half, l->fourth);

  return l;
}

struct model *compute_model(FILE *in) {
  bool sample = false;
  int16_t c;
  unsigned int freq[MAX_SYM] = { 0 }, cum = 0;
  struct model *model = malloc(sizeof(struct model));

  model->limits = compute_limits();

  freq[EOM] = 1;
  while ((c = fgetc(in)) != EOF) {
    freq[c]++;
    cum++;

    /* stop once we hit max_freq */
    /* NOTE: maybe we can keep going and then scale 
             down the frequencies afterwards? that 
             way the relative probabilities are more accurate. */
    if (cum == model->limits->max_freq) {
      sample = true;
      break;
    }
  }
  rewind(in);
  
  /* if we only captured a sample of the data, set a default
     frequency of 1 in order to avoid data loss. */
  if (sample) {
    for (int i = 0; i < MAX_SYM - 1; i++) {
      if (!freq[i])
        freq[i] = 1;
    }
  }

  compute_probs(model, freq);
  
  return model;
}

void compute_probs(struct model *model, code_t freq[MAX_SYM]) {
  uint32_t cum = 0;
  model->probs = malloc(sizeof(struct prob) * MAX_SYM);
  
  for (int i = 0; i < MAX_SYM; i++) {
    model->probs[i] = (struct prob) { cum, cum + freq[i] };
    cum += freq[i];
  }

  model->denom = cum;
}

void write_model(struct model *m, FILE *out) {
  for (int i = 0; i < MAX_SYM - 1; i++) {
    struct prob p = m->probs[i];
    code_t freq = p.upper - p.lower;

    if (freq > 0) {
      fputc(i, out);
      for (int b = 0; b < sizeof(code_t); b++) {
        fputc(freq>>8*b, out);
      }
    }
  }

  // end of header
  fputc(0, out);
  fputc(0, out);
  fputc(0, out);
  fputc(0, out);
  fputc(0, out);
}

struct model *read_model(FILE *in) {
  struct model *model = malloc(sizeof(struct model));
  code_t freq[MAX_SYM] = { 0, [MAX_SYM-1] = 1 };
  uint8_t sym;
  code_t count;
  int bytes = sizeof(code_t);

  for (;;) {
    sym = fgetc(in);
   
    count = 0; 
    for (int i = 0; i < bytes; i++) {
      code_t c = fgetc(in);
      c <<= i*8;
      count |= c;
    }
    
    if (sym | count) {
      freq[sym] = count;
    } else {
      break;
    }
  }

  model->limits = compute_limits();
  compute_probs(model, freq);

  return model;
}

void print_model(struct model *model) {
  struct prob *m = model->probs;
  
  for (int i = 0; i < MAX_SYM; i+=4) {
    printf("%c = { %u - %u / %u } "
           "%c = { %u - %u / %u } "
           "%c = { %u - %u / %u } "
           "%c = { %u - %u / %u }\n",
           i, m[i].lower, m[i].upper, model->denom,
           i+1, m[i+1].lower, m[i+1].upper, model->denom,
           i+2, m[i+2].lower, m[i+2].upper, model->denom,
           i+3, m[i+3].lower, m[i+3].upper, model->denom);
  }
}
