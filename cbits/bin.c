#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "bitio.h"
#include "model.h"
#include "encode.h"
#include "decode.h"
#include "log.h"

enum mode {
  NONE, ENCODE, DECODE
};

void print_usage(const char *prog) {
  printf("Usage: %s -(e|d) [-v] SRC DEST\n", prog);
  exit(1);
}

int main(int argc, char**argv) {
  if (argc < 4) {
    print_usage(argv[0]);
  }

  enum mode mode = ENCODE;
  char opt;

  while ((opt = getopt(argc, argv, "edv")) != -1) {
    switch (opt) {
      case 'e':
        mode = ENCODE;
        break;
      case 'd':
        mode = DECODE;
        break;
      case 'v':
        enable_log();
        break;
    }
  }

  char *in_path = argv[argc-2];
  char *out_path = argv[argc-1];

  FILE *in = fopen(in_path, "rb");
  FILE *out = fopen(out_path, "wb");
   
  switch (mode) {
    case ENCODE:
      {
        struct model *model = compute_model(in);
        struct encoder *enc = new_encoder(model, in, out); 
        encode(enc);
      }
      break;
    case DECODE:
      {
        struct decoder *dec = new_decoder(in, out);
        decode(dec);
      }
      break;
    case NONE:
      print_usage(argv[0]);
  }
  
  fclose(in);
  fclose(out);

  return 0;
}
