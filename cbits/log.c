#include <stdio.h>
#include "log.h"

int noop_log(const char *fmt, ...) { return 0; }
int (*log_)(const char *fmt, ...) = noop_log;

void enable_log() {
  log_ = printf;
}

void disable_log() {
  log_ = noop_log;
}
