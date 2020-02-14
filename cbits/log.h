#ifndef _LOG_H_
#define _LOG_H_

void enable_log();
void disable_log();

extern int (*log_)(const char *, ...);

#endif /* _LOG_H_ */
