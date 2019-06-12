#include <stdlib.h>
#include <stdbool.h>
#include "HsFFI.h"
#include "../duckling.h"


bool ducklingInit() {
  int argc = 0;
  char *argv[] = { NULL };
  char **pargv = argv;
  hs_init(&argc, &pargv);
  return HS_BOOL_TRUE;
}

void ducklingExit() {
  hs_exit();
}
