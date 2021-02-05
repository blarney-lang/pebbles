#ifndef _RAND_H_
#define _RAND_H_

#include <stdint.h>

uint32_t rand(uint32_t* seed) {
  *seed = *seed * 1664525 + 1013904223;
  return *seed;
}

#endif
