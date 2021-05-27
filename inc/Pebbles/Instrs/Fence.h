#ifndef _PEBBLES_FENCE_H_
#define _PEBBLES_FENCE_H_

#include <Pebbles/Common.h>

// Memory fence
INLINE void pebblesFence()
{
  asm volatile("fence rw, rw");
}

#endif
