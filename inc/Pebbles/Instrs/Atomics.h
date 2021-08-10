#ifndef _PEBBLES_INSTRS_ATOMICS_H_
#define _PEBBLES_INSTRS_ATOMICS_H_

#include <Pebbles/Common.h>

// Atomic swap
INLINE int atomicSwap(volatile int *ptr, int val) {
  int x;
  __atomic_exchange(ptr, &val, &x, __ATOMIC_RELAXED);
  return x;
}

// Atomic add
INLINE int atomicAdd(volatile int *ptr, int val) {
  return __atomic_fetch_add(ptr, val, __ATOMIC_RELAXED);
}

// Atomic and
INLINE int atomicAnd(volatile int *ptr, int val) {
  return __atomic_fetch_and(ptr, val, __ATOMIC_RELAXED);
}

// Atomic or
INLINE int atomicOr(volatile int *ptr, int val) {
  return __atomic_fetch_or(ptr, val, __ATOMIC_RELAXED);
}

// Atomic xor
INLINE int atomicXor(volatile int *ptr, int val) {
  return __atomic_fetch_xor(ptr, val, __ATOMIC_RELAXED);
}

#if EnableCHERI

// Atomic max (signed)
INLINE int atomicMax(volatile int *ptr, int val) {
  int x;
  asm volatile("camomax.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "C"(ptr) : "memory");
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMax(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("camomaxu.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "C"(ptr) : "memory");
  return x;
}

// Atomic max (signed)
INLINE int atomicMin(volatile int *ptr, int val) {
  int x;
  asm volatile("camomin.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "C"(ptr) : "memory");
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMin(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("camominu.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "C"(ptr) : "memory");
  return x;
}

#else

// Atomic max (signed)
INLINE int atomicMax(volatile int *ptr, int val) {
  int x;
  asm volatile("amomax.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "r"(ptr) : "memory");
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMax(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("amomaxu.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "r"(ptr) : "memory");
  return x;
}

// Atomic max (signed)
INLINE int atomicMin(volatile int *ptr, int val) {
  int x;
  asm volatile("amomin.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "r"(ptr) : "memory");
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMin(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("amominu.w %0, %1, 0(%2)" :
    "=r"(x) : "r"(val), "r"(ptr) : "memory");
  return x;
}

#endif

#endif
