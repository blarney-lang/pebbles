#ifndef _ATOMICS_H_
#define _ATOMICS_H_

// Atomic swap
INLINE int atomicSwap(volatile int *ptr, int val) {
  int x;
  asm volatile("amoswap.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic add
INLINE int atomicAdd(volatile int *ptr, int val) {
  int x;
  asm volatile("amoadd.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic and
INLINE int atomicAnd(volatile int *ptr, int val) {
  int x;
  asm volatile("amoand.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic or
INLINE int atomicOr(volatile int *ptr, int val) {
  int x;
  asm volatile("amoor.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic xor
INLINE int atomicXor(volatile int *ptr, int val) {
  int x;
  asm volatile("amoxor.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic max (signed)
INLINE int atomicMax(volatile int *ptr, int val) {
  int x;
  asm volatile("amomax.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMax(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("amomaxu.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic max (signed)
INLINE int atomicMin(volatile int *ptr, int val) {
  int x;
  asm volatile("amomin.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

// Atomic max (unsigned)
INLINE unsigned atomicMin(volatile unsigned *ptr, unsigned val) {
  int x;
  asm volatile("amominu.w %0, %1, 0(%2)" : "=r"(x) : "r"(val), "r"(ptr));
  return x;
}

#endif
