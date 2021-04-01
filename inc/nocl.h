// CUDA-like library for compute kernels

#ifndef _NOCL_H_
#define _NOCL_H_

#include <SoC.h>
#include <cpu.h>
#include <simt.h>
#include <stdint.h>

// Alias some useful SIMT macros
#define nocl_aligned simt_aligned

// Utility functions
// =================

// Return input where only first non-zero bit is set, starting from LSB
inline unsigned firstHot(unsigned x) {
  return x & (~x + 1);
}

// Is the given value a power of two?
inline bool isOneHot(unsigned x) {
  return x > 0 && (x & ~firstHot(x)) == 0;
}

// Compute logarithm (base 2) 
inline unsigned log2floor(unsigned x) {
  unsigned count = 0;
  while (x > 1) { x >>= 1; count++; }
  return count;
}

// Data types
// ==========

// Dimensions
struct Dim3 {
  unsigned x, y, z;
  Dim3() : x(1), y(1), z(1) {};
  Dim3(unsigned xd) : x(xd), y(1), z(1) {};
  Dim3(unsigned xd, unsigned yd) : x(xd), y(yd), z(1) {};
  Dim3(unsigned xd, unsigned yd, unsigned zd) : x(xd), y(yd), z(zd) {};
};

// For shared local memory allocation
// Memory is allocated/freed in a stack-like fashion
struct SharedLocalMem {
  // This points to the top of the stack (which grows upwards)
  char* top;

  // Allocate memory on shared memory stack
  template <typename T> T* alloc(unsigned numElems) {
    T* base = (T*) top;
    unsigned size = numElems * sizeof(T);
    size += 4-(size&3);
    top += size;
    return base;
  }

  // Reclaim memory on shared memory stack
  template <typename T> void release(unsigned numElems) {
    unsigned size = numElems * sizeof(T);
    size += 4-(size&3);
    top -= size;
  }
};

// Parameters that are available to any kernel
// All kernels inherit from this
struct Kernel {
  // Blocks per streaming multiprocessor
  unsigned blocksPerSM;

  // Grid and block dimensions
  Dim3 gridDim, blockDim;

  // Block and thread indexes
  Dim3 blockIdx, threadIdx;

  // Shared local memory
  SharedLocalMem shared;
};

// Kernel invocation
// =================

// SIMT main function
// Support only 1D blocks for now
template <typename K> __attribute__ ((noinline)) void _noclSIMTMain_() {
  // Get pointer to kernel closure
  K* kernelPtr = (K*) simtGetKernelClosureAddr();
  K k = *kernelPtr;

  // Block dimensions are all powers of two
  unsigned blockXMask = k.blockDim.x - 1;
  unsigned blockXShift = log2floor(k.blockDim.x);

  // Set thread index
  k.threadIdx.x = simtThreadId() & blockXMask;

  // Set initial block index
  k.blockIdx.x = simtThreadId() >> blockXShift;

  // Set base of shared local memory (per block)
  unsigned localBytes = 4 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank);
  unsigned localBytesPerBlock = localBytes / k.blocksPerSM;
  k.shared.top = &__localBase + localBytesPerBlock * k.blockIdx.x;

  // Invoke kernel
  while (k.blockIdx.x < k.gridDim.x) {
    k.kernel();
    k.blockIdx.x += k.blocksPerSM;
    simtLocalBarrier();
  }

  // Issue a fence ensure all data has reached DRAM
  simtGlobalMemFence();

  // Terminate warp
  simtWarpTerminateSuccess();
}

// SIMT entry point
template <typename K> __attribute__ ((noinline))
  void _noclSIMTEntry_() {
    // Determine stack pointer based on SIMT thread id
    uint32_t top = 0;
    top -= (SIMTLanes * SIMTWarps - 1 - simtThreadId()) <<
             SIMTLogBytesPerStack;
    top -= 8;
    // Set stack pointer
    asm volatile("mv sp, %0\n" : : "r"(top));
    // Invoke main function
    _noclSIMTMain_<K>();
  }

// Trigger SIMT kernel execution from CPU
template <typename K> __attribute__ ((noinline))
  int noclRunKernel(K* k) {
    // Constraints (some of which are simply limitations)
    cpuAssert(isOneHot(k->blockDim.x),
      "NoCL: blockDim.x is not a power of two");
    cpuAssert(k->blockDim.x >= SIMTLanes,
      "NoCL: blockDim.x is not a multiple of warp size");
    cpuAssert(k->blockDim.x <= SIMTWarps * SIMTLanes,
      "NoCL: blockDim.x is too large (exceeds SIMT thread count)");

    // Set number of warps per block
    // (for fine-grained barrier synchronisation)
    unsigned warpsPerBlock = k->blockDim.x >> SIMTLogLanes;
    while (!cpuSIMTCanPut()) {}
    cpuSIMTSetWarpsPerBlock(warpsPerBlock);

    // Set number of blocks per streaming multiprocessor
    k->blocksPerSM = (SIMTWarps * SIMTLanes) / k->blockDim.x;
    cpuAssert((k->gridDim.x % k->blocksPerSM) == 0,
      "NoCL: blocksPerSM not a multiple of number of blocks");

    // Set address of kernel closure
    uintptr_t kernelAddr = (uintptr_t) k;
    while (!cpuSIMTCanPut()) {}
    cpuSIMTSetKernel(kernelAddr);

    // Flush cache
    cpuCacheFlushFull();

    // Start kernel on SIMT core
    uintptr_t entryAddr = (uintptr_t) _noclSIMTEntry_<K>;
    while (! cpuSIMTCanPut()) {}
    cpuSIMTStartKernel(entryAddr);

    // Wait for kernel response
    while (!cpuSIMTCanGet()) {}
    return cpuSIMTGet();
  }

// Barrier synchronisation
INLINE void __syncthreads() {
  simtLocalBarrier();
}

#endif
