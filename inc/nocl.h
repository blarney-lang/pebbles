// CUDA-like library for compute kernels

#ifndef _NOCL_H_
#define _NOCL_H_

#include <SoC.h>
#include <cpu.h>
#include <simt.h>
#include <stdint.h>
#include <atomics.h>

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

// 1D arrays
template <typename T> struct Array {
  T* base;
  unsigned size;
  INLINE T& operator[](int index) const {
    return base[index];
  }
};

// 2D arrays
template <typename T> struct Array2D {
  T* base;
  unsigned size0, size1;
  INLINE const Array<T> operator[](int index) const {
    Array<T> a; a.base = &base[index * size1]; a.size = size1; return a;
  }
};

// 3D arrays
template <typename T> struct Array3D {
  T* base;
  unsigned size0, size1, size2;
  INLINE const Array2D<T> operator[](int index) const {
    Array2D<T> a; a.base = &base[index * size1 * size2];
    a.size0 = size1; a.size1 = size2; return a;
  }
};

// For shared local memory allocation
// Memory is allocated/released using a stack
struct SharedLocalMem {
  // This points to the top of the stack (which grows upwards)
  char* top;

  // Allocate memory on shared memory stack (static)
  template <unsigned numBytes> void* alloc() {
    void* ptr = (void*) top;
    constexpr unsigned bytes =
      (numBytes & 3) ? (numBytes & ~3) + 4 : numBytes;
    top += bytes;
    return ptr;
  }

  // Allocate memory on shared memory stack (dynamic)
  void* alloc(unsigned numBytes) {
    void* ptr = (void*) top;
    unsigned bytes = (numBytes & 3) ? (numBytes & ~3) + 4 : numBytes;
    top += bytes;
    return ptr;
  }

  // Allocate 1D array with static size
  template <typename T, unsigned dim1> T* array() {
    return (T*) alloc<dim1 * sizeof(T)>();
  }

  // Allocate 2D array with static size
  template <typename T, unsigned dim1, unsigned dim2> auto array() {
    return (T (*)[dim2]) alloc<dim1 * dim2 * sizeof(T)>();
  }

  // Allocate 3D array with static size
  template <typename T, unsigned dim1,
              unsigned dim2, unsigned dim3> auto array() {
    return (T (*)[dim2][dim3]) alloc<dim1 * dim2 * dim3 * sizeof(T)>();
  }

  // Allocate 1D array with dynamic size
  template <typename T> Array<T> array(unsigned n) {
    Array<T> a; a.base = (T*) alloc(n * sizeof(T));
    a.size = n; return a;
  }

  // Allocate 2D array with dynamic size
  template <typename T> Array2D<T> array(unsigned n0, unsigned n1) {
    Array2D<T> a; a.base = (T*) alloc(n0 * n1 * sizeof(T));
    a.size0 = n0; a.size1 = n1; return a;
  }

  template <typename T> Array3D<T>
    array(unsigned n0, unsigned n1, unsigned n2) {
      Array3D<T> a; a.base = (T*) alloc(n0 * n1 * n2 * sizeof(T));
      a.size0 = n0; a.size1 = n1; a.size2 = n2; return a;
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

  // Invoke kernel
  while (k.blockIdx.x < k.gridDim.x) {
    k.shared.top = &__localBase + localBytesPerBlock * k.blockIdx.x;
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
