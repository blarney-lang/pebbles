#include <nocl.h>

// Helper function
template <typename T> INLINE void swap(T& a, T& b)
  { T tmp = a; a = b; b = tmp; }

// Kernel for computing the parallel prefix sum (inclusive scan).
// Assumptions: (1) a single thread block; (2) the block size divides
// evenly into the input size.
struct Scan : Kernel {
  int len;
  int *in, *out;

  void kernel() {
    // Shared arrays
    Array<int> acc = shared.array<int>(blockDim.x);
    Array<int> totalA = shared.array<int>(blockDim.x);
    Array<int> totalB = shared.array<int>(blockDim.x);
    Array<int> result = shared.array<int>(blockDim.x);

    // Shorthand for local thread id
    int t = threadIdx.x;
    
    // Initialise inter-block accumulator array
    acc[t] = 0;
    __syncthreads();

    for (int x = t; x < len; x += blockDim.x) {
      // Initialise arrays
      result[t] = totalA[t] = in[x];
      __syncthreads();

      // Local scan (variant of Sklansky's algorithm)
      for (int i = 1; i < blockDim.x; i <<= 1) {
        int offset = i;
        if (t&i) {
          result[t] += totalA[t-i];
          offset = -i;
        }
        totalB[t] = totalA[t] + totalA[t+offset];
        swap(totalA, totalB);
        __syncthreads();
      }

      // Write to global memory
      out[x] = result[t] + acc[t];
      acc[t] += totalA[t];
    }
  }
};

int main()
{
  // Vector size for benchmarking
  int N = 4096;

  // Input and output vectors
  simt_aligned int in[N], out[N];

  // Initialise inputs
  for (int i = 0; i < N; i++) in[i] = i;

  // Instantiate kernel
  Scan k;

  // Use a single block of threads
  k.blockDim.x = SIMTWarps * SIMTLanes;

  // Assign parameters
  k.len = N;
  k.in = in;
  k.out = out;

  // Invoke kernel
  noclRunKernel(&k);

  // Check result
  bool ok = true;
  int acc = 0;
  for (int i = 0; i < N; i++) {
    acc += i;
    ok = ok && out[i] == acc;
  }

  // Display result
  puts("Self test: ");
  puts(ok ? "PASSED" : "FAILED");
  putchar('\n');

  return 0;
}
