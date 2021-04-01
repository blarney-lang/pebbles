#include <nocl.h>
#include <rand.h>

// Euclid's algorithm on vectors
struct VecGCD : Kernel {
  int len;
  int* a;
  int* b;
  int* result;

  void kernel() {
    for (int i = threadIdx.x; i < len; i += blockDim.x) {
      int x = a[i];
      int y = b[i];
      while (x != y) {
        if (x > y) 
          x = x-y;
        else
          y = y-x;

        simtConverge();
      }
      result[i] = x;
    }
  }
};

// Vector size for benchmarking
#define N 100

int main()
{
  // Input and output vectors
  simt_aligned int a[N], b[N], result[N];

  // Initialise inputs
  uint32_t seed = 100;
  for (int i = 0; i < N; i++) {
    a[i] = 1 + (rand(&seed) & 0xff);
    b[i] = 1 + (rand(&seed) & 0xff);
  }

  // Instantiate kernel
  VecGCD k;

  // Use single block of threads
  k.blockDim.x = SIMTLanes * SIMTWarps;

  // Assign parameters
  k.len = N;
  k.a = a;
  k.b = b;
  k.result = result;

  // Invoke kernel
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < N; i++) printf("%x\n", result[i]);

  return 0;
}
