#include <nocl.h>
#include <rand.h>
#include <cpu/io.h>

// Euclid's algorithm on vectors
struct VectorGCD : Kernel {
  int* a;
  int* b;
  int* result;

  void kernel() {
    int x = a[id];
    int y = b[id];
    while (x != y) {
      if (x > y) 
        x = x-y;
      else
        y = y-x;

      simtConverge();
    }
    result[id] = x;
  }
};

// Vector size for benchmarking
#define N 100

int main()
{
  // Input and output vectors
  aligned int a[N], b[N], result[N];

  // Initialise inputs
  uint32_t seed = 100;
  for (int i = 0; i < N; i++) {
    a[i] = 1 + (rand(&seed) & 0xff);
    b[i] = 1 + (rand(&seed) & 0xff);
  }

  // Invoke kernel
  VectorGCD k;
  k.numWorkItems = N;
  k.a = a;
  k.b = b;
  k.result = result;
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < N; i++) printf("%x\n", result[i]);

  return 0;
}
