#include <cpu.h>
#include <cpu/io.h>
#include <nocl.h>

// Kernel for adding vectors
struct VectorAdd : Kernel {
  int* a;
  int* b;
  int* result;

  void kernel() {
    result[id] = a[id] + b[id];
  }
};

// Vector size for benchmarking
#define N 3000

int main()
{
  // Input and output vectors
  aligned int a[N], b[N], result[N];

  // Initialise inputs
  for (int i = 0; i < N; i++) {
    a[i] = i;
    b[i] = 2*i;
  }

  // Invoke kernel
  VectorAdd k;
  k.numWorkItems = N;
  k.a = a;
  k.b = b;
  k.result = result;
  noclRunKernel(&k);

  // Display result
  int sum = 0;
  for (int i = 0; i < N; i++) sum += result[i];
  printf("%x\n", sum);

  return 0;
}
