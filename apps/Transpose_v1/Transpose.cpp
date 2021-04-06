#include <nocl.h>

// One thread per matrix element
// Assumption: blockDim.x == blockDim.y
struct Transpose : Kernel {
  Array2D<int> in, out;

  void kernel() {
    Array2D<int> block = shared.array<int>(blockDim.y, blockDim.x + 1);

    // Origin of block within matrix
    unsigned originX = blockIdx.x * blockDim.x;
    unsigned originY = blockIdx.y * blockDim.y;

    // Load block
    block[threadIdx.y][threadIdx.x] =
      in[originY + threadIdx.y][originX + threadIdx.x];

    __syncthreads();

    // Store block
    out[originX + threadIdx.y][originY + threadIdx.x] =
      block[threadIdx.x][threadIdx.y];
  }
};

// Matrix size for benchmarking
// Assumed to be a multiple of 32
#define WIDTH 128
#define HEIGHT 64

int main()
{
  // Input and output matrix data
  nocl_aligned int matInData[WIDTH*HEIGHT];
  nocl_aligned int matOutData[WIDTH*HEIGHT];

  // Friendly array wrappers
  Array2D<int> matIn(matInData, HEIGHT, WIDTH);
  Array2D<int> matOut(matOutData, WIDTH, HEIGHT);

  // Initialise inputs
  for (int i = 0; i < HEIGHT; i++) {
    for (int j = 0; j < WIDTH; j++)
      matIn[i][j] = j;
  }

  // Instantiate kernel
  Transpose k;

  // One thread per matrix element
  k.blockDim.x = 32;
  k.blockDim.y = 32;
  k.gridDim.x = WIDTH / k.blockDim.x;
  k.gridDim.y = HEIGHT / k.blockDim.y;

  // Assign parameters
  k.in = matIn;
  k.out = matOut;

  // Invoke kernel
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < WIDTH; i++) {
    for (int j = 0; j < HEIGHT; j++)
      printf("%x ", matOut[i][j]);
    printf("\n");
  }

  return 0;
}
