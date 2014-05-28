#include <stdio.h>
#include <cuda.h>

#define CHECK_CUDA_ERROR(file, line) {                                  \
    cudaError_t err_t;                                                  \
    if ((err_t = cudaGetLastError() ) != cudaSuccess) {                 \
      printf("Cuda error: %s \n", cudaGetErrorString(err_t));             \
      printf("File: %s; line %d\n", file, line);                          \
      exit(1);                                                          \
    }                                                                   \
  }   


const int N = 1000000; // 1M
const int blocksize = 16;

__global__
void vupdate(const int n, const int *x, int *y)
{

  int i = blockIdx.x * blockDim.x + threadIdx.x;
    
    if (i < n)
      y[i] = x[i] + y[i];

}

int main()
{
  int *h_x, *h_y;
  int *d_x, *d_y;

  cudaEvent_t start, stop;
  float elapsedTime;

  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  cudaEventRecord(start, 0);

  // Allocating on the CPU
  h_x =  (int *) malloc(N*sizeof(int));
  h_y =  (int *) malloc(N*sizeof(int));

  // init data
  for (int i=0; i<N; ++i) {
    h_x[i] = i;
    h_y[i] = i-10;
  }

  // Allocating on the GPU
  cudaMalloc( (void**)&d_x, N*sizeof(int) );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);
  cudaMalloc( (void**)&d_y, N*sizeof(int) );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);

  // Copy data from CPU to GPU
  cudaMemcpy( d_x, h_x, N*sizeof(int), cudaMemcpyHostToDevice );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);
  cudaMemcpy( d_y, h_y, N*sizeof(int), cudaMemcpyHostToDevice );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);

  dim3 dimBlock(blocksize);
  dim3 dimGrid(N / blocksize + 1);

  for (int test=0; test<500; ++test) {
    vupdate<<<dimGrid, dimBlock>>>(N, d_x, d_y);
    CHECK_CUDA_ERROR(__FILE__, __LINE__);
  }

  // copy back
  cudaMemcpy( h_x, d_x , N*sizeof(int), cudaMemcpyDeviceToHost );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);
  cudaMemcpy( h_y, d_y , N*sizeof(int), cudaMemcpyDeviceToHost );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);

  // Free on GPU
  cudaFree( d_x );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);
  cudaFree( d_y );
  CHECK_CUDA_ERROR(__FILE__, __LINE__);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  cudaEventElapsedTime(&elapsedTime, start, stop);

  printf ("Time for the kernel: %f ms\n", elapsedTime);

  // Free on host
  free(h_x);
  free(h_y);

  return 0;
}
