/**********************************************************************
 * Example: Loop parallelism in OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

void work(int c){
    sleep(c);
}

int main(int argc, char *argv[]) {
  int i,n=10;
  int *A;
  double timer1;
 
  A = (int *)malloc(n*sizeof(int));
  for (i=0;i<n;i++){
    A[i]=i;
  }

  timer1=omp_get_wtime();
#pragma omp parallel num_threads(5)
  {
#pragma omp for 
    for (i=0;i<n;i++){
      work(A[i]);
    }
  }
  timer1=omp_get_wtime()-timer1;
  printf("Time: %f sec\n",timer1);

  return 0;
}
