/**********************************************************************
 * Example: Loop parallelism in OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>


double work(double c){
    double d=0.0;
    int k;
    for (k=0;k<200;k++){
        d=0.0;
        while (d<c)
            d=d+1.0;
    }
  return d;
}    

int main(int argc, char *argv[]) {
  int i,n=10000;
  double *A,*B;
  double timer1;
 
  A = (double *)malloc(n*sizeof(double));
  B = (double *)malloc(n*sizeof(double));
  for (i=0;i<n;i++){
    A[i]=(double)i;
  }

  timer1=omp_get_wtime();
#pragma omp parallel
  {
#pragma omp for schedule(dynamic,atoi(argv[1])) 
    for (i=0;i<n;i++){
      B[i]=work(A[i]);
    }
  }
  timer1=omp_get_wtime()-timer1;
  printf("Time: %f sec\n",timer1);

  return 0;
}
