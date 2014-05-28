/**********************************************************************
 * Enumeration sort
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

  int len,seed,i,j,rank,nthreads;
  double *indata, *outdata, timer;
  
  printf("Give number of elements: ");
  scanf("%d",&len);
  indata=(double *)malloc(len*sizeof(double));
  outdata=(double *)malloc(len*sizeof(double));

  // Generate random numbers (Wichmann-Hill)
  seed=171;
  for (i=0;i<len;i++){
    seed=(171*seed)%30269;
    indata[i]=seed/30268.0;
    outdata[i]=-1;
  }


  // Enumeration sort
  timer=omp_get_wtime();
  for (j=0;j<len;j++)
    {
      rank=0;
      for (i=0;i<len;i++)
	    if (indata[i]<indata[j]) rank++;
      outdata[rank]=indata[j];
    }
  timer=omp_get_wtime()-timer;
  printf("Time: %f\n",timer);

  // Check results, -1 implies data same as the previous element
    for (i=0; i<len-1; i++)
      if (outdata[i]>outdata[i+1] && outdata[i+1]>-1)
	printf("ERROR: %f,%f\n", outdata[i],outdata[i+1]);

  return 0;
}
