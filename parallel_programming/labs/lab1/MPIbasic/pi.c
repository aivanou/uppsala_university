/**********************************************************************
 * This program calculates pi using MPI/C
 *
 * Andreas K�h�ri
 * 2000-01-16
 **********************************************************************/

#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  int rank, size;
  const long int intervals = 100000000L ; /* The sum is [globally]
                                             divided into this many
                                             intervals     */
  int chunk;             /* This many iterations will I do */
  int i, istart, istop;  /* Variables for the local loop   */
  double sum, dx, globsum;

  MPI_Init(&argc, &argv); /* Initialize MPI */

  MPI_Comm_size(MPI_COMM_WORLD, &size); /* Get the number of processors */
  MPI_Comm_rank(MPI_COMM_WORLD, &rank); /* Get my number                */

  chunk  = intervals/size;       /* Number of intervals per processor */
  istart = rank*chunk+1;         /* Calculate start and stop indices  */
  istop  = (rank + 1)*chunk;     /* for the local loop                */
  if (rank == size-1 ) {
    istop = intervals;           /* Make sure the last processor      */
  }                              /* computes until the end            */
  
  dx  = 1.0/intervals;
  sum = 0.0;
  for (i = istart; i <= istop; i++) { /* The local loop */
    double x = dx*(i - 0.5);
    sum += dx*4.0/(1.0 + x*x);
  }

  /* COMPUTE THE FINAL RESULT ADDING THE PARTIAL SUMS */
  globsum=0;
  MPI_Reduce(&sum,&globsum,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);
  if (rank == 0) {
      printf("PI is approx. %.16f\n",  globsum);
  
  }


  MPI_Finalize(); /* Shut down and clean up MPI */

  return 0;
}
