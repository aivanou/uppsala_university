/**********************************************************************
 * A simple "hello world" program for MPI/C
 *
 **********************************************************************/

#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

    int rank;

    MPI_Init(&argc, &argv); /* Initialize MPI               */

    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    
    printf("hey there! my rank is: %d \n",rank); /* Print a message              */

    MPI_Finalize(); /* Shut down and clean up MPI   */

    return 0;
}
