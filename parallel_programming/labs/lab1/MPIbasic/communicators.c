/*********************************************************************
       Example: Virtual topologies and communicators

 *********************************************************************/
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

    int myid, nproc, rank, i, j, subrank, data, mydata;
    MPI_Comm proc_grid, proc_col;
    int coords[3], pos[3], reorder = 1, ndim = 3, dims[3] = {0, 0, 0}, periods[3] = {0, 0, 0};
    MPI_Status stat;



    /* Initialize MPI  */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);




    /* Create a virtual 2D-grid topology */
    MPI_Dims_create(nproc, ndim, dims);
    MPI_Cart_create(MPI_COMM_WORLD, ndim, dims, periods, reorder, &proc_grid);
    MPI_Comm_rank(proc_grid, &myid); /* Note: use proc_grid */




    /* Create a communicator for each row */
    MPI_Cart_coords(proc_grid, myid, ndim, coords);
    
    MPI_Comm_split(proc_grid, 0, coords[1], &proc_col);
//    MPI_Comm_split(proc_grid, 2, coords[2], &proc_col);
    MPI_Comm_rank(proc_col, &subrank);




    /* Broadcast within a row */
    if (subrank == 0)
        mydata = coords[0];
    MPI_Bcast(&mydata, 1, MPI_INT, 0, proc_col);


    int k=0;
    /* Check the result of Broadcast */
    for (i = 0; i < dims[0]; i++) {
        for (j = 0; j < dims[1]; j++) {
            for (k = 0; k < dims[2]; ++k) {

                pos[0] = i;
                pos[1] = j;
                pos[2]=k;
                MPI_Cart_rank(proc_grid, pos, &rank);
                if (rank == myid)
                    printf("Rank: %d, Coords: %d %d,%d  Data: %d\n", myid, i, j,k, mydata);
            }
        }
    }



    /* Exit and clean up MPI variables */
    MPI_Comm_free(&proc_col);
    MPI_Comm_free(&proc_grid);
    MPI_Finalize();
}




