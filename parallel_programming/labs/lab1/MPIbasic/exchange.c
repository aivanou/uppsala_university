/**********************************************************************
 * Point-to-point communication using MPI
 *
 **********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    int rank, size;
    double a, b;
    MPI_Status status;

    MPI_Init(&argc, &argv); /* Initialize MPI               */
    MPI_Comm_size(MPI_COMM_WORLD, &size); /* Get the number of processors */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank); /* Get my number                */

    a = 100.0 + (double) rank; /* Different a on different processors */

    /* Exchange variable a, notice the send-recv order */
    MPI_Request send_req;
    MPI_Request recv_req;
    if (rank == 0) {
        MPI_Isend(&a, 1, MPI_DOUBLE, 1, 111, MPI_COMM_WORLD, &send_req);
        MPI_Wait(&send_req, &status);
        //    MPI_Send(&a, 1, MPI_DOUBLE, 1, 111, MPI_COMM_WORLD);
        MPI_Irecv(&b, 1, MPI_DOUBLE, 1, 222, MPI_COMM_WORLD, &recv_req);
        MPI_Wait(&recv_req, &status);
        //      MPI_Recv(&b, 1, MPI_DOUBLE, 1, 222, MPI_COMM_WORLD, &status);
        printf("Processor 0 got %f from processor 1\n", b);
    } else if (rank == 1) {
        MPI_Irecv(&b, 1, MPI_DOUBLE, 0, 111, MPI_COMM_WORLD, &recv_req);
        MPI_Wait(&recv_req, &status);
        //    MPI_Recv(&b, 1, MPI_DOUBLE, 0, 111, MPI_COMM_WORLD, &status);
        MPI_Isend(&a, 1, MPI_DOUBLE, 0, 222, MPI_COMM_WORLD, &send_req);
        MPI_Wait(&send_req, &status);

        //    MPI_Send(&a, 1, MPI_DOUBLE, 0, 222, MPI_COMM_WORLD);
        printf("Processor 1 got %f from processor 0\n", b);
    }


    MPI_Finalize();

    return 0;
}
