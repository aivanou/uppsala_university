/**********************************************************************
 * Derived datatypes in MPI/C
 *
 **********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    int rank, size, nx, ny, row, col, count, blocklen, stride;
    double *A;
    MPI_Status status;
    MPI_Datatype newtype;

    MPI_Init(&argc, &argv); /* Initialize MPI               */
    MPI_Comm_size(MPI_COMM_WORLD, &size); /* Get the number of processors */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank); /* Get my number                */

    nx = 8;
    ny = 16;
    A = (double *) calloc(nx*ny, sizeof (double));
    if (rank == 0) {
        printf("Matrix A on proc 0\n");
        for (row = 0; row < nx; row++) {
            for (col = 0; col < ny; col++) {
                A[row * ny + col] = (double) col + 100 * row + 1000;
                printf("%d ", (int) A[row * ny + col]);
            }
            printf("\n");
        }
        printf("\n");
    }

    count = 4;
    blocklen = 8;
    stride = 16;
    MPI_Type_vector(count, blocklen, stride, MPI_DOUBLE, &newtype);
    MPI_Type_commit(&newtype);

    /* Send last column, notice the message length = 1 ! */
    if (rank == 0) {
        MPI_Send(&A[0] + 16 * 4 + 8, 1, newtype, 1, 111, MPI_COMM_WORLD);

    } else if (rank == 1) {
        MPI_Recv(&A[0] + 16 * 4, 1, newtype, 0, 111, MPI_COMM_WORLD, &status);
        printf("Matrix A on proc 1\n");
        for (row = 0; row < nx; row++) {
            for (col = 0; col < ny; col++)
                printf("%4d ", (int) A[row * ny + col]);
            printf("\n");
        }
    }

    free(A);
    MPI_Type_free(&newtype);
    MPI_Finalize();

    return 0;
}
