/**********************************************************************
 * Point-to-point communication using MPI
 *
 **********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


int main(int argc, char *argv[]) {
    int rank, size, i;
    double a;
    MPI_Status status;

    MPI_Init(&argc, &argv); /* Initialize MPI               */
    MPI_Comm_size(MPI_COMM_WORLD, &size); /* Get the number of processors */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank); /* Get my number                */

//    if (rank == 0) {
//        int a = 999.999;
//        MPI_Send(&a, 1, MPI_DOUBLE, 1, 111, MPI_COMM_WORLD);
//        printf("Processor: %d sent variable\n", rank);
//    } else if (rank == size - 1) {
//        int a;
//        MPI_Status status;
//        MPI_Recv(&a, 1, MPI_DOUBLE, rank - 1, 111, MPI_COMM_WORLD, &status);
//        printf("Last processor: %d received value: %d\n", rank, a);
//    } else {
//        int a;
//        MPI_Status status;
//        MPI_Recv(&a, 1, MPI_DOUBLE, rank - 1, 111, MPI_COMM_WORLD, &status);
//        printf("Processor %d received value: %d\n", rank, a);
//        MPI_Send(&a, 1, MPI_DOUBLE, rank + 1, 111, MPI_COMM_WORLD);
//        printf("Processor %d sent value: %d\n", rank, a);
//    }
    
    if(rank==0){
        int a=99;
        int dest_processor=1;
        int accel_processes=1;
        int round=1;
        while(dest_processor<size){
            MPI_Send(&a,1,MPI_DOUBLE,dest_processor,111,MPI_COMM_WORLD);
            printf("Processor: %d sent variable to %d\n", rank,dest_processor);
            round*=2;
//           0 -> 1 2 4 8 ;  1 -> 2+1 
            dest_processor=round;
        }
    }
    else if (rank == size - 1) {
        int a;
        MPI_Status status;
        int t1=(int)((int)log2(rank))+1;
        // 0 -> 1 |  1-> 2 0 -> 3  |  3-> 4 2 -> 5 1-> 6  0 -> 7 |
        int round=(int)pow(2,t1);
        int recv_proc=rank-round/2;

        MPI_Recv(&a, 1, MPI_DOUBLE, recv_proc, 111, MPI_COMM_WORLD, &status);
        printf("Last processor: %d received value: %d from %d\n", rank, a,rank-1);
    } else {
        int a;
        int t1=(int)((int)log2(rank))+1;
        // 0 -> 1 |  1-> 2 0 -> 3  |  3-> 4 2 -> 5 1-> 6  0 -> 7 |
        int round=(int)pow(2,t1);
        int send_proc=round+rank;
        int dest_proc=(int)pow((int)log2(rank)+1,2)+rank;
        
        int recv_proc=rank-round/2;
        
        MPI_Recv(&a,1,MPI_DOUBLE,recv_proc,111,MPI_COMM_WORLD,&status);
        printf("Processor %d received value: %d from %d\n", rank, a,recv_proc);
        while(send_proc<size){        
            MPI_Send(&a,1,MPI_DOUBLE,send_proc,111,MPI_COMM_WORLD);
            printf("Processor: %d send value to %d\n", rank, send_proc);
            round*=2;
            send_proc=round+rank;
        }
    }

    MPI_Finalize();

    return 0;
}


