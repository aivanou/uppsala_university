#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

#define NTHREADS 4

void lu_seq(double** A, int length);

double **A;
int n;


pthread_barrier_t barr;


int main(int argc, char *argv[]) {
    int i, j, k, m, time;
    double t, error, d;

    n = atoi(argv[1]);

    //Allocate and fill matrices
    A = (double **) malloc(n * sizeof (double *));
    for (i = 0; i < n; i++) {
        A[i] = (double *) malloc(n * sizeof (double));
    }

    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++) {
            A[i][j] = 1.0 / (i + j + 1);
        }

    pthread_barrier_init(&barr, NULL, NTHREADS);
    
    lu_seq(A, n);

    // Correctness check (L*U-A)
    error = 0.0;
    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++) {
            d = 0.0;
            m = MIN(i, j);
            for (k = 0; k < m; k++)
                d += A[i][k] * A[k][j];
            if (i == m)
                d += A[m][j];
            else
                d += A[i][m] * A[m][j];
            error += fabs(d - 1.0 / (i + j + 1));
        }
    printf("Error: %e \n", error);


    return 0;

}

void *thread_work(void* args) {
    thread_data_t* data = (thread_data_t*) args;
    int k;
    int start=data->start,end=data->end;
    int min_ind=data->min_ind,max_ind=data->max_ind;
    for (k = 0; k < n; k++) {
        for (i = k + 1; i < n; i++) {
            if(i>=min_ind &&i<max_ind){
                 A[i][k] = A[i][k] / A[k][k];
            }
            pthread_barrier_wait(&barr);
            for (j = k + 1; j < n; j++)
                A[i][j] = A[i][j] - A[i][k] * A[k][j];
        }
        pthread_barrier_wait(&barr);
    }
}

void lu_seq(double** A, int length) {
    int k, i, j;
    int time = timer();

    // Factorize A=L*U (In situ, no pivoting)	  
    for (k = 0; k < n; k++) {
        for (i = k + 1; i < n; i++) {
            A[i][k] = A[i][k] / A[k][k];
            for (j = k + 1; j < n; j++)
                A[i][j] = A[i][j] - A[i][k] * A[k][j];
        }
    }

    time = timer() - time;
    printf("Elapsed time for sequential: %f \n", time / 1000000.0);

}