#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <omp.h>

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
double **A;
int n;

int main(int argc, char *argv[]) {
    int i, j, k, m;
    double t, error, d, time;

    if (argc != 2) {
        printf("Use: ./lu n\n");
        return 0;
    }
    n = atoi(argv[1]);
    time = omp_get_wtime();

    //Allocate and fill matrices
    A = (double **) malloc(n * sizeof (double *));
    for (i = 0; i < n; i++) {
        A[i] = (double *) malloc(n * sizeof (double));
    }

    for (i = 0; i < n; i++)
        for (j = 0; j < n; j++) {
            A[i][j] = 1.0 / (i + j + 1);
        }


    // Factorize A=L*U (In situ, no pivoting)	  

    for (k = 0; k < n; k++) {
#pragma omp parallel for schedule(guided,16)
        for (i = k + 1; i < n; i++)
            A[i][k] = A[i][k] / A[k][k];
#pragma omp parallel for schedule(dynamic,20)
        for (i = k + 1; i < n; i++)
            for (j = k + 1; j < n; j++)
                A[i][j] = A[i][j] - A[i][k] * A[k][j];
    }

    time = omp_get_wtime() - time;
    printf("Elapsed time: %f \n", time);

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
