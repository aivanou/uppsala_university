/**********************************************************************
 * A simple "hello world" program for OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

    printf("Starting\n");

#pragma omp parallel 
    {
        printf("Hello World!\n");
    }

    printf("Stopping\n");

    return 0;
}
