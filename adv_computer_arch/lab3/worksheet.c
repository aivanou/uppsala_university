#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define NTHREADS 500

int main(){

    int i;
#pragma omp parallel for private(i)
    for(i=0;i<NTHREADS;++i){
        printf("Hi there! %d\n",i);
    }

    return 1;
}
