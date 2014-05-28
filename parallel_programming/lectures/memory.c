/**********************************************************************
 * Memory consistency in OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
	volatile int id;
	 int nthr;
    
#pragma omp parallel private(id) num_threads(12)
	{
		id=omp_get_thread_num();
#pragma omp single
        {	
		nthr=omp_get_num_threads();}
		printf("Hello World! %d %d\n",id,nthr);
	}	
	printf("Stopping\n"); 
	return 0;
}
