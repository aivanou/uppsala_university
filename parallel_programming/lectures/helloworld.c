/**********************************************************************
 * A simple "hello world" program for OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

#pragma omp parallel
	{
		printf("Hello World! %d\n",omp_get_thread_num());
	}	
}
