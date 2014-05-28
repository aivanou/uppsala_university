/**********************************************************************
 * Example task-directive
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>

int fib(int n) { 
	int i, j; 
	if (n<2) 
		return n; 
	else { 
		i=fib(n-1); 
		j=fib(n-2); 
		return i+j; 
	} 
} 

int main(int argc, char *argv[]) {
	int f,i,id,nth;

  printf("Computing Fibonacci %d\n", omp_get_max_threads());
 
#pragma omp parallel
  {
#pragma omp single private(i) //nowait
	  {
          printf("Task master 1: %d\n",omp_get_thread_num());
		  for (i=1;i<=42;i++){
#pragma omp task private(id,f) firstprivate(i) //if(0)
			  {
			  f=fib(i);
			  id=omp_get_thread_num();
			  printf("Single 1: %d %d %d\n" ,id,i,f);	
			  }}
	  }
#pragma omp single private(i) 
	  {
        printf("Task master 2: %d\n",omp_get_thread_num());
		  for (i=1;i<=42;i++){
#pragma omp task private(id,f) firstprivate(i)
			  {
				  f=fib(i);
				  id=omp_get_thread_num();
				  printf("Single 2: %d %d %d\n" ,id,i,f);	
			  }}
	  }
  }

  
  printf("Stopping\n"); 

  return 0;
}
