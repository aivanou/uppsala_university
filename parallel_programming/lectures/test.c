#include <stdio.h>
#include <omp.h>

int main(){
	int b=0,a=100;
#pragma omp parallel private(a) num_threads(1000)
	{
		b=b+1000;
		a=b+a;
	}
printf("%d %d \n",a,b);
}
