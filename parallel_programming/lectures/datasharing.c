/**********************************************************************
 * Example: Datasharing in OpenMP/C
 *
 *
 **********************************************************************/

#include <omp.h>
#include <stdio.h>

int add1000(int x){
    int i;
    for (i=0; i<1000;i++) x=x+1;
    return x;
}

int main(int argc, char *argv[]) {
  int a,b;
  a=100;b=0;
 
#pragma omp parallel private(a) num_threads(10)
  {
      b=add1000(b);
      a=b+a;
    }
  printf("a= %d b= %d\n",a,b);
  return 0;
}
