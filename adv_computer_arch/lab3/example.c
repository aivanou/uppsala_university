#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define NITER 500000000


int main (){
   long val = 0;
   clock_t starttime = clock ();
    while (val < NITER){
      while (1){
        long current = val;
        long next = current+1;
        if ( __sync_bool_compare_and_swap (&val, current, next))
            break;
      }
     }
   clock_t castime = (clock()-starttime)/ (CLOCKS_PER_SEC / 1000);
   printf ("Time taken : %d ",castime);
}
