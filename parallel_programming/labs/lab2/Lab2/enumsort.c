/**********************************************************************
 * Enumeration sort
 *
 **********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#define NUM_THREADS	400
#define len 100000

void findrank(long j);
void* findranks(void* args);

struct thread_data{
    int start;
    int end;
};

typedef struct thread_data thread_data_t;

double indata[len], outdata[len];

void* findranks(void* args){
    
    thread_data_t* data=(thread_data_t*)args;
    int i;
    for(i=data->start;i<data->end;++i){
        findrank(i);
    }
}

void findrank(long j)
{
	int rank,i;
	
	rank=0;
	for (i=0;i<len;i++)
		if (indata[i]<indata[j]) rank++;

        outdata[rank]=indata[j];
}


int main(int argc, char *argv[]) {
	
  pthread_t threads[NUM_THREADS];
  pthread_attr_t attr;
  int seed,i,j,rank,nthreads,ttime,t;
  long el;
  void *status;
  
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
	

  // Generate random numbers
  for (i=0;i<len;i++){
      indata[i]=drand48();
      outdata[i]=-1.0;
  }


  // Enumeration sort
  ttime=timer();
  int start=0,backet=len/NUM_THREADS,end=backet;
  for(j=0;j<NUM_THREADS-1;++j){
      thread_data_t* data=(thread_data_t*)malloc(sizeof(thread_data_t));
      data->start=j*backet;
      data->end=(j+1)*backet;
      pthread_create(&threads[j],&attr,findranks,(void*)data);
  }
  thread_data_t data;
  data.start=j*backet;
  data.end=len;
  pthread_create(&threads[NUM_THREADS-1],&attr,findranks,(void*)&data);
  
  for(j=0;j<NUM_THREADS;++j){
      pthread_join(threads[j],&status);
  }
  
  ttime=timer()-ttime;
  printf("Time: %f %d\n",ttime/1000000.0,NUM_THREADS);

  // Check results, -1 implies data same as the previous element
    for (i=0; i<len-1; i++)
      if (outdata[i]>outdata[i+1] && outdata[i+1]>-1)
	printf("ERROR: %f,%f\n", outdata[i],outdata[i+1]);
    printf("\n");
    for (i=0; i<len-1; i++)
    {
        printf("%.3f  ",outdata[i]);
    }
  printf("\n");
  return 0;
}
