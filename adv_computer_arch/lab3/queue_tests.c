#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "queue.h"

queue* test_queue;

typedef struct test_th{
    int id;
    int iterations;
}test_th;

void single_thread_instert(int count){
    for(int i=0;i<count;++i){
        int k=i;
        queue_enq(test_queue,(void*)k);
    }
}

void single_thread_remove(int count){
    for(int i=0;i<count;++i){
//        int* i_addr=&i;// omg, what the fuck is going here ??
        int* value=(int*)malloc(sizeof(int));
        queue_deq(test_queue,(void**)&value);

        printf("read value: %d  %d\n",i,value);
    }
}

void* multi_th_insert(void* arg){
    test_th* self=(test_th*)arg;
    for(int i=0;i<self->iterations;++i){
        queue_enq(test_queue,(void*)i);
        printf("%d  inserted data: %d\n",self->id,i);
    }
}

void* multi_th_remove(void* arg){
    test_th* self=(test_th*)arg;
    for(int i=0;i<self->iterations;++i){
        int* value=(int*)malloc(sizeof(int));
        queue_deq(test_queue,(void**)&value);
        printf("%d  removed data: %d %d\n",self->id,value,i);
    }
}

void run_threads(int producer, int consumer){
    pthread_t threads[producer+consumer];

    printf("starting threads\n");
    for(int i=0;i<producer;++i){
        test_th *data=(test_th*)malloc(sizeof(test_th));
        data->id=i;
        data->iterations=100;
        pthread_create(&threads[i],NULL,multi_th_insert,data);
    }
    for(int i=0;i<consumer;++i){
        test_th *data=(test_th*)malloc(sizeof(test_th));
        data->id=i+consumer;
        data->iterations=100;
        pthread_create(&threads[consumer+i],NULL,multi_th_remove,data);
    }
    for(int i=0;i<consumer+producer;++i){
        pthread_join(threads[i],NULL);
    }
}


int main(){
    test_queue=queue_init(100300);
//    single_thread_instert(100);
//    single_thread_remove(100);
    run_threads(1,1);
    return 1;
}
