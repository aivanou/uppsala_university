#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#include "queue.h"
#include "threadpool.h"

thread_data** threads;

static int nthreads;
static volatile unsigned int max_task_id=0;

#define MAX_OUTPUT_QUEUES 1
#define MAX_QUEUE_SIZE 2048

pthread_cond_t input_queue_cond;
pthread_cond_t output_queue_cond;

queue* input_queue=NULL;
queue** output_queues=NULL;

static int VERBOSE=0;

#define PVERBOSE(message) if (VERBOSE==1) printf("%s\n",message);

#define OQ_INDEX(ind) (ind%MAX_OUTPUT_QUEUES)

void threadpool_init(int active_threads){
    nthreads=active_threads;
    output_queues=(queue**)malloc(MAX_OUTPUT_QUEUES*sizeof(queue*));
    for(int i=0;i<MAX_OUTPUT_QUEUES;++i){
        output_queues[i]=queue_init(MAX_QUEUE_SIZE);
    }

    input_queue=queue_init(MAX_QUEUE_SIZE);

    threads=(thread_data**)malloc(nthreads*sizeof(thread_data*));
    for(int i=0;i<nthreads;++i){
        thread_data* td=(thread_data*)malloc(sizeof(thread_data));
        td->state=0;
        td->tid=i;
        threads[i]=td;
    }
    for(int i=0;i<nthreads;++i){
        pthread_create(&threads[i]->thread,NULL,threadpool_worker,(void*)threads[i]);
    }

}

void* threadpool_worker(void* _self){
    thread_data* self=(thread_data*)_self;

    while(1){
        task* next_task;
        int q_response_code=queue_deq(input_queue,(void**)&next_task);
        //we got the task, with the intial state !\ Started, so we do not know what to do with it
        if(next_task->state != TASK_STARTED){
            //of course a message instead of code will be better
            printf("%d got task with wrong state : %d , skipping it\n",self->tid,next_task->state);
            continue;
        }

        if(self->state == THREAD_TERMINATE){
            next_task->state=TASK_TERMINATED;
            // we are adding task to outpup queue, because synchronized method threadpool_submit_task
            // is waiting on the output_queue
            q_response_code=queue_enq(output_queues[OQ_INDEX(next_task->task_id)],next_task);
            return (void*)THREAD_TERMINATE;
        }
        if(q_response_code==QUEUE_FAULT){
            printf("something very bad happend  :( \n");
            return;
        }

        printf("%d:starting task:  %d \n",self->tid,next_task->task_id);

        next_task->output=next_task->func(next_task->input);
        next_task->state=TASK_FINISHED;

        printf("%d: finished task: %d \n",self->tid,next_task->task_id);

        q_response_code=queue_enq(output_queues[OQ_INDEX(next_task->task_id)],next_task);
        if(q_response_code==QUEUE_TERMINATE){
            return (void*)THREAD_TERMINATE;
        }
    }

}

int threadpool_submit_task(void*(*function)(void*),void* params,void** output){

    int task_id=asm_atomic_inc_int32_ret(&max_task_id);
    printf("task id:  %d\n",task_id);
    task* proc_task=(task*)malloc(sizeof(task));
    proc_task->func=function;
    proc_task->input=params;
    proc_task->task_id=task_id;
    proc_task->state=TASK_STARTED;

    printf("adding task in the queue:  %d\n",proc_task->task_id);
    int q_response_code=queue_enq(input_queue,proc_task);
    printf("queue status: %d\n ",q_response_code);
    if(q_response_code==QUEUE_TERMINATE){
        printf("WARNING: queue is trying to finish its work\n");
        return TASK_TERMINATED;
    }

    while(1){
        void* out_data=NULL;
        q_response_code = queue_top(output_queues[OQ_INDEX(task_id)],(void**)&out_data);
        if(q_response_code==QUEUE_TERMINATE){
            printf("WARNING: queue is trying to finish its work\n");
            return TASK_TERMINATED;
        }
//        printf("checking top task %d  %d\n ",task_id,((task*)out_data)->task_id );
        //if on the top not our task, we are skipping it
        while(out_data==NULL){
            q_response_code = queue_top(output_queues[OQ_INDEX(task_id)],(void**)&out_data);
        }
        if(((task*)out_data)->task_id!=task_id){ continue; }

        printf("%d %d \n",task_id,((task*)out_data)->task_id);
        q_response_code=queue_deq(output_queues[OQ_INDEX(task_id)],(void**)&proc_task);
        if(q_response_code==QUEUE_TERMINATE){
            printf("WARNING: queue is trying to finish its work\n");
            return TASK_TERMINATED;
        }
        if(proc_task->state==TASK_TERMINATED){
            printf("got terminated state from queue to task: %d \n",proc_task->task_id);
            return TASK_TERMINATED;
        }
        *output=proc_task->output;
        return TASK_FINISHED;
    }

    return TASK_FINISHED;

}

void threadpool_free(){
    // generally queue will not provide method  interrupt
    // so we added Thread states
    for(int i=0;i<nthreads;++i){
        threads[i]->state=THREAD_TERMINATE;
    }
    queue_interrupt_all(input_queue);
    for(int i=0;i<MAX_OUTPUT_QUEUES;++i){
        queue_interrupt_all(output_queues[i]);
    }
}

void* print_hello(void* arg){

    printf("HELLO! \n");
    for(int i=0;i<1000;++i){}
    return (void*)1;
}

void* process(void* args){

    for(int i=0;i<1000;++i){
        int value=0;
        int* i_addr=&i;
        threadpool_submit_task(print_hello,NULL,(void**)&value);
    }
}


int main(int argc, char *argv[]){

    threadpool_init(1);
    int thread_count=1;
    pthread_t threads[thread_count];
    for(int i=0;i<thread_count;++i){
        pthread_create(&threads[i],NULL,process,NULL);
    }
    for(int i=0;i<thread_count;++i){
        pthread_join(threads[i],NULL);
    }

    free(input_queue);
    for(int i=0;i<MAX_OUTPUT_QUEUES;++i){
        free(output_queues[i]);
    }
    free(output_queues);

    return 0;
}
