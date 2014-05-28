#include "queue.h"

#include <pthread.h>

pthread_mutex_t locker=PTHREAD_MUTEX_INITIALIZER;

queue* queue_init(int capacity){
    queue* new_queue=(queue*)malloc(sizeof(queue));

    node* dummy_node=(node*)malloc(sizeof(node));
    dummy_node->next=NULL;
    new_queue->head=dummy_node;
    new_queue->tail=dummy_node;

    pthread_mutex_init(&new_queue->locker,NULL);
    pthread_cond_init(&new_queue->empty_cond,NULL);
    pthread_cond_init(&new_queue->full_cond,NULL);

    new_queue->state=QUEUE_SUCCESS;
    new_queue->size=0;
    new_queue->capacity=capacity;
    return new_queue;
}


int queue_top(queue* q,void** ret_data){
    while(q->head==NULL){}
    *ret_data=q->head;
    return q->head;
}

int queue_enq(queue* q, void* data){

    node* new_node=(node*)malloc(sizeof(node));
    new_node->data=data;
    new_node->next=NULL;

    node* tail=NULL;
    node* next=NULL;
    while(1){
        tail=q->tail;
        next=tail->next;
        if(tail==q->tail){
            if(next==NULL){
                if(__sync_bool_compare_and_swap((void**)&tail->next,(void*)next,(void*)new_node)){
                    break;
                }
            }
            else{
                __sync_bool_compare_and_swap((void**)q->tail,(void*)tail,(void*)next);
            }
        }
    }
    __sync_bool_compare_and_swap((void**)&q->tail,(void*)tail,(void*)new_node);
    return QUEUE_SUCCESS;

}

int queue_deq(queue* q, void** ret_data){
    while(try_queue_deq(q,ret_data)==-1){}
}

int try_queue_deq(queue* q, void** ret_data){
    node* head;
    node* tail;
    node* next;
    while(1){
//        printf("lalala\n");
        head=q->head;
        tail=q->tail;
        next=q->head->next;
        if(head==q->head){
            if(head==tail){
                if(next==NULL){
                    return -1;
                }
                __sync_bool_compare_and_swap((void**)&q->tail,tail,next);
            }
            else{
                if(next==NULL) continue;
                *ret_data=next->data;
                if(__sync_bool_compare_and_swap((void**)&q->head,(void*)head,(void*)head->next)){
                    break;
                }
            }
        }
    }
    free(head);
    return QUEUE_SUCCESS;
}

void queue_interrupt_all(queue* q){}
