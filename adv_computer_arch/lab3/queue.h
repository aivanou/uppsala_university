#ifndef QUEUE_H
#define QUEUE_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define QUEUE_SUCCESS 1
#define QUEUE_FAULT 2
#define QUEUE_TERMINATE 3


typedef struct node{
    void* data;
    struct node* next;
} node;


typedef struct {
    node* head;
    node* tail;
    pthread_mutex_t locker;
    pthread_cond_t empty_cond;
    pthread_cond_t full_cond;
    unsigned int size;
    unsigned int capacity;
    unsigned int state;

}queue;

queue* queue_init(int capacity);

int queue_top(queue* q,void** ret_data);
int queue_enq(queue* q, void* data);
int queue_deq(queue* q, void** ret_data);
void queue_interrupt_all(queue* q);
int try_queue_deq(queue* q, void** ret_data);


static inline void* __attribute__((always_inline))
asm_cas(void **var, void* old, void* new) {
    int32_t ret,*ivar,iold,inew;
    ivar=(int32_t*)(intptr_t)var;
    iold=(int32_t)(intptr_t)old;
    inew=(int32_t)(intptr_t)new;
    /* cmpxchg uses eax as an implicit operand */
    __asm__ ("lock cmpxchgl %3, %0;"
         : "+m" (*ivar), "=a" (ret)
         : "a" (iold), "r" (inew));

    return (void*)(intptr_t)ret;
}


#endif // QUEUE_H
