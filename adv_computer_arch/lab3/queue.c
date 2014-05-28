#include "queue.h"

queue* queue_init(int capacity){
    queue* new_queue=(queue*)malloc(sizeof(queue));
    new_queue->head=NULL;
    new_queue->tail=new_queue->head;

    pthread_mutex_init(&new_queue->locker,NULL);
    pthread_cond_init(&new_queue->empty_cond,NULL);
    pthread_cond_init(&new_queue->full_cond,NULL);

    new_queue->state=QUEUE_SUCCESS;
    new_queue->size=0;
    new_queue->capacity=capacity;
    return new_queue;
}


int queue_top(queue* q,void** ret_data){

    pthread_mutex_lock(&q->locker);

    if(q->state == QUEUE_TERMINATE){
        return QUEUE_TERMINATE;
    }

    while(q->size==0){
        pthread_cond_wait(&q->empty_cond,&q->locker);
        if(q->state == QUEUE_TERMINATE){
            pthread_mutex_unlock(&q->locker);
            return QUEUE_TERMINATE;
        }
    }
    *ret_data=q->head->data;
    pthread_mutex_unlock(&q->locker);


    return QUEUE_SUCCESS;
}

int queue_enq(queue* q, void* data){
    node* new_node=(node*)malloc(sizeof(node));
    new_node->data=data;

    pthread_mutex_lock(&q->locker);

    while(q->size==q->capacity){
        pthread_cond_wait(&q->full_cond,&q->locker);
        if(q->state == QUEUE_TERMINATE){
            pthread_mutex_unlock(&q->locker);
            return QUEUE_TERMINATE;
        }
    }

    if(q->head==NULL){
        q->head=new_node;
        q->tail=q->head;
        q->size+=1;


        pthread_mutex_unlock(&q->locker);
        pthread_cond_broadcast(&q->empty_cond);
        return QUEUE_SUCCESS;
    }

    q->tail->next=new_node;
    q->tail=new_node;
    q->size+=1;


    pthread_mutex_unlock(&q->locker);
    pthread_cond_broadcast(&q->empty_cond);

    return QUEUE_SUCCESS;
}

int queue_deq(queue* q, void** ret_data){

    pthread_mutex_lock(&q->locker);

    while(q->head==NULL || q->size==0){
        //TODO: if there are no elements, we should call pthread_cond_wait ().
        pthread_cond_wait(&q->empty_cond,&q->locker);
        if(q->state == QUEUE_TERMINATE){
            pthread_mutex_unlock(&q->locker);
            return QUEUE_TERMINATE;
        }
    }

    node* curr_element=q->head;
    q->head=q->head->next;
    q->size-=1;
    void* data=curr_element->data;
    *ret_data=data;

//    why cannot I free the memory??
//    if(curr_element!=NULL)
//        free(curr_element);

    pthread_mutex_unlock(&q->locker);
    pthread_cond_broadcast(&q->full_cond);

    return QUEUE_SUCCESS;
}

void queue_interrupt_all(queue* q){
    pthread_mutex_lock(&q->locker);
    q->state=QUEUE_TERMINATE;
    node* next_el=NULL;
    while(q->head!=NULL){
        next_el=q->head->next;
        free(q->head);
        q->head=next_el;
    }
    pthread_cond_broadcast(&q->empty_cond);
    pthread_mutex_unlock(&q->locker);
}

int try_queue_deq(queue* q, void** ret_data){}
