#ifndef THREADPOOL_H
#define THREADPOOL_H


#define THREAD_WORK 0
#define THREAD_FINISH 1
#define THREAD_TERMINATE 2

#define TASK_STARTED 0
#define TASK_FINISHED 1
#define TASK_TERMINATED 2

typedef void*(*thread_function)(void*);

typedef struct {
    int task_id;

    thread_function func;
    void* input;
    void* output;
    int state;

}task;

typedef struct {

    pthread_t thread;
    int tid;
    int state;

}thread_data;


void* threadpool_worker(void* self);

void thread_pool_init(int active_threads);

int threadpool_submit_task(void*(*function)(void*),void* params,void** output);

/**
 * Atomically increase a 32-bit variable by 1.
 *
 * \param var Pointer to variable to manipulate
 */
static inline void __attribute__((always_inline))
asm_atomic_inc_int32(int32_t *var)
{
    __asm__ ("lock incl %0;"
         : "+m" (*var));
}


/**
 * Atomically increase a 32-bit variable by 1.
 *
 * \param var Pointer to variable to manipulate
 */
static inline int32_t __attribute__((always_inline))
asm_atomic_inc_int32_ret(int32_t *var)
{
    __asm__ ("lock incl %0;"
         : "+m" (*var));
    return *var;
}

#endif // THREADPOOL_H
