/**
 * Gauss-Seidel implementation using pthreads.
 *
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 3
 *
 * Original author: Frédéric Haziza <daz@it.uu.se>
 * Heavily modified by: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: gsi_pth.c 1182 2011-10-27 23:46:43Z ansan501 $
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "gs_interface.h"

/**
 * Tell the startup code that we want run in parallel mode.
 */
const int gsi_is_parallel = 1;

/**
 * Thread data structure passed to the thread entry function.
 */
typedef struct {
        int thread_id;
        pthread_t thread;


//        char padding1[64-sizeof(int)-sizeof(pthread_t)];

        volatile double error;
//        pthread_mutex_t padding_lock;
//        char padding2[64-sizeof(double)];

        volatile int finished_row;
        volatile int finished_iteration;

} thread_info_t;

pthread_barrier_t barr;

/** Define to enable debug mode */
#define DEBUG 0 /* 1 */

/** Debug output macro. Only active when DEBUG is non-0 */
#define dprintf(...)                            \
        if (DEBUG)                              \
                fprintf(stderr, __VA_ARGS__)

/** Vector with information about all active threads */
thread_info_t *threads = NULL;

/** The global error for the last iteration */
static double global_error;



void
gsi_init()
{
        gs_verbose_printf("\t****  Initializing the  environment ****\n");

        threads = (thread_info_t *)malloc(gs_nthreads * sizeof(thread_info_t));
        if (!threads) {
                fprintf(stderr,
                        "Failed to allocate memory for thread information.\n");
                exit(EXIT_FAILURE);
        }

        for(int i=0;i<gs_nthreads;++i){
            threads[i].finished_row=0;
            threads[i].finished_iteration=-1;
        }
        pthread_barrier_init(&barr,NULL,gs_nthreads);

        /* Initialize global_error to something larget than the
         * tolerance to get the algorithm started */
        global_error = gs_tolerance + 1;
}

void
gsi_finish()
{
        gs_verbose_printf("\t****  Cleaning environment ****\n");

        /* TASK: Be nice and cleanup the stuff you initialized in
         * gsi_init()
         */

        if (threads)
                free(threads);
        pthread_barrier_destroy(&barr);

}

static void
thread_sweep(int tid, int iter, int lbound, int rbound)
{
        threads[tid].error = 0.0;
        double error=0.0;
        for (int row = 1; row < gs_size - 1; row++) {
                dprintf("%d: checking wait condition\n"
                        "\titeration: %i, row: %i\n",
                        tid,
                        iter, row);

                /* TASK: Wait for data to be available from the thread
                 * to the left */

//                while(1){
//                        if(tid!=gs_nthreads-1 &&
//                           threads[tid].finished_iteration>threads[tid+1].finished_iteration) {continue;}
//                        if(tid==0) break;
//                        if(threads[tid-1].finished_iteration<threads[tid].finished_iteration)
//                        {continue;}
//                        else if(threads[tid-1].finished_iteration==threads[tid].finished_iteration){
//                            int r=threads[tid-1].finished_row;
//                            if(r>=row) break;
//                        }
//                        else if(threads[tid-1].finished_iteration>threads[tid].finished_iteration) break;
//                }

                while(tid!=0 && threads[tid-1].finished_row<=threads[tid].finished_row){}


                dprintf("%d: Starting on row: %d\n", tid, row);

                /* Update this thread's part of the matrix */
                for (int col = lbound; col < rbound; col++) {
                        double new_value = 0.25 * (
                                gs_matrix[GS_INDEX(row + 1, col)] +
                                gs_matrix[GS_INDEX(row - 1, col)] +
                                gs_matrix[GS_INDEX(row, col + 1)] +
                                gs_matrix[GS_INDEX(row, col - 1)]);
                        error +=
                                fabs(gs_matrix[GS_INDEX(row, col)] - new_value);
                        gs_matrix[GS_INDEX(row, col)] = new_value;
                }


                threads[tid].finished_row+=1;
//memfence
                dprintf("%d: row %d done\n", tid, row);
        }
        threads[tid].error=error;
//memfence
}

/**
 * Computing routine for each thread
 */
static void *
thread_compute(void *_self)
{
        thread_info_t *self = (thread_info_t *)_self;
        const int tid = self->thread_id;

        int lbound=0, rbound=0;

        /* TASK: Compute bounds for this thread */

        lbound=gs_size/gs_nthreads*tid+1;
        rbound=gs_size/gs_nthreads*(tid+1)+1;
        if(rbound>gs_size) rbound=gs_size-1;

        gs_verbose_printf("%i: lbound: %i, rbound: %i\n",
                          tid, lbound, rbound);

        for (int iter = 0;
             iter < gs_iterations && global_error > gs_tolerance;
             iter++) {

                dprintf("%i: Starting iteration %i\n", tid, iter);

                thread_sweep(tid, iter, lbound, rbound);

                /* TASK: Update global error */
                /* Note: The reduction should only be done by one
                 * thread after all threads have updated their local
                 * errors */
                /* Hint: Which thread is guaranteed to complete its
                 * sweep last? */

                if(tid == gs_nthreads-1) {
                    global_error=0.0;
                    //memfence
                    for(int i=0;i<gs_nthreads;++i){
                        global_error += threads[i].error;
                    }
                }

                dprintf("%d: iteration %d done\n", tid, iter);

                /* TASK: Iteration barrier */

//                if(tid!=0)
//                    threads[tid-1].finished_row=0;
                //memfence
                threads[tid].finished_iteration=iter;

                pthread_barrier_wait(&barr);


        }

        gs_verbose_printf(
                "\t****  Thread %d done after %d iterations ****\n",
                tid, gs_iterations);

        return NULL;
}

/**
 * Parallel implementation of the GS algorithm. Called from
 * gs_common.c to start the solver.
 */
void
gsi_calculate()
{
        int err;

        for (int t = 0; t < gs_nthreads; t++) {
                gs_verbose_printf("\tSpawning thread %d\n",t);
                threads[t].thread_id = t;
                err = pthread_create(&threads[t].thread, NULL,
                                     thread_compute, &threads[t]);
                if (err) {
                        fprintf(stderr,
                                "Error: pthread_create() failed: %d, "
                                "thread %d\n",
                                err, t);
                        exit(EXIT_FAILURE);
                }
        }
  
        /* Calling pthread_join on a thread will block until the
         * thread terminates. Since we are joining all threads, we
         * wait until all threads have exited. */
        for (int t = 0; t < gs_nthreads; t++) {
                err = pthread_join(threads[t].thread, NULL);
                if (err) {
                        fprintf(stderr,
                                "Error: pthread_join() failed: %d, "
                                "thread %d\n",
                                err, t);
                        exit(EXIT_FAILURE);
                }
        }

        if (global_error <= gs_tolerance)
                printf("Solution converged!\n");
        else {
                printf("Solution did NOT converge. %f  %f \n",global_error,gs_tolerance);
                printf("Note: This is normal if you are using the "
                       "default settings.\n");
        }

}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
