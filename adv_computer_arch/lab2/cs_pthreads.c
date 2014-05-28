/**
 * Experimenting with synchronization and memory consistency. Pthreads
 * version of critical sections.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: cs_pthreads.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "lab2.h"

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

/**
 * Enter a critical section. Pthreads version, used as a reference.
 *
 * Do not change this function.
 *
 * \param thread Thread ID - ignored.
 */
static void
impl_enter_critical(int thread)
{
        if (pthread_mutex_lock(&mutex) != 0) {
                perror("pthread_mutex_lock failed");
                abort();
        }
}

/**
 * Exit from a critical section. Pthreads version, used as a
 * reference.
 *
 * Do not change this function.
 *
 * \param thread Thread ID - ignored.
 */
static void
impl_exit_critical(int thread)
{
        if (pthread_mutex_unlock(&mutex) != 0) {
                perror("pthread_mutex_lock failed");
                abort();
        }
}

critical_section_impl_t cs_impl_pthreads = {
        .name = "pthreads",
        .desc = "Pthread mutexes",

        .max_threads = (unsigned int)-1,

        .enter = &impl_enter_critical,
        .exit = &impl_exit_critical
};

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
