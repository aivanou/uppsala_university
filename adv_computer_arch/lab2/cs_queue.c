/**
 * Experimenting with synchronization and memory consistency. Queue
 * locks version of critical sections.
 *
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: cs_queue.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

/* Enable padding of data structures. Setting the correct padding
 * significantly improves performance of the lock. Disable this define
 * to disable padding.
 */
#define PAD_SIZE 4096

/* Define the following to disable checking of asserts. Improves
 * performanc   e slightly.
 */
/* #define NDEBUG */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "lab2.h"
#include "lab2_asm.h"

typedef struct {
    int *i;
    int *p;
#ifdef PAD_SIZE
    char pad1[PAD_SIZE - 2 * sizeof (int *)];
#endif
} lh_thread_t;

typedef struct {
    int v;
#ifdef PAD_SIZE
    char pad[PAD_SIZE - 1 * sizeof (int)];
#endif
} lh_cell_t;

#ifdef PAD_SIZE
#define COND_ALIGN __attribute__((aligned (PAD_SIZE)))
#else
#define COND_ALIGN
#endif

static lh_thread_t *lh_threads COND_ALIGN;
static lh_cell_t *lock_cells COND_ALIGN;
static int *lock_l COND_ALIGN;
static int lock_cell0 COND_ALIGN;

static int num_threads;

/**
 * Acquire a lock using the CLH locking algorithm. See the lecture
 * notes for details.
 */
static void
lh_acquire(int * * volatile l, int * * volatile i, volatile int * * volatile p) {
    assert(**i == 0);
    assert(*i == *p);
    /* BONUS TASK: Implement the acquire part of the CLH locking
     * algorithm as described in the lecture notes. */
    **i = 1;
    *p = asm_atomic_xchg_voidp((void**)l, (void*) *p);
//    *l = asm_atomic_xchg_voidp((void**)p, (void*) *l);
    while (**p != 0) {
    }

}

/**
 * Release a lock using the CLH locking algorithm. See the lecture
 * notes for details.
 */
static void
lh_release(int **i, int **p) {
    assert(**i != 0);
    assert(*i != *p);
    /* BONUS TASK: Implement the release part of the CLH locking
     * algorithm as described in the lecture notes. */
    **i = 0;
    *i = *p;
}

static void
impl_init(int _num_threads) {
    num_threads = _num_threads;

    lh_threads = malloc(sizeof (*lh_threads) * num_threads);
    lock_cells = malloc(sizeof (*lock_cells) * num_threads);

    assert(lh_threads && lock_cells);

    lock_l = &lock_cell0;
    for (int i = 0; i < num_threads; i++) {
        lh_threads[i].i = &lock_cells[i].v;
        lh_threads[i].p = lh_threads[i].i;
    }
}

static void
impl_fini() {
    free(lh_threads);
    free(lock_cells);
}

static void
impl_enter_critical(int thread) {
    assert(thread >= 0 && thread < num_threads);

    lh_acquire((int * * volatile) & lock_l,
            (int * * volatile) & lh_threads[thread].i,
            (volatile int **) &lh_threads[thread].p);
}

static void
impl_exit_critical(int thread) {
    assert(thread >= 0 && thread < num_threads);

    lh_release(&lh_threads[thread].i, &lh_threads[thread].p);
}

critical_section_impl_t cs_impl_queue = {
    .name = "queue",
    .desc = "CLH Queue Locks",

    .max_threads = (unsigned int) - 1,

    .init = &impl_init,
    .fini = &impl_fini,

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
