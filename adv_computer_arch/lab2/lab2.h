/**
 * Declarations of common data structures for all test and critical
 * section implementations.
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: lab2.h 1009 2011-07-28 15:02:57Z ansan501 $
 */

#ifndef LAB2_H
#define LAB2_H

/** Pointer to currently configured procedure to enter a critical section */
void (* enter_critical)(int thread);
/** Pointer to currently configured procedure to exit a critical section */
void (* exit_critical)(int thread);

typedef struct {
        const char *name;
        const char *desc;
        unsigned int max_threads;

        void (* init)(int num_threads);
        void (* fini)();

        void (* enter)(int thread);
        void (* exit)(int thread);
} critical_section_impl_t;

extern critical_section_impl_t cs_impl_pthreads;
extern critical_section_impl_t cs_impl_null;
extern critical_section_impl_t cs_impl_dekker;
extern critical_section_impl_t cs_impl_queue;

typedef void (* test_func_ptr_t)(int thread, int iterations, volatile int *data);


typedef struct {
        const char *name;
        const char *desc;
        int num_threads;

        test_func_ptr_t test[];
} test_impl_t;

extern test_impl_t test_impl_critical;
extern test_impl_t test_impl_critical4;
extern test_impl_t test_impl_critical8;
extern test_impl_t test_impl_incdec_atomic;
extern test_impl_t test_impl_incdec_no_atomic;
extern test_impl_t test_impl_cmpxchg_atomic;
extern test_impl_t test_impl_cmpxchg_no_atomic;

#endif

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
