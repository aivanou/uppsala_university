/**
 * Implementation of test cases using atomic and non-atomic increments
 * and decrements.
 *
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: test_incdec.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#include "lab2.h"

#include "lab2_asm.h"

static void
increase(int thread, int iterations, volatile int *data) {
    /* TASK: Implement a loop that increments *data by 1 using
     * non-atomic increment instructions. See lab2_asm.h.
     */
    for (int i = 0; i < iterations; ++i)
        asm_inc_int32((int32_t*) data);
}

static void
decrease(int thread, int iterations, volatile int *data) {
    /* TASK: Implement a loop that decrements *data by 1 using
     * non-atomic decrement instructions. See lab2_asm.h.
     */
    for (int i = 0; i < iterations; ++i)
        asm_dec_int32((int32_t*) data);
}

static void
increase_atomic(int thread, int iterations, volatile int *data) {
    /* TASK: Implement a loop that increments *data by 1 using
     * atomic increment instructions. See lab2_asm.h.
     */
    for (int i = 0; i < iterations; ++i) {
        asm_atomic_inc_int32((int32_t*) data);
    }
}

static void
decrease_atomic(int thread, int iterations, volatile int *data) {
    /* TASK: Implement a loop that decrements *data by 1 using
     * atomic decrement instructions. See lab2_asm.h.
     */
    for (int i = 0; i < iterations; ++i)
        asm_atomic_dec_int32((int32_t*) data);
}

test_impl_t test_impl_incdec_atomic = {
    .name = "incdec_no_atomic",
    .desc = "Modify a shared variable using inc/dec instructions",

    .num_threads = 2,
    .test =
    { &increase, &decrease}
};

test_impl_t test_impl_incdec_no_atomic = {
    .name = "incdec_atomic",
    .desc = "Modify a shared variable using atomic inc/dec instructions",

    .num_threads = 2,
    .test =
    { &increase_atomic, &decrease_atomic}
};

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */

