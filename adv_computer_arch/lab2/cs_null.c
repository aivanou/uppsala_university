/**
 * Experimenting with synchronization and memory consistency. Stub
 * version of the critical sections. Warning: This implementation does
 * NOT, and should not, enforce mutual exclusion.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: cs_null.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#include "lab2.h"

/**
 * Enter a critical section. Stub version, does not enforce mutual
 * exclusion.
 *
 * \param thread Thread ID - ignored.
 */
static void
impl_enter_critical(int thread)
{
}

/**
 * Exit from a critical section. Stub version, does not enforce mutual
 * exclusion.
 *
 * \param thread Thread ID - ignored.
 */
static void
impl_exit_critical(int thread)
{
}

critical_section_impl_t cs_impl_null = {
        .name = "null",
        .desc = "NULL implementation that does not enforce mutual exclusion",

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
