/**
 * Declarations of common data structures for all test and critical
 * section implementations.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: lab2_asm.h 1166 2011-10-07 08:10:55Z ansan501 $
 */

#ifndef ATOMIC_H
#define ATOMIC_H

#include <stdint.h>

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
 * Increase a 32-bit variable by 1 without requesting atomicity.
 *
 * \param var Pointer to variable to manipulate
 */
static inline void __attribute__((always_inline))
asm_inc_int32(int32_t *var)
{
    __asm__ ("incl %0;"
	     : "+m" (*var));
}

/**
 * Atomically decrease a 32-bit variable by 1.
 *
 * \param var Pointer to variable to manipulate
 */
static inline void __attribute__((always_inline))
asm_atomic_dec_int32(int32_t *var)
{
    __asm__ ("lock decl %0;"
	     : "+m" (*var));
}

/**
 * Decrease a 32-bit variable by 1 without requesting atomicity.
 *
 * \param var Pointer to variable to manipulate
 */
static inline void __attribute__((always_inline))
asm_dec_int32(int32_t *var)
{
    __asm__ ("decl %0;"
	     : "+m" (*var));
}

/**
 * Compare a register (old) to a value in memory (var) and replace it
 * with a new value (new) if the comparison is successful. The return
 * value is the value of var before the exchange executes, i.e. old on
 * success and the value of var otherwise.
 *
 * Pseudo code:
 *   asm_atomic_cmpxchg(*mem, old, new):
 *     load mem -> tmp
 *     if tmp == old:
 *       store new -> mem
 *     return tmp
 *
 * \param var Variable in memory to manipulate
 * \param old Value to use in comparison
 * \param new New value to insert if var == old
 * \return Value of old on success and var otherwise.
 */
static inline int32_t __attribute__((always_inline))
asm_atomic_cmpxchg_int32(int32_t *var, int32_t old, int32_t new) {
    int32_t ret;
    /* cmpxchg uses eax as an implicit operand */
    __asm__ ("lock cmpxchgl %3, %0;"
	     : "+m" (*var), "=a" (ret)
         : "a" (old), "r" (new));

    return ret;
}

/**
 * Compare a register (old) to a value in memory (var) and replace it
 * with a new value (new) if the comparison is successful. The return
 * value is the value of var before the exchange executes, i.e. old on
 * success and the value of var otherwise.
 *
 * Pseudo code:
 *   asm_cmpxchg(*mem, old, new):
 *     load mem -> tmp
 *     if tmp == old:
 *       store new -> mem
 *     return tmp
 *
 * \param var Variable in memory to manipulate
 * \param old Value to use in comparison
 * \param new New value to insert if var == old
 * \return Value of old on success and var otherwise.
 */
static inline int32_t __attribute__((always_inline))
asm_cmpxchg_int32(int32_t *var, int32_t old, int32_t new)
{
    int32_t ret;
    /* cmpxchg uses eax as an implicit operand */
    __asm__ ("cmpxchgl %3, %0;"
	     : "+m" (*var), "=a" (ret)
	     : "a" (old), "r" (new));

    return ret;
}

/**
 * Atomically exchange the value in the memory location a with the
 * contents of b and return the old value of a.
 *
 * The compiler will force the contents of b into a register and use
 * this register and the memory operand a for the xchg operation. The
 * new value of the register is then returned by the function.
 *
 * \param a Memory location to operate on
 * \param b Content to load into the register to operate on
 * \return New value of register operand (the old contents of *a)
 */
static inline void *__attribute__((always_inline))
asm_atomic_xchg_voidp(void **a, void *b)
{
    __asm__ ("lock xchg %0, %1;"
	     : "+m" (*a), "+r" (b));

    return b;
}

#endif
