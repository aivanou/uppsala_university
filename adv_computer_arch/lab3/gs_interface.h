/**
 * Declarations of the interface used to execute an implementation of
 * the Gauss Seidel algorithm
 *
 * The gs_* symbols are exported TO the implementation and are runtime
 * parameters or helper functions.
 *
 * The gsi_* symbols must be exported BY the GS implementation.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 3
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: gs_interface.h 1009 2011-07-28 15:02:57Z ansan501 $
 */

#ifndef GS_INTERFACE_H
#define GS_INTERFACE_H

/** 1 if command line options for parallel execution should be enabled. */
extern const int gsi_is_parallel;

/**
 * Initialize local data used by the GS implementation.
 */
extern void gsi_init();
/**
 * Cleanup local data used by the GS implementation.
 */
extern void gsi_finish();

/**
 * Execute the implementation of the GS algorithm.
 */
extern void gsi_calculate();


/** 1 if verbose output is enabled */
extern int gs_verbose;

/** size of matrix (excluding padding) */
extern int gs_size;
/** width of matrix (size + padding) */
extern int gs_width;
/** number of iterations to run */
extern int gs_iterations;
/** maximum error for convergence */
extern double gs_tolerance;
/** number elements of padding on the sides of the matrix */
extern int gs_padding;
/** number of threads to use */
extern int gs_nthreads;

/** pointer to the matrix to run GS on */
extern double *gs_matrix;

/**
 * Calculate the index of an element in the matrix based on the row
 * and column
 */
#define GS_INDEX(row, col) (gs_width * (row) + (col))

/**
 * Print verbose output.
 */
extern void gs_verbose_printf(const char *fmt, ...);

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#endif
