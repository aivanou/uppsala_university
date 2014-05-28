/**
 * Utility functions for lab4.
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: util.h 1009 2011-07-28 15:02:57Z ansan501 $
 */

#ifndef UTIL_H
#define UTIL_H

#include <stdlib.h>
#include <time.h>

#include <emmintrin.h>

/**
 * Utility function to get the time since some arbitrary point using a
 * monotonic clock. Prints an error message and calls abort(3) on
 * error.
 *
 * @param ts pointer to a timespec structure that where the results
 * are returned
 */
void util_monotonic_time(struct timespec *ts);

/**
 * Calculate the time difference between two timespec structures and
 * returns the results in seconds. Assumes that ts_start happens
 * before ts_stop.
 *
 * @param ts_start start time
 * @param ts_stop stop time
 * @return Time difference in seconds
 */
double util_time_diff(struct timespec *ts_start, struct timespec *ts_stop);

/**
 * Print the contents of a vector of characters.
 *
 * @param v Vector to print
 */
void print_vector_epi8(__m128i v);

/**
 * Print the contents of a vector of doubles
 *
 * @param v Vector to print
 */
void print_vector_pd(__m128d v);

/**
 * Print the contents of a vector of floats
 *
 * @param v Vector to print
 */
void print_vector_ps(__m128 v);

#endif
