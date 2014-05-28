/**
 * Cache simulation using a functional system simulator.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: avdark-cache.h 13 2011-08-24 09:50:25Z ansan501 $
 */

#ifndef AVDARK_CACHE_H
#define AVDARK_CACHE_H

#include <stdint.h>

/** Physical address representation within the cache model */
typedef uint64_t avdc_pa_t;

typedef unsigned avdc_size_t;
typedef unsigned avdc_block_size_t;
typedef unsigned avdc_assoc_t;
typedef avdc_pa_t avdc_tag_t;

/**
 * Memory access type to simulate.
 */
typedef enum {
        AVDC_READ = 0,  /** Single read access */
        AVDC_WRITE,     /** Single write access */
} avdc_access_type_t;

/**
 * Forward declaration of the avdc_cache_line struct which is declared
 * in avdark-cache.c. The forward declaration is needed for the
 * simulator configuration struct.
 */
typedef struct avdc_cache_line avdc_cache_line_t;

typedef struct avdc_cache_element avdc_cache_element_t;

/**
 * Cache simulator instance variables
 */
typedef struct {
        /**
         * Debug printing enabled?
         */
        int                dbg;
        /**
         * Name to print to prepend to debug printouts, may be NULL
         */
        const char        *dbg_name;

        /**
         * A pointer to an array of cache blocks. You will need to
         * change this structure, the strucutre declaration is in
         * avdark-cache.c. This pointer is initialized by
         * avdc_resize().
         *
         * HINT: You may need to change how the internal cache state
         * is stored
         */
        avdc_cache_line_t *lines;

        /**
         * Cache parameters. Use avdc_resize() update them.
         *
         * @{
         */
        avdc_size_t        size;
        avdc_block_size_t  block_size;
        avdc_assoc_t       assoc;
        /** @} */

        /**
         * Cached internal data. These values are computed by
         * avdc_resize() and used to speedup cache lookups.
         *
         * @{
         */
        int                tag_shift;
        int                block_size_log2;
        int                number_of_sets;
        /** @} */

        /**
         * Statistics. These values are used by the test cases and the
         * simulator glue code. Do not change these unless you know
         * what you are doing.
         *
         * @{
         */
        int                stat_data_write;
        int                stat_data_write_miss;
        int                stat_data_read;
        int                stat_data_read_miss;
        /** @} */
} avdark_cache_t;

/**
 * Create a new instance of the cache simulator
 *
 * @param size Cache size in bytes
 * @param block_size Cache block size in bytes
 * @param assoc Cache associativiy
 */
avdark_cache_t *avdc_new(avdc_size_t size, avdc_block_size_t block_size,
			 avdc_assoc_t assoc);
/**
 * Destroy an instance of the cache simulator.
 *
 * @param self Simulator instance
 */
void avdc_delete(avdark_cache_t *self);

/**
 * Resize the cache.
 *
 * Initializes the cache using the new cache size parameters. This
 * function also precomputes a set of values used in various functions
 * used by the cache simulator.
 *
 * @param self Simulator instance
 * @param size Cache size in bytes
 * @param block_size Cache block size in bytes
 * @param assoc Cache associativiy
 * @return 0 on error, 1 on success
 */
int avdc_resize(avdark_cache_t *self, avdc_size_t size,
		avdc_block_size_t block_size, avdc_assoc_t assoc);

/**
 * Debug printing. This function works just like printf but the first
 * argument must be the avdark_cache_t structure. This function only
 * prints information if debug printing is enabled, this can be
 * controled with the dbg-enable and dbg-disable commands (from
 * python)
 *
 * @param self Simulator instance
 * @param msg printf formated string
 */
void avdc_dbg_log(avdark_cache_t *self, const char *msg, ...)
    __attribute__((format (printf, 2, 3)));

/**
 * Simulate a full cache flush
 *
 * @param self Simulator instance
 */
void avdc_flush_cache(avdark_cache_t *self);

/**
 * Execute a cache line access
 *
 * @param self Simulator instance
 * @param pa Physical address to access
 * @param type Access type
 */
void avdc_access(avdark_cache_t *self, avdc_pa_t pa, avdc_access_type_t type);

/**
 * Reset cache statistics
 *
 * @param self Simulator instance
 */
void avdc_reset_statistics(avdark_cache_t *self);

/**
 * Print information about the cache, e.g. size and other parameters.
 *
 * @param self Simulator instance
 */
void avdc_print_info(avdark_cache_t *self);

/**
 * Dump the internal state of the cache simulator, useful for
 * debuging.
 *
 * @param self Simulator instance
 */
void avdc_print_internals(avdark_cache_t *self);

#endif

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * compile-command: "make -k -C ../../"
 * End:
 */
