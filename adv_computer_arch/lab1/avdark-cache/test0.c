/**
 * Cache simulator test case - Direct mapped caches
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: test0.c 13 2011-08-24 09:50:25Z ansan501 $
 */

#include "avdark-cache.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define STAT_ASSERT(c, r, rm, w, wm) do {                       \
                assert(c->stat_data_read == (r));               \
                assert(c->stat_data_read_miss == (rm));         \
                assert(c->stat_data_write == (w));              \
                assert(c->stat_data_write_miss == (wm));        \
        } while (0)

#define TEST_SIMPLE_STAT() \
        if (type == AVDC_READ)                                  \
                STAT_ASSERT(cache, hits+misses, misses, 0, 0);   \
        else if (type == AVDC_WRITE)                            \
                STAT_ASSERT(cache, 0, 0, hits+misses, misses);  \
        else                                                    \
                abort()

/* Test simple accesses to two consecutive cache lines */
static void
test_simple(avdark_cache_t *cache, avdc_access_type_t type)
{
        int i;
        int hits = 0;
        int misses = 0;

        avdc_reset_statistics(cache);
        STAT_ASSERT(cache, 0, 0, 0, 0);

        /* Access the first cache line, we expect a read miss after this */
        avdc_access(cache, 0, type);
        misses++;
        TEST_SIMPLE_STAT();

        for (i = 0; i < cache->block_size; i++) {
                /* Read every byte of the first cache line, expect hits */
                avdc_access(cache, i, type);
                hits++;
                TEST_SIMPLE_STAT();
        }

        /** Access the second cache line */
        avdc_access(cache, cache->block_size, type);
        misses++;
        TEST_SIMPLE_STAT();

        for (i = 0; i < cache->block_size; i++) {
                /* Read every byte of the 2nd cache line, expect hits */
                avdc_access(cache, cache->block_size + i, type);
                hits++;
                TEST_SIMPLE_STAT();
        }
}

int
main(int argc, char *argv[])
{
        avdark_cache_t *cache;

        cache = avdc_new(512, 64, 1);
        assert(cache);

        assert(cache->stat_data_write == 0 &&
               cache->stat_data_write_miss == 0 &&
               cache->stat_data_read == 0 &&
               cache->stat_data_read_miss == 0);

        assert(cache->size == 512 &&
               cache->block_size == 64 &&
               cache->assoc == 1);

        assert(cache->tag_shift == 9);
        assert(cache->block_size_log2 == 6);
        assert(cache->number_of_sets == 8);

        avdc_print_info(cache);

        printf("Simple [read]\n");
        /* Don't flush the cache here, the cache should be clean for
         * the first test */
        test_simple(cache, AVDC_READ);

        printf("Simple [write]\n");
        avdc_flush_cache(cache);
        test_simple(cache, AVDC_WRITE);
 
        avdc_delete(cache);

        printf("%s done.\n", argv[0]);
        return 0;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * compile-command: "make -k -C ../../"
 * End:
 */
