/**
 * Cache simulator test case - Aliasing
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: test1.c 13 2011-08-24 09:50:25Z ansan501 $
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

static void
test_aliasing(avdark_cache_t *cache, avdc_pa_t alias_offset, avdc_access_type_t type)
{
        int i;
        int hits = 0;
        int misses = 0;

        avdc_reset_statistics(cache);
        STAT_ASSERT(cache, 0, 0, 0, 0);

        /* Access all ways in the first set, we should get 1 miss per way */
        for (i = 0; i < cache->assoc; i++) {
                avdc_access(cache, alias_offset * i, type);
                misses++;
                TEST_SIMPLE_STAT();
        }

        /* Now, access all the ways again, we shouldn't get any misses */
        for (i = 0; i < cache->assoc; i++) {
                avdc_access(cache, alias_offset * i, type);
                hits++;
                TEST_SIMPLE_STAT();
        }

        /* Access 1 cache line that aliases into set 0 and replaces
         * a line that we just loaded */
        avdc_access(cache, alias_offset * cache->assoc, type);
        misses++;
        TEST_SIMPLE_STAT();

        /* Access all lines except for the first one we touched (which
         * has been replaced by the LRU algorithm). We should only get
         * hits here. */
        for (i = 1; i < cache->assoc; i++) {
                avdc_access(cache, alias_offset * i, type);
                hits++;
                TEST_SIMPLE_STAT();
        }

        /* Now, access the first line again, this should be a miss */
        avdc_access(cache, 0, type);
        misses++;
        TEST_SIMPLE_STAT();
}

int
main(int argc, char *argv[])
{
        avdark_cache_t *cache;

        cache = avdc_new(512, 64, 1);
        assert(cache);
        avdc_print_info(cache);

        printf("Aliasing [read]\n");
        test_aliasing(cache, 512, AVDC_READ);
        printf("Aliasing [write]\n");
        avdc_flush_cache(cache);
        test_aliasing(cache, 512, AVDC_WRITE);


        avdc_resize(cache, 512, 128, 1);
        avdc_print_info(cache);

        printf("Aliasing [read]\n");
        test_aliasing(cache, 512, AVDC_READ);
        printf("Aliasing [write]\n");
        avdc_flush_cache(cache);
        test_aliasing(cache, 512, AVDC_WRITE);


        avdc_resize(cache, 256, 64, 1);
        avdc_print_info(cache);

        printf("Aliasing [read]\n");
        test_aliasing(cache, 256, AVDC_READ);
        printf("Aliasing [write]\n");
        avdc_flush_cache(cache);
        test_aliasing(cache, 256, AVDC_WRITE);


        printf("Switching to assoc 2, assuming LRU\n");

        avdc_resize(cache, 512, 64, 2);
        avdc_print_info(cache);

        printf("Aliasing [read]\n");
        test_aliasing(cache, 256, AVDC_READ);
        printf("Aliasing [write]\n");
        avdc_flush_cache(cache);
        test_aliasing(cache, 256, AVDC_WRITE);

 
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
