/**
 * Cache simulation using a functional system simulator.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Original authors: UART 1.0(?)
 * Modified by: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: avdark-cache.c 14 2011-08-24 09:55:20Z ansan501 $
 */

#include "avdark-cache.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <rpc/types.h>

#ifdef SIMICS
/* Simics stuff  */
#include <simics/api.h>
#include <simics/alloc.h>
#include <simics/utils.h>

#define AVDC_MALLOC(nelems, type) MM_MALLOC(nelems, type)
#define AVDC_FREE(p) MM_FREE(p)

#else

#define AVDC_MALLOC(nelems, type) malloc(nelems * sizeof(type))
#define AVDC_FREE(p) free(p)

#endif

struct avdc_cache_element {
    avdc_tag_t tag;
    int valid;
    char age;
};

/**
 * Cache block information.
 *
 * HINT: You will probably need to change this structure
 */
struct avdc_cache_line {
    struct avdc_cache_element* block;
};

/**
 * Extract the cache line tag from a physical address.
 *
 * You probably don't want to change this function, instead you may
 * want to change how the tag_shift field is calculated in
 * avdc_resize().
 */
static inline avdc_pa_t tag_from_pa(avdark_cache_t *self, avdc_pa_t pa) {
    return pa >> self->tag_shift;
}

/**
 * Calculate the cache line index from a physical address.
 *
 * Feel free to experiment and change this function
 */
static inline int index_from_pa(avdark_cache_t *self, avdc_pa_t pa) {
    return (pa >> self->block_size_log2) & (self->number_of_sets - 1);
}

/**
 * Computes the log2 of a 32 bit integer value. Used in dc_init
 *
 * Do NOT modify!
 */
static int log2_int32(uint32_t value) {
    int i;

    for (i = 0; i < 32; i++) {
        value >>= 1;
        if (value == 0)
            break;
    }
    return i;
}

/**
 * Check if a number is a power of 2. Used for cache parameter sanity
 * checks.
 *
 * Do NOT modify!
 */
static int is_power_of_two(uint64_t val) {
    return ((((val) & (val - 1)) == 0) && (val > 0));
}

void avdc_dbg_log(avdark_cache_t *self, const char *msg, ...) {
    va_list ap;

    if (self->dbg) {
        const char *name = self->dbg_name ? self->dbg_name : "AVDC";
        fprintf(stderr, "[%s] dbg: ", name);
        va_start(ap, msg);
        vfprintf(stderr, msg, ap);
        va_end(ap);
    }
}

int avdc_lru(avdark_cache_t *self, int index, avdc_tag_t tag) {
    int hit_index = 0;
    int hit=0;

    for (int i = 0; i < self->assoc; ++i) {
        hit = self->lines[index].block[i].valid
                && self->lines[index].block[i].tag == tag;
        if (hit) {
            hit_index = i;
            break;
        }
    }
    if (!hit) {
        int min_age = self->assoc + 1;
        for (int i = 0; i < self->assoc; ++i) {
            if (self->lines[index].block[i].age < min_age) {
                min_age = self->lines[index].block[i].age;
                hit_index = i;
            }
        }
        self->lines[index].block[hit_index].valid = 1;
        self->lines[index].block[hit_index].tag = tag;
    }
    for (int i = 0; i < self->assoc; ++i) {
        if (self->lines[index].block[i].age != 0)
            self->lines[index].block[i].age -= 1;
    }
    self->lines[index].block[hit_index].age = self->assoc;
    return hit;
}

void avdc_random(avdark_cache_t *self, int hit, int hit_intex, int index, avdc_tag_t tag) {
    if (hit) {
        return;
    }
    for (int i = 0; i < self->assoc; ++i) {
        if (self->lines[index].block[i].age == 0) {
            self->lines[index].block[i].age = 1;
            self->lines[index].block[i].valid = 1;
            self->lines[index].block[i].tag = tag;
            return;
        }
    }
    int ch_index = rand() / (double) RAND_MAX * self->assoc;
    self->lines[index].block[ch_index].valid = 1;
    self->lines[index].block[ch_index].tag = tag;
}

int* avdc_sk_indexes(avdc_tag_t tag, int index, int lines, int assoc) {
    int* idxs = (int*) malloc(assoc * sizeof (int));

    return idxs;
}
        
unsigned int hash_1(unsigned int parts[], int prime, int cache_size) {
//        return ((parts[0]^parts[3])&(parts[2]^parts[1]))%cache_size;
    return (parts[0]^(parts[1] & prime)) % cache_size;
}

unsigned int hash_2(unsigned long parts[], int prime, int cache_size) {
    //    return (((parts[0] >> 2)^parts[3])&(parts[2]^(parts[1] >> 2)))%cache_size;
    return (parts[1]^((parts[0] >> 2) & prime)) % cache_size;
}

void avdc_skewed_replace(avdark_cache_t *self, avdc_tag_t tag, int valid, char age, int l_index, int b_index) {
    self->lines[l_index].block[b_index].age = age;
    self->lines[l_index].block[b_index].tag = tag;
    self->lines[l_index].block[b_index].valid = valid;
}

int avdc_skewed_cache(avdark_cache_t *self, avdc_pa_t pa) {
    //handles only 2-way associative skewed cache
    unsigned long parts[4], prime = 8191;
    parts[0] = (pa & 0xFFFF);
    parts[1] = ((pa >> 16)&0xFFFF);
    parts[2] = ((pa >> 32)&0xFFFF);
    parts[3] = ((pa >> 48)&0xFFFF);


    avdc_tag_t tag = tag_from_pa(self, pa);

    int f_index = hash_1(parts, prime, self->size / (self->assoc * self->block_size));
    int s_index = hash_2(parts, 0xFFFFFFFF - prime, self->size / (self->assoc * self->block_size));
    //    printf("%d %d  %d  %lu  %lu\n",f_index,s_index,parts[0],parts[1]);

    if (self->lines[f_index].block[0].valid && self->lines[f_index].block[0].tag == tag) {
        self->lines[f_index].block[0].age = 2;
        self->lines[s_index].block[1].age = 1;
        return TRUE;
    } else if (self->lines[s_index].block[1].valid && self->lines[s_index].block[1].tag == tag) {
        self->lines[f_index].block[0].age = 1;
        self->lines[s_index].block[1].age = 2;
        return TRUE;
    }

    if (self->lines[f_index].block[0].age == 0) {
        avdc_skewed_replace(self, tag, 1, 2, f_index, 0);
    } else if (self->lines[s_index].block[1].age == 0) {
        avdc_skewed_replace(self, tag, 1, 2, s_index, 1);
        self->lines[f_index].block[0].age = 1;
    } else {
        if (self->lines[f_index].block[0].age >= self->lines[s_index].block[1].age) {
            self->lines[f_index].block[0].age = 1;
            avdc_skewed_replace(self, tag, 1, 2, s_index, 1);
        } else {
            self->lines[s_index].block[1].age = 1;
            avdc_skewed_replace(self, tag, 1, 2, f_index, 0);
        }
    }
    return FALSE;

}

void avdc_access(avdark_cache_t *self, avdc_pa_t pa, avdc_access_type_t type) {
    /* HINT: You will need to update this function */
    avdc_tag_t tag = tag_from_pa(self, pa);
    int index = index_from_pa(self, pa);
    int hit = 0;
    if (self->assoc == 22) {
        hit = avdc_skewed_cache(self, pa);
    } else {
        // hit = self->lines[index].block[1].valid && self->lines[index].block[1].tag == tag;
        hit=avdc_lru(self, index, tag);
    }
    switch (type) {
        case AVDC_READ: /* Read accesses */
            avdc_dbg_log(self,
                    "read: pa: 0x%.16lx, tag: 0x%.16lx, index: %d, hit: %d\n",
                    (unsigned long) pa, (unsigned long) tag, index, hit);
            self->stat_data_read += 1;
            if (!hit)
                self->stat_data_read_miss += 1;
            break;

        case AVDC_WRITE: /* Write accesses */
            avdc_dbg_log(self,
                    "write: pa: 0x%.16lx, tag: 0x%.16lx, index: %d, hit: %d\n",
                    (unsigned long) pa, (unsigned long) tag, index, hit);
            self->stat_data_write += 1;
            if (!hit)
                self->stat_data_write_miss += 1;
            break;
    }
}

void avdc_flush_cache(avdark_cache_t *self) {
    /* HINT: You will need to update this function */
    for (int i = 0; i < self->number_of_sets; i++) {
        for (int j = 0; j < self->assoc; ++j) {
            self->lines[i].block[j].valid = 0;
            self->lines[i].block[j].tag = 0;
            self->lines[i].block[j].age = 0;
        }
    }
}

int avdc_resize(avdark_cache_t *self, avdc_size_t size,
        avdc_block_size_t block_size, avdc_assoc_t assoc) {
    /* HINT: This function precomputes some common values and
     * allocates the self->lines array. You will need to update
     * this to reflect any changes to how this array is supposed
     * to be allocated.
     */

    /* Verify that the parameters are sane */
    if (!is_power_of_two(size) || !is_power_of_two(block_size)
            || !is_power_of_two(assoc)) {
        fprintf(stderr,
                "size, block-size and assoc all have to be powers of two and > zero\n");
        return 0;
    }

    /* Update the stored parameters */
    self->size = size;
    self->block_size = block_size;
    self->assoc = assoc;

    /* Cache some common values */
    self->number_of_sets = (self->size / self->block_size) / self->assoc;
    self->block_size_log2 = log2_int32(self->block_size);
    self->tag_shift = self->block_size_log2 + log2_int32(self->number_of_sets);

    /* (Re-)Allocate space for the tags array */
    if (self->lines)
        AVDC_FREE(self->lines);
    /* HINT: If you change this, you may have to update
     * avdc_delete() to reflect changes to how thie self->lines
     * array is allocated. */
    self->lines = AVDC_MALLOC(self->number_of_sets, avdc_cache_line_t);

    for (int i = 0; i < self->number_of_sets; ++i) {
        self->lines[i].block = AVDC_MALLOC(self->assoc, avdc_cache_element_t);
    }

    /* Flush the cache, this initializes the tag array to a known state */
    avdc_flush_cache(self);

    return 1;
}

void avdc_print_info(avdark_cache_t *self) {
    fprintf(stderr, "Cache Info\n");
    fprintf(stderr, "size: %d, assoc: %d, line-size: %d\n", self->size,
            self->assoc, self->block_size);
}

void avdc_print_internals(avdark_cache_t *self) {
    int i;

    fprintf(stderr, "Cache Internals\n");
    fprintf(stderr, "size: %d, assoc: %d, line-size: %d\n", self->size,
            self->assoc, self->block_size);

    for (i = 0; i < self->number_of_sets; i++) {
        for (int j = 0; j < self->assoc; ++j) {
            fprintf(stderr, "tag: <0x%.16lx> valid: %d\n",
                    (long unsigned int) self->lines[i].block[j].tag,
                    self->lines[i].block[j].valid);
        }
    }
}

void avdc_reset_statistics(avdark_cache_t *self) {
    self->stat_data_read = 0;
    self->stat_data_read_miss = 0;
    self->stat_data_write = 0;
    self->stat_data_write_miss = 0;
}

avdark_cache_t *
avdc_new(avdc_size_t size, avdc_block_size_t block_size, avdc_assoc_t assoc) {
    avdark_cache_t *self;

    self = AVDC_MALLOC(1, avdark_cache_t);

    memset(self, 0, sizeof (*self));
    self->dbg = 0;

    if (!avdc_resize(self, size, block_size, assoc)) {
        AVDC_FREE(self);
        return NULL;
    }

    return self;
}

void avdc_delete(avdark_cache_t *self) {
    for (int i = 0; i < self->assoc; ++i) {
        if (self->lines[i].block)
            AVDC_FREE(self->lines[i].block);
    }

    if (self->lines)
        AVDC_FREE(self->lines);

    AVDC_FREE(self);
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
