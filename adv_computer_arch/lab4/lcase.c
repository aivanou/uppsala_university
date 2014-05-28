/**
 * Conversion to lower case experiment using SSE.
 *
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 4
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: lcase.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <pmmintrin.h>

#ifdef __SSE4_1__
#include <smmintrin.h>
#endif

#include "util.h"

#define CASE_BIT 0x20
#define XMM_ALIGNMENT_BYTES 16 

/* Use align data if required by the access type */
#define ALIGN_AS_REQUIRED 1
/* Always align data */
#define ALIGN_ALIGNED 2
/* Make sure that data is unaligned */
#define ALIGN_UNALIGNED 3

/* Alignment settings */
#define ALIGN ALIGN_AS_REQUIRED

#define ACCESS_MOVDQU 1
#define ACCESS_MOVDQA 2
#define ACCESS_MOVNTDQ 3

/* Access type is set by the build scripts */
/* #define ACCESS_TYPE ACCESS_MOVDQU */

#if ACCESS_TYPE == ACCESS_MOVDQU

/* Load/Store using MOVDQU (Move Unaligned Double Quadword) */
#define LOAD_SI128(PTR) _mm_loadu_si128(PTR)
#define STORE_SI128(PTR, V) _mm_storeu_si128(PTR, V)

#elif ACCESS_TYPE == ACCESS_MOVDQA

/* Load/Store using MOVDQA (Move Aligned Double Quadword) */
#define LOAD_SI128(PTR) _mm_load_si128(PTR)
#define STORE_SI128(PTR, V) _mm_store_si128(PTR, V)

#elif ACCESS_TYPE == ACCESS_MOVNTDQ

#ifndef __SSE4_1__
#error ACCESS_MOVNTDQ requires an SSE4.1 compatible target
#endif

/* Load using MOVNTDQA (Load Double Quadword Non-Temporal Aligned Hint) */
#define LOAD_SI128(PTR) _mm_stream_load_si128(PTR)
/* Load using MOVNTDQ (Store Double Quadword Using Non-Temporal Hint) */
/* Note: This store instruction requires alignment, even though the
 * name seems to indicate otherwise. */
#define STORE_SI128(PTR, V) _mm_stream_si128(PTR, V)

#else
#error Invalid access type
#endif

#if (ALIGN == ALIGN_UNALIGNED) || \
        ((ALIGN == ALIGN_AS_REQUIRED) && (ACCESS_TYPE == ACCESS_MOVDQU))

typedef void (* test_function_t)(char *, const char *, size_t);

static void *
my_malloc(size_t size)
{
        /* Make sure that we return an unaligned pointer by requesting
         * aligned memory and adding 1 to that pointer */
        void *p;
        fprintf(stderr, "Allocating %zi bytes of unaligned memory\n", size);
        if (!(p = _mm_malloc(size + 1, XMM_ALIGNMENT_BYTES))) {
                fprintf(stderr, "Failed to allocate memory.\n");
                abort();
        }
        return (void *)((char *)p + 1);
}


static void
my_free(void *p)
{
        /* The pointer to the unaligned memory was offset by +1 at the
         * time of allocation, we need to undo this to be able to free
         * the memory */
        _mm_free((char *)p - 1);
}

#else

static void *
my_malloc(size_t size)
{
        void *p;
        fprintf(stderr, "Allocating %zi bytes of aligned memory\n", size);
        if (!(p = _mm_malloc(size, XMM_ALIGNMENT_BYTES))) {
                fprintf(stderr, "Failed to allocate memory.\n");
                abort();
        }
        return p;
}


static void
my_free(void *p)
{
        _mm_free(p);
}

#endif

static void
lcase_ref_simple(char *restrict dst, const char *restrict src, size_t len)
{
        const char *cur = src;
        while (cur != src + len)
                *(dst++) = *(cur++) | CASE_BIT;
}

static void
lcase_ref_cond(char *restrict dst, const char *restrict src, size_t len)
{
        const char *cur = src;
        while (cur != src + len) {
                const char c = *(cur++);
                *(dst++) = (c >= 'A' && c <= 'Z') ? c | CASE_BIT : c;
        }
}

static void
lcase_sse_simple(char *restrict dst, const char *restrict src, size_t len)
{
        /* We assume that the data size is an even multiple of 16
         * bytes (one 128 bit vector register). */
        assert(!(len & 0xF));

        /* TASK: Implement the simple algorithm for converting text to
         * lower case without checking that the characters are within
         * the allowed range. Use SSE instructions and the
         * LOAD_SI128/STORE_SI128 macros. See lcase_ref_simple() for a
         * reference implementation.
         */
        /* HINT: Check out the documentation for the following:
         *  - _mm_set1_epi8
         *  - _mm_or_si128 (the por instruction)
         */

        __m128i mm_const=_mm_set1_epi8(CASE_BIT);
        for(int i=0;i<len;i+=16){
            __m128i val=LOAD_SI128((__m128i*)(src+i));
            __m128i out=_mm_or_si128(mm_const,val);
            STORE_SI128((__m128i*)(dst+i),out);
        }


}

static void
lcase_sse_cond(char *restrict dst, const char *restrict src, size_t len)
{
        /* We assume that the data size is an even multiple of 16
         * bytes (one 128 bit vector register). */
        assert(!(len & 0xF));

        /* TASK: Implement the "full" algorithm for converting text to
         * lower case using SSE instructions and the
         * LOAD_SI128/STORE_SI128 macros.
         */
        /* HINT: Check out the documentation for the following:
         *  - _mm_cmpgt_epi8 (the pcmpgtb instruction)
         *  - _mm_and_si128 (the pand instruction)
         */

        __m128i mm_const=_mm_set1_epi8(CASE_BIT);
        __m128i lbound=_mm_set1_epi8('A'-1);
        __m128i hbound=_mm_set1_epi8('Z'+1);
        __m128i bit_mask;

        for(int i=0;i<len;i+=16){
            __m128i val=LOAD_SI128((__m128i*)(src+i));
            bit_mask=_mm_and_si128(_mm_cmpgt_epi8(hbound,val),_mm_cmpgt_epi8(val,lbound));
            bit_mask=_mm_and_si128(bit_mask,mm_const);
            __m128i out=_mm_or_si128(val,bit_mask);
            STORE_SI128((__m128i*)(dst+i),out);
        }

}

static char *
generate_test_data(size_t len)
{
        char *data;
        int i = 0;

        data = my_malloc(len);

        /* TODO: We should initiate the random seed */
        for (; i + 4 <= len; i += 4)
                *(int32_t *)(data + i) = (int32_t)mrand48();
        for (; i < len; i++)
                data[i] = mrand48() & 0xFF;

        return data;
}

static void
run_tests(const char *restrict in, size_t len,
          void (* f_ref)(char *restrict, const char * restrict, size_t), char *restrict ref_area,
          void (* f_test)(char *restrict, const char * restrict, size_t), char *restrict test_area)
{
        struct timespec ts_start, ts_stop;
        double runtime_ref, runtime_sse;

        printf("Starting reference run...\n");
        util_monotonic_time(&ts_start);
        f_ref(ref_area, in, len);
        util_monotonic_time(&ts_stop);
        runtime_ref = util_time_diff(&ts_start, &ts_stop);
        printf("Reference run completed in %.2f s (%.1f MB/s)\n",
               runtime_ref,
               len / runtime_ref / (1 << 20));

        printf("Starting simple SSE run...\n");
        util_monotonic_time(&ts_start);
        f_test(test_area, in, len);
        util_monotonic_time(&ts_stop);
        runtime_sse = util_time_diff(&ts_start, &ts_stop);
        printf("SSE run completed in %.2f s (%.1f MB/s)\n",
               runtime_sse,
               len / runtime_sse / (1 << 20));

        printf("Speedup: %.2f\n",
               runtime_ref / runtime_sse);

        if (memcmp(ref_area, test_area, len))
                printf("Error: Reference run and SSE run produce inconsistent results.\n");
        else
                printf("Reference run and SSE are consistent.\n");
}

int
main(int argc, char *argv[])
{
        char *in, *out, *ref;
        size_t len = 512 * (1 << 20);

        printf("Generating test data...\n");
        in = generate_test_data(len);
        out = my_malloc(len);
        ref = my_malloc(len);

        /* Make sure that the kernel actually attaches memory to the
         * allocation. We might get "funny" numbers otherwise. */
        memset(ref, 0xFF, len);
        memset(out, 0xFF, len);

        printf("---\n"
               "Simple\n"
               "---\n");
        run_tests(in, len,
                  lcase_ref_simple, ref,
                  lcase_sse_simple, out);

        printf("---\n"
               "With conditional\n"
               "---\n");
        run_tests(in, len,
                  lcase_ref_cond, ref,
                  lcase_sse_cond, out);

        my_free(in);
        my_free(out);
        my_free(ref);

        return 0;
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
