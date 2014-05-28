/**
 * SSE matrix vector multiplication.
 *
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 4
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: matmul.c 601 2010-08-02 15:06:56Z ansan501 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <smmintrin.h>

#ifndef __SSE4_1__
#error This example requires SSE4.1
#endif

#include "util.h"

/* Size of the matrices to multiply */
#define SIZE2 14
#define SIZE (1 << SIZE2)

#define MINDEX(n, m) (((n) << SIZE2) | (m))

#define XMM_ALIGNMENT_BYTES 16 

static float *mat_a __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float *vec_b __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float *vec_c __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float *vec_ref __attribute__((aligned (XMM_ALIGNMENT_BYTES)));

static void
matvec_sse()
{
        /* Assume that the data size is an even multiple of the 128 bit
         * SSE vectors (i.e. 4 floats) */
        assert(!(SIZE & 0x3));

        /* TASK: Implement your SSE version of the matrix-vector
         * multiplication here.
         */
        /* HINT: You might find at least the following instructions
         * useful:
         *  - _mm_setzero_ps
         *  - _mm_load_ps
         *  - _mm_hadd_ps
         *  - _mm_cvtss_f32
         *
         * HINT: You can create the sum of all elements in a vector
         * using two hadd instructions.
         */

        __m128 dummy=_mm_setzero_ps();
        for(int i=0;i<SIZE;++i){
            __m128 temp=_mm_setzero_ps();
            for(int j=0;j<SIZE;j+=4){

                __m128 mm_vec_b=_mm_load_ps((__m128*)(vec_b+j));
                __m128 mm_matr=_mm_load_ps((__m128*)(mat_a+MINDEX(i,j)));
                __m128 out=_mm_mul_ps(mm_vec_b,mm_matr);
                temp=_mm_add_ps(temp,out);

//                vec_c[i]+=_mm_cvtss_f32(_mm_dp_ps(mm_matr,mm_vec_b,0xf1));
            }
            __m128 res=_mm_hadd_ps(_mm_hadd_ps(temp,dummy),dummy);
            vec_c[i]=_mm_cvtss_f32(res);
        }

}

/**
 * Reference implementation of the matrix vector multiply
 * algorithm. Used to verify the answer. Do NOT change this function.
 */
static void
matvec_ref()
{
        int i, j;

	for (i = 0; i < SIZE; i++)
                for (j = 0; j < SIZE; j++)
                        vec_ref[i] += mat_a[MINDEX(i, j)] * vec_b[j];
}

/**
 * Function used to verify the result. No need to change this one.
 */
static int
verify_result()
{
        float e_sum;
        int i;

        e_sum = 0;
        for (i = 0; i < SIZE; i++) {
                e_sum += vec_c[i] < vec_ref[i] ?
                        vec_ref[i] - vec_c[i] :
                        vec_c[i] - vec_ref[i];
        }

        printf("e_sum: %.e\n", e_sum);

        return e_sum < 1E-6;
}

/**
 * Initialize mat_a and vec_b with "random" data. Write to every
 * element in mat_c to make sure that the kernel allocates physical
 * memory to every page in the matrix before we start doing
 * benchmarking.
 */
static void
init()
{
        int i, j;

        mat_a = _mm_malloc(sizeof(*mat_a) * SIZE * SIZE, XMM_ALIGNMENT_BYTES);
        vec_b = _mm_malloc(sizeof(*vec_b) * SIZE, XMM_ALIGNMENT_BYTES);
        vec_c = _mm_malloc(sizeof(*vec_c) * SIZE, XMM_ALIGNMENT_BYTES);
        vec_ref = _mm_malloc(sizeof(*vec_ref) * SIZE, XMM_ALIGNMENT_BYTES);

        if (!mat_a || !vec_b || !vec_c || !vec_ref) {
                fprintf(stderr, "Memory allocation failed\n");
                abort();
        }

        for (i = 0; i < SIZE; i++) {
                for (j = 0; j < SIZE; j++)
                        mat_a[MINDEX(i, j)] = ((7 * i + j) & 0x0F) * 0x1P-2F;
                vec_b[i] = ((i * 17) & 0x0F) * 0x1P-2F;
        }

        memset(vec_c, 0, sizeof(vec_c));
        memset(vec_ref, 0, sizeof(vec_ref));
}

static void
run_multiply()
{
        struct timespec ts_start, ts_stop;
        double runtime_ref, runtime_sse;

        printf("Starting SSE run...\n");
        util_monotonic_time(&ts_start);
        /* vec_c = mat_a * vec_b */
        matvec_sse();
        util_monotonic_time(&ts_stop);
        runtime_sse = util_time_diff(&ts_start, &ts_stop);
        printf("SSE run completed in %.2f s\n",
               runtime_sse);

        printf("Starting reference run...\n");
        util_monotonic_time(&ts_start);
	matvec_ref();
        util_monotonic_time(&ts_stop);
        runtime_ref = util_time_diff(&ts_start, &ts_stop);
        printf("Reference run completed in %.2f s\n",
               runtime_ref);

        printf("Speedup: %.2f\n",
               runtime_ref / runtime_sse);


	if (verify_result())
	    printf("OK\n");
	else
	    printf("MISMATCH\n");
}

int
main(int argc, char *argv[])
{
        /* Initialize the matrices with some "random" data. */
        init();

        run_multiply();

        _mm_free(mat_a);
        _mm_free(vec_b);
        _mm_free(vec_c);
        _mm_free(vec_ref);

        return 0;
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * compile-command: "make -k"
 * End:
 */
