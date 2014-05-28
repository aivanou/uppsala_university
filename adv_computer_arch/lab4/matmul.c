/**
 * SSE matrix multiplication. Bonus assignment.
 *
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 4
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: matmul.c 1009 2011-07-28 15:02:57Z ansan501 $
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
#define SIZE 2048

#define SSE_BLOCK_SIZE 4

#ifndef L1_BLOCK_SIZE
#define L1_BLOCK_SIZE 64
#endif

#ifndef L2_BLOCK_SIZE
#define L2_BLOCK_SIZE 1024
#endif

/* A mode that controls how the matrix multiplication is optimized may
 * be specified at compile time. The following modes are defined:
 *
 * MODE_SSE - A simple, non-blocked, implementation of the matrix
 * multiplication.
 *
 * MODE_SSE_BLOCKED - A blocked matrix multiplication with implemented
 * using a 4x4 SSE block.
 *
 * MODE_BLOCKED - Blocked matrix mutliplication using ordinary
 * floating point math.
 */
#define MODE_SSE_BLOCKED 1
#define MODE_SSE 2
#define MODE_BLOCKED 3

#ifndef MODE
#define MODE MODE_SSE_BLOCKED
#endif


#define XMM_ALIGNMENT_BYTES 16 

static float mat_a[SIZE][SIZE] __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float mat_b[SIZE][SIZE] __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float mat_c[SIZE][SIZE] __attribute__((aligned (XMM_ALIGNMENT_BYTES)));
static float mat_ref[SIZE][SIZE] __attribute__((aligned (XMM_ALIGNMENT_BYTES)));

#if MODE == MODE_SSE_BLOCKED

static inline void
load_4_4(int ind_i,int ind_j,
              __m128* row1,__m128* row2,__m128* row3,__m128* row4,float m[SIZE][SIZE]){

    *row1=_mm_load_ps((__m128*)(&m[ind_i][ind_j]));
    *row2=_mm_load_ps((__m128*)(&m[ind_i+1][ind_j]));
    *row3=_mm_load_ps((__m128*)(&m[ind_i+2][ind_j]));
    *row4=_mm_load_ps((__m128*)(&m[ind_i+3][ind_j]));
}

static inline void
or_4_times(__m128* out, __m128 v1,__m128 v2,__m128 v3,__m128 v4){
    *out=_mm_or_ps(v1,v2);
    *out=_mm_or_ps(*out,v3);
    *out=_mm_or_ps(*out,v4);
}

/**
 * Blocked matrix multiplication, SSE block (4x4 matrix). Implement
 * your solution to the bonus assignment here.
 */
static inline void
matmul_block_sse(int i, int j, int k)
{
        /* BONUS TASK: Implement your SSE 4x4 matrix multiplication
         * block here. */
        /* HINT: You might find at least the following instructions
         * useful:
         *  - _mm_dp_ps
         *  - _MM_TRANSPOSE4_PS
         *
         * HINT: The result of _mm_dp_ps is scalar. The third
         * parameter can be used to restrict to which elements the
         * result is stored, all other elements are set to zero.
         */


    __m128 mm_a1,mm_a2,mm_a3,mm_a4;
    load_4_4(i,k,&mm_a1,&mm_a2,&mm_a3,&mm_a4,mat_a);
    __m128 mm_b1,mm_b2,mm_b3,mm_b4;
    load_4_4(k,j,&mm_b1,&mm_b2,&mm_b3,&mm_b4,mat_b);
    _MM_TRANSPOSE4_PS(mm_b1,mm_b2,mm_b3,mm_b4);
    __m128 mm_c1,mm_c2,mm_c3,mm_c4;
    load_4_4(i,j,&mm_c1,&mm_c2,&mm_c3,&mm_c4,mat_c);


//    mm_c1=_mm_add_ps(mm_c1,_mm_hadd_ps(_mm_hadd_ps(_mm_mul_ps(mm_a1,mm_b1),dummy),dummy));

    __m128 out1,out2,out3,out4;

    or_4_times(&out1,_mm_dp_ps(mm_a1,mm_b1,0xf1),_mm_dp_ps(mm_a1,mm_b2,0xf2),
                    _mm_dp_ps(mm_a1,mm_b3,0xf4),_mm_dp_ps(mm_a1,mm_b4,0xf8));

    or_4_times(&out2,_mm_dp_ps(mm_a2,mm_b1,0xf1),_mm_dp_ps(mm_a2,mm_b2,0xf2),
                    _mm_dp_ps(mm_a2,mm_b3,0xf4),_mm_dp_ps(mm_a2,mm_b4,0xf8));

    or_4_times(&out3,_mm_dp_ps(mm_a3,mm_b1,0xf1),_mm_dp_ps(mm_a3,mm_b2,0xf2),
                    _mm_dp_ps(mm_a3,mm_b3,0xf4),_mm_dp_ps(mm_a3,mm_b4,0xf8));

    or_4_times(&out4,_mm_dp_ps(mm_a4,mm_b1,0xf1),_mm_dp_ps(mm_a4,mm_b2,0xf2),
                    _mm_dp_ps(mm_a4,mm_b3,0xf4),_mm_dp_ps(mm_a4,mm_b4,0xf8));

    mm_c1=_mm_add_ps(mm_c1,out1);
    mm_c2=_mm_add_ps(mm_c2,out2);
    mm_c3=_mm_add_ps(mm_c3,out3);
    mm_c4=_mm_add_ps(mm_c4,out4);

//    mm_c1=_mm_add_ps(mm_c1, _mm_dp_ps(mm_a1,mm_b1,0xf1));
//    mm_c1=_mm_add_ps(mm_c1, _mm_dp_ps(mm_a1,mm_b2,0xf2));
//    mm_c1=_mm_add_ps(mm_c1, _mm_dp_ps(mm_a1,mm_b3,0xf4));
//    mm_c1=_mm_add_ps(mm_c1, _mm_dp_ps(mm_a1,mm_b4,0xf8));

//    mm_c2=_mm_add_ps(mm_c2, _mm_dp_ps(mm_a2,mm_b1,0xf1));
//    mm_c2=_mm_add_ps(mm_c2, _mm_dp_ps(mm_a2,mm_b2,0xf2));
//    mm_c2=_mm_add_ps(mm_c2, _mm_dp_ps(mm_a2,mm_b3,0xf4));
//    mm_c2=_mm_add_ps(mm_c2, _mm_dp_ps(mm_a2,mm_b4,0xf8));

//    mm_c3=_mm_add_ps(mm_c3, _mm_dp_ps(mm_a3,mm_b1,0xf1));
//    mm_c3=_mm_add_ps(mm_c3, _mm_dp_ps(mm_a3,mm_b2,0xf2));
//    mm_c3=_mm_add_ps(mm_c3, _mm_dp_ps(mm_a3,mm_b3,0xf4));
//    mm_c3=_mm_add_ps(mm_c3, _mm_dp_ps(mm_a3,mm_b4,0xf8));

//    mm_c4=_mm_add_ps(mm_c4, _mm_dp_ps(mm_a4,mm_b1,0xf1));
//    mm_c4=_mm_add_ps(mm_c4, _mm_dp_ps(mm_a4,mm_b2,0xf2));
//    mm_c4=_mm_add_ps(mm_c4, _mm_dp_ps(mm_a4,mm_b3,0xf4));
//    mm_c4=_mm_add_ps(mm_c4, _mm_dp_ps(mm_a4,mm_b4,0xf8));

    _mm_store_ps((__m128*)(&mat_c[i][j]),mm_c1);
    _mm_store_ps((__m128*)(&mat_c[i+1][j]),mm_c2);
    _mm_store_ps((__m128*)(&mat_c[i+2][j]),mm_c3);
    _mm_store_ps((__m128*)(&mat_c[i+3][j]),mm_c4);

//    float * f=(float*)malloc(4*sizeof(float));
//    f[0]=1;
//    f[1]=2;
//    f[2]=3;
//    f[3]=4;

//    __m128 m1=_mm_load_ps(f);
//    __m128 m2=_mm_load_ps(f);
//    __m128 ans=_mm_dp_ps(m1,m2,0xf1);
//    _mm_store_ps(f,ans);
//    for(int i=0;i<4;++i){
//        printf("%f  ",f[i]);
//    }
//    printf("\n");
//    free(f);


}
#elif MODE == MODE_BLOCKED
/**
 * Blocked matrix multiplication, SSE block (4x4 matrix) implemented
 * using ordinary floating point math.
 */
static inline void
matmul_block_sse(int i, int j, int k)
{
        mat_c[i][j] += 
                mat_a[i][k] * mat_b[k][j]
                + mat_a[i][k + 1] * mat_b[k + 1][j]
                + mat_a[i][k + 2] * mat_b[k + 2][j]
                + mat_a[i][k + 3] * mat_b[k + 3][j];

        mat_c[i][j + 1] += 
                mat_a[i][k] * mat_b[k][j + 1]
                + mat_a[i][k + 1] * mat_b[k + 1][j + 1]
                + mat_a[i][k + 2] * mat_b[k + 2][j + 1]
                + mat_a[i][k + 3] * mat_b[k + 3][j + 1];

        mat_c[i][j + 2] += 
                mat_a[i][k] * mat_b[k][j + 2]
                + mat_a[i][k + 1] * mat_b[k + 1][j + 2]
                + mat_a[i][k + 2] * mat_b[k + 2][j + 2]
                + mat_a[i][k + 3] * mat_b[k + 3][j + 2];

        mat_c[i][j + 3] += 
                mat_a[i][k] * mat_b[k][j + 3]
                + mat_a[i][k + 1] * mat_b[k + 1][j + 3]
                + mat_a[i][k + 2] * mat_b[k + 2][j + 3]
                + mat_a[i][k + 3] * mat_b[k + 3][j + 3];



        mat_c[i + 1][j] += 
                mat_a[i + 1][k] * mat_b[k][j]
                + mat_a[i + 1][k + 1] * mat_b[k + 1][j]
                + mat_a[i + 1][k + 2] * mat_b[k + 2][j]
                + mat_a[i + 1][k + 3] * mat_b[k + 3][j];

        mat_c[i + 1][j + 1] += 
                mat_a[i + 1][k] * mat_b[k][j + 1]
                + mat_a[i + 1][k + 1] * mat_b[k + 1][j + 1]
                + mat_a[i + 1][k + 2] * mat_b[k + 2][j + 1]
                + mat_a[i + 1][k + 3] * mat_b[k + 3][j + 1];

        mat_c[i + 1][j + 2] += 
                mat_a[i + 1][k] * mat_b[k][j + 2]
                + mat_a[i + 1][k + 1] * mat_b[k + 1][j + 2]
                + mat_a[i + 1][k + 2] * mat_b[k + 2][j + 2]
                + mat_a[i + 1][k + 3] * mat_b[k + 3][j + 2];

        mat_c[i + 1][j + 3] += 
                mat_a[i + 1][k] * mat_b[k][j + 3]
                + mat_a[i + 1][k + 1] * mat_b[k + 1][j + 3]
                + mat_a[i + 1][k + 2] * mat_b[k + 2][j + 3]
                + mat_a[i + 1][k + 3] * mat_b[k + 3][j + 3];



        mat_c[i + 2][j] += 
                mat_a[i + 2][k] * mat_b[k][j]
                + mat_a[i + 2][k + 1] * mat_b[k + 1][j]
                + mat_a[i + 2][k + 2] * mat_b[k + 2][j]
                + mat_a[i + 2][k + 3] * mat_b[k + 3][j];

        mat_c[i + 2][j + 1] += 
                mat_a[i + 2][k] * mat_b[k][j + 1]
                + mat_a[i + 2][k + 1] * mat_b[k + 1][j + 1]
                + mat_a[i + 2][k + 2] * mat_b[k + 2][j + 1]
                + mat_a[i + 2][k + 3] * mat_b[k + 3][j + 1];

        mat_c[i + 2][j + 2] += 
                mat_a[i + 2][k] * mat_b[k][j + 2]
                + mat_a[i + 2][k + 1] * mat_b[k + 1][j + 2]
                + mat_a[i + 2][k + 2] * mat_b[k + 2][j + 2]
                + mat_a[i + 2][k + 3] * mat_b[k + 3][j + 2];

        mat_c[i + 2][j + 3] += 
                mat_a[i + 2][k] * mat_b[k][j + 3]
                + mat_a[i + 2][k + 1] * mat_b[k + 1][j + 3]
                + mat_a[i + 2][k + 2] * mat_b[k + 2][j + 3]
                + mat_a[i + 2][k + 3] * mat_b[k + 3][j + 3];



        mat_c[i + 3][j] += 
                mat_a[i + 3][k] * mat_b[k][j]
                + mat_a[i + 3][k + 1] * mat_b[k + 1][j]
                + mat_a[i + 3][k + 2] * mat_b[k + 2][j]
                + mat_a[i + 3][k + 3] * mat_b[k + 3][j];

        mat_c[i + 3][j + 1] += 
                mat_a[i + 3][k] * mat_b[k][j + 1]
                + mat_a[i + 3][k + 1] * mat_b[k + 1][j + 1]
                + mat_a[i + 3][k + 2] * mat_b[k + 2][j + 1]
                + mat_a[i + 3][k + 3] * mat_b[k + 3][j + 1];

        mat_c[i + 3][j + 2] += 
                mat_a[i + 3][k] * mat_b[k][j + 2]
                + mat_a[i + 3][k + 1] * mat_b[k + 1][j + 2]
                + mat_a[i + 3][k + 2] * mat_b[k + 2][j + 2]
                + mat_a[i + 3][k + 3] * mat_b[k + 3][j + 2];

        mat_c[i + 3][j + 3] += 
                mat_a[i + 3][k] * mat_b[k][j + 3]
                + mat_a[i + 3][k + 1] * mat_b[k + 1][j + 3]
                + mat_a[i + 3][k + 2] * mat_b[k + 2][j + 3]
                + mat_a[i + 3][k + 3] * mat_b[k + 3][j + 3];
}

#endif

#if MODE == MODE_SSE_BLOCKED || MODE == MODE_BLOCKED
/**
 * Blocked matrix multiplication, L1 block.
 */
static inline void
matmul_block_l1(int i, int j, int k)
{
        int ii, jj, kk;

        for (ii = i; ii < i + L1_BLOCK_SIZE; ii += SSE_BLOCK_SIZE)
                for (kk = k; kk < k + L1_BLOCK_SIZE; kk += SSE_BLOCK_SIZE)
                        for (jj = j; jj < j + L1_BLOCK_SIZE; jj += SSE_BLOCK_SIZE)
                                matmul_block_sse(ii, jj, kk);
}

/**
 * Blocked matrix multiplication, L2 block.
 */
static inline void
matmul_block_l2(int i, int j, int k)
{
        int ii, jj, kk;

        for (ii = i; ii < i + L2_BLOCK_SIZE; ii += L1_BLOCK_SIZE)
                for (kk = k; kk < k + L2_BLOCK_SIZE; kk += L1_BLOCK_SIZE)
                        for (jj = j; jj < j + L2_BLOCK_SIZE; jj += L1_BLOCK_SIZE)
                                matmul_block_l1(ii, jj, kk);
}

/**
 * Blocked matrix multiplication, entry function for multiplying two
 * matrices.
 */
static void
matmul_sse()
{
        int i, j, k;

        for (i = 0; i < SIZE; i += L2_BLOCK_SIZE)
                for (k = 0; k < SIZE; k += L2_BLOCK_SIZE)
                        for (j = 0; j < SIZE; j += L2_BLOCK_SIZE)
                                matmul_block_l2(i, j, k);
}

#elif MODE == MODE_SSE

/**
 * Matrix multiplication. This is the procedure you should try to
 * optimize.
 */
static void
matmul_sse()
{
    int i, j, k;

    /* Assume that the data size is an even multiple of the 128 bit
     * SSE vectors (i.e. 4 floats) */
    assert(!(SIZE & 0x3));

    /* TASK: Implement your simple matrix multiplication using SSE
     * here.
     */

    for(int i=0;i<SIZE;++i){
        for(int j=0;j<SIZE;j++){
            __m128 mm_a=_mm_set1_ps(mat_a[i][j]);
            for(int k=0;k<SIZE;k+=4){
                __m128 mm_c=_mm_load_ps((__m128*)(&mat_c[i][k]));
                __m128 mm_b=_mm_load_ps((__m128*)(&mat_b[j][k]));
                __m128 out=_mm_mul_ps(mm_a,mm_b);
                out=_mm_add_ps(out,mm_c);
                _mm_store_ps((__m128*)(&mat_c[i][k]),out);
            }
        }
    }


}


#else

#error Invalid mode

#endif

/**
 * Reference implementation of the matrix multiply algorithm. Used to
 * verify the answer from matmul_opt. Do NOT change this function.
 */
static void
matmul_ref()
{
        int i, j, k;

	for (i = 0; i < SIZE; i++) {
	    for (k = 0; k < SIZE; k++) {
		for (j = 0; j < SIZE; j++) {
                                mat_ref[i][j] += mat_a[i][k] * mat_b[k][j];
                        }
                }
        }
}

/**
 * Function used to verify the result. No need to change this one.
 */
static int
verify_result()
{
        float e_sum;
        int i, j;

        e_sum = 0;
        for (i = 0; i < SIZE; i++) {
                for (j = 0; j < SIZE; j++) {
                        e_sum += mat_c[i][j] < mat_ref[i][j] ?
                                mat_ref[i][j] - mat_c[i][j] :
                                mat_c[i][j] - mat_ref[i][j];
                }
        }

        printf("e_sum: %f\n", e_sum);

        return e_sum < 1E-6;
}

/**
 * Initialize mat_a and mat_b with "random" data. Write to every
 * element in mat_c to make sure that the kernel allocates physical
 * memory to every page in the matrix before we start doing
 * benchmarking.
 */
static void
init_matrices()
{
        int i, j;

        for (i = 0; i < SIZE; i++) {
                for (j = 0; j < SIZE; j++) {
                        mat_a[i][j] = ((i + j) & 0x0F) * 0x1P-4F;
                        mat_b[i][j] = (((i << 1) + (j >> 1)) & 0x0F) * 0x1P-4F;
                }
        }

        memset(mat_c, 0, sizeof(mat_c));
        memset(mat_ref, 0, sizeof(mat_ref));
}

static void
run_multiply()
{
        struct timespec ts_start, ts_stop;
        double runtime_ref, runtime_sse;

        printf("Starting SSE run...\n");
        util_monotonic_time(&ts_start);
        /* mat_c = mat_a * mat_b */
        matmul_sse();
        util_monotonic_time(&ts_stop);
        runtime_sse = util_time_diff(&ts_start, &ts_stop);
        printf("SSE run completed in %.2f s\n",
               runtime_sse);

        printf("Starting reference run...\n");
        util_monotonic_time(&ts_start);
	matmul_ref();
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
        init_matrices();

        run_multiply();

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
