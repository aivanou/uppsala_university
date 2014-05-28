/**
 * A code skeleton for the matrix multiply bonus assignment.
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: multiply.c 81 2012-09-13 08:01:46Z andse541 $
 */

#include <unistd.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

/* Size of the matrices to multiply */
#define SIZE 500

/* HINT: The Makefile allows you to specify L1 and L2 block sizes as
 * compile time options.These may be specified when calling make,
 * e.g. "make L1_BLOCK_SIZE=256 L2_BLOCK_SIZE=1024". If the block
 * sizes have been specified on the command line, makefile defines the
 * macros L1_BLOCK_SIZE and L2_BLOCK_SIZE. If you decide to use them,
 * you should setup defaults here if they are undefined.
 */


static double mat_a[SIZE][SIZE];
static double mat_b[SIZE][SIZE];
static double mat_c[SIZE][SIZE];
static double mat_ref[SIZE][SIZE];

static int els_in_block = 42;

static pthread_mutex_t LOCK = PTHREAD_MUTEX_INITIALIZER;

/**
 * Matrix multiplication. This is the procedure you should try to
 * optimize.
 */


static int min(int a, int b) {
    if (a > b) return b;
    return a;

}

static void swap_d(double*d1, double*d2) {
    double t = *d1;
    *d1 = *d2;
    *d2 = t;
}

static void
transpose(double matrix[SIZE][SIZE]) {

    for (int i = 0; i < SIZE; ++i) {
        for (int j = i + 1; j < SIZE; ++j) {
            swap_d(&matrix[i][j], &matrix[j][i]);
        }
    }
}

static void
print_matrix(double matrix[SIZE][SIZE]) {
    for (int i = 0; i < SIZE; ++i) {
        for (int j = 0; j < SIZE; ++j) {
            printf("%f ", matrix[i][j]);
        }
        printf("\n");
    }
}

/* this structure serves as input parameter to the pthread main function (matmul_opt_block) 
   @s_el - start element of the partial matrix
 * @e_el - end element of the partial matrix
 * @els - block size
 */
struct th_input {
    int s_el;
    int e_el;
    int els;
};

typedef struct th_input th_input_t;

int cas_matrix(double old_value, int k, int i, double new_value) {
//    pthread_mutex_lock(&LOCK);
    if (old_value != mat_c[k][i]){ 
        return -1;
    };
    mat_c[k][i]+=new_value;
//    pthread_mutex_unlock(&LOCK);
    return 1;
}

/*all computation is here*/
void matmul_block(int s_el_i, int e_el_i, int s_el_j, int e_el_j, int els_i, int els_j) {

    for (int ii = s_el_i; ii < e_el_i; ii += els_i) {
        for (int jj = s_el_j; jj < e_el_j; jj += els_j) {
            for (int k = 0; k < SIZE; ++k) {
                for (int i = ii; i < min(ii + els_i, SIZE); ++i) {
                    double t = 0.0;
                    for (int j = jj; j < min(jj + els_j, SIZE); ++j) {
                        t += mat_a[k][j] * mat_b[i][j];
                    }
                    while(!cas_matrix(mat_c[k][i],k,i,t)){
                    }
//                    mat_c[k][i] += t;
                }
            }
        }
    }

}

void *matmul_opt_block(void* input) {
    struct th_input* t_input;
    t_input = (struct th_input*) input;
    int last_block_els_i = (t_input->e_el - t_input->s_el) % t_input->els;
    int last_block_els_j = SIZE % t_input->els;

    /*in general the block size can be indivisible to the partial matrix size
     as a result, we should make 4 calls of matmul_block with:
     * i: 0 -- size-residue_i  j: 0 - SIZE-residue_j
       i: size - residue_i -- size ; j: 0 - SIZE-residue_j
     * i: 0 -- size-residue_i; j: SIZE-residue_j -- SIZE
       size-residue_i -- size ; j: SIZE-residue_j -- SIZE
     * -- is interval
     */

    matmul_block(t_input->s_el, t_input->e_el - last_block_els_i, 0, SIZE - last_block_els_j, t_input->els, t_input->els);
    matmul_block(t_input->e_el - last_block_els_i, t_input->e_el, 0, SIZE - last_block_els_j, last_block_els_i, t_input->els);
    matmul_block(t_input->s_el, t_input->e_el - last_block_els_i, SIZE - last_block_els_j, SIZE, t_input->els, last_block_els_j);
    matmul_block(t_input->e_el - last_block_els_i, t_input->e_el, SIZE - last_block_els_j, SIZE, last_block_els_i, last_block_els_j);
}

static void clear_matrix() {
    for (int i = 0; i < SIZE; ++i) {
        for (int j = 0; j < SIZE; ++j) {
            mat_c[i][j] = 0.0;
        }
    }
}

static void
matmul_opt_seq() {

    matmul_block(0, SIZE, 0, SIZE, els_in_block, els_in_block);
}

static void
matmul_opt_parallel() {
    /* TASK: Implement your optimized matrix multiplication
     * here. It should calculate mat_c := mat_a * mat_b. See
     * matmul_ref() for a reference solution.
     */

    int tasks = 4, els_in_task = SIZE / tasks;
    int e_blck = min(els_in_block, els_in_task);
    pthread_t threads[tasks];
    th_input_t* th_params = (th_input_t*) malloc(tasks * sizeof (th_input_t));

    for (int i = 0; i < tasks - 1; ++i) {
        th_input_t t_input = th_params[i];
        th_params[i].s_el = i*els_in_task;
        th_params[i].e_el = min((i + 1) * els_in_task, SIZE);
        th_params[i].els = e_blck;
        int rt = pthread_create(&threads[i], NULL, matmul_opt_block, &th_params[i]);
    }
    th_params[tasks - 1].s_el = (tasks - 1) * els_in_task;
    th_params[tasks - 1].e_el = SIZE;
    th_params[tasks - 1].els = e_blck;
    int rt = pthread_create(&threads[tasks - 1], NULL, matmul_opt_block, &th_params[tasks - 1]);
    for (int i = 0; i < tasks; ++i) {
        pthread_join(threads[i], NULL);
    }
    free(th_params);

}

/**
 * Reference implementation of the matrix multiply algorithm. Used to
 * verify the answer from matmul_opt. Do NOT change this function.
 */
static void
matmul_ref() {
    int i, j, k;

    for (j = 0; j < SIZE; j++) {
        for (i = 0; i < SIZE; i++) {
            for (k = 0; k < SIZE; k++) {
                mat_ref[i][j] += mat_a[i][k] * mat_b[k][j];
            }
        }
    }
}

/**
 * Function used to verify the result. No need to change this one.
 */
static int
verify_result() {
    double e_sum;
    int i, j;

    e_sum = 0;
    for (i = 0; i < SIZE; i++) {
        for (j = 0; j < SIZE; j++) {
            e_sum += mat_c[i][j] < mat_ref[i][j] ?
                    mat_ref[i][j] - mat_c[i][j] :
                    mat_c[i][j] - mat_ref[i][j];
        }
    }

    return e_sum < 1E-6;
}

/**
 * Get the time in seconds since some arbitrary point. Used for high
 * precision timing measurements.
 */
static double
get_time() {
    struct timeval tv;

    if (gettimeofday(&tv, NULL)) {
        fprintf(stderr, "gettimeofday failed. Aborting.\n");
        abort();
    }
    return tv.tv_sec + tv.tv_usec * 1E-6;
}

/**
 * Initialize mat_a and mat_b with "random" data. Write to every
 * element in mat_c to make sure that the kernel allocates physical
 * memory to every page in the matrix before we start doing
 * benchmarking.
 */
static void
init_matrices() {
    int i, j;

    for (i = 0; i < SIZE; i++) {
        for (j = 0; j < SIZE; j++) {
            mat_a[i][j] = ((i + j) & 0x0F) * 0x1P-4;
            mat_b[i][j] = (((i << 1) + (j >> 1)) & 0x0F) * 0x1P-4;
        }
    }

    memset(mat_c, 0, sizeof (mat_c));
    memset(mat_ref, 0, sizeof (mat_ref));
}

static void
run_multiply(int verify) {
    double time_start, time_stop;
    clear_matrix();
    time_start = get_time();
    transpose(mat_b);

    /* mat_c = mat_a * mat_b */
    printf("computing parallel algorithm: \n");
    matmul_opt_parallel();

    time_stop = get_time();
    transpose(mat_b);
    printf("Time: %.4f\n", time_stop - time_start);

    clear_matrix();
    time_start = get_time();
    transpose(mat_b);
    printf("computing sequential algorithm: \n");

    matmul_opt_seq();

    time_stop = get_time();
    printf("Time: %.4f\n", time_stop - time_start);
    transpose(mat_b);

    if (verify) {
        printf("Verifying solution... ");
        time_start = get_time();
        matmul_ref();
        time_stop = get_time();

        if (verify_result())
            printf("OK\n");
        else
            printf("MISMATCH\n");

        printf("Reference runtime: %f\n", time_stop - time_start);
    }
}

static void
usage(FILE *out, const char *argv0) {
    fprintf(out,
            "Usage: %s [OPTION]...\n"
            "\n"
            "Options:\n"
            "\t-v\tVerify solution\n"
            "\t-h\tDisplay usage\n",
            argv0);
}

int
main(int argc, char *argv[]) {
    int c;
    int errexit;
    int verify;
    extern char *optarg;
    extern int optind, optopt, opterr;

    errexit = 0;
    verify = 0;
    while ((c = getopt(argc, argv, "vh")) != -1) {
        switch (c) {
            case 'v':
                verify = 1;
                break;
            case 'h':
                usage(stdout, argv[0]);
                exit(0);
                break;
            case ':':
                fprintf(stderr, "%s: option -%c requries an operand\n",
                        argv[0], optopt);
                errexit = 1;
                break;
            case '?':
                fprintf(stderr, "%s: illegal option -- %c\n",
                        argv[0], optopt);
                errexit = 1;
                break;
            default:
                abort();
        }
    }

    if (errexit) {
        usage(stderr, argv[0]);
        exit(2);
    }

    /* Initialize the matrices with some "random" data. */
    init_matrices();
    
    run_multiply(verify);
    run_multiply(verify);

//             print_matrix(mat_c);
//             printf("\n---------------\n");
//             print_matrix(mat_ref);

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
