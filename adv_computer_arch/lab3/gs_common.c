/**
 * Common functions for the Gauss-Seidel problem.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 3
 *
 * Original author: Frédéric Haziza <daz@it.uu.se>
 * Heavily modified by: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: gs_common.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#if defined(__sun)
/* Must be defined to get memalign() since it isn't POSIX. We'd really
 * like to use posix_memalign instead, but Solaris doesn't implement
 * that.
 */
#define __EXTENSIONS__
#endif

#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <time.h>

#include "timing.h"
#include "gs_interface.h"

/* Align the matrix on a 4kB boundary */
#define MATRIX_ALIGNMENT 0x1000

/*
 * Default values for parameters that can be specified on the command
 * line.
 */
#define DEFAULT_SIZE               2048
#define DEFAULT_ITERATIONS           20
#define DEFAULT_TOLERANCE             1.0
#define DEFAULT_NTHREADS              4
#define DEFAULT_PAD                   0

/* Parameter values */
int gs_verbose = 0;
FILE* gs_output = NULL;

int gs_size = DEFAULT_SIZE;
int gs_pad = DEFAULT_PAD;
int gs_width;
int gs_iterations = DEFAULT_ITERATIONS;
double gs_tolerance = DEFAULT_TOLERANCE;
int gs_nthreads = DEFAULT_NTHREADS;

double *gs_matrix = NULL;

void
gs_verbose_printf(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);

        if (gs_verbose)
                vprintf(fmt, ap);

        va_end(ap);
}

/**
 * Initialization function that fills the matrix with suitable data.
 */
static void
init_matrix()
{
        const double sqrt3 = 1.73205080757;
        const double size_inv = 1.0 / (gs_size - 1);
        
        gs_verbose_printf("\t****  Initializing the matrix  ****\n");

        for (int i = 0; i < gs_size; i++) {
                for(int j = 0; j < gs_size; j++)
                        gs_matrix[GS_INDEX(i, j)] =
                                sin(M_PI * i * size_inv) *
                                sin(M_PI * j * size_inv) *
                                sin(M_PI * M_SQRT2 * i * size_inv) * 
                                sin(M_PI * sqrt3 * j * size_inv);
        }
}

/**
 * Print runtime parameters
 */
static void
print_info()
{
        printf("****  Runtime parameters ****\n");
        printf("Grid size is: %d x %d\n", gs_size, gs_size);
        printf("Iterations : %d\n", gs_iterations);
        printf("Tolerance : %f\n", gs_tolerance);
        printf("Pad = %d elements\n", gs_pad);
        if (gsi_is_parallel)
                printf("Number of threads : %d\n", gs_nthreads);
        printf("Matrix using %dx%d * sizeof(double) bytes of memory\n",
               gs_size, gs_width);
        printf("*****************************\n");
}

/**
 * Test if an input is a power of 2.
 */
static int
is_power_of_two(int val)
{
        return ((((val)&(val-1)) == 0) && (val > 0));
}

/**
 * Write the matrix to a file.
 */
static void
write_matrix(FILE *file)
{
        gs_verbose_printf("\t****  Storing matrix to file  ****\n");

        for (int j = 0; j < gs_size; j++) {
                for (int i = 0; i < gs_size; i++)
                        fprintf(file, "%g ",
                                gs_matrix[GS_INDEX(i, j)]);

                fprintf(file, "\n");  
        }
        fprintf(file, "\n");
}

/**
 * The "real" main function. This is called by main() after all
 * parameters have been handled.
 */
static void
run_gs()
{
        struct timespec ts;
        double exec_time;

        gs_verbose_printf("\t****  Initializing...  ****\n");

#if defined(__sun)
        /* Sigh... posix_memalign isn't implemented in Solaris, so we
         * have to use the legacy version instead. */
        gs_matrix = (double *)memalign(MATRIX_ALIGNMENT,
                                       gs_size * gs_width * sizeof(double));
        if (!gs_matrix) {
#else
                if (posix_memalign((void **)&gs_matrix, MATRIX_ALIGNMENT,
                                   gs_size * gs_width * sizeof(double)) != 0) {
#endif

                        fprintf(stderr,
                                "Error: Failed to allocate memory for matrix.\n");
                        exit(EXIT_FAILURE);
                }

                init_matrix();
                gsi_init();

                gs_verbose_printf("\t****  Running GS...  ****\n");
                timing_start(&ts);
                gsi_calculate();
                exec_time = timing_stop(&ts);

                if (gs_output)
                        write_matrix(gs_output);

                gs_verbose_printf("\t****  Cleaning up...  ****\n");
                gsi_finish();
                free(gs_matrix);
  
                fprintf(stdout,"**** Summary ****\n");
                fprintf(stdout,"   Execution time: %f s\n", exec_time);
                fprintf(stdout,"*****************\n");
        }

        static void
                usage(FILE *out, const char *argv0)
        {
                fprintf(out, "Usage: %s [OPTION]...\n"
                        "\n"
                        "Options:\n", argv0);

                fprintf(out, "\t-v\t\tEnable verbose output\n");
                fprintf(out, "\t-h\t\tDisplay usage\n");
                fprintf(out,
                        "\t-i ITER\t\tRun a maximum of ITER matrix sweeps. "
                        "Default: %i\n",
                        DEFAULT_ITERATIONS);
                fprintf(out,
                        "\t-e ERROR\tMaximum error tolerance. Default: %f\n",
                        DEFAULT_TOLERANCE);
                fprintf(out,
                        "\t-s SIZE\t\tUse a matrix of SIZExSIZE elements. "
                        "Default: %i\n", DEFAULT_SIZE);
                fprintf(out, "\t-o FILE\t\tWrite result to FILE.\n");
                fprintf(out,
                        "\t-p PAD\t\tIntroduce padding of PAD elements. "
                        "Default: %i\n", DEFAULT_PAD);

                if (gsi_is_parallel)
                        fprintf(out,
                                "\t-t NUM\t\tStart NUM worker threads. Default: %i\n",
                                DEFAULT_NTHREADS);
        }

        int
                main(int argc, char *argv[])
        {
                int c;
                int errexit = 0;
                extern char *optarg;
                extern int optind, optopt, opterr;

                while ((c = getopt(argc, argv, "vhi:e:s:t:p:o:")) != -1) {
                        switch (c) {
                        case 'v':
                                gs_verbose = 1; 
                                break;

                        case 'i':
                                gs_iterations = atoi(optarg);
                                if (gs_iterations <= 0) {
                                        fprintf(stderr,
                                                "Number of iterations must be "
                                                "strictly greater than 1.\n");
                                        errexit = 1;
                                }
                                break;

                        case 'e':
                                gs_tolerance = atof(optarg);
                                break;

                        case 's':
                                gs_size = atoi(optarg);
                                if (gs_size <= 0) {
                                        fprintf(stderr,
                                                "Size must be larger than 1.\n");
                                        errexit = 1;
                                } else if (!is_power_of_two(gs_size)) {
                                        fprintf(stderr,
                                                "Size is not a power of two.\n");
                                        errexit = 1;
                                }
                                break;

                        case 't':
                                gs_nthreads = atoi(optarg);
                                if (!gsi_is_parallel) {
                                        fprintf(stderr,
                                                "This is the sequential version "
                                                "-- -t doesn't make sense!\n");
                                        errexit = 1;
                                } else if (gs_nthreads <= 0) {
                                        fprintf(stderr,
                                                "Number of threads must be "
                                                "positive.\n");
                                        errexit = 1;
                                } else if (!is_power_of_two(gs_nthreads)) {
                                        fprintf(stderr,
                                                "Number is not a power of two.\n");
                                        errexit = 1;
                                }
                                break;

                        case 'p':
                                gs_pad = atoi(optarg);
                                if (gs_pad < 0) {
                                        fprintf(stderr,
                                                "Padding must be greater or equal "
                                                "to 0.\n");
                                        errexit = 1;
                                }
                                break;

                        case 'o':
                                gs_output = fopen(optarg, "w");
                                if (!gs_output) {
                                        perror("Failed to open output");
                                        errexit = 1;
                                }
                                break;

                        case 'h':
                                usage(stdout, argv[0]);
                                exit(EXIT_SUCCESS);

                        case ':':
                                fprintf(stderr,
                                        "%s: option -%c requries an operand\n",
                                        argv[0], optopt);
                                errexit = 1;
                                break;
                        case '?':
                                fprintf(stderr,
                                        "%s: illegal option -- %c\n",
                                        argv[0], optopt);
                                errexit = 1;
                                break;
                        default:
                                abort();
                        }
                }

                if (errexit) {
                        usage(stderr, argv[0]);
                        exit(EXIT_FAILURE);
                }

                gs_width = gs_size + gs_pad;
  
                /* At this point all the options have been processed. */
                if (gs_verbose)
                        print_info();

                run_gs();

                return EXIT_SUCCESS;
        }

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
