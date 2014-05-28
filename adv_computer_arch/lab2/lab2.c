/**
 * Experimenting with synchronization and memory consistency.
 * 
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 2
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: lab2.c 1009 2011-07-28 15:02:57Z ansan501 $
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>

#include "lab2.h"

typedef struct {
        pthread_t thread;
        int thread_id;
        test_func_ptr_t func;
        double run_time;
} thread_conf_t;

critical_section_impl_t *cs_impls[] = {
        &cs_impl_pthreads,
        &cs_impl_null,
        &cs_impl_dekker,
        &cs_impl_queue,
        NULL
};

test_impl_t *test_impls[] = {
        &test_impl_critical,
        &test_impl_critical4,
        &test_impl_critical8,
        &test_impl_incdec_atomic,
        &test_impl_incdec_no_atomic,
        &test_impl_cmpxchg_atomic,
        &test_impl_cmpxchg_no_atomic,
        NULL
};

#define DEFAULT_ITERATIONS 1000000

#define DEFAULT_TEST_IMPL test_impl_critical
#define DEFAULT_CS_IMPL cs_impl_pthreads

/** Number of iterations to run of the increment/decrement loops. */
static int iterations = DEFAULT_ITERATIONS;

static critical_section_impl_t *cs_impl = &DEFAULT_CS_IMPL;
static test_impl_t *test_impl = &DEFAULT_TEST_IMPL;

void (* enter_critical)(int thread);
void (* exit_critical)(int thread);

/** Shared variable used in the increment/decrement loops */
static int shared_data = 0;

static void
monotonic_time(struct timespec *ts)
{
        if (clock_gettime(CLOCK_MONOTONIC, ts) != 0) {
                perror("clock_gettime failed");
                abort();
        }
}

static double
time_diff(struct timespec *ts_start, struct timespec *ts_stop)
{
        /* Sanity check, make sure that the stop time is after the start
         * time. */
        assert(ts_stop->tv_sec > ts_start->tv_sec ||
               (ts_stop->tv_sec == ts_start->tv_sec &&
                ts_stop->tv_nsec >= ts_start->tv_nsec));

        return ts_stop->tv_sec - ts_start->tv_sec +
                (ts_stop->tv_nsec - ts_start->tv_nsec) * 1E-9;
}

void *
call_test_impl(void *_conf)
{
        thread_conf_t *conf = (thread_conf_t *)_conf;
        struct timespec ts_start, ts_stop;

        monotonic_time(&ts_start);
        conf->func(conf->thread_id, iterations, &shared_data);
        monotonic_time(&ts_stop);

        conf->run_time = time_diff(&ts_start, &ts_stop);

        return NULL;
}

static void
usage(FILE *out, const char *argv0)
{
        fprintf(out, "Usage: %s [OPTION]...\n"
                "\n"
                "Options:\n", argv0);

        fprintf(out,
                "\t-c IMPL\t\tUse critical section implementation IMPL, use the\n"
                "\t\t\t'help' IMPL to get a list of available implementations\n"
                "\t\t\t(Default: %s)\n",
                DEFAULT_CS_IMPL.name);

        fprintf(out,
                "\t-t TEST\t\tUse test implementation TEST, use the 'help' TEST\n"
                "\t\t\tto list available implementations. (Default: %s)\n",
                DEFAULT_TEST_IMPL.name);

        fprintf(out,
                "\t-i ITER\t\tRun ITER iterations (Default: %i)\n ",
                DEFAULT_ITERATIONS);

        fprintf(out, "\t-h\t\tDisplay usage\n");
}

static void
print_stats(thread_conf_t *threads)
{
        double run_time_sum = 0;
        int num_threads = test_impl->num_threads;

        printf("Statistics:\n");
        for (int i = 0; i < num_threads; i++) {
                thread_conf_t *t = &threads[i];
                printf("\tThread %i: %.4f s (%.4e iterations/s)\n",
                       i, t->run_time,
                       iterations / t->run_time);
                run_time_sum += t->run_time;
        }

        printf("\tAverage execution time: %.4f s\n"
               "\tAvergage iterations/second: %.4e\n",
               run_time_sum / num_threads,
               num_threads * iterations / run_time_sum);
}

static void
start_threads()
{
        int num_threads = test_impl->num_threads;
        thread_conf_t *threads;

        if (!(threads = malloc(sizeof(*threads) * num_threads)))
                abort();

        /* Spawn the test threads */
        for (int i = 0; i < num_threads; i++) {
                thread_conf_t *t = &threads[i];
                t->thread_id = i;
                t->func = test_impl->test[i];
                if (pthread_create(&t->thread, NULL,
                                   &call_test_impl, t))
                        abort();
        }
  
        /* Join the threads. Causes an implicit barrier since the
         * pthread_join() call waits until the exits. */
        for (int i = 0; i < num_threads; i++)
                pthread_join(threads[i].thread, NULL);

        print_stats(threads);
        printf("\n");

        free(threads);

        if (shared_data)
                printf("INCONSISTENCY after %d iterations - "
                       "data = %d, but started at 0\n",
                       iterations, shared_data);
        else
                printf("NO INCONSISTENCY after %d iterations\n",
                       iterations);
}

static int
set_test_impl(const char *name)
{
        if (!strcmp("help", name)) {
                printf("Test implementations:\n");
                for (test_impl_t **impl = test_impls; *impl; impl++) {
                        if ((**impl).desc)
                                printf("\t%s - %s\n",
                                       (**impl).name, (**impl).desc);
                        else
                                printf("\t%s\n", (**impl).name);
                }
                exit(EXIT_SUCCESS);
        } else {
                for (test_impl_t **impl = test_impls; *impl; impl++) {
                        if (!strcmp((**impl).name, name)) {
                                test_impl = *impl;
                                return 1;
                        }
                }
                fprintf(stderr,
                        "Invalid test implementation specified, use 'help' "
                        "to list available implementations.\n");
                return 0;
        }
}

static int
set_cs_impl(const char *name)
{
        if (!strcmp("help", name)) {
                printf("Critical section implementations:\n");
                for (critical_section_impl_t **impl = cs_impls; *impl; impl++) {
                        if ((**impl).desc)
                                printf("\t%s - %s\n",
                                       (**impl).name, (**impl).desc);
                        else
                                printf("\t%s\n", (**impl).name);
                }
                exit(EXIT_SUCCESS);
        } else {
                for (critical_section_impl_t **impl = cs_impls; *impl; impl++) {
                        if (!strcmp((**impl).name, name)) {
                                cs_impl = *impl;
                                return 1;
                        }
                }
                fprintf(stderr,
                        "Invalid critical section implementation specified, "
                        "use 'help' to list available implementations.\n");
                return 0;
        }
}

/**
 * The main procedure parses arguments to get the incrementation
 * count, spawns 2 threads and waits for them to finish.  It then
 * checks whether the shared variable is has returned to its original
 * value.
 */
int
main(int argc, char *argv[])
{
        int opt;
        int errexit = 0;
        extern char *optarg;
        extern int optind, optopt;

        while ((opt = getopt(argc, argv, "c:t:i:h")) != -1) {
                switch (opt) {
                case 'c':
                        if (!set_cs_impl(optarg))
                                errexit = 1;
                        break;

                case 't':
                        if (!set_test_impl(optarg))
                                errexit = 1;
                        break;

                case 'i':
                        iterations = atoi(optarg);
                        if(iterations <= 0){
                                fprintf(stderr,
                                        "Invalid argument '%s'. Number of iterations "
                                        "must be positive.\n",
                                        optarg);

                                errexit = 1;
                        }
                        break;

                case 'h':
                        usage(stdout, argv[0]);
                        exit(EXIT_SUCCESS);

                case ':':
                case '?':
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

        if (cs_impl->init)
                cs_impl->init(test_impl->num_threads);

        enter_critical = cs_impl->enter;
        exit_critical = cs_impl->exit;

        printf("Configuration:\n"
               "\tTest implementation: %s\n"
               "\tCritical sections implementation: %s\n"
               "\tIterations: %i\n"
               "\n",
               test_impl->name, cs_impl->name, iterations);

        start_threads();

        if (cs_impl->fini)
                cs_impl->fini();

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

