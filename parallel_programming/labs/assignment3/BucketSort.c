/* 
 * File:   BucketSort.c
 * Author:
 *       Christos Sakalis Christos.Sakalis.3822@student.uu.se 
 *       Aliaksandr Ivanou  Aliaksandr.Ivanou.1364@student.uu.se
 *
 * this file represents bucket sort algorithm with different load-balancing strategies
 * Created on March 2, 2014, 3:03 PM
 */

#include <omp.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>

struct bucket_struct {
    double* array;
    int length;
    double min;
    double max;
};

typedef struct bucket_struct bucket_t;


void sort_pragma_schedule_static(bucket_t** buckets, int nbucket);
void sort_pragma_schedule_dynamic(bucket_t** buckets, int nbucket);
void sort_pragma_schedule_guided(bucket_t** buckets, int nbucket);
void sort_pragma_task(bucket_t** buckets, int nbucket);
void sort_runtime(bucket_t** buckets, int nbucket);
void sort_nested_parallelism(bucket_t** buckets, int nbucket);
void sort_binbucket(bucket_t** buckets, int nbucket);

void sort_pragma_task_worker(bucket_t** buckets, int nbucket, int ind);

void scatter(bucket_t** buckets, int nbuckets, const double* array, int len);

void sort_bucket(bucket_t* bucket);
void qsort_impl(double* input, int left, int right);
int random_in_range(unsigned int min, unsigned int max);

double* gen_drand(int length);
double* gen_normal_arr(int length);
double* gen_random_array(int length);

void gen_normal(double mu, double sigma, double* dest1, double* dest2);

void print_arr(double* arr, int len);

void swap(double* a, double*b);
int find_max(double* arr, int len);

int verify(double* input_arr, double* output_arr, int len);
void print_help();
int timer(void);

int scheduler_value = 1;

int
main(int argc, char** argv)
{

    double* (*gen_data)(int);
    void (*sort)(bucket_t**, int);
    char gend_data_function_name[32], sort_function_name[32];
    double min_value = 0.0, max_value = 1.0;
    int v = 0;
    int nthreads = 1;
    int nbucket = 32;
    int len = 2000, i = 0;
    int gen_data_flag = 0;

    //parse command line arguments
    int c;
    while ((c = getopt(argc, argv, "vl:t:g:b:d:i:a:")) != -1) {
        if (c == 'l')
            len = atoll(optarg);
        else if (c == 'v')
            v = 1;
        else if (c == 't')
            nthreads = atoi(optarg);
        else if (c == 'g') {
            strcpy(gend_data_function_name, optarg);
        }
        else if (c == 'b') {
            nbucket = atoi(optarg);
        }
        else if (c == 'd') {
            scheduler_value = atoi(optarg);
        }
        else if (c == 'i') {
            min_value = atof(optarg);
        }
        else if (c == 'a') {
            max_value = atof(optarg);
        }
        else {
            print_help(argv[0]);
            return 1;
        }
    }


    if (strcmp(gend_data_function_name, "dr") == 0) {
        gen_data = gen_drand;
        gen_data_flag = 1;
    }
    else if (strcmp(gend_data_function_name, "bm") == 0) {
        gen_data = gen_normal_arr;
        gen_data_flag = 2;
    }
    else {
        print_help();
        return 1;
    }

    if (optind < argc) {
        strncpy(sort_function_name, argv[optind], 32);
    }
    else {
        print_help();
        return 1;
    }


    if (strcmp(sort_function_name, "pss") == 0) {
        sort = sort_pragma_schedule_static;
    }
    else if (strcmp(sort_function_name, "psd") == 0) {
        sort = sort_pragma_schedule_dynamic;
    }
    else if (strcmp(sort_function_name, "psg") == 0) {
        sort = sort_pragma_schedule_guided;
    }
    else if (strcmp(sort_function_name, "pt") == 0) {
        sort = sort_pragma_task;
    }
    else if (strcmp(sort_function_name, "bb") == 0) {
        sort = sort_binbucket;
    }
    else if (strcmp(sort_function_name, "rt") == 0) {
        sort = sort_runtime;
    }
    else if (strcmp(sort_function_name, "np") == 0) {
        sort = sort_nested_parallelism;
    }
    else {
        print_help();
        return 1;
    }

    double* arr = gen_data(len);
    float range_from;
    float range_to;
    float step = 0.0;
    range_from = min_value;
    step = (max_value - min_value) / (float) nbucket;
    range_to = range_from + step;
    bucket_t** buckets = (bucket_t**) malloc(nbucket * sizeof (bucket_t*));
    for (i = 0; i < nbucket; ++i) {

        bucket_t* bucket = (bucket_t*) malloc(sizeof (bucket_t));
        bucket->min = range_from;
        bucket->max = range_to;
        range_from = range_to;
        range_to += step;
        buckets[i] = bucket;
    }
    scatter(buckets, nbucket, arr, len);
    omp_set_num_threads(nthreads);
    int start_time = timer();
    sort(buckets, nbucket);
    printf("parallel execution took time: %f\n", (timer() - start_time) / 1000000.0);

    start_time = timer();
    qsort_impl(arr, 0, len);
    printf("usual quicksort took time: %f\n", (timer() - start_time) / 1000000.0);

    if (v == 1) {
        verify(arr, buckets[0]->array, len);
    }
    //    print_arr(buckets[0]->array, len);
    for (i = 0; i < nbucket; ++i) {
        free(buckets[i]);
    }
    free(buckets);
    return (EXIT_SUCCESS);
}

void
sort_pragma_schedule_static(bucket_t** buckets, int nbucket)
{
    int i;
#pragma omp parallel
    {
#pragma omp for schedule(static,scheduler_value) 
        for (i = 0; i < nbucket; ++i) {
            sort_bucket(buckets[i]);
        }
    }
}

void
sort_pragma_schedule_dynamic(bucket_t** buckets, int nbucket)
{
    int i;

#pragma omp parallel for private(i) schedule(dynamic,scheduler_value) 
    for (i = 0; i < nbucket; ++i) {
        sort_bucket(buckets[i]);
    }
}

void
sort_pragma_schedule_guided(bucket_t** buckets, int nbucket)
{
    int i;

#pragma omp parallel for private(i) schedule(guided,scheduler_value) 
    for (i = 0; i < nbucket; ++i) {
        sort_bucket(buckets[i]);
    }
}

void
sort_pragma_task(bucket_t** buckets, int nbucket)
{
    int i;
    bucket_t* bucket;
#pragma omp parallel 
    {
        sort_pragma_task_worker(buckets, nbucket, 0);
    }
#pragma omp taskwait
}

void
sort_pragma_task_worker(bucket_t** buckets, int nbucket, int ind)
{
    if (ind == nbucket) return;
    sort_bucket(buckets[ind]);
#pragma omp task
    sort_pragma_task_worker(buckets, nbucket, ind + 1);
}

void
sort_runtime(bucket_t** buckets, int nbucket)
{
    int i;
    omp_set_num_threads(nbucket);
#pragma omp parallel for private(i)
    for (i = 0; i < nbucket; ++i) {
        sort_bucket(buckets[i]);
    }
}



/***
 * we are starting n threads
 * in each thread we explicitly sorting next bucket
 */
omp_lock_t writelock;

void
sort_binbucket(bucket_t** buckets, int nbucket)
{
    int threads = omp_get_num_threads();
    int i = 0, current_bucket = 0;
    bucket_t * bucket;
    omp_init_lock(&writelock);
#pragma omp parallel shared(current_bucket)
    {
#pragma omp for private(bucket) 
        for (i = 0; i < threads; ++i) {
            while (current_bucket < nbucket) {
                omp_set_lock(&writelock);
                {
                    bucket = buckets[current_bucket];
                    current_bucket++;
                }
                omp_unset_lock(&writelock);
                sort_bucket(bucket);
            }
        }
    }
}

/***
 * we are starting N threads
 * in each thread, we divide bucket on smaller parts
 * then, in N threads we are sorting each part
 * at the end, we are gathering the sorted values
 */

void
sort_nested_parallelism(bucket_t** buckets, int nbucket)
{

    int i;

#pragma omp parallel 
    {
#pragma omp for 
        for (i = 0; i < nbucket; ++i) {
            int tid = omp_get_thread_num();
            bucket_t* bucket = buckets[i];
            /*represents N sorted arrays*/
            double** private_arrs = (double**) malloc(nbucket * sizeof (double*));
            int j;
#pragma omp parallel
            {
#pragma omp for
                for (j = 0; j < nbucket; ++j) {
                    int arr_size = 0;
                    if (j == nbucket - 1) {
                        private_arrs[j] = (double*) malloc((bucket->length - bucket->length / nbucket * j) * sizeof (double));
                        arr_size = bucket->length - bucket->length / nbucket * j;
                    }
                    else {
                        private_arrs[j] = (double*) malloc(bucket->length / nbucket * sizeof (double));
                        arr_size = bucket->length / nbucket;
                    }
                    memcpy(private_arrs[j], &bucket->array[bucket->length / nbucket * j], arr_size * sizeof (double));
                    qsort_impl(private_arrs[j], 0, arr_size);
                }
            }
            int* arr_indexes = (int*) malloc(nbucket * sizeof (int));
            memset(arr_indexes, 0, nbucket * sizeof (int));
            /*gather the sorted values from private_arrs*/
#pragma omp parallel
            {
#pragma omp for 
                for (j = 0; j < bucket->length; ++j) {
                    int k;
                    int min_el_index = 0;
                    double min_el = 1000000.0;
                    for (k = 0; k < nbucket; ++k) {
                        if (k == nbucket - 1 && arr_indexes[k] >= (bucket->length - bucket->length / nbucket * k)) continue;
                        else if (k != nbucket - 1 && arr_indexes[k] >= bucket->length / nbucket) continue;
                        if (private_arrs[k][arr_indexes[k]] < min_el) {
                            min_el = private_arrs[k][arr_indexes[k]];
                            min_el_index = k;
                        }
                    }
                    bucket->array[j] = min_el;
                    arr_indexes[min_el_index] += 1;
                }
            }
            for (j = 0; j < nbucket; ++j) {
                free(private_arrs[j]);
            }
            free(arr_indexes);
            free(private_arrs);
        }
    }
}

void
sort_bucket(bucket_t * bucket)
{
    qsort_impl(bucket->array, 0, bucket->length);
}

/***
 * sequentially scatter array to the buckets
 * in order to keep memory each bucket points on the same array, 
 * but different elements
 * 
 * of course, every bucket can have its own array, and then
 * we can parallelise this method
 */

void
scatter(bucket_t** buckets, int nbuckets, const double* array, int len)
{

    int i, j;
    buckets[0]->array = (double*) malloc(len * sizeof (double));
    for (i = 0; i < nbuckets; ++i) {
        buckets[i]->length = 0;
        for (j = 0; j < len; ++j) {
            if (i == 0 && buckets[i]->max > array[j]) {
                buckets[i]->array[buckets[i]->length] = array[j];
                buckets[i]->length++;
            }
            else if (i == nbuckets - 1 && buckets[i]->min <= array[j]) {
                buckets[i]->array[buckets[i]->length] = array[j];
                buckets[i]->length++;
            }
            else if (buckets[i]->min <= array[j] && buckets[i]->max > array[j]) {
                buckets[i]->array[buckets[i]->length] = array[j];
                buckets[i]->length++;
            }
        }
        if (i != nbuckets - 1) {
            buckets[i + 1]->array = &buckets[i]->array[buckets[i]->length];
        }
    }

}

void
qsort_impl(double* input, int left, int right)
{
    if (left >= right) return;
    double pivot = input[right - 1];
    int st_index = left, i = 0;
    for (i = left; i < right - 1; ++i) {
        if (input[i] <= pivot) {
            swap(&input[i], &input[st_index]);
            st_index += 1;
        }
    }
    swap(&input[right - 1], &input[st_index]);
    qsort_impl(input, left, st_index);
    qsort_impl(input, st_index + 1, right);
}

double*
gen_drand(int length)
{
    double* arr = (double*) malloc(length * sizeof (double));
    int i;

    for (i = 0; i < length; ++i) {
        arr[i] = drand48();
    }
    return arr;
}

/**
 * generate data according to http://en.wikipedia.org/wiki/Box_Muller_transform
 * @param length
 * @return 
 */
double*
gen_normal_arr(int length)
{
    double* arr = (double*) malloc(length * sizeof (double));

    int i;
    for (i = 0; i < length - length % 2; i += 2) {
        double v1, v2;
        gen_normal(0.0, 1.0, &v1, &v2);
        arr[i] = v1;
        arr[i + 1] = v2;
    }
    if (length % 2 == 1) {
        arr[length - 1] = 0.0;
    }
    return arr;
}

void
gen_normal(double mu, double sigma, double* dest1, double* dest2)
{

    double v1 = drand48();
    double v2 = drand48();

    *dest1 = sqrt(-2 * log(v1)) * sin(2 * M_PI * v2);
    *dest2 = sqrt(-2 * log(v1)) * cos(2 * M_PI * v2);

    *dest1 = mu + *dest1*sigma;
    *dest2 = mu + *dest2*sigma;

}

double*
gen_random_array(int length)
{
    double* arr = (double*) malloc(length * sizeof (double));
    int i;
    for (i = 0; i < length; ++i) {
        arr[i] = (double) random_in_range(0, length * 10);
    }
    return arr;
}

int
random_in_range(unsigned int min, unsigned int max)
{
    int base_random = rand(); /* in [0, RAND_MAX] */
    if (RAND_MAX == base_random) return random_in_range(min, max);
    /* now guaranteed to be in [0, RAND_MAX) */
    int range = max - min,
            remainder = RAND_MAX % range,
            bucket = RAND_MAX / range;
    /* There are range buckets, plus one smaller interval
       within remainder of RAND_MAX */
    if (base_random < RAND_MAX - remainder) {
        return min + base_random / bucket;
    }
    else {
        return random_in_range(min, max);
    }
}

void
print_arr(double* arr, int len)
{
    int i;
    for (i = 0; i < len; ++i) {
        printf("%.1f ", arr[i]);
    }
    printf("\n");
}

void
swap(double* a, double*b)
{
    double temp = *a;
    *a = *b;
    *b = temp;
}

int
find_max(double* arr, int len)
{
    int max = 0;
    int i;
    for (i = 0; i < len; ++i) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    return max;
}

int
verify(double* input_arr, double* output_arr, int len)
{
    int i;
    int errors = 0;
    double error = 0.01;
    for (i = 0; i < len; ++i) {
        if (fabs(input_arr[i] - output_arr[i]) > error) {
            printf("ERROR: %d %.3f %.3f \n", i, input_arr[i], output_arr[i]);
            errors += 1;
        }
    }
    printf("sorting algorithm has %d errors\n", errors);
    return 1;
}

void
print_help()
{
    printf("help \n\n");
    printf("-iNUM - min value for buckets (buckets are uniform from MIN to MAX value)\n");
    printf("-aNUM - max value for buckets (buckets are uniform from MIN to MAX value)\n");
    printf("-bNUM - amount of buckets\n");
    printf("-tNUM - amount of threads\n");
    printf("-lNUM - array length\n");
    printf("-gTYPE - where TYPE is dr(drang48()), or bm(Box-Muller method) \n");
    printf("-v - run with result verification\n");
    printf("-dNUM - parameter for the scheduler\n");
    printf("-h - print this useage \n");
    printf("Load balancing algorithms: \n");
    printf("Pragma schedule with static loop scheduling: pss \n");
    printf("Pragma schedule with dynamic loop scheduling: psd \n");
    printf("Pragma schedule with guided loop scheduling: psg \n");
    printf("Run time balancing: rt \n");
    printf("Pragma task: pt \n");
    printf("Bin bucket: bb \n");
    printf("Nested parallelism: np \n\n");

}

int
timer(void)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone*) 0);
    return (tv.tv_sec * 1000000 + tv.tv_usec);
}