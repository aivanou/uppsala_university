/* 
 * File:   QuickSort.c
 * Author: tierex
 *
 * Created on February 9, 2014, 3:36 PM
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <pthread.h>
#include <sys/time.h>
#include <time.h>


int timer(void);


struct dev_conq_data {
    int* array;
    int length;
    int depth;
    int right;
    int left;
};

struct peer_data {
    int* array;
    int length;
    int right;
    int left;
    int th_id;
    pthread_barrier_t barr;
    int iterations;
    int nthreads;
};

typedef struct dev_conq_data dev_conq_data_t;
typedef struct peer_data peer_data_t;

pthread_barrier_t pbarr;

void swap(int*, int*);
void qsort_impl(int*, int, int);
int quick_sort_serial(int*, int);
int random_in_range(unsigned int, unsigned int);
int* gen_random_array(int);
void print_arr(int*, int);

void* quick_sort_dev_conq(void* args);
void run_quick_sort_dc(int* input, int length);

void* quick_sort_peer_worker(void* args);
void quick_sort_peer(int* input, int length, int nthreads);

int main(int argc, char** argv) {


    random();

    int len = 200000;
    int* arr = gen_random_array(len);
    print_arr(arr, len);
    //    quick_sort_serial(arr, len);
    //    run_quick_sort_dc(arr, len);
    quick_sort_peer(arr, len, 4);
       print_arr(arr, len);



    return (EXIT_SUCCESS);
}

void* quick_sort_peer_worker(void* args) {
    peer_data_t* data = (peer_data_t*) args;
    int i;
    qsort_impl(data->array, data->left, data->right);
    for (i = 0; i < data->iterations; ++i) {
        int left = data->left;
        int right = data->right;
        if (data->th_id == data->nthreads - 2) {
            right = data->length;
        } else right += data->length / data->nthreads;

        if (i % 2 == 0 && data->th_id % 2 == 0 && data->th_id != data->nthreads - 1) {
            qsort_impl(data->array, left, right);
        } else if (i % 2 == 1 && data->th_id % 2 == 1 && data->th_id != data->nthreads - 1) {
            qsort_impl(data->array, left, right);
        }
        pthread_barrier_wait(&pbarr);
    }
}

void quick_sort_peer(int* input, int length, int nthreads) {
    pthread_t threads[nthreads];
    pthread_barrier_init(&pbarr, NULL, nthreads);
    int i = 0, bucket = length / nthreads;
    for (i = 0; i < nthreads; ++i) {
        peer_data_t* data = (peer_data_t*) malloc(sizeof (peer_data_t));
        data->left = i*bucket;
        data->right = (i + 1) * bucket;
        if (i == nthreads - 1) {
            data->right = length;
        }
        data->th_id = i;
        data->array = input;
        data->length = length;
        //        data->barr = pbarr;
        data->iterations = nthreads;
        data->nthreads = nthreads;
        pthread_create(&threads[i], NULL, quick_sort_peer_worker, (void*) data);
    }
    for (i = 0; i < nthreads; ++i) {
        pthread_join(threads[i], NULL);
    }
    pthread_barrier_destroy(&pbarr);
}

void run_quick_sort_dc(int* input, int length) {
    dev_conq_data_t* data = (dev_conq_data_t*) malloc(sizeof (dev_conq_data_t));
    data->array = input;
    data->length = length;
    data->left = 0;
    data->right = length;
    data->depth = 2;
    quick_sort_dev_conq((void*) data);
}

void* quick_sort_dev_conq(void* args) {

    dev_conq_data_t* data = (dev_conq_data_t*) args;

    if (data->left >= data->right) return;
    if (data->depth == 0) {
        qsort_impl(data->array, data->left, data->right);
        return;
    }
    int pivot = data->array[data->right - 1];
    int st_index = data->left, i = 0;
    for (i = data->left; i < data->right - 1; ++i) {
        if (data->array[i] <= pivot) {
            swap(&data->array[i], &data->array[st_index]);
            st_index += 1;
        }
    }
    swap(&data->array[data->right - 1], &data->array[st_index]);
    pthread_t left, rigth;
    dev_conq_data_t* left_data = (dev_conq_data_t*) malloc(sizeof (dev_conq_data_t));
    dev_conq_data_t* rigth_data = (dev_conq_data_t*) malloc(sizeof (dev_conq_data_t));
    left_data->array = data->array;
    left_data->depth = data->depth - 1;
    left_data->left = left;
    left_data->right = st_index;
    left_data->length = data->length;

    rigth_data->array = data->array;
    rigth_data->depth = data->depth - 1;
    rigth_data->left = st_index + 1;
    rigth_data->right = data->right;
    rigth_data->length = data->length;

    pthread_create(&left, NULL, quick_sort_dev_conq, (void*) left_data);
    pthread_create(&rigth, NULL, quick_sort_dev_conq, (void*) rigth_data);
    pthread_join(left, NULL);
    pthread_join(rigth, NULL);
    free(data);


}

int quick_sort_serial(int* input, int length) {
    qsort_impl(input, 0, length);
    return 1;
}

void qsort_impl(int* input, int left, int right) {
    if (left >= right) return;
    int pivot = input[right - 1];
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

void swap(int* a, int*b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int* gen_random_array(int length) {
    int* arr = (int*) malloc(length * sizeof (int));
    int i;
    for (i = 0; i < length; ++i) {
        arr[i] = random_in_range(0, length * 10);
    }
    return arr;
}

int random_in_range(unsigned int min, unsigned int max) {
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
    } else {
        return random_in_range(min, max);
    }
}

void print_arr(int* arr, int len) {
    int i;
    for (i = 0; i < len; ++i) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

int timer(void)
{
  struct timeval tv;
  gettimeofday(&tv, (struct timezone*)0);
  return (tv.tv_sec*1000000+tv.tv_usec);
}
