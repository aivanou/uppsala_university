#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

struct row {
    int *row_data;

};

struct matrix {
    struct row * rows;
};

typedef struct matrix matrix_t;
typedef struct row row_t;

void (* some_func)(int id);

void print_some_func(int id) {
    printf("%d \n", id);
}

typedef struct {
    void(*some)(int id);
} test_struct;

int main() {
    int a=110;
    int *b=&a;
    int **c=&b;
    printf("%d\n",**c);
    return 0;
}

void mm() {
    for (int i = 0; i < 10; ++i) {
        random();
        int x = random() / (double) RAND_MAX;
        printf("%d\n", x);
    }
}

void matrix_creation() {
    int size = 100;
    matrix_t* m = (matrix_t*) malloc(sizeof (matrix_t));

    m->rows = (row_t*) malloc(size * sizeof (row_t));
    for (int i = 0; i < size; ++i) {
        m->rows[i].row_data = (int*) malloc(size * sizeof (int));
    }

    for (int i = 0; i < size; ++i) {
        for (int j = 0; j < size; ++j) {
            m->rows[i].row_data[j] = i + j;
        }
    }

    for (int i = 0; i < size; ++i) {
        for (int j = 0; j < size; ++j) {
            printf("%d  ", m->rows[i].row_data[j]);
        }
        printf("\n");
    }


    for (int i = 0; i < size; ++i) {
        free(m->rows[i].row_data);
    }
    free(m->rows);
    free(m);
}