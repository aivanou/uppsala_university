/* 
 * File:   kmeans.c
 * Author: Aliaksandr Ivanou
 *
 * This file represents the implementation of k-means clustering using MPI libraries
 * Note: The preferable amount of centroids should be greater than the amount of processors
 * Created on March 10, 2014, 9:16 AM
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include <mpi.h>

#include "file_work.h"

struct feature {
    int centroid_id;
    int ndims;
    double min_distance;
    double *vector;
};

typedef struct feature feature_t;

/** reads file using mpi, each processor gains its own chunk of data */
feature_t* read_file(char* filename, int total_processes, int grid_rank, int* npoints, int* nfeatrues, int* max_points_per_processor);
char** split_string(char* string, char delim, int* nfeatures);
feature_t* parse_string_line(char* line);

void find_nearest_centroids(feature_t* points, int npoints, feature_t* centroinds, int ncentroids);
void update_centroids(feature_t* points, int npoints, feature_t* centroids, int ncentroids);
double euklidian_distance(double* v1, double* v2, int length);

feature_t* build_centroids(double* centroids, int* centroid_ids, int ncentroids, int nfeatures);
double** generate_centroids(int ncentroids, int nfeatures);

/**builds array of values, where each values corresponds to the amount of centroids assigned to the processor*/
int* build_ncentroids_array_scatter(int row_size, int col_size, int centroids_per_group, int ncentroids);
double* build_centroids_array_scatter(int row_size, int col_size, int nfeatures, int max_centroids_per_group,
                                      int* ncentroids_per_group_scatter, double** centroids, int** centroids_id_array);

int compare_centroid_coords(feature_t* centroids, double* coords, int ncentroids, int nfeatures);

void shift_points(MPI_Comm comm_group, int rank, int group_size, feature_t* points, int npoints, int nfeatures);
void pack(int npoints, int nfeatures, double* vector, int* centroid_ids, double* min_distances, feature_t* points);
void unpack(feature_t* points, int npoints, int nfeatures, double** vector, int** centroid_ids, double** min_distances);

void merge_centroids(feature_t* centroids, double* alien_centroids, int ncentroids, int nfeatures);

double* unpack_centroids(feature_t* centroids, int ncentroids, int nfeatures);

void print_points(feature_t* points, int npoints);

int
main(int argc, char** argv)
{

    char* data_file = "mpi_input.bin";
    int i, j, k;
    int rank, grid_rank;
    int total_processes = 0;

    MPI_Status status;

    int row_rank, row_size;
    int col_rank, col_size;
    int ncentroids;
    if (argc == 1) {
        ncentroids = 9;
    }
    else {
        ncentroids = atoi(argv[1]);
    }

    MPI_Init(&argc, &argv); /* Initialize MPI               */

    MPI_Comm_size(MPI_COMM_WORLD, &total_processes);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm comm_grid, comm_row, comm_col;
    int coords[2], dims[2] = {total_processes / 2, 2}, periods[2] = {0, 0};


    // Create a 2D grid topology
    //    MPI_Dims_create(total_processes, 2, dims);

    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &comm_grid);
    MPI_Comm_rank(comm_grid, &grid_rank);
    MPI_Cart_coords(comm_grid, grid_rank, 2, coords);
    dims[0] = 1;
    dims[1] = 0;
    MPI_Cart_sub(comm_grid, dims, &comm_row);
    dims[0] = 0;
    dims[1] = 1;
    MPI_Cart_sub(comm_grid, dims, &comm_col);
    MPI_Cart_sub(comm_grid, dims, &comm_col);
    MPI_Comm_rank(comm_row, &row_rank);
    MPI_Comm_rank(comm_col, &col_rank);
    MPI_Comm_size(comm_row, &row_size);
    MPI_Comm_size(comm_col, &col_size);


    int plength = 0, nfeatures = 0, max_points_per_processor = 0;
    feature_t* points = read_file(data_file, total_processes, grid_rank, &plength, &nfeatures, &max_points_per_processor);

    plength = max_points_per_processor;
    int centroids_per_group, mcentroids_per_group;
    double** centroids;
    int* centroids_id_scatter;
    int* ncentroids_per_group_scatter, *max_centroids_per_group;
    double* scatter_centroids;
    if (grid_rank == 0) {

        centroids = generate_centroids(ncentroids, nfeatures);

        centroids_per_group = ncentroids / row_size;

        ncentroids_per_group_scatter = build_ncentroids_array_scatter(row_size, col_size, centroids_per_group, ncentroids);

        max_centroids_per_group = (int*) malloc((total_processes) * sizeof (int));

        for (i = 0; i < total_processes; ++i) {
            max_centroids_per_group[i] = ncentroids_per_group_scatter[0];
        }

        scatter_centroids = build_centroids_array_scatter(row_size, col_size, nfeatures, max_centroids_per_group[0],
                                                          ncentroids_per_group_scatter, centroids, &centroids_id_scatter);
    }

    int rmax_centroids, rncentroids;

    MPI_Scatter(max_centroids_per_group, 1, MPI_INT, &rmax_centroids, 1, MPI_INT, 0, comm_grid);
    MPI_Scatter(ncentroids_per_group_scatter, 1, MPI_INT, &rncentroids, 1, MPI_INT, 0, comm_grid);
    double* rcentroids = (double*) malloc(nfeatures * rmax_centroids * sizeof (double));
    int* rcentroids_ids = (int*) malloc(rmax_centroids * sizeof (int));


    MPI_Scatter(centroids_id_scatter, rmax_centroids, MPI_INT, rcentroids_ids, rmax_centroids, MPI_INT, 0, comm_grid);
    MPI_Scatter(scatter_centroids, rmax_centroids * nfeatures, MPI_DOUBLE, rcentroids, rmax_centroids*nfeatures, MPI_DOUBLE, 0, comm_grid);

    feature_t* fcentroids = build_centroids(rcentroids, rcentroids_ids, rncentroids, nfeatures);

    int is_coords_changed = -1, is_finished = -1;

    /**basically, it is the k-mean clustering algorithm*/

    while (is_finished == -1) {

        int l;
        for (l = 0; l < row_size; ++l) {
            find_nearest_centroids(points, plength, fcentroids, rncentroids);

            shift_points(comm_row, row_rank, row_size, points, plength, nfeatures);
        }

        double* prev_centroid_coords = (double*) malloc(rncentroids * nfeatures * sizeof (double));
        for (i = 0; i < rncentroids; ++i) {
            for (j = 0; j < nfeatures; ++j) {
                prev_centroid_coords[i * nfeatures + j] = fcentroids[i].vector[j];
            }
            memset(fcentroids[i].vector, 0, fcentroids[i].ndims * sizeof (double));
        }

        for (l = 0; l < row_size; ++l) {
            update_centroids(points, plength, fcentroids, rncentroids);

            shift_points(comm_row, row_rank, row_size, points, plength, nfeatures);

        }

        double* centroids_coords_send = unpack_centroids(fcentroids, rncentroids, nfeatures);
        double* centroids_coords_recv = (double*) malloc(rncentroids * nfeatures * sizeof (double));

        for (i = 0; i < col_size; ++i) {
            MPI_Sendrecv(centroids_coords_send, rncentroids*nfeatures, MPI_DOUBLE, (col_rank + 1) % col_size, 0,
                         centroids_coords_recv, rncentroids*nfeatures, MPI_DOUBLE, (col_rank - 1) % col_size, 0, comm_col, &status);

            merge_centroids(fcentroids, centroids_coords_recv, rncentroids, nfeatures);
            for (i = 0; i < rncentroids * nfeatures; ++i) {
                centroids_coords_send[i] = centroids_coords_recv[i];
            }
        }


        is_coords_changed = compare_centroid_coords(fcentroids, prev_centroid_coords, rncentroids, nfeatures);

        int* is_coords_changed_buf = (int*) malloc(total_processes * sizeof (int));

        MPI_Allgather(&is_coords_changed, 1, MPI_INT, is_coords_changed_buf, 1, MPI_INT, comm_grid);

        is_finished = 1;
        for (i = 0; i < total_processes; ++i) {
            if (is_coords_changed_buf[i] == -1) {
                is_finished = -1;
                break;
            }
        }

        free(prev_centroid_coords);
        free(centroids_coords_send);
        free(centroids_coords_recv);
        free(is_coords_changed_buf);
    }

    int* ncentroids_gather = (int*) malloc(row_size * sizeof (int));

    double* final_centroids = (double*) malloc(row_size * nfeatures * rmax_centroids * sizeof (double));
    double* send_centroids = (double*) malloc(nfeatures * rmax_centroids * sizeof (double));

    for (i = 0; i < rncentroids; ++i) {
        for (j = 0; j < nfeatures; ++j) {
            send_centroids[i * nfeatures + j] = fcentroids[i].vector[j];
        }
    }
    for (i = rncentroids; i < rmax_centroids; ++i) {
        for (j = 0; j < nfeatures; ++j) {
            send_centroids[i * nfeatures + j] = -1;
        }
    }


    MPI_Gather(&rncentroids, 1, MPI_INT,
               ncentroids_gather, 1, MPI_INT, 0, comm_row);


    MPI_Gather(send_centroids, nfeatures*rmax_centroids, MPI_DOUBLE,
               final_centroids, rmax_centroids * nfeatures, MPI_DOUBLE, 0, comm_row);

    feature_t* res_centroids = (feature_t*) malloc(ncentroids * sizeof (feature_t));


    /**gather centroids from all groups*/
    if (grid_rank == 0) {
        int res_ind = 0, l = 0;
        for (i = 0; i < row_size; ++i) {
            for (j = 0; j < ncentroids_gather[i]; ++j) {
                feature_t* cent = (feature_t*) malloc(sizeof (feature_t));
                cent->vector = (double*) malloc(nfeatures * sizeof (double));
                for (l = 0; l < nfeatures; ++l) {
                    cent->vector[l] = final_centroids[rmax_centroids * i + j * nfeatures + l];
                }
                res_centroids[res_ind++] = *cent;
            }
        }

        for (i = 0; i < ncentroids; ++i) {
            printf("\n[%d] received:\n  ", 0);
            for (j = 0; j < nfeatures; ++j) {

                printf("%.2f  ", res_centroids[i].vector[j]);
            }
            printf("\n");
        }
    }

    MPI_Finalize();

    free(final_centroids);
    free(send_centroids);


    return (EXIT_SUCCESS);
}

void
merge_centroids(feature_t* centroids, double* alien_centroids, int ncentroids, int nfeatures)
{
    int i, j;
    for (i = 0; i < ncentroids; ++i) {
        for (j = 0; j < nfeatures; ++j) {

            centroids[i].vector[j] = alien_centroids[i * nfeatures + j];
        }
    }
}

double*
unpack_centroids(feature_t* centroids, int ncentroids, int nfeatures)
{
    double* cents = (double*) malloc(ncentroids * nfeatures * sizeof (double));
    int i, j;
    for (i = 0; i < ncentroids; ++i) {
        for (j = 0; j < nfeatures; ++j) {

            cents[i * nfeatures + j] = centroids[i].vector[j];
        }
    }
    return cents;
}

/**
 * 
 * @param centroids
 * @param coords -array of features, the lenght of array is ncentroids*nfeatures
 * @param ncentroids
 * @param nfeatures
 * @returns 1 if centroids have the same values as in coords, otherwise returns -1
 */
int
compare_centroid_coords(feature_t* centroids, double* coords, int ncentroids, int nfeatures)
{
    int i, j;
    double error = 0.01;
    for (i = 0; i < ncentroids; ++i) {
        for (j = 0; j < nfeatures; ++j) {
            if (fabs(centroids[i].vector[j] - coords[i * nfeatures + j]) > error) {

                return -1;
            }
        }
    }
    return 1;
}

/**
 * Shifts array of points to the next row, the same column(to the next group)
 * @param comm_group 
 * @param rank
 * @param group_size
 * @param points
 * @param npoints
 * @param nfeatures
 * 
 * returns points, gained from processor in another group
 */
void
shift_points(MPI_Comm comm_group, int rank, int group_size, feature_t* points, int npoints, int nfeatures)
{

    MPI_Status status;
    double* features_send;
    int* point_ids;
    double* min_distances;

    int npoints_recv, i, j;

    unpack(points, npoints, nfeatures, &features_send, &point_ids, &min_distances);


    MPI_Sendrecv(&npoints, 1, MPI_INT, (rank + 1) % group_size, 0,
                 &npoints_recv, 1, MPI_INT, (rank - 1) % group_size, 0, comm_group, &status);

    int* point_ids_recv = (int*) malloc(npoints_recv * sizeof (int));
    double *min_distances_recv = (double*) malloc(npoints_recv * sizeof (double));
    double* features_recv = (double*) malloc(npoints_recv * nfeatures * sizeof (double));

    MPI_Sendrecv(point_ids, npoints, MPI_INT, (rank + 1) % group_size, 0,
                 point_ids_recv, npoints_recv, MPI_INT, (rank - 1) % group_size, 0, comm_group, &status);

    MPI_Sendrecv(min_distances, npoints, MPI_DOUBLE, (rank + 1) % group_size, 0,
                 min_distances_recv, npoints, MPI_DOUBLE, (rank - 1) % group_size, 0, comm_group, &status);


    MPI_Sendrecv(features_send, npoints*nfeatures, MPI_DOUBLE, (rank + 1) % group_size, 1,
                 features_recv, npoints_recv*nfeatures, MPI_DOUBLE, (rank - 1) % group_size, 1, comm_group, &status);

    pack(npoints, nfeatures, features_recv, point_ids_recv, min_distances_recv, points);

    free(point_ids_recv);
    free(min_distances_recv);
    free(features_recv);
    free(point_ids);
    free(features_send);
    free(min_distances);

}

void
print_points(feature_t* points, int npoints)
{
    int i, j;
    for (i = 0; i < npoints; ++i) {
        printf("m_dist: %.2f  cent_id: %d  ", points[i].min_distance, points[i].centroid_id);
        for (j = 0; j < points[i].ndims; ++j) {

            printf(" %.2f ", points[i].vector[j]);
        }
    }
    printf("\n");
}

feature_t *
read_file(char* filename, int total_processes, int grid_rank, int* npoints, int* nfeatures, int* max_points_per_processor)
{
    MPI_File mpi_file;

    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &mpi_file);
    int lines;
    MPI_Status status;
    MPI_File_read(mpi_file, &lines, 1, MPI_INT, &status);

    int read_lines = lines / total_processes;
    int start_line = read_lines*grid_rank;
    int end_line = start_line + read_lines;
    if (grid_rank == total_processes - 1) {
        end_line = lines;
    }
    int i, pind, plength = (end_line - start_line), j;
    feature_t* points = (feature_t*) malloc(plength * sizeof (feature_t));
    for (i = start_line; i < end_line; ++i) {

        MPI_Offset line_offset = i * DATA_LENGTH + sizeof (MPI_INT);
        char* line = (char*) malloc(DATA_LENGTH * sizeof (char));
        MPI_File_read_at(mpi_file, line_offset, line, DATA_LENGTH, MPI_CHAR, &status);
        feature_t* point = parse_string_line(line);
        points[pind++] = *point;
    }

    MPI_File_close(&mpi_file);

    *max_points_per_processor = read_lines;
    *nfeatures = points[0].ndims;
    *npoints = plength;
    return points;
}

/***
 * Each row represents a group, with its own centroids
 * This function builds centroids array for each processor, 
 *      each row will contain centroids with the same id and coordinates, but different set of points
 * Input: 
 * @param row_size - amount of rows in topology(cols*rows = total number of processes)
 * @param col_size - amount of cols in topology(cols*rows = total number of processes)      
 * @param nfeatures - amount of features(coords) that each centroid row contains
 * @param max_centroids_per_group - max number of centroids per group ( since each group can
 * contain different amount of centroids, we should track of @max_centroids_per_group and @ncentroids_per_group_scatter
 * @param ncentroids_per_group_scatter - array, represents the amount of centroids for each processor
 * @param centroids - is a matrix, where each row is the set of features(coordinates)
 * 
 * Output: 
 *      @param scatter_centroids - contains centroid coordinates
 *      @param centroids_id_array - contains centroids ids(it is required for points)
 */
double*
build_centroids_array_scatter(int row_size, int col_size, int nfeatures, int max_centroids_per_group,
                              int* ncentroids_per_group_scatter, double** centroids, int** centroids_id_array)
{
    double* scatter_centroids = (double*) malloc(nfeatures * max_centroids_per_group * row_size * col_size * sizeof (double));
    int* centroids_id_scatter = (int*) malloc(max_centroids_per_group * row_size * col_size * sizeof (int));
    int i, j, k, l, ind = 0, id_ind = 0;
    for (i = 0; i < row_size; ++i) {
        for (j = 0; j < col_size; ++j) {
            for (k = 0; k < ncentroids_per_group_scatter[i * col_size + j]; ++k) {
                for (l = 0; l < nfeatures; ++l) {
                    scatter_centroids[ind] = centroids[i * ncentroids_per_group_scatter[i * col_size + j] + k][l];
                    ind += 1;
                }
                centroids_id_scatter[id_ind++] = i * ncentroids_per_group_scatter[i * col_size + j] + k;
            }
            int v;
            for (l = ncentroids_per_group_scatter[i * col_size + j]; l < max_centroids_per_group; ++l) {
                for (v = 0; v < nfeatures; ++v) {

                    scatter_centroids[ind++] = -1;
                }
                centroids_id_scatter[id_ind++] = -1;
            }
        }
    }
    *centroids_id_array = centroids_id_scatter;
    return scatter_centroids;
}

int*
build_ncentroids_array_scatter(int row_size, int col_size, int centroids_per_group, int ncentroids)
{
    int* ncentroids_per_group_scatter = (int*) malloc(row_size * col_size * sizeof (int));
    int i, j;
    for (i = 0; i < row_size; ++i) {
        for (j = 0; j < col_size; ++j) {
            ncentroids_per_group_scatter[i * col_size + j] = centroids_per_group;
        }
        if (i < ncentroids % row_size) {
            for (j = 0; j < col_size; ++j) {

                ncentroids_per_group_scatter[i * col_size + j] += 1;
            }
        }
    }
    return ncentroids_per_group_scatter;
}

double**
generate_centroids(int ncentroids, int nfeatures)
{
    int i, j;
    double** centroids = (double**) malloc(ncentroids * sizeof (double*));
    for (i = 0; i < ncentroids; ++i) {
        centroids[i] = (double*) malloc(nfeatures * sizeof (double));
        for (j = 0; j < nfeatures; ++j) {

            centroids[i][j] = drand48();
        }
    }
    return centroids;
}

/***
 * Unpacks points into the arrays
 * Input:
 * @param points 
 * @param npoints
 * @param nfeatures
 * output:
 * @param vector - pointer the to array of features
 * @param centroid_ids - pointer to the array of centroid ids
 * @param min_distance - pointer to the array of minimal distance to the centroid
 */

void
unpack(feature_t* points, int npoints, int nfeatures, double** vector, int** centroid_ids, double** min_distances)
{
    *vector = (double*) malloc(npoints * nfeatures * sizeof (double));
    *centroid_ids = (int*) malloc(npoints * sizeof (int));
    *min_distances = (double*) malloc(npoints * sizeof (double));
    int i, j;
    for (i = 0; i < npoints; ++i) {
        for (j = 0; j < nfeatures; ++j) {
            (
                    *vector)[i * nfeatures + j] = points[i].vector[j];
        }
        (*centroid_ids)[i] = points[i].centroid_id;
        (*min_distances)[i] = points[i].min_distance;
    }
}

/***
 * packs arrays into structure
 * input:
 * @param npoints
 * @param nfeatures
 * @param vector - array of features(length = nfeatures*npoints)
 * @param centroid_ids - array of centroid id that assigned to point
 * @param min_distances - array of min distances
 * output:
 * @param points -packed values, where for point I correspond values: 
 * vector[i*nfeatures - (i+1)*nfeatures], centroid_ids[i], min_distances[i]
 */

void
pack(int npoints, int nfeatures, double* vector, int* centroid_ids, double* min_distances, feature_t* points)
{
    int i, j;
    for (i = 0; i < npoints; ++i) {
        points[i].centroid_id = centroid_ids[i];
        points[i].ndims = nfeatures;
        points[i].min_distance = min_distances[i];
        for (j = 0; j < nfeatures; ++j) {
            points[i].vector[j] = vector[i * nfeatures + j];
        }
    }
}

feature_t *
build_centroids(double* centroids, int* centroid_ids, int ncentroids, int nfeatures)
{
    feature_t* fcentroids = (feature_t*) malloc(ncentroids * sizeof (feature_t));
    int i, j;
    for (i = 0; i < ncentroids; ++i) {
        feature_t* fcentroid = (feature_t*) malloc(sizeof (feature_t));
        fcentroid->ndims = nfeatures;
        fcentroid->centroid_id = centroid_ids[i];
        fcentroid->vector = (double*) malloc(nfeatures * sizeof (double));
        for (j = 0; j < nfeatures; ++j) {

            fcentroid->vector[j] = centroids[i * nfeatures + j];
        }
        fcentroids[i] = *fcentroid;
    }

    return fcentroids;
}

feature_t *
parse_string_line(char* line)
{
    int line_length = strlen(line);
    int nfeatures, i;
    char** svector = split_string(line, ',', &nfeatures);
    feature_t* point = (feature_t*) malloc(sizeof (feature_t));
    point->ndims = nfeatures - 1;
    point->vector = (double*) malloc(nfeatures * sizeof (double));
    point->centroid_id = -1;
    point->min_distance = -1;
    for (i = 0; i < point->ndims; ++i) {

        point->vector[i] = atof(svector[i]);
    }
    //free svector
    return point;
}

void
find_nearest_centroids(feature_t* points, int npoints, feature_t* centroinds, int ncentroids)
{
    int i, j;
    for (i = 0; i < npoints; ++i) {
        for (j = 0; j < ncentroids; ++j) {
            double curr_dist = euklidian_distance(points[i].vector, centroinds[j].vector, points[i].ndims);
            if (points[i].min_distance >= curr_dist || points[i].min_distance == -1) {

                points[i].min_distance = curr_dist;
                points[i].centroid_id = centroinds[j].centroid_id;
            }
        }
    }
}

void
update_centroids(feature_t* points, int npoints, feature_t* centroids, int ncentroids)
{
    int i, j, k;
    for (i = 0; i < ncentroids; ++i) {
        int npoints_in_centroid = 0;
        for (j = 0; j < npoints; ++j) {
            if (centroids[i].centroid_id == points[j].centroid_id) {
                for (k = 0; k < centroids[i].ndims; ++k) {
                    centroids[i].vector[k] += points[j].vector[k];
                }
                npoints_in_centroid += 1;
            }
        }
        for (k = 0; k < centroids[i].ndims; ++k) {
            if (npoints_in_centroid != 0) {
                centroids[i].vector[k] = centroids[i].vector[k] / (double) npoints_in_centroid;
            }
        }
    }
}

double
euklidian_distance(double* v1, double* v2, int length)
{
    double dist = 0.0;
    int i;
    for (i = 0; i < length; ++i) {

        dist += (v1[i] - v2[i])*(v1[i] - v2[i]);
    }
    return sqrt(dist);
}

int
get_number_features(char* string, int slength, char delim)
{
    int i;
    int nfeatures = 0;
    for (i = 0; i < slength; ++i) {
        if (string[i] == delim) {

            nfeatures += 1;
        }
    }
    return nfeatures;
}

char**
split_string(char* string, char delim, int* nfeatures)
{
    int str_length = strlen(string);
    int i;
    *nfeatures = get_number_features(string, str_length, delim) + 1;
    char** sfeatures = (char**) malloc(*nfeatures * sizeof (char*));
    char buffer[128];
    int curr_feature = 0, buffer_ind = 0;
    for (i = 0; i < str_length; ++i) {
        if (string[i] == ' ')
            continue;
        else if (string[i] == delim) {
            buffer[buffer_ind] = '\0';
            sfeatures[curr_feature] = (char*) malloc(strlen(buffer) * sizeof (char));
            strcpy(sfeatures[curr_feature++], buffer);
            memset(buffer, 0, 128 * sizeof (char));
            buffer_ind = 0;
            continue;
        }
        buffer[buffer_ind++] = string[i];
    }
    buffer[buffer_ind] = '\0';
    sfeatures[curr_feature] = (char*) malloc(strlen(buffer) * sizeof (char));
    strcpy(sfeatures[curr_feature++], buffer);
    return sfeatures;
}

