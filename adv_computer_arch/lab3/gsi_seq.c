/**
 * Sequential Gauss-Seidel implementation.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 3
 *
 * Original author: Frédéric Haziza <daz@it.uu.se>
 * Heavily modified by: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: gsi_seq.c 1023 2011-08-23 15:07:12Z ansan501 $
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "gs_interface.h"

const int gsi_is_parallel = 0;

void
gsi_init()
{
        gs_verbose_printf("\t****  Initializing the environment ****\n");
}

void
gsi_finish()
{
        gs_verbose_printf("\t****  Cleaning environment ****\n");
}

/**
 * Computing routine for each element: That's a whole sweep
 *
 * \return Error size
 */
static double
sweep()
{
        double error = 0.0;

        for (int i = 1; i < gs_size - 1; i++) {
                for (int j = 1; j < gs_size - 1; j++) {
                        double new_value = 0.25 * (
                                gs_matrix[GS_INDEX(i + 1, j)] +
                                gs_matrix[GS_INDEX(i - 1, j)] +
                                gs_matrix[GS_INDEX(i, j + 1)] +
                                gs_matrix[GS_INDEX(i, j - 1)]);

                        /* Accumulate the solution error, we do this
                         * while computing the new solution to avoid
                         * having to store both the new and old
                         * solution. Also avoids an additional
                         * sweep. */
                        error += fabs(gs_matrix[GS_INDEX(i, j)] - new_value);
                        gs_matrix[GS_INDEX(i, j)] = new_value;
                }
        }

        return error;
}

/**
 *  Wrapper for the whole job. Fires the sweep a given number of time.
 */
void
gsi_calculate()
{
        int i;
        double error = gs_tolerance + 1;

        for(i = 0; i < gs_iterations && error > gs_tolerance; i++) {
                error = sweep();
                gs_verbose_printf("Iteration: %i, Error: %f\n", i, error);
        }

        if (error <= gs_tolerance)
                printf("Solution converged after %i iterations.\n", i);
        else
                printf("Reached maximum number of iterations. Solution did "
                       "NOT converge. Error: %f  GS_tolerance: %f \n",error,gs_tolerance);
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
