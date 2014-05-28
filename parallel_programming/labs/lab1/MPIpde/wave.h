// Funktionsdeklarationer

#ifndef wave_h
#define wave_h

#include <math.h>
#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>

double h(double z);
double F(double x, double y);
double up(double x, double y);
void diffop(double *du,double *u,int NNx,int NNy,
	    int nreq, int nnx, int nny, int stride,
	    MPI_Request *reqarr, int *coords,int *dims);
void bound(double *unew, double *uold, double t, double dt,
	   double dx, double dy, int nx1, int ny1, int nnx, 
	   int nny, int stride, MPI_Comm proc_grid);
double residual(double *u,double t,double dx, double dy, int nx1,
		int ny1,  int nnx, int nny, int stride, MPI_Comm proc_grid);
void init_comm(double *u, MPI_Comm proc_grid, 
	       int *nreq, int nnx, int nny, int stride,
	       MPI_Request *reqarr);

#endif
