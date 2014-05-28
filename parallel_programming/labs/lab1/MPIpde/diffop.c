#include "wave.h"

void diffop(double *du, double *u,int NNx,int NNy,
	    int nreq, int lnx, int lny, int stride,
	    MPI_Request *reqarr, int *coords,int *dims)
{
  int i,j,Nx,Ny;
  MPI_Status mpistat[8];
  Nx=lnx+2; Ny=lny+2;

  // Start up all the communication
  MPI_Startall(nreq,reqarr);

  // Differentiate inner points while waiting for the communication 
  for (i=2; i<Nx-2; i++)
    for (j=2; j<Ny-2; j++)
      du[i*stride+j]=(u[(i+1)*stride+j]-u[(i-1)*stride+j])/2.0*NNx+
	(u[i*stride+j+1]-u[i*stride+j-1])/2.0*NNy;

  // Wait for the communication to complete
  MPI_Waitall(nreq,reqarr,mpistat);

  // Differentiate inner boundaries
  if (coords[0]>0)
    {
      for (j=1; j<Ny-1; j++)
	du[1*stride+j]=(u[2*stride+j]-u[j])/2.0*NNx+
	  (u[1*stride+j+1]-u[1*stride+j-1])/2.0*NNy;
    }
  if (coords[0]<(dims[0]-1))
    {
      for (j=1; j<Ny-1; j++)
	du[(Nx-2)*stride+j]=(u[(Nx-1)*stride+j]-u[(Nx-3)*stride+j])/2.0*NNx+
	  (u[(Nx-2)*stride+j+1]-u[(Nx-2)*stride+j-1])/2.0*NNy;
    }
  if (coords[1]>0)
    {
      for (i=1; i<Nx-1; i++)
	du[i*stride+1]=(u[(i+1)*stride+1]-u[(i-1)*stride+1])/2.0*NNx+
	  (u[i*stride+2]-u[i*stride])/2.0*NNy;
    }
  if (coords[1]<(dims[1]-1))
    {
      for (i=1; i<Nx-1; i++)
	du[i*stride+Ny-2]=(u[(i+1)*stride+Ny-2]-u[(i-1)*stride+Ny-2])/2.0*NNx+
	  (u[i*stride+Ny-1]-u[i*stride+Ny-3])/2.0*NNy;
    }

}
