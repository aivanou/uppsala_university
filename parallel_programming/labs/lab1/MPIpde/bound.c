#include "wave.h"

void bound(double *unew, double *uold, double t, double dt,
	   double dx, double dy, int nx1, int ny1, int nnx, 
	   int nny, int stride, MPI_Comm proc_grid)
{
  int i,j,Nx,Ny;
  int ndim=2,dims[2],periods[2],coords[2];
  double x,y;
  MPI_Cart_get(proc_grid,ndim,dims,periods,coords);

  // Physical boundary conditions
  if (coords[0]==0)
    for (j=1; j<=nny; j++) 
      {
	y=(j-1+ny1)*dy; 
	unew[1*stride+j]=h(y-2*t)+up(0.0,y);
      }
  if (coords[1]==0)
    for (i=1; i<=nnx; i++)
      {
	x=(i-1+nx1)*dx;
	unew[i*stride+1]=h(x-2*t)+up(x,0.0);
      }

  // Exact boundary conditions
  if (coords[0]==(dims[0]-1))
    for (j=1; j<=nny; j++) 
      {
	x=1.0; y=(j-1+ny1)*dy;
	unew[nnx*stride+j]=up(x,y)+h(x+y-2*t);
      }
  if (coords[1]==(dims[1]-1))
    for (i=1; i<=nnx; i++)
      {
	y=1.0; x=(nx1+i-1)*dx;
	unew[i*stride+nny]=up(x,y)+h(x+y-2*t);
      }
}

/*
  // Numerical boundary conditions (2:nd order accurate if 2dt=dx=dy) 
  // Interpolation along the characteristic
  x=1.0-0.5/Nx; 
  for (j=1; j<=Ny; j++) 
    {
      y=(j-0.5)/Ny;
      unew(Nx,j)=uold(Nx-1,j-1)+2*dt*F(x,y);
    }
  y=1.0-0.5/Ny;
  for (i=1; i<Nx; i++)
    {
      x=(i-0.5)/Nx; 
      unew(i,Ny)=uold(i-1,Ny-1)+2*dt*F(x,y);
    }
*/
