#include "wave.h"

double residual(double *u,double t,double dx, double dy, int nx1,
		int ny1, int nnx, int nny, int stride,MPI_Comm proc_grid)
{
  int Nx,Ny,i,j;
  double sum,x,y,v,buf;

  sum=0.0; 
  for (i=1; i<=nnx; i++)
    for (j=1; j<=nny; j++)
      {
        x=(nx1+i-1)*dx; y=(j-1+ny1)*dy;
        v=h(x+y-2*t)+up(x,y);
        sum=sum+(u[i*stride+j]-v)*(u[i*stride+j]-v);
      }
  MPI_Allreduce(&sum,&buf,1,MPI_DOUBLE,MPI_SUM,proc_grid);
  return sqrt(buf*dx*dy);
}
