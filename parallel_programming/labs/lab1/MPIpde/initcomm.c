#include "wave.h"

void init_comm(double *u, MPI_Comm proc_grid, 
	       int *NNreq,  int nnx, int nny, int stride,
	       MPI_Request *reqarr)
{
  int Nx,Ny,nreq;
  int myid,ndim,dims[2],coords[2],periods[2];
  int p_left,p_right,p_up,p_down,tagx1=1,tagx2=2,tagy1=3,tagy2=4;

  // Topology information
  ndim=2;
  MPI_Comm_rank(proc_grid,&myid);
  MPI_Cart_get(proc_grid,ndim,dims,periods,coords);
  MPI_Cart_shift(proc_grid,0,1,&p_left,&p_right);
  MPI_Cart_shift(proc_grid,1,1,&p_up,&p_down);
  
  // Non-contigious data
  MPI_Datatype STRIDED;
  MPI_Type_vector(nnx,1,stride,MPI_DOUBLE,&STRIDED);
  MPI_Type_commit(&STRIDED);
  
  nreq=0;
  // Initiate send to left neighbor
  if (coords[0]>0) 
    {
      MPI_Send_init(&u[1*stride+1],nny,MPI_DOUBLE,p_left,tagx1,proc_grid,
		    &reqarr[nreq]);
      nreq++;
    }
  // initiate recieve from right and initiate send to right neighbor
  if (coords[0]<(dims[0]-1))
    {
      MPI_Recv_init(&u[(nnx+1)*stride+1],nny,MPI_DOUBLE,p_right,tagx1,proc_grid,
		    &reqarr[nreq]);
      nreq++;
      MPI_Send_init(&u[(nnx)*stride+1],nny,MPI_DOUBLE,p_right,tagx2,proc_grid,
		    &reqarr[nreq]); 
      nreq++;
    }
  // Initiate recieve from left
  if (coords[0]>0) 
    {
      MPI_Recv_init(&u[1],nny,MPI_DOUBLE,p_left,tagx2,proc_grid,
		    &reqarr[nreq]);
      nreq++;
    }
  // Initiate send to up neighbor
  if (coords[1]>0) 
    {
      MPI_Send_init(&u[1*stride+1],1,STRIDED,p_up,tagy1,proc_grid,
		    &reqarr[nreq]);
      nreq++;
    }
  // initiate recieve from down and initiate send to down neighbor
  if (coords[1]<(dims[1]-1))
    {
      MPI_Recv_init(&u[1*stride+nny+1],1,STRIDED,p_down,tagy1,proc_grid,
		    &reqarr[nreq]);
      nreq++;
      MPI_Send_init(&u[1*stride+nny],1,STRIDED,p_down,tagy2,proc_grid,
		    &reqarr[nreq]);
      nreq++;
    }
  // Initiate recieve from up
  if (coords[1]>0) 
    {
      MPI_Recv_init(&u[1*stride],1,STRIDED,p_up,tagy2,proc_grid,
		    &reqarr[nreq]);
      nreq++;
    }
  *NNreq=nreq;
}
