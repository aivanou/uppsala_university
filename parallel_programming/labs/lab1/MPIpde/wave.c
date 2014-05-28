//     *********************************************************************
//     Programming high-performance parallel computers using message passing
//     Example wave.cc
//     
//     Solve 2D advection equation with forcing term numerically with
//     the Leap-frog scheme.
//     
//     Advanced example: Virtual topologies, persistent communication,
//                       derived datatypes
//
//     Author: Jarmo Rantakokko
//     Date:   2001-01-24
//
//     *********************************************************************

#include  "wave.h"

int main(int argc, char **argv)
{

  // Initialize MPI and create a 2D-grid topology.
  int myid,nproc,reorder=1,ndim=2,dims[2]={0,0},periods[2]={0,0};
  int coords[2];
  const int root=0;
  MPI_Comm proc_grid;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nproc);
  MPI_Dims_create(nproc,ndim,dims);
  MPI_Cart_create(MPI_COMM_WORLD,ndim,dims,periods,reorder,&proc_grid);
  MPI_Comm_rank(proc_grid,&myid);
  MPI_Cart_coords(proc_grid,myid,ndim,coords);

  // Variables
  int Nx,Ny,i,j,k,nnx,nny,rest,nx1,ny1,stride;
  double x,y,dt,t,norm,dx,dy,t1,t2,Nt;
  
  // Problem size
  if (argc!=3){
    if (myid==root)
      printf("Use: mpirun -np p wave Nx Ny\n");
    MPI_Finalize();
    return 0;
  }
      
  Nx= atoi(argv[1]);
  Ny= atoi(argv[2]);
  if (myid==root)
    printf("\nRunning: %s %s %s\n",argv[0],argv[1],argv[2]);
  dx=1.0/(Nx-1); dy=1.0/(Ny-1); dt=1.0/(Nx+Ny); Nt=1.0/dt;
 

  // Local arrays
  nnx=Nx/dims[0]; rest=Nx%dims[0];
  if (rest>coords[0]) 
    {
      nnx=nnx+1;
      nx1=nnx*coords[0]; 
    }
  else
    {
      nx1=rest*(nnx+1)+(coords[0]-rest)*nnx;
    }
  nny=Ny/dims[1]; rest=Ny%dims[1];
  if (rest>coords[1]) 
    {
      nny=nny+1;
      ny1=nny*coords[1]; 
    }
  else
    {
      ny1=rest*(nny+1)+(coords[1]-rest)*nny;
    }
  stride=nny+2;

  // Add ghost points (one on each side)
  double *uold=(double *)malloc((nnx+2)*(nny+2)*sizeof(double));
  double *u=(double *)malloc((nnx+2)*(nny+2)*sizeof(double));
  double *unew=(double *)malloc((nnx+2)*(nny+2)*sizeof(double));
  double *du=(double *)malloc((nnx+2)*(nny+2)*sizeof(double));
  double *u1=uold;
  double *u2=u;
  double *u3=unew;
  double *temp;

  // Initiate persistent communication objects
  MPI_Request *reqarr1=(MPI_Request *)malloc(8*sizeof(MPI_Request));
  MPI_Request *reqarr2=(MPI_Request *)malloc(8*sizeof(MPI_Request));
  MPI_Request *reqarr3=(MPI_Request *)malloc(8*sizeof(MPI_Request));
  MPI_Request *reqarrtemp;
  int n1,n2,n3;
  int *nreq1=&n1;
  int *nreq2=&n2;
  int *nreq3=&n3;
  int *nreqtemp;
  init_comm(uold,proc_grid,&n1,nnx,nny,stride,reqarr1);
  init_comm(u,proc_grid,&n2,nnx,nny,stride,reqarr2);
  init_comm(unew,proc_grid,&n3,nnx,nny,stride,reqarr3);

  // Initial values
  t1=MPI_Wtime(); 
  for (i=1; i<=nnx; i++)
    for (j=1; j<=nny; j++)
      {
	x=(double)(i-1+nx1)*dx; y=(double)(j-1+ny1)*dy;
	u[i*stride+j]=h(x+y)+up(x,y);
	unew[i*stride+j]=h(x+y-2*dt)+up(x,y);
      }


  // Integrate numerically with Leap-frog
  for (k=2; k<=Nt; k++)
    {
      temp=u1; u1=u2; u2=u3; u3=temp;
      reqarrtemp=reqarr1; reqarr1=reqarr2; 
      reqarr2=reqarr3; reqarr3=reqarrtemp;
      nreqtemp=nreq1; nreq1=nreq2; nreq2=nreq3; nreq3=nreqtemp;
      diffop(du,u2,(Nx-1),(Ny-1),*nreq2,nnx,nny,stride,reqarr2,coords,dims);
      for (i=1; i<=nnx; i++) 
	for (j=1; j<=nny; j++)
	  {
	    x=(double)(i-1+nx1)*dx; y=(double)(j-1+ny1)*dy;
	    u3[i*stride+j]=u1[i*stride+j]+2*dt*(F(x,y)-du[i*stride+j]);
	  }
      t=k*dt; 
      bound(u3,u1,t,dt,dx,dy,nx1,ny1,nnx,nny,stride,proc_grid);
    }

  // Residual
  norm=residual(u3,t,dx,dy,nx1,ny1,nnx,nny,stride,proc_grid);
  t2=MPI_Wtime();



  if (myid==root)
    {
      printf("-------------------------------\n");
      printf("Problem size = %dx%d\n", Nx, Ny);
      printf("Error norm   = %e\n",norm);
      printf("Wall time    = %f s\n",t2-t1);
      printf("Processors   = %dx%d\n",dims[0],dims[1]);
      printf("-------------------------------\n");
    }

  // Exit MPI
  MPI_Finalize();
}




