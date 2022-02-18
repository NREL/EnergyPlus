#include <stdio.h>
#include <mgl2/mpi.h>
#include <mpi.h>

int main(int argc, char *argv[])
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	// initialize MPI
	int rank=0, numproc=1;
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD,&numproc);
	MPI_Comm_rank(MPI_COMM_WORLD,&rank);
	if(rank==0)	printf("Use %d processes.\n", numproc);

	// initialize data similarly for all ranks
	mglData a(128,128);
	mglGraphMPI gr;
	// do the same plot for its own range
	char buf[64];
	sprintf(buf,"xrange %g %g",2.*rank/numproc-1,2.*(rank+1)/numproc-1);
	gr.Fill(a,"sin(2*pi*x)",buf);
	// plot data in each rank
	gr.Rotate(40,60);
	gr.Surf(a,"",buf);
	// collect information
	if(rank!=0)	gr.MPI_Send(0);
	else	for(int i=1;i<numproc;i++)	gr.MPI_Recv(i);

	if(rank==0)
	{
		gr.Box();	gr.Axis();		// some post processing
		gr.WritePNG("test.png");	// save result
	}
	sprintf(buf,"test%d.png",rank);
	gr.WritePNG(buf);

	MPI_Finalize();
	return 0;
}
