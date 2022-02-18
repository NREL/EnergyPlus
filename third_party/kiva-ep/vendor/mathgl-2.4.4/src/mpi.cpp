#include "mgl2/mpi.h"
#include "mgl2/canvas.h"
#include <mpi.h>
#define MCW		MPI_COMM_WORLD
#define TAG_DATA_Z	0
#define TAG_DATA_C	1
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mpi_send(HMGL gr, int id)
{
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(!g)	return;
	g->Finish();
	long n = g->GetWidth()*g->GetHeight();
	MPI_Send(g->Z,3*n,MPI_FLOAT,id,TAG_DATA_Z,MCW);
	MPI_Send(g->C,12*n,MPI_CHAR,id,TAG_DATA_C,MCW);
	MPI_Send(g->OI,n,MPI_INT,id,TAG_DATA_C,MCW);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mpi_recv(HMGL gr, int id)
{
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(!g)	return;
	g->Finish();
	MPI_Status status;
	long w = g->GetWidth(), h = g->GetHeight(), n = w*h;
	float *zz = new float[3*n];
	int *oi = new int[n];
	unsigned char *cc = new unsigned char[12*n];
	MPI_Recv(zz,3*n,MPI_FLOAT,id,TAG_DATA_Z,MCW,&status);
	MPI_Recv(cc,12*n,MPI_CHAR,id,TAG_DATA_C,MCW,&status);
	MPI_Recv(oi,n,MPI_INT,id,TAG_DATA_C,MCW,&status);
	// NOTE: No need for check errors. The matter is MPI docs:
	// "All MPI routines return an error value. Before the value is returned,
	// the current MPI error handler is called. By default, this error handler aborts the MPI job."
#pragma omp parallel for
	for(long k=0;k<n;k++)
	{	// i0=x+Width*(Height-1-y)
		long i = k%w, j = h-1-(k/w);
		if(g->GetQuality()&MGL_DRAW_NORM)
		{
			g->pnt_plot(i,j,zz[3*k+2],cc+12*k+8,oi[k]);
			g->pnt_plot(i,j,zz[3*k+1],cc+12*k+4,oi[k]);
		}
		g->pnt_plot(i,j,zz[3*k],cc+12*k,oi[k]);
	}
	g->set(MGL_FINISHED);
	delete []oi; 	delete []zz; 	delete []cc;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mpi_send_(uintptr_t *gr, int *id)	{	mgl_mpi_send(_GR_, *id);	}
void MGL_EXPORT mgl_mpi_recv_(uintptr_t *gr, int *id)	{	mgl_mpi_recv(_GR_, *id);	}
//-----------------------------------------------------------------------------
