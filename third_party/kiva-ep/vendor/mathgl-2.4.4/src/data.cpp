/***************************************************************************
 * data.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 3 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <time.h>
#include "mgl2/data.h"
#include "mgl2/datac.h"
#include "mgl2/eval.h"
#include "mgl2/thread.h"
#include "interp.hpp"

MGL_EXPORT int mglNumThr=0;
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHREAD
#ifdef WIN32
#include <windows.h>
#include <process.h>
#elif defined(__APPLE__) || defined (__FreeBSD__)
#include <sys/sysctl.h>
#elif defined(unix) || defined(__unix) || defined(__unix__)
#include <sys/sysinfo.h>
#endif
void MGL_EXPORT mgl_set_num_thr(int n)
{
#ifdef WIN32
	SYSTEM_INFO systemInfo;
	GetSystemInfo(&systemInfo);
	mglNumThr = n>0 ? n : systemInfo.dwNumberOfProcessors;
#elif defined (__APPLE__) || defined(__FreeBSD__)
	int numProcessors = 1;
	size_t size = sizeof(numProcessors);
	sysctlbyname("hw.ncpu", &numProcessors, &size, NULL, 0);
	mglNumThr = n>0 ? n : numProcessors;
#else
	mglNumThr = n>0 ? n : get_nprocs_conf();
#endif
}
#else
void MGL_EXPORT mgl_set_num_thr(int)	{	mglNumThr = 1;	}
#endif
void MGL_EXPORT mgl_set_num_thr_(int *n)	{	mgl_set_num_thr(*n);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mglStartThread(void *(*func)(void *), void (*post)(mglThreadD *,mreal *), long n,
					mreal *a, const mreal *b, const mreal *c, const long *p,
					const void *v, const mreal *d, const mreal *e, const char *s)
{
	if(!func)	return;
#if MGL_HAVE_PTHREAD
	if(mglNumThr<1)	mgl_set_num_thr(0);
	if(mglNumThr>1)
	{
		pthread_t *tmp=new pthread_t[mglNumThr];
		mglThreadD *par=new mglThreadD[mglNumThr];
		for(long i=0;i<mglNumThr;i++)	// put parameters into the structure
		{	par[i].n=n;	par[i].a=a;	par[i].b=b;	par[i].c=c;	par[i].d=d;
			par[i].p=p;	par[i].v=v;	par[i].s=s;	par[i].e=e;	par[i].id=i;	}
		for(long i=0;i<mglNumThr;i++)	pthread_create(tmp+i, 0, func, par+i);
		for(long i=0;i<mglNumThr;i++)	pthread_join(tmp[i], 0);
		if(post)	post(par,a);
		delete []tmp;	delete []par;
	}
	else
#endif
	{
		mglNumThr = 1;
		mglThreadD par;
		par.n=n;	par.a=a;	par.b=b;	par.c=c;	par.d=d;
		par.p=p;	par.v=v;	par.s=s;	par.e=e;	par.id=0;
		func(&par);
		if(post)	post(&par,a);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mglStartThreadV(void *(*func)(void *), long n, mreal *a, const void *b,
					 const void *c, const long *p, const void *v, const mreal *d)
{
	if(!func)	return;
#if MGL_HAVE_PTHREAD
	if(mglNumThr<1)	mgl_set_num_thr(0);
	if(mglNumThr>1)
	{
		pthread_t *tmp=new pthread_t[mglNumThr];
		mglThreadV *par=new mglThreadV[mglNumThr];
		for(long i=0;i<mglNumThr;i++)	// put parameters into the structure
		{	par[i].n=n;	par[i].a=a;	par[i].b=b;	par[i].c=c;	par[i].d=d;
			par[i].p=p;	par[i].v=v;	par[i].id=i;par[i].aa=0;	}
		for(long i=0;i<mglNumThr;i++)	pthread_create(tmp+i, 0, func, par+i);
		for(long i=0;i<mglNumThr;i++)	pthread_join(tmp[i], 0);
		delete []tmp;	delete []par;
	}
	else
#endif
	{
		mglNumThr = 1;
		mglThreadV par;
		par.n=n;	par.a=a;	par.b=b;	par.c=c;	par.d=d;
		par.p=p;	par.v=v;	par.id=0;	par.aa=0;
		func(&par);
	}
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT_PURE mglSpline3s(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z)
{	return mglSpline3st<mreal>(a,nx,ny,nz,x,y,z);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mglSpline3(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z,mreal *dx, mreal *dy, mreal *dz)
{	return mglSpline3t<mreal>(a,nx,ny,nz,x,y,z,dx,dy,dz);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT_PURE mglLinear(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z)
{	return mglLineart<mreal>(a,nx,ny,nz,x,y,z);	}
//-----------------------------------------------------------------------------
double MGL_EXPORT_CONST mgl_ipow(double x,int n)
{
	double t;
	if(n==2)	return x*x;
	if(n==1)	return x;
	if(n<0)		return 1./mgl_ipow(x,-n);
	if(n==0)	return 1;
	t = mgl_ipow(x,n/2);	t = t*t;
	if(n%2==1)	t *= x;
	return t;
}
double MGL_EXPORT_PURE mgl_ipow_(mreal *x,int *n)	{	return mgl_ipow(*x,*n);	}
//-----------------------------------------------------------------------------
double mgl_get_time(const char *time, const char *fmt)
{
#if !defined(WIN32)
	tm t;
	strptime(time,fmt,&t);
	return timegm(&t);
#else
	return NAN;
#endif
}
double mgl_get_time_(const char *time, const char *fmt,int l,int m)
{	char *s=new char[l+1];	memcpy(s,time,l);	s[l]=0;
	char *f=new char[m+1];	memcpy(f,fmt,m); 	f[m]=0;
	double t=mgl_get_time(s,f);	delete []s;	delete []f;	return t;	}
//-----------------------------------------------------------------------------
static void *mgl_smth_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], kind=t->p[2];
	mreal *b=t->a, delta=t->c[0];
	const mreal *a=t->b;
	if(kind>0)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
			if(mgl_isnum(a[i]))	// bypass NAN values
			{
				long j = i%nx, nk = 2*kind+1;
				for(long k=-kind;k<=kind;k++)
					if(j+k>=0 && j+k<nx && mgl_isnum(a[i+k])) b[i] += a[i+k];	else nk--;
				b[i] /= nk;
			}	else	b[i] = a[i];
	else if(kind==-1)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i%nx;
			if(j>1 && j<nx-2)	b[i] = (12*a[i-2] - 3*a[i-1] + 17*a[i] - 3*a[i+1] + 12*a[i+2])/35.;
			else if(j==1 || j==nx-2)	b[i] = (a[i-1] + a[i] + a[i+1])/3.;
			else	b[i] = a[i];
		}
	else if(kind==-2)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i%nx;
			mreal v = (j>0 && j<nx-1) ? (a[i-1]+a[i+1])/2 : NAN;
			b[i] = v<a[i]?v:a[i];
		}
	else if(kind==-3)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i%nx;
			mreal v = (j>0 && j<nx-1) ? (a[i-1]+a[i+1])/2 : NAN;
			b[i] = v>a[i]?v:a[i];
		}
	if(delta>0)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			double ab = fabs(a[i]-b[i]);
			if(ab>delta)	b[i] = a[i]+(delta/ab)*(b[i]-a[i]);
		}
		return 0;
}
static void *mgl_smth_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0],ny=t->p[1], kind=t->p[2];
	mreal *b=t->a, delta=t->c[0];
	const mreal *a=t->b;
	if(kind>0)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
			if(mgl_isnum(a[i]))	// bypass NAN values
			{
				long j = (i/nx)%ny, nk = 2*kind+1;
				for(long k=-kind;k<=kind;k++)
					if(j+k>=0 && j+k<ny && mgl_isnum(a[i+k*nx])) b[i] += a[i+k*nx];	else nk--;
				b[i] /= nk;
			}	else	b[i] = a[i];
	else if(kind==-1)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = (i/nx)%ny;
			if(j>1 && j<ny-2)	b[i] = (12*a[i-2*nx] - 3*a[i-nx] + 17*a[i] - 3*a[i+nx] + 12*a[i+2*nx])/35.;
			else if(j==1 || j==ny-2)	b[i] = (a[i-nx] + a[i] + a[i+nx])/3.;
			else	b[i] = a[i];
		}
	else if(kind==-2)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = (i/nx)%ny;
			mreal v = (j>0 && j<ny-1) ? (a[i-nx]+a[i+nx])/2 : NAN;
			b[i] = v<a[i]?v:a[i];
		}
	else if(kind==-3)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = (i/nx)%ny;
			mreal v = (j>0 && j<ny-1) ? (a[i-nx]+a[i+nx])/2 : NAN;
			b[i] = v>a[i]?v:a[i];
		}
	if(delta>0)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			double ab = fabs(a[i]-b[i]);
			if(ab>delta)	b[i] = a[i]+(delta/ab)*(b[i]-a[i]);
		}
		return 0;
}
static void *mgl_smth_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nn=t->p[0]*t->p[1], nz=t->n/nn, kind=t->p[2];
	mreal *b=t->a, delta=t->c[0];
	const mreal *a=t->b;
	if(kind>1)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
			if(mgl_isnum(a[i]))	// bypass NAN values
			{
				long j = i/nn, nk = 2*kind+1;
				for(long k=-kind;k<=kind;k++)
					if(j+k>=0 && j+k<nz && mgl_isnum(a[i+k*nn])) b[i] += a[i+k*nn];	else nk--;
				b[i] /= nk;
			}	else	b[i] = a[i];
	else if(kind==-1)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i/nn;
			if(j>1 && j<nz-2)	b[i] = (12*a[i-2*nn] - 3*a[i-nn] + 17*a[i] - 3*a[i+nn] + 12*a[i+2*nn])/35.;
			else if(j==1 || j==nz-2)	b[i] = (a[i-nn] + a[i] + a[i+nn])/3.;
			else	b[i] = a[i];
		}
	else if(kind==-2)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i/nn;
			mreal v = (j>0 && j<nz-1) ? (a[i-nn]+a[i+nn])/2 : NAN;
			b[i] = v<a[i]?v:a[i];
		}
	else if(kind==-3)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long j = i/nn;
			mreal v = (j>0 && j<nz-1) ? (a[i-nn]+a[i+nn])/2 : NAN;
			b[i] = v>a[i]?v:a[i];
		}
	if(delta>0)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			double ab = fabs(a[i]-b[i]);
			if(ab>delta)	b[i] = a[i]+(delta/ab)*(b[i]-a[i]);
		}
	return 0;
}
void MGL_EXPORT mgl_data_smooth(HMDT d, const char *dirs, mreal delta)
{
	long Type = -1;
	if(mglchr(dirs,'0'))	return;
	bool xdir=mglchr(dirs,'x'), ydir=mglchr(dirs,'y'), zdir=mglchr(dirs,'z');
	if(!xdir && !ydir && !zdir)	xdir=ydir=zdir=true;
	if(mglchr(dirs,'d'))
	{
		if(mglchr(dirs,'1'))	Type = 1;
		if(mglchr(dirs,'2'))	Type = 2;
		if(mglchr(dirs,'3'))	Type = 3;
		if(mglchr(dirs,'4'))	Type = 4;
		if(mglchr(dirs,'5'))	Type = 5;
		if(mglchr(dirs,'6'))	Type = 6;
		if(mglchr(dirs,'7'))	Type = 7;
		if(mglchr(dirs,'8'))	Type = 8;
		if(mglchr(dirs,'9'))	Type = 9;
	}
	else
	{
		if(mglchr(dirs,'1'))	return;
		if(mglchr(dirs,'3'))	Type = 1;
		if(mglchr(dirs,'5'))	Type = 2;
	}
	if(mglchr(dirs,'_'))	Type = -2;
	if(mglchr(dirs,'^'))	Type = -3;
	long nx=d->nx,ny=d->ny,nz=d->nz;
//	if(Type == SMOOTH_NONE)	return;
	long p[3]={nx,ny,Type};
	mreal *b = new mreal[nx*ny*nz],dd=delta;
	// ����������� �� x
	memset(b,0,nx*ny*nz*sizeof(mreal));
	if(nx>4 && xdir)
	{
		mglStartThread(mgl_smth_x,0,nx*ny*nz,b,d->a,&dd,p);
		memcpy(d->a,b,nx*ny*nz*sizeof(mreal));
		memset(b,0,nx*ny*nz*sizeof(mreal));
	}
	if(ny>4 && ydir)
	{
		mglStartThread(mgl_smth_y,0,nx*ny*nz,b,d->a,&dd,p);
		memcpy(d->a,b,nx*ny*nz*sizeof(mreal));
		memset(b,0,nx*ny*nz*sizeof(mreal));
	}
	if(nz>4 && zdir)
	{
		mglStartThread(mgl_smth_z,0,nx*ny*nz,b,d->a,&dd,p);
		memcpy(d->a,b,nx*ny*nz*sizeof(mreal));
	}
	delete []b;
}
void MGL_EXPORT mgl_data_smooth_(uintptr_t *d, const char *dir, mreal *delta,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_smooth(_DT_,s,*delta);		delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_csum_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nz=t->p[2], nn=t->n;
	mreal *b=t->a;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		b[i] = a[i];
		for(long j=1;j<nz;j++)	b[i+j*nn] = b[i+j*nn-nn] + a[i+j*nn];
	}
	return 0;
}
static void *mgl_csum_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
	mreal *b=t->a;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = (i%nx)+nx*ny*(i/nx);		b[k] = a[k];
		for(long j=1;j<ny;j++)	b[k+j*nx] = b[k+j*nx-nx] + a[k+nx*j];
	}
	return 0;
}
static void *mgl_csum_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n;
	mreal *b=t->a;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = i*nx;			b[k] = a[k];
		for(long j=1;j<nx;j++)	b[j+k] = b[j+k-1] + a[j+k];
	}
	return 0;
}
void MGL_EXPORT mgl_data_cumsum(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx=d->nx,ny=d->ny,nz=d->nz,nn=nx*ny*nz;
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nn];
	memcpy(b,d->a,nn*sizeof(mreal));
	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_csum_z,0,nx*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_csum_y,0,nx*nz,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_csum_x,0,nz*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	delete []b;
}
void MGL_EXPORT mgl_data_cumsum_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_cumsum(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_int_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nz=t->p[2], nn=t->n;
	mreal *b=t->a, dd=0.5/nz;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		b[i] = 0;
		for(long j=1;j<nz;j++)	b[i+j*nn] = b[i+j*nn-nn] + (a[i+nn*j]+a[i+j*nn-nn])*dd;
	}
	return 0;
}
static void *mgl_int_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
	mreal *b=t->a, dd=0.5/ny;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = (i%nx)+nx*ny*(i/nx);	b[k] = 0;
		for(long j=1;j<ny;j++)	b[k+j*nx] = b[k+j*nx-nx] + (a[k+j*nx]+a[k+j*nx-nx])*dd;
	}
	return 0;
}
static void *mgl_int_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n;
	mreal *b=t->a, dd=0.5/nx;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = i*nx;			b[k] = 0;
		for(long j=1;j<nx;j++)	b[j+k] = b[j+k-1] + (a[j+k]+a[j+k-1])*dd;
	}
	return 0;
}
void MGL_EXPORT mgl_data_integral(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx=d->nx,ny=d->ny,nz=d->nz,nn=nx*ny*nz;
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nn];
	memcpy(b,d->a,nn*sizeof(mreal));
	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_int_z,0,nx*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_int_y,0,nx*nz,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_int_x,0,nz*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	delete []b;
}
void MGL_EXPORT mgl_data_integral_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_integral(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_dif_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nz=t->p[2], nn=t->n;
	mreal *b=t->a, dd=0.5*nz;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		b[i] = -(3*a[i]-4*a[i+nn]+a[i+2*nn])*dd;
		b[i+(nz-1)*nn] = (3*a[i+(nz-1)*nn]-4*a[i+(nz-2)*nn]+a[i+(nz-3)*nn])*dd;
		for(long j=1;j<nz-1;j++)		b[i+j*nn] = (a[i+j*nn+nn]-a[i+j*nn-nn])*dd;
	}
	return 0;
}
static void *mgl_dif_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
	mreal *b=t->a, dd=0.5*ny;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = (i%nx)+nx*ny*(i/nx);
		b[k] = -(3*a[k]-4*a[k+nx]+a[k+2*nx])*dd;
		b[k+(ny-1)*nx] = (3*a[k+(ny-1)*nx]-4*a[k+(ny-2)*nx]+a[k+(ny-3)*nx])*dd;
		for(long j=1;j<ny-1;j++)	b[k+j*nx] = (a[k+j*nx+nx]-a[k+j*nx-nx])*dd;
	}
	return 0;
}
static void *mgl_dif_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n;
	mreal *b=t->a, dd=0.5*nx;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = i*nx;
		b[k] = -(3*a[k]-4*a[k+1]+a[k+2])*dd;
		b[k+nx-1] = (3*a[k+nx-1]-4*a[k+nx-2]+a[k+nx-3])*dd;
		for(long j=1;j<nx-1;j++)	b[j+k] = (a[j+k+1]-a[j+k-1])*dd;
	}
	return 0;
}
void MGL_EXPORT mgl_data_diff(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx=d->nx,ny=d->ny,nz=d->nz,nn=nx*ny*nz;
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nn];
	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_dif_z,0,nx*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_dif_y,0,nx*nz,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_dif_x,0,nz*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	delete []b;
}
void MGL_EXPORT mgl_data_diff_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_diff(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_dif2_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nz=t->p[2], nn=t->n;
	mreal *b=t->a, dd=nz*nz;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		b[i] = b[i+(nz-1)*nn] = 0;
		for(long j=1;j<nz-1;j++)		b[i+j*nn] = (a[i+j*nn+nn]+a[i+j*nn-nn]-2*a[i+j*nn])*dd;
	}
	return 0;
}
static void *mgl_dif2_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
	mreal *b=t->a, dd=ny*ny;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = (i%nx)+nx*ny*(i/nx);	b[k] = b[k+(ny-1)*nx] = 0;
		for(long j=1;j<ny-1;j++)	b[k+j*nx] = (a[k+j*nx+nx]+a[k+j*nx-nx]-2*a[k+j*nx])*dd;
	}
	return 0;
}
static void *mgl_dif2_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n;
	mreal *b=t->a, dd=nx*nx;
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = i*nx;			b[k] = b[k+nx-1] = 0;
		for(long j=1;j<nx-1;j++)	b[j+k] = (a[j+k+1]+a[j+k-1]-2*a[j+k])*dd;
	}
	return 0;
}
void MGL_EXPORT mgl_data_diff2(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx=d->nx,ny=d->ny,nz=d->nz,nn=nx*ny*nz;
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nn];
	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_dif2_z,0,nx*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_dif2_y,0,nx*nz,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_dif2_x,0,nz*ny,b,d->a,0,p);
		memcpy(d->a,b,nn*sizeof(mreal));
	}
	delete []b;
}
void MGL_EXPORT mgl_data_diff2_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_diff2(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_swap(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	if(strchr(dir,'z') && d->nz>1)	mgl_data_roll(d,'z',d->nz/2);
	if(strchr(dir,'y') && d->ny>1)	mgl_data_roll(d,'y',d->ny/2);
	if(strchr(dir,'x') && d->nx>1)	mgl_data_roll(d,'x',d->nx/2);
}
void MGL_EXPORT mgl_data_swap_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_swap(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_roll(HMDT dd, char dir, long num)
{
	long nx=dd->nx,ny=dd->ny,nz=dd->nz, d;
	mreal *b,*a=dd->a;
	if(dir=='z' && nz>1)
	{
		d = num>0 ? num%nz : (num+nz*(1-num/nz))%nz;
		if(d==0)	return;		// nothing to do
		b = new mreal[nx*ny*nz];
		memcpy(b,a+nx*ny*d,nx*ny*(nz-d)*sizeof(mreal));
		memcpy(b+nx*ny*(nz-d),a,nx*ny*d*sizeof(mreal));
		memcpy(a,b,nx*ny*nz*sizeof(mreal));	delete []b;
	}
	if(dir=='y' && ny>1)
	{
		d = num>0 ? num%ny : (num+ny*(1-num/ny))%ny;
		if(d==0)	return;		// nothing to do
		b = new mreal[nx*ny*nz];
		memcpy(b,a+nx*d,(nx*ny*nz-nx*d)*sizeof(mreal));
#pragma omp parallel for
		for(long i=0;i<nz;i++)
			memcpy(b+nx*(ny-d)+nx*ny*i,a+nx*ny*i,nx*d*sizeof(mreal));
		memcpy(a,b,nx*ny*nz*sizeof(mreal));	delete []b;
	}
	if(dir=='x' && nx>1)
	{
		d = num>0 ? num%nx : (num+nx*(1-num/nx))%nx;
		if(d==0)	return;		// nothing to do
		b = new mreal[nx*ny*nz];
		memcpy(b,a+d,(nx*ny*nz-d)*sizeof(mreal));
#pragma omp parallel for
		for(long i=0;i<nz*ny;i++)
			memcpy(b+nx-d+nx*i,a+nx*i,d*sizeof(mreal));
		memcpy(a,b,nx*ny*nz*sizeof(mreal));	delete []b;
	}
}
void MGL_EXPORT mgl_data_roll_(uintptr_t *d, const char *dir, int *num, int)
{	mgl_data_roll(_DT_,*dir,*num);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_mirror(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx=d->nx,ny=d->ny,nz=d->nz;
	mreal *a=d->a;
	if(strchr(dir,'z') && nz>1)
	{
#pragma omp parallel for collapse(2)
		for(long j=0;j<nz/2;j++)	for(long i=0;i<nx*ny;i++)
		{
			long i0 = i+j*nx*ny, j0 = i+(nz-1-j)*nx*ny;
			mreal b = a[i0];	a[i0] = a[j0];	a[j0] = b;
		}
	}
	if(strchr(dir,'y') && ny>1)
	{
#pragma omp parallel for
		for(long i=0;i<nx*nz;i++)
		{
			long j0 = (i%nx)+nx*ny*(i/nx);
			for(long j=0;j<ny/2;j++)
			{
				long i0 = j0+(ny-1-j)*nx;
				mreal b = a[j0+j*nx];	a[j0+j*nx] = a[i0];	a[i0] = b;
			}
		}
	}
	if(strchr(dir,'x') && nx>1)
	{
#pragma omp parallel for
		for(long j=0;j<ny*nz;j++)
		{
			long j0 = j*nx;
			for(long i=0;i<nx/2;i++)
			{
				long i0 = nx-1-i+j0;
				mreal b = a[i+j0];	a[i+j0] = a[i0];	a[i0] = b;
			}
		}
	}
}
void MGL_EXPORT mgl_data_mirror_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_mirror(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_clean(HMDT d, long id)
{
	if(id<0 || id+1>d->nx)	return;
	long i,j,n=d->nx,m=d->ny;
	mreal *b = new mreal[m*n], *a=d->a;
	for(i=j=0;i+1<m;i++)
	{
		if(a[id+n*i]!=a[id+n*i+n])	// this can be saved
		{
#pragma omp parallel for
			for(long k=0;k<n;k++)	b[k+n*j]=a[k+n*i];
			j++;
		}
	}
	// always save last row
	i=n*(m-1);
#pragma omp parallel for
	for(long k=0;k<n;k++)	b[k+n*j]=a[k+i];
	j++;
	memcpy(a,b,n*j*sizeof(mreal));	d->ny = j;
	delete []b;
}
void MGL_EXPORT mgl_data_clean_(uintptr_t *d, int *id)	{	mgl_data_clean(_DT_,*id);	}
//-----------------------------------------------------------------------------
static void *mgl_solve_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	HCDT d=(HCDT)t->v;
	long nx=t->p[0], ny=t->p[1], nz=t->p[2], n1=t->p[3]?nx-1:1, nn=t->n;
	const mreal *a=t->b, *ii=t->c;
	mreal *b=t->a,val=t->d[0],da = 1e-5*(val?fabs(val):1);
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long l=t->id;l<nn;l+=mglNumThr)
	{
		long j=l%ny, k=l/ny;	b[l] = NAN;
		if(ii && ii[l]!=ii[l])	continue;
		long i0 = ii?(ii[l]*n1+1):0;
		if(i0>nx-2)	continue;
		if(a)	for(long i=i0+1;i<nx;i++)
		{
			mreal y1=a[i-1+nx*l], y2=a[i+nx*l];
			if((y1-val)*(y2-val)<=0)
			{
				mreal x = i-1 + (val-y1)/(y2-y1), dx;
				mreal v0=mglSpline3(a,nx,ny,nz,x,j,k, &dx,0,0), v=v0;
				unsigned kk=0;
				while(fabs(v-val)>da || dx==0)
				{
					x += (val-v)/dx;		kk++;
					v = mglSpline3(a,nx,ny,nz,x,j,k, &dx,0,0);
					if(kk>=10)
					{
						b[l] = x = fabs(v-val)<fabs(v0-val) ? x:i-1 + (val-y1)/(y2-y1);
						break;
					}
				}
				b[l] = x;	break;
			}
		}
		else 	for(long i=i0+1;i<nx;i++)
		{
			mreal y1=d->v(i-1,j,k), y2=d->v(i,j,k);
			if((y1-val)*(y2-val)<=0)
			{	b[l] = i-1 + (val-y1)/(y2-y1);	break;	}
		}
		b[l] /= n1;
	}
	return 0;
}
static void *mgl_solve_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	HCDT d=(HCDT)t->v;
	long nx=t->p[0], ny=t->p[1], nz=t->p[2], n1=t->p[3]?ny-1:1, nn=t->n;
	const mreal *a=t->b, *ii=t->c;
	mreal *b=t->a,val=t->d[0],da = 1e-5*(val?fabs(val):1);
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long l=t->id;l<nn;l+=mglNumThr)
	{
		long j=l%nx, k=l/nx;	b[l] = NAN;
		if(ii && ii[l]!=ii[l])	continue;
		long i0 = ii?(ii[l]*n1+1):0;
		if(i0>ny-2)	continue;
		if(a)	for(long i=i0+1;i<ny;i++)
		{
			mreal y1=a[j+nx*(i-1+ny*k)], y2=a[j+nx*(i+ny*k)];
			if((y1-val)*(y2-val)<=0)
			{
				mreal x = i-1 + (val-y1)/(y2-y1), dy;
				mreal v0=mglSpline3(a,nx,ny,nz,j,x,k, 0,&dy,0), v=v0;
				unsigned kk=0;
				while(fabs(v-val)>da || dy==0)
				{
					x += (val-v)/dy;		kk++;
					v = mglSpline3(a,nx,ny,nz,j,x,k, 0,&dy,0);
					if(kk>=10)
					{
						b[l] = x = fabs(v-val)<fabs(v0-val) ? x:i-1 + (val-y1)/(y2-y1);
						break;
					}
				}
				b[l] = x;	break;
			}
		}
		else 	for(long i=i0+1;i<ny;i++)
		{
			mreal y1=d->v(j,i-1,k), y2=d->v(j,i,k);
			if((y1-val)*(y2-val)<=0)
			{	b[l] = i-1 + (val-y1)/(y2-y1);	break;	}
		}
		b[l] /= n1;
	}
	return 0;
}
static void *mgl_solve_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	HCDT d=(HCDT)t->v;
	long nx=t->p[0], ny=t->p[1], nz=t->p[2], n1=t->p[3]?nz-1:1, nn=t->n;
	const mreal *a=t->b, *ii=t->c;
	mreal *b=t->a,val=t->d[0],da = 1e-5*(val?fabs(val):1);
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long l=t->id;l<nn;l+=mglNumThr)
	{
		long j=l%nx, k=l/nx;	b[l] = NAN;
		if(ii && ii[l]!=ii[l])	continue;
		long i0 = ii?(ii[l]*n1+1):0;
		if(i0>nz-2)	continue;
		if(a)	for(long i=i0+1;i<nz;i++)
		{
			mreal y1=a[nn*i-nn+l], y2=a[nn*i+l];
			if((y1-val)*(y2-val)<=0)
			{
				mreal x = i-1 + (val-y1)/(y2-y1), dz;
				mreal v0=mglSpline3(a,nx,ny,nz,j,k,x, 0,0,&dz), v=v0;
				unsigned kk=0;
				while(fabs(v-val)>da || dz==0)
				{
					x += (val-v)/dz;		kk++;
					v = mglSpline3(a,nx,ny,nz,j,k,x, 0,0,&dz);
					if(kk>=10)
					{
						b[l] = x = fabs(v-val)<fabs(v0-val) ? x:i-1 + (val-y1)/(y2-y1);
						break;
					}
				}
				b[l] = x;	break;
			}
		}
		else 	for(long i=i0+1;i<nz;i++)
		{
			mreal y1=d->v(j,k,i-1), y2=d->v(j,k,i);
			if((y1-val)*(y2-val)<=0)
			{	b[l] = i-1 + (val-y1)/(y2-y1);	break;	}
		}
		b[l] /= n1;
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_solve(HCDT dat, mreal val, char dir, HCDT i0, int norm)
{
	const mglData *i = dynamic_cast<const mglData *>(i0);
	const mglData *d = dynamic_cast<const mglData *>(dat);
	long p[4]={dat->GetNx(), dat->GetNy(), dat->GetNz(), norm};
	const mreal *ii=0;
	mglData *r=new mglData, id0;
	if(i0 && !i)	{	id0.Set(i0);	i=&id0;	}	// <-- slow but should work
	if(dir=='x' && p[0]>1)
	{
		r->Create(p[1],p[2]);
		ii = (i && i->nx*i->ny==p[1]*p[2])?i->a:0;
		mglStartThread(mgl_solve_x,0,p[1]*p[2],r->a,d?d->a:0,ii,p,dat,&val);
	}
	if(dir=='y' && p[1]>1)
	{
		r->Create(p[0],p[2]);
		ii = (i && i->nx*i->ny==p[0]*p[2])?i->a:0;
		mglStartThread(mgl_solve_y,0,p[0]*p[2],r->a,d?d->a:0,ii,p,dat,&val);
	}
	if(dir=='z' && p[2]>1)
	{
		r->Create(p[0],p[1]);
		ii = (i && i->nx*i->ny==p[0]*p[1])?i->a:0;
		mglStartThread(mgl_solve_z,0,p[0]*p[1],r->a,d?d->a:0,ii,p,dat,&val);
	}
	return r;
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_solve_1d(HCDT d, mreal val, int spl, long i0)
{
	mreal x=0, y1, y2, a, a0, dx=0, da = 1e-5*(val?fabs(val):1);
	long nx = d->GetNx();
	if(i0<0 || i0>=nx)	i0=0;
	if(val==d->v(i0+1))	return i0+1;
	const mglData *dd=dynamic_cast<const mglData *>(d);
	const mglDataC *dc=dynamic_cast<const mglDataC *>(d);
	if(dd)	for(long i=i0+1;i<nx;i++)
	{
		y1=dd->a[i-1];	y2=dd->a[i];
		if((y1-val)*(y2-val)<=0)
		{
			x = i-1 + (val-y1)/(y2-y1);
			a0 = a = mglSpline1t<mreal>(dd->a,nx,x,&dx);
			if(spl)	for(unsigned k=0;fabs(a-val)>da || dx==0;)
			{
				x += (val-a)/dx;		k++;
				a = mglSpline1t<mreal>(dd->a,nx,x,&dx);
				if(k>=10)
					return fabs(a-val)<fabs(a0-val) ? x:i-1 + (val-y1)/(y2-y1);
			}
			return x;
		}
	}
	else if(dc)	for(long i=i0+1;i<nx;i++)
	{
		y1=abs(dc->a[i-1]);	y2=abs(dc->a[i]);
		if((y1-val)*(y2-val)<=0)
		{
			x = i-1 + (val-y1)/(y2-y1);
			dual cx, ca = mglSpline1t<dual>(dc->a,nx,x,&cx);
			a0 = a = abs(ca);	dx = a?(cx.real()*ca.real()+cx.imag()*ca.imag())/a:0;
			if(spl)	for(unsigned k=0;fabs(a-val)>da || dx==0;)
			{
				x += (val-a)/dx;		k++;
				ca = mglSpline1t<dual>(dc->a,nx,x,&cx);
				a = abs(ca);	dx = a?(cx.real()*ca.real()+cx.imag()*ca.imag())/a:0;
				if(k>=10)
					return fabs(a-val)<fabs(a0-val) ? x:i-1 + (val-y1)/(y2-y1);
			}
			return x;
		}
	}
	else 	for(long i=i0+1;i<nx;i++)
	{
		y1=d->v(i-1);	y2=d->v(i);
		if((y1-val)*(y2-val)<=0)
			return i-1 + (val-y1)/(y2-y1);
	}
	return NAN;
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_linear_ext(HCDT d, mreal x,mreal y,mreal z, mreal *dx,mreal *dy,mreal *dz)
{
	if(!d)	return NAN;
	long kx=long(x), ky=long(y), kz=long(z);
	mreal b0,b1;
	const mglData *dd=dynamic_cast<const mglData *>(d);
	if(dd)
	{
		long nx=dd->nx, ny=dd->ny, nz=dd->nz, dn=ny>1?nx:0;
		kx = kx>=0 ? kx:0;	kx = kx<nx-1 ? kx:nx-2;
		ky = ky>=0 ? ky:0;	ky = ky<ny-1 ? ky:ny-2;
		kz = kz>=0 ? kz:0;	kz = kz<nz-1 ? kz:nz-2;
		x -= kx;	y -= ky;	z -= kz;
		const mreal *aa = dd->a, *bb;
		if(kz>=0)
		{
			aa=dd->a+kx+nx*(ky+ny*kz);	bb = aa+nx*ny;
			b0 = aa[0]*(1-x-y+x*y) + x*(1-y)*aa[1] + y*(1-x)*aa[dn] + x*y*aa[1+dn];
			b1 = bb[0]*(1-x-y+x*y) + x*(1-y)*bb[1] + y*(1-x)*bb[dn] + x*y*bb[1+dn];
		}
		else
		{
			z=0;
			if(ky>=0)
			{
				aa=dd->a+kx+nx*ky;
				b0 = b1 = aa[0]*(1-x-y+x*y) + x*(1-y)*aa[1] + y*(1-x)*aa[dn] + x*y*aa[1+dn];
			}
			else if(kx>=0)
			{
				aa=dd->a+kx;	b0 = b1 = aa[0]*(1-x) + x*aa[1];
			}
			else	b0 = b1 = dd->a[0];
		}
		if(dx)	*dx = kx>=0?aa[1]-aa[0]:0;
		if(dy)	*dy = ky>=0?aa[dn]-aa[0]:0;
		if(dz)	*dz = b1-b0;
	}
	else
	{
		long nx=d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
		kx = kx>=0 ? kx:0;	kx = kx<nx-1 ? kx:nx-2;
		ky = ky>=0 ? ky:0;	ky = ky<ny-1 ? ky:ny-2;
		kz = kz>=0 ? kz:0;	kz = kz<nz-1 ? kz:nz-2;
		x -= kx;	y -= ky;	z -= kz;
		mreal a0 = 0, a1 = 0, a2 = 0;
		if(kz>=0)
		{
			a0 = d->v(kx,ky,kz);	a1 = d->v(kx+1,ky,kz);	a2 = d->v(kx,ky+1,kz);
			b0 = a0*(1-x-y+x*y) + x*(1-y)*a1 + y*(1-x)*a2 + x*y*d->v(kx+1,ky+1,kz);;
			b1 = d->v(kx,ky,kz+1)*(1-x-y+x*y) + x*(1-y)*d->v(kx+1,ky,kz+1) + y*(1-x)*d->v(kx,ky+1,kz+1) + x*y*d->v(kx+1,ky+1,kz+1);
		}
		else
		{
			z=0;
			if(ky>=0)
			{
				a0 = d->v(kx,ky);	a1 = d->v(kx+1,ky);	a2 = d->v(kx,ky+1);
				b0 = b1 = a0*(1-x-y+x*y) + x*(1-y)*a1 + y*(1-x)*a2 + x*y*d->v(kx+1,ky+1);
			}
			else if(kx>=0)
			{
				a2=a0 = d->v(kx);	a1 = d->v(kx+1);	b0 = b1 = a0*(1-x) + x*a1;
			}
			else	b0 = b1 = d->v(0);
		}
		if(dx)	*dx = a1-a0;
		if(dy)	*dy = a2-a0;
		if(dz)	*dz = b1-b0;
	}
	return b0 + z*(b1-b0);
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_linear(HCDT d, mreal x,mreal y,mreal z)
{	return mgl_data_linear_ext(d, x,y,z, 0,0,0);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_spline(HCDT d, mreal x,mreal y,mreal z)
{
	if(mgl_isbad(x) || mgl_isbad(y) || mgl_isbad(z))	return NAN;
	return d->value(x,y,z);
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_spline_ext(HCDT d, mreal x,mreal y,mreal z, mreal *dx,mreal *dy,mreal *dz)
{
	if(mgl_isbad(x) || mgl_isbad(y) || mgl_isbad(z))	return NAN;
	return d->valueD(x,y,z,dx,dy,dz);
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_spline_(uintptr_t *d, mreal *x,mreal *y,mreal *z)
{	return mgl_data_spline(_DA_(d),*x,*y,*z);	}
mreal MGL_EXPORT mgl_data_linear_(uintptr_t *d, mreal *x,mreal *y,mreal *z)
{	return mgl_data_linear(_DA_(d),*x,*y,*z);	}
mreal MGL_EXPORT mgl_data_spline_ext_(uintptr_t *d, mreal *x,mreal *y,mreal *z, mreal *dx,mreal *dy,mreal *dz)
{	return mgl_data_spline_ext(_DA_(d),*x,*y,*z,dx,dy,dz);	}
mreal MGL_EXPORT mgl_data_linear_ext_(uintptr_t *d, mreal *x,mreal *y,mreal *z, mreal *dx,mreal *dy,mreal *dz)
{	return mgl_data_linear_ext(_DA_(d),*x,*y,*z,dx,dy,dz);	}
mreal MGL_EXPORT mgl_data_solve_1d_(uintptr_t *d, mreal *val, int *spl, int *i0)
{	return mgl_data_solve_1d(_DA_(d),*val, *spl, *i0);	}
uintptr_t MGL_EXPORT mgl_data_solve_(uintptr_t *d, mreal *val, const char *dir, uintptr_t *i0, int *norm,int)
{	return uintptr_t(mgl_data_solve(_DA_(d),*val, *dir, _DA_(i0), *norm));	}
//-----------------------------------------------------------------------------
long MGL_LOCAL_CONST int_pow(long x, long n)
{
	if(n==2)	return x*x;
	if(n==1)	return x;
	if(n==0)	return 1;
	if(n<0)		return 0;
	long t = int_pow(x,n/2);	t = t*t;
	if(n%2==1)	t *= x;
	return t;
}
long MGL_NO_EXPORT mgl_powers(long N, const char *how)
{
	bool k2 = mglchr(how,'2'), k3 = mglchr(how,'3'), k5 = mglchr(how,'5');
	const double lN=log(N), l2=log(2), l3=log(3), l5=log(5);
	if(k2 && k3 && k5)
	{
		double dm=lN;	long im=0, jm=0, km=0;
		for(long i=0;i<=lN/l2;i++)	for(long j=0;j<=(lN-i*l2)/l3;j++)	for(long k=0;k<=(lN-i*l2-j*l3)/l5;k++)
		{
			double d = lN-i*l2-j*l3-k*l5;
			if(d>0 && d<dm)	{	im=i;	jm=j;	km=k;	dm=d;	}
		}
		return int_pow(2,im)*int_pow(3,jm)*int_pow(5,km);
	}
	else if(k2 && !k3 && !k5)	return int_pow(2,lN/l2);
	else if(k3 && !k2 && !k5)	return int_pow(3,lN/l3);
	else if(k5 && !k3 && !k2)	return int_pow(5,lN/l5);
	else if(k2 && k3 && !k5)
	{
		double dm=lN;	long im=0, jm=0;
		for(long i=0;i<=lN/l2;i++)	for(long j=0;j<=(lN-i*l2)/l3;j++)
		{
			double d = lN-i*l2-j*l3;
			if(d>0 && d<dm)	{	im=i;	jm=j;	dm=d;	}
		}
		return int_pow(2,im)*int_pow(3,jm);
	}
	else if(k2 && k5 && !k3)
	{
		double dm=lN;	long im=0, jm=0;
		for(long i=0;i<=lN/l2;i++)	for(long j=0;j<=(lN-i*l2)/l5;j++)
		{
			double d = lN-i*l2-j*l5;
			if(d>0 && d<dm)	{	im=i;	jm=j;	dm=d;	}
		}
		return int_pow(2,im)*int_pow(5,jm);
	}
	else if(k5 && k3 && !k2)
	{
		double dm=lN;	long im=0, jm=0;
		for(long i=0;i<=lN/l5;i++)	for(long j=0;j<=(lN-i*l5)/l3;j++)
		{
			double d = lN-i*l5-j*l3;
			if(d>0 && d<dm)	{	im=i;	jm=j;	dm=d;	}
		}
		return int_pow(5,im)*int_pow(3,jm);
	}
	return 0;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_crop_opt(HMDT d, const char *how)
{
	const char *h = "235";
	if(mglchr(how,'2') || mglchr(how,'3') || mglchr(how,'5'))	h = how;
	if(mglchr(how,'x'))	mgl_data_crop(d, 0, mgl_powers(d->nx, h), 'x');
	if(mglchr(how,'y'))	mgl_data_crop(d, 0, mgl_powers(d->ny, h), 'y');
	if(mglchr(how,'z'))	mgl_data_crop(d, 0, mgl_powers(d->nz, h), 'z');
}
void MGL_EXPORT mgl_data_crop_opt_(uintptr_t *d, const char *how, int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	mgl_data_crop_opt(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_crop(HMDT d, long n1, long n2, char dir)
{
	long nx=d->nx,ny=d->ny,nz=d->nz, nn;
	mreal *b;
	if(n1<0)	n1=0;
	switch(dir)
	{
	case 'x':
		if(n1>=nx)	break;
		n2 = n2>0 ? n2 : nx+n2;
		if(n2<0 || n2>=nx || n2<n1)	n2 = nx;
		nn = n2-n1;	b = new mreal[nn*ny*nz];
#pragma omp parallel for
		for(long i=0;i<ny*nz;i++)
			memcpy(b+nn*i,d->a+nx*i+n1,nn*sizeof(mreal));
		d->nx = nn;	if(!d->link)	delete []d->a;
		d->a = b;	d->link=false;	d->NewId();
		break;
	case 'y':
		if(n1>=ny)	break;
		n2 = n2>0 ? n2 : ny+n2;
		if(n2<0 || n2>=ny || n2<n1)	n2 = ny;
		nn = n2-n1;	b = new mreal[nn*nx*nz];
#pragma omp parallel for collapse(2)
		for(long j=0;j<nz;j++)	for(long i=0;i<nn;i++)
			memcpy(b+nx*(i+nn*j),d->a+nx*(n1+i+ny*j),nx*sizeof(mreal));
		d->ny = nn;	if(!d->link)	delete []d->a;
		d->a = b;	d->link=false;
		break;
	case 'z':
		if(n1>=nz)	break;
		n2 = n2>0 ? n2 : nz+n2;
		if(n2<0 || n2>=nz || n2<n1)	n2 = nz;
		nn = n2-n1;	b = new mreal[nn*nx*ny];
		memcpy(b,d->a+nx*ny*n1,nn*nx*ny*sizeof(mreal));
		d->nz = nn;	if(!d->link)	delete []d->a;
		d->a = b;	d->link=false;
		break;
	}
}
void MGL_EXPORT mgl_data_crop_(uintptr_t *d, int *n1, int *n2, const char *dir,int)
{	mgl_data_crop(_DT_,*n1,*n2,*dir);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_last(HCDT d, const char *cond, long *i, long *j, long *k)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	if(!cond)	cond = "u";
	mglFormula eq(cond);
	if(*i<0 || *i>=nx)	*i=nx;
	if(*j<0 || *j>=ny)	*j=ny-1;
	if(*k<0 || *k>=nz)	*k=nz-1;
	long i0 = *i+nx*(*j+ny*(*k))-1;
	mreal x,y,z,dx=nx>1?1/(nx-1.):0,dy=ny>1?1/(ny-1.):0,dz=nz>1?1/(nz-1.):0;
	for(;i0>=0;i0--)
	{
		x = dx*(i0%nx);		y = dy*((i0/nx)%ny);	z = dz*(i0/(nx*ny));
		if(eq.Calc(x,y,z,d->vthr(i0)))	break;
	}
	*i = i0%nx;	*j = (i0/nx)%ny;	*k = i0/(nx*ny);
	return i0>=0 ? d->vthr(i0) : NAN;	// NOTE: Return NAN if false
}
mreal MGL_EXPORT mgl_data_last_(uintptr_t *d, const char *cond, int *i, int *j, int *k, int l)
{	long ii=*i,jj=*j,kk=*k;	char *s=new char[l+1];	memcpy(s,cond,l);	s[l]=0;
	mreal res = mgl_data_last(_DT_,s,&ii,&jj,&kk);	*i=ii;	*j=jj;	*k=kk;
	delete []s;		return res;	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_first(HCDT d, const char *cond, long *i, long *j, long *k)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	if(!cond)	cond = "u";
	mglFormula eq(cond);
	if(*i<0 || *i>=nx)	*i=nx;
	if(*j<0 || *j>=ny)	*j=ny-1;
	if(*k<0 || *k>=nz)	*k=nz-1;
	long i0 = *i+nx*(*j+ny*(*k))-1;
	mreal x,y,z,dx=nx>1?1/(nx-1.):0,dy=ny>1?1/(ny-1.):0,dz=nz>1?1/(nz-1.):0;
	for(;i0<nx*ny*nz;i0--)
	{
		x = dx*(i0%nx);		y = dy*((i0/nx)%ny);	z = dz*(i0/(nx*ny));
		if(eq.Calc(x,y,z,d->vthr(i0)))	break;
	}
	*i = i0%nx;	*j = (i0/nx)%ny;	*k = i0/(nx*ny);
	return i0<nx*ny*nz ? d->vthr(i0) : NAN;	// NOTE: Return NAN if false
}
mreal MGL_EXPORT mgl_data_first_(uintptr_t *d, const char *cond, int *i, int *j, int *k, int l)
{	long ii=*i,jj=*j,kk=*k;	char *s=new char[l+1];	memcpy(s,cond,l);	s[l]=0;
	mreal res = mgl_data_first(_DT_,s,&ii,&jj,&kk);	*i=ii;	*j=jj;	*k=kk;
	delete []s;		return res;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT mgl_data_find(HCDT d, const char *cond, char dir, long i, long j, long k)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	long m=-1;
	if(!cond)	cond = "u";
	mglFormula eq(cond);
	mreal x=i/(nx-1.),y=j/(ny-1.),z=k/(nz-1.);
	if(dir=='x' && nx>1)	for(m=i;m<nx;m++)
		if(eq.Calc(m/(nx-1.),y,z,d->v(m,j,k)))	break;
	if(dir=='y' && ny>1)	for(m=j;m<ny;m++)
		if(eq.Calc(x,m/(ny-1.),z,d->v(i,m,k)))	break;
	if(dir=='z' && nz>1)	for(m=k;m<nz;m++)
		if(eq.Calc(x,y,m/(nz-1.),d->v(i,j,m)))	break;
	return m;
}
int MGL_EXPORT mgl_data_find_(uintptr_t *d, const char *cond, char *dir, int *i, int *j, int *k, int l, int)
{	char *s=new char[l+1];	memcpy(s,cond,l);	s[l]=0;
	int res = mgl_data_find(_DT_,s,*dir,*i,*j,*k);	delete []s;	return res;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_find_any(HCDT d, const char *cond)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	bool cc = false;
	if(!cond || *cond==0)	cond = "u";
	mglFormula eq(cond);
#pragma omp parallel for collapse(3)
	for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
	{
		if(cc)	continue;
		if(eq.Calc(i/(nx-1.),j/(ny-1.),k/(nz-1.),d->v(i,j,k)))	cc = true;
	}
	return cc;
}
int MGL_EXPORT mgl_data_find_any_(uintptr_t *d, const char *cond, int l)
{	char *s=new char[l+1];	memcpy(s,cond,l);	s[l]=0;
	int res = mgl_data_find_any(_DT_,s);	delete []s;	return res;	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_momentum_val(HCDT dd, char dir, mreal *x, mreal *w, mreal *s, mreal *k)
{
	long nx=dd->GetNx(),ny=dd->GetNy(),nz=dd->GetNz();
	mreal i0=0,i1=0,i2=0,i3=0,i4=0;
	switch(dir)
	{
	case 'x':
#pragma omp parallel for reduction(+:i0,i1,i2,i3,i4)
		for(long i=0;i<nx*ny*nz;i++)
		{
			mreal d = mreal(i%nx), t = d*d, v = dd->vthr(i);
			i0+= v;	i1+= v*d;	i2+= v*t;
			i3+= v*d*t;		i4+= v*t*t;
		}
		break;
	case 'y':
#pragma omp parallel for reduction(+:i0,i1,i2,i3,i4)
		for(long i=0;i<nx*ny*nz;i++)
		{
			mreal d = mreal((i/nx)%ny), t = d*d, v = dd->vthr(i);
			i0+= v;	i1+= v*d;	i2+= v*t;
			i3+= v*d*t;		i4+= v*t*t;
		}
		break;
	case 'z':
#pragma omp parallel for reduction(+:i0,i1,i2,i3,i4)
		for(long i=0;i<nx*ny*nz;i++)
		{
			mreal d = mreal(i/(nx*ny)), t = d*d, v = dd->vthr(i);
			i0+= v;	i1+= v*d;	i2+= v*t;
			i3+= v*d*t;		i4+= v*t*t;
		}
		break;
	default:	// "self-dispersion"
		i0 = nx*ny*nz;
#pragma omp parallel for reduction(+:i1,i2,i3,i4)
		for(long i=0;i<nx*ny*nz;i++)
		{
			mreal v = dd->vthr(i), t = v*v;
			i1+= v;			i2+= t;
			i3+= v*t;		i4+= t*t;
		}
	}
	if(i0==0)	return 0;
	mreal d=i1/i0;
	if(x)	*x=d;
	if(w)	*w=i2>d*d*i0 ? sqrt(i2/i0-d*d) : 0;
	if(s)	*s=i3/i0;
	if(k)	*k=i4/(i0*3);
	return i0;
}
mreal MGL_EXPORT mgl_data_momentum_val_(uintptr_t *d, char *dir, mreal *m, mreal *w, mreal *s, mreal *k,int)
{	mreal mm=0,ww=0,ss=0,kk=0,aa=0;
	aa = mgl_data_momentum_val(_DT_,*dir,&mm,&ww,&ss,&kk);
	*m=mm;	*w=ww;	*s=ss;	*k=kk;	return aa;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_norm_slice(HMDT d, mreal v1,mreal v2,char dir,long keep_en,long sym)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	mreal *a=d->a;
	mglData b(*d);
	mreal e0=1, e, m1, m2, aa;
	if(sym)	{	v2 = -v1>v2 ? -v1:v2;	v1 = -v2;	}
	if(dir=='z' && nz>1)
	{
//#pragma omp parallel for private(m1,m2,aa,e,e0)	// TODO add omp comparison here
		for(long k=0;k<nz;k++)
		{
			m1 = INFINITY;	m2 = -INFINITY;	e=0;
			for(long i=0;i<nx*ny;i++)
			{
				aa = a[i+nx*ny*k];
				m1 = m1<aa ? m1 : aa;
				m2 = m2>aa ? m2 : aa;
				e += aa*aa;
			}
			if(m1==m2)	m2+=1;
			if(sym)	{	m2 = -m1>m2 ? -m1:m2;	m1 = -m2;	}
			if(keep_en && k)	e = e0>0?sqrt(e/e0):1;
			else	{	e0 = e;	e=1;	}
			for(long i=0;i<nx*ny;i++)
				b.a[i+nx*ny*k] = (v1 + (v2-v1)*(a[i+nx*ny*k]-m1)/(m2-m1))*e;
		}
	}
	else if(dir=='y' && ny>1)
	{
//#pragma omp parallel for private(m1,m2,aa,e,e0)	// TODO add omp comparison here
		for(long j=0;j<ny;j++)
		{
			m1 = INFINITY;	m2 = -INFINITY;	e=0;
			for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)
			{
				aa = a[i+nx*(j+ny*k)];
				m1 = m1<aa ? m1 : aa;
				m2 = m2>aa ? m2 : aa;
				e += aa*aa;
			}
			if(m1==m2)	m2+=1;
			if(sym)	{	m2 = -m1>m2 ? -m1:m2;	m1 = -m2;	}
			if(keep_en && j)	e = e0>0?sqrt(e/e0):1;
			else	{	e0 = e;	e=1;	}
#pragma omp parallel for collapse(2)
			for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)
				b.a[i+nx*(j+ny*k)] = (v1 + (v2-v1)*(a[i+nx*(j+ny*k)]-m1)/(m2-m1))*e;
		}
	}
	else if(dir=='x' && nx>1)
	{
//#pragma omp parallel for private(m1,m2,aa,e,e0)	// TODO add omp comparison here
		for(long i=0;i<nx;i++)
		{
			m1 = INFINITY;	m2 = -INFINITY;	e=0;
			for(long k=0;k<ny*nz;k++)
			{
				aa = a[i+nx*k];
				m1 = m1<aa ? m1 : aa;
				m2 = m2>aa ? m2 : aa;
				e += aa*aa;
			}
			if(m1==m2)	m2+=1;
			if(sym)	{	m2 = -m1>m2 ? -m1:m2;	m1 = -m2;	}
			if(keep_en && i)	e = e0>0?sqrt(e/e0):1;
			else	{	e0 = e;	e=1;	}
#pragma omp parallel for
			for(long k=0;k<ny*nz;k++)
				b.a[i+nx*k] = (v1 + (v2-v1)*(a[i+nx*k]-m1)/(m2-m1))*e;
		}
	}
	memcpy(d->a, b.a, nx*ny*nz*sizeof(mreal));
}
void MGL_EXPORT mgl_data_norm_slice_(uintptr_t *d, mreal *v1,mreal *v2,char *dir,int *keep_en,int *sym,int )
{	mgl_data_norm_slice(_DT_,*v1,*v2,*dir,*keep_en,*sym);	}
//-----------------------------------------------------------------------------
MGL_EXPORT const char *mgl_data_info(HCDT d)	// NOTE: Not thread safe function!
{
	static char buf[512];
	char s[128];	buf[0]=0;
	snprintf(s,128,"nx = %ld\tny = %ld\tnz = %ld\n",d->GetNx(),d->GetNy(),d->GetNz());
	s[127]=0;	strcat(buf,s);

	long i=0,j=0,k=0;
	mreal A=0,Wa=0,X=0,Y=0,Z=0,Wx=0,Wy=0,Wz=0, b;
	b = mgl_data_max_int(d,&i,&j,&k);
	snprintf(s,128,_("Maximum is %g\t at x = %ld\ty = %ld\tz = %ld\n"), b,i,j,k);
	s[127]=0;	strcat(buf,s);
	b = mgl_data_min_int(d,&i,&j,&k);
	snprintf(s,128,_("Minimum is %g\t at x = %ld\ty = %ld\tz = %ld\n"), b,i,j,k);
	s[127]=0;	strcat(buf,s);

	mgl_data_momentum_val(d,'a',&A,&Wa,0,0);	mgl_data_momentum_val(d,'x',&X,&Wx,0,0);
	mgl_data_momentum_val(d,'y',&Y,&Wy,0,0);	mgl_data_momentum_val(d,'z',&Z,&Wz,0,0);
	snprintf(s,128,_("Averages are:\n<a> = %g\t<x> = %g\t<y> = %g\t<z> = %g\n"), A,X,Y,Z);
	s[127]=0;	strcat(buf,s);
	snprintf(s,128,_("Widths are:\nWa = %g\tWx = %g\tWy = %g\tWz = %g\n"), Wa,Wx,Wy,Wz);
	s[127]=0;	strcat(buf,s);
	return buf;
}
int MGL_EXPORT mgl_data_info_(uintptr_t *d, char *out, int len)
{
	const char *res = mgl_data_info(_DA_(d));
	if(out)	mgl_strncpy(out,res,len);
	return strlen(res);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_insert(HMDT d, char dir, long at, long num)
{
	if(num<1)	return;
	at = at<0 ? 0:at;
	long nn, nx=d->nx, ny=d->ny, nz=d->nz, nxy=nx*ny;
	mglData b;
	if(dir=='x')
	{
		if(at>nx)	at=nx;
		nn=nx+num;	b.Create(nn,ny,nz);
#pragma omp parallel for
		for(long k=0;k<ny*nz;k++)
		{
			if(at>0)	memcpy(b.a+nn*k, d->a+nx*k,at*sizeof(mreal));
			if(at<nx)	memcpy(b.a+at+num+nn*k, d->a+at+nx*k,(nx-at)*sizeof(mreal));
			if(at<nx)	for(long i=0;i<num;i++)	b.a[nn*k+at+i]=d->a[nx*k+at];	// copy values
			else		for(long i=0;i<num;i++)	b.a[nn*k+at+i]=d->a[nx*k+nx-1];	// copy values
		}
		d->Set(b);	nx+=num;
	}
	if(dir=='y')
	{
		if(at>ny)	at=ny;
		nn=num+ny;	b.Create(nx,nn,nz);
#pragma omp parallel for
		for(long k=0;k<nz;k++)
		{
			if(at>0)	memcpy(b.a+nx*nn*k, d->a+nxy*k,at*nx*sizeof(mreal));
			if(at<ny)	memcpy(b.a+nx*(at+num+nn*k), d->a+nx*(at+ny*k),(ny-at)*nx*sizeof(mreal));
			if(at<ny)	for(long i=0;i<num;i++)
				memcpy(b.a+nx*(nn*k+at+i),d->a+nx*(ny*k+at),nx*sizeof(mreal));
			else	for(long i=0;i<num;i++)
				memcpy(b.a+nx*(nn*k+at+i),d->a+nx*(ny*k+ny-1),nx*sizeof(mreal));
		}
		d->Set(b);	ny+=num;
	}
	if(dir=='z')
	{
		if(at>nz)	at=nz;
		b.Create(nx,ny,nz+num);
		if(at>0)	memcpy(b.a, d->a,at*nxy*sizeof(mreal));
		if(at<nz)	memcpy(b.a+nxy*(at+num), d->a+nxy*at,(nz-at)*nxy*sizeof(mreal));
		if(at<nz)
#pragma omp parallel for
			for(long i=0;i<num;i++)	memcpy(b.a+nxy*(at+i),d->a+nxy*at,nxy*sizeof(mreal));
		else
#pragma omp parallel for
			for(long i=0;i<num;i++)	memcpy(b.a+nxy*(at+i),d->a+nxy*(nz-1),nxy*sizeof(mreal));
		d->Set(b);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_delete(HMDT d, char dir, long at, long num)
{
	if(num<1 || at<0)	return;
	mglData b;
	long nx=d->nx, ny=d->ny, nz=d->nz, nn;
	if(dir=='x')
	{
		if(at+num>=nx)	return;
		nn=nx-num;	b.Create(nn,ny,nz);
#pragma omp parallel for
		for(long k=0;k<ny*nz;k++)
		{
			if(at>0)	memcpy(b.a+nn*k, d->a+nx*k,at*sizeof(mreal));
			memcpy(b.a+at+nn*k, d->a+at+num+nx*k,(nx-at-num)*sizeof(mreal));
		}
		d->Set(b);	nx-=num;
	}
	if(dir=='y')
	{
		if(at+num>=ny)	return;
		nn=ny-num;	b.Create(nx,nn,nz);
#pragma omp parallel for
		for(long k=0;k<nz;k++)
		{
			if(at>0)	memcpy(b.a+nx*nn*k, d->a+nx*ny*k,at*nx*sizeof(mreal));
			memcpy(b.a+nx*(at+nn*k), d->a+nx*(at+num+ny*k),(ny-at-num)*nx*sizeof(mreal));
		}
		d->Set(b);	ny-=num;
	}
	if(dir=='z')
	{
		if(at+num>=nz)	return;
		b.Create(nx,ny,nz-num);
		if(at>0)	memcpy(b.a, d->a,at*nx*ny*sizeof(mreal));
		memcpy(b.a+nx*ny*at, d->a+nx*ny*(at+num),(nz-at-num)*nx*ny*sizeof(mreal));
		d->Set(b);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_insert_(uintptr_t *d, const char *dir, int *at, int *num, int)
{	mgl_data_insert(_DT_,*dir,*at,*num);	}
void MGL_EXPORT mgl_data_delete_(uintptr_t *d, const char *dir, int *at, int *num, int)
{	mgl_data_delete(_DT_,*dir,*at,*num);	}
//-----------------------------------------------------------------------------
#define omod(x,y)	(y)*((x)>0?int((x)/(y)+0.5):int((x)/(y)-0.5))
void static mgl_omod(mreal *a, mreal da, int nx, int n)
{
	bool qq=true;
	for(long i=1;i<nx;i++)
	{
		long ii = i*n;
		if(mgl_isnan(a[ii-n]))	{	qq=true;	continue;	}
		if(qq)
		{
			a[ii] += omod(a[ii-n]-a[ii], da);
			qq=false;
		}
		else
		{
			mreal q = 2*a[ii-n]-a[ii-2*n];
			a[ii] += omod(q-a[ii], da);
		}
	}
}
//-----------------------------------------------------------------------------
static void *mgl_sew_z(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nz=t->p[2], nn=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)	mgl_omod(t->a+i, t->b[0], nz, nn);
	return 0;
}
static void *mgl_sew_y(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)	mgl_omod(t->a+(i%nx)+nx*ny*(i/nx), t->b[0], ny, nx);
	return 0;
}
static void *mgl_sew_x(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)	mgl_omod(t->a+i*nx, t->b[0], nx, 1);
	return 0;
}
void MGL_EXPORT mgl_data_sew(HMDT d, const char *dirs, mreal delta)
{
	if(!dirs || *dirs==0)	return;
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long p[3]={nx,ny,nz};
	mreal da = delta;
	if(strchr(dirs,'x') && nx>1)	mglStartThread(mgl_sew_x,0,nz*ny,d->a,&da,0,p);
	if(strchr(dirs,'y') && ny>1)	mglStartThread(mgl_sew_y,0,nz*nx,d->a,&da,0,p);
	if(strchr(dirs,'z') && nz>1)	mglStartThread(mgl_sew_z,0,nx*ny,d->a,&da,0,p);
}
void MGL_EXPORT mgl_data_sew_(uintptr_t *d, const char *dirs, mreal *da, int l)
{	char *s=new char[l+1];	memcpy(s,dirs,l);	s[l]=0;
	mgl_data_sew(_DT_,s,*da);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_put_val(HMDT d, mreal val, long xx, long yy, long zz)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	if(xx>=nx || yy>=ny || zz>=nz)	return;
	mreal *a=d->a;
	if(xx>=0 && yy>=0 && zz>=0)	a[xx+nx*(yy+zz*ny)] = val;
	else if(xx<0 && yy<0 && zz<0)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	a[i] = val;
	else if(xx<0 && yy<0)
#pragma omp parallel for
		for(long i=0;i<nx*ny;i++)	a[i+zz*nx*ny] = val;
	else if(yy<0 && zz<0)
#pragma omp parallel for
		for(long i=0;i<nz*ny;i++)	a[xx+i*nx] = val;
	else if(xx<0 && zz<0)
#pragma omp parallel for collapse(2)
		for(long j=0;j<nz;j++)	for(long i=0;i<nx;i++)	a[i+nx*(yy+j*ny)] = val;
	else if(xx<0)
#pragma omp parallel for
		for(long i=0;i<nx;i++)	a[i+nx*(yy+zz*ny)] = val;
	else if(yy<0)
#pragma omp parallel for
		for(long i=0;i<ny;i++)	a[xx+nx*(i+zz*ny)] = val;
	else //if(zz<0)
#pragma omp parallel for
		for(long i=0;i<nz;i++)	a[xx+nx*(yy+i*ny)] = val;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_put_dat(HMDT d, HCDT v, long xx, long yy, long zz)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	if(xx>=nx || yy>=ny || zz>=nz)	return;
	const mglData *mv = dynamic_cast<const mglData *>(v);
	mreal *a=d->a, vv=v->v(0);
	const mreal *b = mv?mv->a:0;
	long vx=v->GetNx(), vy=v->GetNy(), vz=v->GetNz();
	if(xx<0 && yy<0 && zz<0)	// whole array
	{
		long nn = vx>=nx?nx:vx, mm = vy>=ny?ny:vy, ll = vz>=nz?nz:vz;
		if(nn>1 && mm>1 && ll>1)
// #pragma omp parallel for
			for(long k=0;k<ll;k++)	for(long j=0;j<mm;j++)	for(long i=0;i<nn;i++)
				a[i+nx*(j+k*ny)] = b?b[i+vx*(j+k*vy)]:v->v(i,j,k);
		if(nn>1 && mm>1)
// #pragma omp parallel for
			for(long k=0;k<nz;k++)	for(long j=0;j<mm;j++)	for(long i=0;i<nn;i++)
				a[i+nx*(j+k*ny)] = b?b[i+vx*j]:v->v(i,j);
		else if(nn>1)
// #pragma omp parallel for
			for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nn;i++)
				a[i+nx*(j+k*ny)] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long ii=0;ii<nx*ny*nz;ii++)	a[ii] = vv;
	}
	else if(xx<0 && yy<0)	// 2d
	{
		zz*=nx*ny;
		long nn = vx>=nx?nx:vx, mm = vy>=ny?ny:vy;
		if(nn>1 && mm>1)
// #pragma omp parallel for
			for(long j=0;j<mm;j++)	for(long i=0;i<nn;i++)
				a[i+j*nx+zz] = b?b[i+vx*j]:v->v(i,j);
		else if(nn>1)
// #pragma omp parallel for
			for(long j=0;j<ny;j++)	for(long i=0;i<nn;i++)
				a[i+j*nx+xx] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long ii=0;ii<nx*ny;ii++) 	a[ii+zz] = vv;
	}
	else if(yy<0 && zz<0)	// 2d
	{
		long nn = vx>=ny?ny:vx, mm = vy>=nz?nz:vy;
		if(nn>1 && mm>1)
// #pragma omp parallel for collapse(2)
			for(long j=0;j<mm;j++)	for(long i=0;i<nn;i++)
				a[(i+ny*j)*nx+xx] = b?b[i+vx*j]:v->v(i,j);
		else if(nn>1)
// #pragma omp parallel for collapse(2)
			for(long j=0;j<nz;j++)	for(long i=0;i<nn;i++)
				a[(i+ny*j)*nx+xx] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long ii=0;ii<ny*nz;ii++) 	a[ii*nx+xx] = vv;
	}
	else if(xx<0 && zz<0)	// 2d
	{
		yy *= nx;	zz = nx*ny;
		long nn = vx>=nx?nx:vx, mm = vy>=nz?nz:vy;
		if(nn>1 && mm>1)
// #pragma omp parallel for collapse(2)
			for(long j=0;j<mm;j++)	for(long i=0;i<nn;i++)
				a[i+yy+j*zz] = b?b[i+vx*j]:v->v(i,j);
		else if(nn>1)
// #pragma omp parallel for collapse(2)
			for(long j=0;j<nz;j++)	for(long i=0;i<nn;i++)
				a[i+yy+j*zz] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long ii=0;ii<nx*nz;ii++)
			{	long i=ii%nx, j=ii/nx;	a[i+yy+j*zz] = vv;	}
	}
	else if(xx<0)
	{
		xx = nx*(yy+zz*ny);
		long nn = vx>=nx?nx:vx;
		if(nn>1)
// #pragma omp parallel for
			for(long i=0;i<nn;i++)	a[i+xx] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long i=0;i<nx;i++)	a[i+xx] = vv;
	}
	else if(yy<0)
	{
		xx += zz*nx*ny;
		long nn = vx>=ny?ny:vx;
		if(nn>1)
// #pragma omp parallel for
			for(long i=0;i<nn;i++)	a[xx+nx*i] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long i=0;i<ny;i++)	a[xx+nx*i] = vv;
	}
	else if(zz<0)
	{
		xx += nx*yy;	yy = nx*ny;
		long nn = vx>=nz?nz:vx;
		if(nn>1)
// #pragma omp parallel for
			for(long i=0;i<nn;i++)	a[xx+yy*i] = b?b[i]:v->v(i);
		else
// #pragma omp parallel for
			for(long i=0;i<nz;i++)	a[xx+yy*i] = vv;
	}
	else	a[xx+nx*(yy+ny*zz)] = vv;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_put_val_(uintptr_t *d, mreal *val, int *i, int *j, int *k)
{	mgl_data_put_val(_DT_,*val, *i,*j,*k);	}
void MGL_EXPORT mgl_data_put_dat_(uintptr_t *d, uintptr_t *val, int *i, int *j, int *k)
{	mgl_data_put_dat(_DT_,_DA_(val), *i,*j,*k);	}
//-----------------------------------------------------------------------------
static void *mgl_diff_3(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nz=t->p[2], nn=t->n, n2=nx*ny;
	mreal *b=t->a,au,av,aw,xu,xv,xw,yu,yv,yw,zu,zv,zw;
	HCDT x=(HCDT)(t->c),y=(HCDT)(t->d),z=(HCDT)(t->e);
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for private(au,av,aw,xu,xv,xw,yu,yv,yw,zu,zv,zw)
#endif
	for(long i0=t->id;i0<nn;i0+=mglNumThr)
	{
		long i=i0%nx, j=((i0/nx)%ny), k=i0/(nx*ny);
		if(i==0)
		{
			au = 3*a[i0]-4*a[i0+1]+a[i0+2];
			xu = 3*x->vthr(i0)-4*x->vthr(i0+1)+x->vthr(i0+2);
			yu = 3*y->vthr(i0)-4*y->vthr(i0+1)+y->vthr(i0+2);
			zu = 3*z->vthr(i0)-4*z->vthr(i0+1)+z->vthr(i0+2);
		}
		else if(i==nx-1)
		{
			au = 3*a[i0]-4*a[i0-1]+a[i0-2];
			xu = 3*x->vthr(i0)-4*x->vthr(i0-1)+x->vthr(i0-2);
			yu = 3*y->vthr(i0)-4*y->vthr(i0-1)+y->vthr(i0-2);
			zu = 3*z->vthr(i0)-4*z->vthr(i0-1)+z->vthr(i0-2);
		}
		else
		{
			au = a[i0+1]-a[i0-1];
			xu = x->vthr(i0+1)-x->vthr(i0-1);
			yu = y->vthr(i0+1)-y->vthr(i0-1);
			zu = z->vthr(i0+1)-z->vthr(i0-1);
		}
		if(j==0)
		{
			av = 3*a[i0]-4*a[i0+nx]+a[i0+2*nx];
			xv = 3*x->vthr(i0)-4*x->vthr(i0+nx)+x->vthr(i0+2*nx);
			yv = 3*y->vthr(i0)-4*y->vthr(i0+nx)+y->vthr(i0+2*nx);
			zv = 3*z->vthr(i0)-4*z->vthr(i0+nx)+z->vthr(i0+2*nx);
		}
		else if(j==ny-1)
		{
			av = 3*a[i0]-4*a[i0-nx]+a[i0+(ny-3)*nx];
			xv = 3*x->vthr(i0)-4*x->vthr(i0-nx)+x->vthr(i0-2*nx);
			yv = 3*y->vthr(i0)-4*y->vthr(i0-nx)+y->vthr(i0-2*nx);
			zv = 3*z->vthr(i0)-4*z->vthr(i0-nx)+z->vthr(i0-2*nx);
		}
		else
		{
			av = a[i0+nx]-a[i0-nx];
			xv = x->vthr(i0+nx)-x->vthr(i0-nx);
			yv = y->vthr(i0+nx)-y->vthr(i0-nx);
			zv = z->vthr(i0+nx)-z->vthr(i0-nx);
		}
		if(k==0)
		{
			aw = 3*a[i0]-4*a[i0+n2]+a[i0+2*n2];
			xw = 3*x->vthr(i0)-4*x->vthr(i0+n2)+x->vthr(i0+2*n2);
			yw = 3*y->vthr(i0)-4*y->vthr(i0+n2)+y->vthr(i0+2*n2);
			zw = 3*z->vthr(i0)-4*z->vthr(i0+n2)+z->vthr(i0+2*n2);
		}
		else if(k==nz-1)
		{
			aw = 3*a[i0]-4*a[i0-n2]+a[i0-2*n2];
			xw = 3*x->vthr(i0)-4*x->vthr(i0-n2)+x->vthr(i0-2*n2);
			yw = 3*y->vthr(i0)-4*y->vthr(i0-n2)+y->vthr(i0-2*n2);
			zw = 3*z->vthr(i0)-4*z->vthr(i0-n2)+z->vthr(i0-2*n2);
		}
		else
		{
			aw = a[i0+n2]-a[i0-n2];
			xw = x->vthr(i0+n2)-x->vthr(i0-n2);
			yw = y->vthr(i0+n2)-y->vthr(i0-n2);
			zw = z->vthr(i0+n2)-z->vthr(i0-n2);
		}
		b[i0] = (au*yv*zw-av*yu*zw-au*yw*zv+aw*yu*zv+av*yw*zu-aw*yv*zu) / (xu*yv*zw-xv*yu*zw-xu*yw*zv+xw*yu*zv+xv*yw*zu-xw*yv*zu);
	}
	return 0;
}
static void *mgl_diff_2(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n, same=t->p[2];
	mreal *b=t->a,au,av,xu,xv,yu,yv;
	HCDT x=(HCDT)(t->c),y=(HCDT)(t->d);
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for private(au,av,xu,xv,yu,yv)
#endif
	for(long i0=t->id;i0<nn;i0+=mglNumThr)
	{
		long i=i0%nx, j=((i0/nx)%ny), i1 = same ? i0 : i0%(nx*ny);
		if(i==0)
		{
			au = 3*a[i0]-4*a[i0+1]+a[i0+2];
			xu = 3*x->vthr(i1)-4*x->vthr(i1+1)+x->vthr(i1+2);
			yu = 3*y->vthr(i1)-4*y->vthr(i1+1)+y->vthr(i1+2);
		}
		else if(i==nx-1)
		{
			au = 3*a[i0]-4*a[i0-1]+a[i0-2];
			xu = 3*x->vthr(i1)-4*x->vthr(i1-1)+x->vthr(i1-2);
			yu = 3*y->vthr(i1)-4*y->vthr(i1-1)+y->vthr(i1-2);
		}
		else
		{
			au = a[i0+1]-a[i0-1];
			xu = x->vthr(i1+1)-x->vthr(i1-1);
			yu = y->vthr(i1+1)-y->vthr(i1-1);
		}
		if(j==0)
		{
			av = 3*a[i0]-4*a[i0+nx]+a[i0+2*nx];
			xv = 3*x->vthr(i1)-4*x->vthr(i1+nx)+x->vthr(i1+2*nx);
			yv = 3*y->vthr(i1)-4*y->vthr(i1+nx)+y->vthr(i1+2*nx);
		}
		else if(j==ny-1)
		{
			av = 3*a[i0]-4*a[i0-nx]+a[i0-2*nx];
			xv = 3*x->vthr(i1)-4*x->vthr(i1-nx)+x->vthr(i1-2*nx);
			yv = 3*y->vthr(i1)-4*y->vthr(i1-nx)+y->vthr(i1-2*nx);
		}
		else
		{
			av = a[i0+nx]-a[i0-nx];
			xv = x->vthr(i1+nx)-x->vthr(i1-nx);
			yv = y->vthr(i1+nx)-y->vthr(i1-nx);
		}
		b[i0] = (av*yu-au*yv)/(xv*yu-xu*yv);
	}
	return 0;
}
static void *mgl_diff_1(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0], nn=t->n, same=t->p[1];
	mreal *b=t->a,au,xu;
	HCDT x=(HCDT)(t->c);
	const mreal *a=t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for private(au,xu)
#endif
	for(long i0=t->id;i0<nn;i0+=mglNumThr)
	{
		long i=i0%nx, i1 = same ? i0 : i;
		if(i==0)
		{
			au = 3*a[i0]-4*a[i0+1]+a[i0+2];
			xu = 3*x->vthr(i1)-4*x->vthr(i1+1)+x->vthr(i1+2);
		}
		else if(i==nx-1)
		{
			au = 3*a[i0]-4*a[i0-1]+a[i0-2];
			xu = 3*x->vthr(i1)-4*x->vthr(i1-1)+x->vthr(i1-2);
		}
		else
		{
			au = a[i0+1]-a[i0-1];
			xu = x->vthr(i1+1)-x->vthr(i1-1);
		}
		b[i0] = au/xu;
	}
	return 0;
}
void MGL_EXPORT mgl_data_diff_par(HMDT d, HCDT x, HCDT y, HCDT z)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz(), nn=nx*ny*nz;
	if(nx<2 || ny<2)	return;
	mreal *b = new mreal[nn];	memset(b,0,nn*sizeof(mreal));
	long p[3]={nx,ny,nz};

	if(x&&y&&z && x->GetNN()==nn && y->GetNN()==nn && z->GetNN()==nn)
		mglStartThread(mgl_diff_3,0,nn,b,d->a,(const mreal *)x,p,0,(const mreal *)y,(const mreal *)z);
	else if(x&&y && x->GetNx()*x->GetNy()==nx*ny && y->GetNx()*y->GetNy()==nx*ny)
	{
		p[2]=(x->GetNz()==nz && y->GetNz()==nz);
		mglStartThread(mgl_diff_2,0,nn,b,d->a,(const mreal *)x,p,0,(const mreal *)y);
	}
	else if(x && x->GetNx()==nx)
	{
		p[1]=(x->GetNy()*x->GetNz()==ny*nz);
		mglStartThread(mgl_diff_1,0,nn,b,d->a,(const mreal *)x,p,0,0);
	}
	memcpy(d->a,b,nn*sizeof(mreal));	delete []b;
}
void MGL_EXPORT mgl_data_diff_par_(uintptr_t *d, uintptr_t *v1, uintptr_t *v2, uintptr_t *v3)
{	mgl_data_diff_par(_DT_,_DA_(v1),_DA_(v2),_DA_(v3));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_value(HMDT dat, mreal v, long i, long j, long k)
{	if(i>=0 && i<dat->nx && j>=0 && j<dat->ny && k>=0 && k<dat->nz)	dat->a[i+dat->nx*(j+dat->ny*k)]=v;	}
void MGL_EXPORT mgl_data_set_value_(uintptr_t *d, mreal *v, int *i, int *j, int *k)
{	mgl_data_set_value(_DT_,*v,*i,*j,*k);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_get_value(HCDT dat, long i, long j, long k)
{	long nx = dat->GetNx(), ny = dat->GetNy();
	return (i>=0 && i<nx && j>=0 && j<ny && k>=0 && k<dat->GetNz()) ? dat->vthr(i+nx*(j+ny*k)):NAN;	}
mreal MGL_EXPORT mgl_data_get_value_(uintptr_t *d, int *i, int *j, int *k)
{	return mgl_data_get_value(_DA_(d),*i,*j,*k);	}
//-----------------------------------------------------------------------------
MGL_EXPORT_PURE mreal *mgl_data_data(HMDT dat)	{	return dat->a;	}
//-----------------------------------------------------------------------------
MGL_EXPORT mreal *mgl_data_value(HMDT dat, long i,long j,long k)
{	long ii=i*dat->nx*(j+dat->ny*k);
	return	ii>=0 && ii<dat->GetNN() ? dat->a+ii : 0;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT mgl_data_get_nx(HCDT dat)	{	return dat->GetNx();	}
long MGL_EXPORT mgl_data_get_ny(HCDT dat)	{	return dat->GetNy();	}
long MGL_EXPORT mgl_data_get_nz(HCDT dat)	{	return dat->GetNz();	}
long MGL_EXPORT mgl_data_get_nx_(uintptr_t *d)	{	return _DA_(d)->GetNx();	}
long MGL_EXPORT mgl_data_get_ny_(uintptr_t *d)	{	return _DA_(d)->GetNy();	}
long MGL_EXPORT mgl_data_get_nz_(uintptr_t *d)	{	return _DA_(d)->GetNz();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_join(HMDT d, HCDT v)
{
	if(!d || !v)	return;
	long nx=d->nx, ny=d->ny, nz=d->nz, k=nx*ny*nz;
	const mglData *mv = dynamic_cast<const mglData *>(v);
	long vx=v->GetNx(), vy=v->GetNy(), vz=v->GetNz(), m = vx*vy*vz;

	if(nx==vx && ny==vy && ny>1)	d->nz += vz;
	else
	{
		ny *= nz;	vy *= vz;
		if(nx==vx && nx>1)
		{	d->nz = 1;	d->ny = ny+vy;	}
		else
		{	d->ny = d->nz = 1;	d->nx = k+m;	}
	}
	mreal *b = new mreal[k+m];
	memcpy(b,d->a,k*sizeof(mreal));
	if(mv)	memcpy(b+k,mv->a,m*sizeof(mreal));
	else
#pragma omp parallel for
		for(long i=0;i<m;i++)	b[k+i] = v->vthr(i);
	if(!d->link)	delete []d->a;
	d->a = b;	d->link=false;	d->NewId();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_join_(uintptr_t *d, uintptr_t *val)
{	mgl_data_join(_DT_,_DA_(val));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_gs(HMDT dat, HCDT xdat, HCDT vdat, mreal x1, mreal x2, long sl)
{
	HMDT coef = mgl_gspline_init(xdat, vdat);
	if(!coef)	return;	// incompatible dimensions
	const long nx = dat->nx, nn=dat->ny*dat->nz;
	mreal x0 = x1-xdat->v(0), dx = (x2-x1)/(nx-1);
#pragma omp parallel for
	for(long i=0;i<nx;i++)
	{
		mreal d = mgl_gspline(coef,x0+dx*i,0,0);
		if(sl<0)	for(long j=0;j<nn;j++)	dat->a[i+j*nx] = d;
		else	dat->a[i+sl*nx] = d;
	}
	mgl_delete_data(coef);
}
//-----------------------------------------------------------------------------
mreal MGL_NO_EXPORT mgl_index_1(mreal v, HCDT dat)
{
	long mx=dat->GetNx();
	mreal d,d1=0,d2=mx-1,v1,v2;
	v1 = dat->value(d1,0,0);
	v2 = dat->value(d2,0,0);
	long count=0;

	const mreal eps = MGL_EPSILON-1.;
	if(fabs(v-v1)<eps)	return d1;
	if(fabs(v-v2)<eps)	return d2;
	if((v1-v)*(v2-v)>0)	return NAN;
	do
	{
		d = count<10?(d2-d1)*(v-v1)/(v2-v1)+d1:(d1+d2)/2;	count++;
		mreal val = dat->value(d,0,0);
//		if(fabs(val-v)<acx)	break;
		if(val==v || d2-d<eps)	break;
		if((v1-v)*(val-v)<0)	{	v2=val;	d2=d;	}	else	{	v1=val;	d1=d;	}
	} while(fabs(d2-d1)>1e-5);
	return d;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_x(HMDT dat, HCDT xdat, HCDT vdat, mreal x1, mreal x2, long sl)
{
	long nx=dat->nx,mx=vdat->GetNx(),nn=dat->ny*dat->nz;
	if(mx!=xdat->GetNx())	return;	// incompatible dimensions
	mreal dx = (x2-x1)/(nx-1);
#pragma omp parallel for
	for(long i=0;i<nx;i++)
	{
		mreal u = mgl_index_1(x1+dx*i,xdat);
		mreal d = vdat->value(u,0,0);
		if(sl<0)	for(long j=0;j<nn;j++)	dat->a[i+j*nx] = d;
		else	dat->a[i+sl*nx] = d;
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_xy(HMDT dat, HCDT xdat, HCDT ydat, HCDT vdat, mreal x1, mreal x2, mreal y1, mreal y2, long sl)
{
	long nx=dat->nx,ny=dat->ny,nz=dat->nz,mx=vdat->GetNx(),my=vdat->GetNy(),nn=nx*ny;
	bool both=(xdat->GetNN()==vdat->GetNN() && ydat->GetNN()==vdat->GetNN());
	if(!both && (xdat->GetNx()!=mx || ydat->GetNx()!=my))	return;	// incompatible dimensions
	mreal dx = (x2-x1)/(nx-1), dy = (y2-y1)/(ny-1);
	if(both)
	{
#pragma omp parallel for
		for(long i=0;i<nn*nz;i++)	dat->a[i]=NAN;
#pragma omp parallel for collapse(2)
		for(long j=0;j<my-1;j++)	for(long i=0;i<mx-1;i++)
		{
			long i0 = i+mx*j;
			mreal vx0 = (xdat->vthr(i0)-x1)/dx, vy0 = (ydat->vthr(i0)-y1)/dy;
			mreal vx1 = (xdat->vthr(i0+1)-x1)/dx, vy1 = (ydat->vthr(i0+1)-y1)/dy;
			mreal vx2 = (xdat->vthr(i0+mx)-x1)/dx, vy2 = (ydat->vthr(i0+mx)-y1)/dy;
			mreal vx3 = (xdat->vthr(i0+mx+1)-x1)/dx, vy3 = (ydat->vthr(i0+mx+1)-y1)/dy;
			long xx1 = long(mgl_min( mgl_min(vx0,vx1), mgl_min(vx2,vx3) ));	// bounding box
			long yy1 = long(mgl_min( mgl_min(vy0,vy1), mgl_min(vy2,vy3) ));
			long xx2 = long(mgl_max( mgl_max(vx0,vx1), mgl_max(vx2,vx3) ));
			long yy2 = long(mgl_max( mgl_max(vy0,vy1), mgl_max(vy2,vy3) ));
			xx1=mgl_imax(xx1,0);	xx2=mgl_imin(xx2,nx-1);
			yy1=mgl_imax(yy1,0);	yy2=mgl_imin(yy2,ny-1);
			if(xx1>xx2 || yy1>yy2)	continue;

			mreal d1x = vx1-vx0, d1y = vy1-vy0;
			mreal d2x = vx2-vx0, d2y = vy2-vy0;
			mreal d3x = vx3+vx0-vx1-vx2, d3y = vy3+vy0-vy1-vy2;
			mreal dd = d1x*d2y-d1y*d2x;
			mreal dsx =-4*(d2y*d3x - d2x*d3y)*d1y;
			mreal dsy = 4*(d2y*d3x - d2x*d3y)*d1x;

			for(long jj=yy1;jj<=yy2;jj++)	for(long ii=xx1;ii<=xx2;ii++)
			{
				mreal xx = (ii-vx0), yy = (jj-vy0);
				mreal s = dsx*xx + dsy*yy + (dd+d3y*xx-d3x*yy)*(dd+d3y*xx-d3x*yy);
				if(s>=0)
				{
					s = sqrt(s);
					mreal qu = d3x*yy - d3y*xx + dd + s;
					mreal qv = d3y*xx - d3x*yy + dd + s;
					mreal u = 2.f*(d2y*xx - d2x*yy)/qu;
					mreal v = 2.f*(d1x*yy - d1y*xx)/qv;
					if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	// first root bad
					{
						qu = d3x*yy - d3y*xx + dd - s;
						qv = d3y*xx - d3x*yy + dd - s;
						u = 2.f*(d2y*xx - d2x*yy)/qu;
						v = 2.f*(d1x*yy - d1y*xx)/qv;
						if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	continue;	// second root bad
					}
					i0 = ii+nx*jj;	s = vdat->value(i+u,j+v,0);
					if(sl<0)	for(long k=0;k<nz;k++)	dat->a[i0+k*nn] = s;
					else	dat->a[i0+sl*nn] = s;
				}
			}
		}
	}
	else
	{
		mglData u(nx), v(ny);
#pragma omp parallel for
		for(long i=0;i<nx;i++)	u.a[i] = mgl_index_1(x1+dx*i,xdat);
#pragma omp parallel for
		for(long i=0;i<ny;i++)	v.a[i] = mgl_index_1(y1+dy*i,ydat);
#pragma omp parallel for collapse(2)
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			mreal d = vdat->value(u.a[i],v.a[j],0);
			long i0=i+nx*j;
			if(sl<0)	for(long k=0;k<nz;k++)	dat->a[i0+k*nn] = d;
			else	dat->a[i0+sl*nn] = d;
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_xyz(HMDT dat, HCDT xdat, HCDT ydat, HCDT zdat, HCDT vdat, mreal x1, mreal x2, mreal y1, mreal y2, mreal z1, mreal z2)
{
	if(!dat || !xdat || !ydat || !zdat || !vdat)	return;
	long nx=dat->nx,ny=dat->ny,nz=dat->nz,mx=vdat->GetNx(),my=vdat->GetNy(),mz=vdat->GetNz();
	bool both=(xdat->GetNN()==vdat->GetNN() && ydat->GetNN()==vdat->GetNN() && zdat->GetNN()==vdat->GetNN());
	if(!both && (xdat->GetNx()!=mx || ydat->GetNx()!=my || zdat->GetNx()!=mz))	return;	// incompatible dimensions
	const mreal acx=1e-6*fabs(x2-x1), acy=1e-6*fabs(y2-y1), acz=1e-6*fabs(z2-z1);
	if(both)
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			mreal xx = x1+(x2-x1)*i/(nx-1.),dxx,dxy,dxz,vx,dx=0,dd;
			mreal yy = y1+(y2-y1)*j/(ny-1.),dyx,dyy,dyz,vy,dy=0;
			mreal zz = z1+(z2-z1)*k/(nz-1.),dzx,dzy,dzz,vz,dz=0;
			vx = xdat->valueD(dx,dy,dz,&dxx,&dxy,&dxz);
			vy = ydat->valueD(dx,dy,dz,&dyx,&dyy,&dyz);
			vz = zdat->valueD(dx,dy,dz,&dzx,&dzy,&dzz);
			long count=0;
			do	// use Newton method to find root
			{
				if(count>50)	{	dx=NAN;	break;	}	count++;
				dd = -dxx*dyy*dzz+dxy*dyx*dzz+dxx*dyz*dzy-dxz*dyx*dzy-dxy*dyz*dzx+dxz*dyy*dzx;
				dx += ((dyz*dzy-dyy*dzz)*(xx-vx)+(dxy*dzz-dxz*dzy)*(yy-vy)+(dxz*dyy-dxy*dyz)*(zz-vz))/dd;
				dy += ((dyx*dzz-dyz*dzx)*(xx-vx)+(dxz*dzx-dxx*dzz)*(yy-vy)+(dxx*dyz-dxz*dyx)*(zz-vz))/dd;
				dz += ((dyy*dzx-dyx*dzy)*(xx-vx)+(dxx*dzy-dxy*dzx)*(yy-vy)+(dxy*dyx-dxx*dyy)*(zz-vz))/dd;
				vx = xdat->valueD(dx,dy,dz,&dxx,&dxy,&dxz);
				vy = ydat->valueD(dx,dy,dz,&dyx,&dyy,&dyz);
				vz = zdat->valueD(dx,dy,dz,&dzx,&dzy,&dzz);
			}	while(fabs(xx-vx)>acx && fabs(yy-vy)>acy && fabs(zz-vz)>acz);	// this valid for linear interpolation
			dat->a[i+nx*(j+ny*k)] = mgl_isnan(dx)?NAN:vdat->value(dx,dy,dz);
		}
	else
	{
		mglData u(nx), v(ny), w(nz);
		mreal dx = (x2-x1)/(nx-1), dy = (y2-y1)/(ny-1), dz = (z2-z1)/(nz-1);
#pragma omp parallel for
		for(long i=0;i<nx;i++)	u.a[i] = mgl_index_1(x1+dx*i,xdat);
#pragma omp parallel for
		for(long i=0;i<ny;i++)	v.a[i] = mgl_index_1(y1+dy*i,ydat);
#pragma omp parallel for
		for(long i=0;i<nz;i++)	w.a[i] = mgl_index_1(z1+dz*i,zdat);
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			dat->a[i+nx*(j+ny*k)] = vdat->value(u.a[i],v.a[j],w.a[k]);
	}
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_evaluate(HCDT dat, HCDT idat, HCDT jdat, HCDT kdat, int norm)
{
	if(!idat || (jdat && jdat->GetNN()!=idat->GetNN()) || (kdat && kdat->GetNN()!=idat->GetNN()))	return 0;
	const mglData *dd=dynamic_cast<const mglData *>(dat);
	long nx=dat->GetNx(), ny=dat->GetNy(), nz=dat->GetNz();
	mglData *r=new mglData(idat->GetNx(),idat->GetNy(),idat->GetNz());
	mreal dx = nx-1, dy = ny-1, dz = nz-1;
	if(!norm)	dx=dy=dz=1;
	if(dd)
#pragma omp parallel for
		for(long i=0;i<idat->GetNN();i++)
		{
			mreal x=dx*idat->vthr(i), y=jdat?dy*jdat->vthr(i):0, z=kdat?dz*kdat->vthr(i):0;
			r->a[i] = mgl_isnum(x*y*z)?mglSpline3st<mreal>(dd->a,nx,ny,nz, x,y,z):NAN;
		}
	else
#pragma omp parallel for
		for(long i=0;i<idat->GetNN();i++)
		{
			mreal x=dx*idat->vthr(i), y=jdat?dy*jdat->vthr(i):0, z=kdat?dz*kdat->vthr(i):0;
			r->a[i] = mgl_isnum(x*y*z)?dat->linear(x,y,z):NAN;;
		}
	return r;
}
uintptr_t MGL_EXPORT mgl_data_evaluate_(uintptr_t *d, uintptr_t *idat, uintptr_t *jdat, uintptr_t *kdat, int *norm)
{	return uintptr_t(mgl_data_evaluate(_DT_,_DA_(idat),_DA_(jdat),_DA_(kdat),*norm));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_gspline_init(HCDT x, HCDT v)
{
	long n = v->GetNx();
	if(!x || x->GetNx()!=n)	return 0;
	mglData *res = new mglData(5*(n-1));
	mreal *xx=0, *vv=0;
	const mglData *dx = dynamic_cast<const mglData *>(x);
	if(!dx)
	{
		xx = new mreal[n];
		for(long i=0;i<n;i++)	xx[i] = x->v(i);
	}
	const mglData *dv = dynamic_cast<const mglData *>(v);
	if(!dv)
	{
		vv = new mreal[n];
		for(long i=0;i<n;i++)	vv[i] = v->v(i);
	}
	mgl_gspline_init(n,dx?dx->a:xx,dv?dv->a:vv,res->a);
	if(xx)	delete []xx;
	if(vv)	delete []vv;
	return res;
}
uintptr_t MGL_EXPORT mgl_gspline_init_(uintptr_t *x, uintptr_t *v)
{	return uintptr_t(mgl_gspline_init(_DA_(x),_DA_(v)));	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_gspline(HCDT c, mreal dx, mreal *d1, mreal *d2)
{
	long i=0, n = c->GetNx();
	if(n%5 || dx<0)	return NAN;	// not the table of coefficients
	while(dx>c->v(5*i))
	{
		dx-=c->v(5*i);	i++;
		if(5*i>=n)	return NAN;
	}
	if(d1)	*d1 = c->v(5*i+2)+dx*(2*c->v(5*i+3)+3*dx*c->v(5*i+4));
	if(d2)	*d2 = 2*c->v(5*i+3)+6*dx*c->v(5*i+4);
	return c->v(5*i+1)+dx*(c->v(5*i+2)+dx*(c->v(5*i+3)+dx*c->v(5*i+4)));
}
mreal MGL_EXPORT mgl_gspline_(uintptr_t *c, mreal *dx, mreal *d1, mreal *d2)
{	return mgl_gspline(_DA_(c),*dx,d1,d2);	}
//-----------------------------------------------------------------------------
struct pnt	{	mreal x,y;	pnt(mreal X, mreal Y):x(X),y(Y){}	};
mreal static mgl_find_pnt(std::vector<mreal> &mpos, mreal est, mreal dj)
{
	mreal mv = dj, val=NAN;
	size_t n = mpos.size(), kk=0;
	for(size_t k=0;k<n;k++)
	{
		mreal de = mpos[k]-est;
		if(fabs(de)<fabs(mv))	{	mv=de;	val=de+est;	kk=k;	}
	}
	if(mgl_isnum(val))	mpos.erase(mpos.begin()+kk);
	return val;
}
HMDT MGL_EXPORT mgl_data_detect(HCDT d, mreal lvl, mreal dj, mreal di, mreal min_len)
{
	long nx=d->GetNx(), ny=d->GetNy();
	std::vector<mreal> *max_pos = new std::vector<mreal>[nx];
	if(di<=0)	di=dj;
	for(long i=0;i<nx;i++)	// first collect maximums for each i
	{
		for(long j=1;j<ny-1;j++)
		{
			mreal v = d->v(i,j), v1 = d->v(i,j-1), v2 = d->v(i,j+1);
			if(v>lvl && v1<v && v>=v2)	// NOTE only one edge is required
//			if(v>lvl && ((v1<=v && v>v2) || (v1<v && v>=v2)))	// NOTE only edges are required
			{
				bool c1=false, c2=false;
				for(long j1=j-1;j1>=0;j1--)
				{
					mreal vv = d->v(i,j1);
					if(vv>v)	break;
					if(vv<v/2)	{	c1=true;	break;	}
				}
				for(long j2=j+1;j2<ny;j2++)
				{
					mreal vv = d->v(i,j2);
					if(vv>v)	break;
					if(vv<v/2)	{	c2=true;	break;	}
				}
				if(c1 && c2)	max_pos[i].push_back(j + (v2-v1)/(2*v-v2-v1)/2);
			}
		}
	}
	std::vector<pnt> curv;
	for(long ii=0;ii<nx-1;ii++)	// now join points into curves
	{
		while(max_pos[ii].size())	// try to start curve
		{
			mreal vv = max_pos[ii].back();
			max_pos[ii].pop_back();
			pnt p1(ii,vv), p2(ii-1,vv);
			size_t ini = curv.size();
			curv.push_back(p1);
			for(long i=ii+1;i<nx;i++)	// join points to selected curve
			{
				bool none=true;
				for(long k=0;k<di && i+k<nx;k++)	// try next points
				{
					mreal val = mgl_find_pnt(max_pos[i+k],p1.y,dj);
					if(mgl_isnum(val))	// first try closest point (for noise data)
					{	p2=p1;	p1=pnt(i+k,val);	curv.push_back(p1);
						none=false;	i+=k;	break;	}
					else	// next try linear approximation
					{
						mreal est = p1.y + (k+1)*(p1.y-p2.y)/(p1.x-p2.x);
						val = mgl_find_pnt(max_pos[i+k],est,dj);
						if(mgl_isnum(val))
						{	p2=p1;	p1=pnt(i+k,val);	curv.push_back(p1);
							none=false;	i+=k;	break;	}
					}
				}
				if(none)	// break curve
				{	curv.push_back(pnt(NAN,NAN));	break;	}
			}
			if(mgl_isnum(curv.back().x))	curv.push_back(pnt(NAN,NAN));
			if(curv[curv.size()-2].x-curv[ini].x<min_len)
				curv.erase(curv.begin()+ini,curv.end());
		}
	}
	size_t nn = curv.size();
	HMDT res = new mglData(2,nn);
	for(size_t k=0;k<nn;k++)
	{	res->a[2*k] = curv[k].x;	res->a[2*k+1] = curv[k].y;	}
	delete []max_pos;	return res;
}
uintptr_t MGL_EXPORT mgl_data_detect_(uintptr_t *d, mreal *lvl, mreal *dj, mreal *di, mreal *min_len)
{	return uintptr_t(mgl_data_detect(_DT_,*lvl,*dj, *di, *min_len));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_dilate(HMDT d, mreal val, long step)
{
	long nx = d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
	if(step<1 || nx<2) return;
	long *dist = new long[nx*ny*nz], nn = nx*ny;
	bool done=false;
	if(nz>1 && ny>1)
	{
		done = true;
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = i+nx*(j+ny*k);
			if(d->vthr(i0)>=val)	dist[i0]=0;
			else
			{
				dist[i0] = nx+ny;
				if(i>0 && dist[i0-1]+1<dist[i0])	dist[i0]=dist[i0-1]+1;
				if(j>0 && dist[i0-nx]+1<dist[i0])	dist[i0]=dist[i0-nx]+1;
				if(k>0 && dist[i0-nn]+1<dist[i0])	dist[i0]=dist[i0-nn]+1;
			}
		}
		for(long k=nz-1;k>=0;k--)	for(long j=ny-1;j>=0;j--)	for(long i=nx-1;i>=0;i--)
		{
			long i0 = i+nx*(j+ny*k);
			if(i<nx-1 && dist[i0+1]+1<dist[i0])		dist[i0]=dist[i0+1]+1;
			if(j<ny-1 && dist[i0+nx]+1<dist[i0])	dist[i0]=dist[i0+nx]+1;
			if(k<nz-1 && dist[i0+nn]+1<dist[i0])	dist[i0]=dist[i0+nn]+1;
		}
	}
	else if(ny>1)
	{
		done = true;
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = i+nx*j;
			if(d->vthr(i0)>=val)	dist[i0]=0;
			else
			{
				dist[i0] = nx+ny;
				if(i>0 && dist[i0-1]+1<dist[i0])	dist[i0]=dist[i0-1]+1;
				if(j>0 && dist[i0-nx]+1<dist[i0])	dist[i0]=dist[i0-nx]+1;
			}
		}
		for(long j=ny-1;j>=0;j--)	for(long i=nx-1;i>=0;i--)
		{
			long i0 = i+nx*j;
			if(i<nx-1 && dist[i0+1]+1<dist[i0])		dist[i0]=dist[i0+1]+1;
			if(j<ny-1 && dist[i0+nx]+1<dist[i0])	dist[i0]=dist[i0+nx]+1;
		}
	}
	else
	{
		done = true;
		for(long i=0;i<nx;i++)
			if(d->v(i)>=val)	dist[i]=0;
			else
			{
				dist[i] = nx;
				if(i>0 && dist[i-1]+1<dist[i])	dist[i]=dist[i-1]+1;
			}
		for(long i=nx-2;i>=0;i--)
			if(dist[i+1]+1<dist[i])	dist[i]=dist[i+1]+1;
	}
	if(done)	for(long i=0;i<nx*ny*nz;i++)	d->a[i] = dist[i]<=step?1:0;
	delete []dist;
}
void MGL_EXPORT mgl_data_dilate_(uintptr_t *d, mreal *val, int *step)
{	mgl_data_dilate(_DT_,*val,*step);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_erode(HMDT d, mreal val, long step)
{
	long nx = d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
	if(step<1 || nx<2) return;
	long *dist = new long[nx*ny*nz], nn = nx*ny;
	bool done=false;
	if(nz>1 && ny>1)
	{
		done = true;
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = i+nx*(j+ny*k);
			if(d->vthr(i0)<val)	dist[i0]=0;
			else
			{
				dist[i0] = nx+ny;
				if(i>0 && dist[i0-1]+1<dist[i0])	dist[i0]=dist[i0-1]+1;
				if(j>0 && dist[i0-nx]+1<dist[i0])	dist[i0]=dist[i0-nx]+1;
				if(k>0 && dist[i0-nn]+1<dist[i0])	dist[i0]=dist[i0-nn]+1;
			}
		}
		for(long k=nz-1;k>=0;k--)	for(long j=ny-1;j>=0;j--)	for(long i=nx-1;i>=0;i--)
		{
			long i0 = i+nx*(j+ny*k);
			if(i<nx-1 && dist[i0+1]+1<dist[i0])		dist[i0]=dist[i0+1]+1;
			if(j<ny-1 && dist[i0+nx]+1<dist[i0])	dist[i0]=dist[i0+nx]+1;
			if(k<nz-1 && dist[i0+nn]+1<dist[i0])	dist[i0]=dist[i0+nn]+1;
		}
	}
	else if(ny>1)
	{
		done = true;
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = i+nx*j;
			if(d->vthr(i0)<val)	dist[i0]=0;
			else
			{
				dist[i0] = nx+ny;
				if(i>0 && dist[i0-1]+1<dist[i0])	dist[i0]=dist[i0-1]+1;
				if(j>0 && dist[i0-nx]+1<dist[i0])	dist[i0]=dist[i0-nx]+1;
			}
		}
		for(long j=ny-1;j>=0;j--)	for(long i=nx-1;i>=0;i--)
		{
			long i0 = i+nx*j;
			if(i<nx-1 && dist[i0+1]+1<dist[i0])		dist[i0]=dist[i0+1]+1;
			if(j<ny-1 && dist[i0+nx]+1<dist[i0])	dist[i0]=dist[i0+nx]+1;
		}
	}
	else
	{
		done = true;
		for(long i=0;i<nx;i++)
			if(d->v(i)<val)	dist[i]=0;
			else
			{
				dist[i] = nx;
				if(i>0 && dist[i-1]+1<dist[i])	dist[i]=dist[i-1]+1;
			}
		for(long i=nx-2;i>=0;i--)
			if(dist[i+1]+1<dist[i])	dist[i]=dist[i+1]+1;
	}
	if(done)	for(long i=0;i<nx*ny*nz;i++)	d->a[i] = dist[i]>step?1:0;
	delete []dist;
}
void MGL_EXPORT mgl_data_erode_(uintptr_t *d, mreal *val, int *step)
{	mgl_data_erode(_DT_,*val,*step);	}
//-----------------------------------------------------------------------------
