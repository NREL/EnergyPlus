/***************************************************************************
 * data_new.cpp is part of Math Graphic Library
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
#include <set>
#include <stdarg.h>
#include <ctype.h>
#include "mgl2/data.h"
#include "mgl2/eval.h"
#include "mgl2/thread.h"
#include "interp.hpp"
void MGL_NO_EXPORT mgl_txt_func(const mreal *x, mreal *dx, void *par);
HMDT MGL_NO_EXPORT mglFormulaCalc(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_trace(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx);
	if(ny>=nx && nz>=nx)
#pragma omp parallel for
		for(long i=0;i<nx;i++)	r->a[i] = d->v(i,i,i);
	else if(ny>=nx)
#pragma omp parallel for
		for(long i=0;i<nx;i++)	r->a[i] = d->v(i,i);
	else
#pragma omp parallel for
		for(long i=0;i<nx;i++)	r->a[i] = d->v(i);
	return r;
}
uintptr_t MGL_EXPORT mgl_data_trace_(uintptr_t *d)
{	return uintptr_t(mgl_data_trace(_DT_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_subdata_ext(HCDT d, HCDT xx, HCDT yy, HCDT zz)
{
	if(!xx || !yy || !zz)
	{
		mglData tmp;	tmp.a[0]=-1;
		return mgl_data_subdata_ext(d,xx?xx:&tmp,yy?yy:&tmp,zz?zz:&tmp);
	}

	long n=0,m=0,l=0,j,k;
	bool ix=false, iy=false, iz=false;
	if(xx->GetNz()>1)	// 3d data
	{
		n = xx->GetNx();	m = xx->GetNy();	l = xx->GetNz();
		j = yy->GetNN();	if(j>1 && j!=n*m*l)	return 0;	// wrong sizes
		k = zz->GetNN();	if(k>1 && k!=n*m*l)	return 0;	// wrong sizes
		ix = true;	iy = j>1;	iz = k>1;
	}
	else if(yy->GetNz()>1)
	{
		n = yy->GetNx();	m = yy->GetNy();	l = yy->GetNz();
		j = xx->GetNN();	if(j>1 && j!=n*m*l)	return 0;	// wrong sizes
		k = zz->GetNN();	if(k>1 && k!=n*m*l)	return 0;	// wrong sizes
		iy = true;	ix = j>1;	iz = k>1;
	}
	else if(zz->GetNz()>1)
	{
		n = zz->GetNx();	m = zz->GetNy();	l = zz->GetNz();
		j = yy->GetNN();	if(j>1 && j!=n*m*l)	return 0;	// wrong sizes
		k = xx->GetNN();	if(k>1 && k!=n*m*l)	return 0;	// wrong sizes
		iz = true;	iy = j>1;	ix = k>1;
	}
	else if(xx->GetNy()>1)	// 2d data
	{
		n = xx->GetNx();	m = xx->GetNy();	l = 1;
		j = yy->GetNx()*yy->GetNy();	if(j>1 && j!=n*m)	return 0;	// wrong sizes
		k = zz->GetNx()*zz->GetNy();	if(k>1 && k!=n*m)	return 0;	// wrong sizes
		ix = true;	iy = j>1;	iz = k>1;
	}
	else if(yy->GetNy()>1)
	{
		n = yy->GetNx();	m = yy->GetNy();	l = 1;
		j = xx->GetNx()*xx->GetNy();	if(j>1 && j!=n*m)	return 0;	// wrong sizes
		k = zz->GetNx()*zz->GetNy();	if(k>1 && k!=n*m)	return 0;	// wrong sizes
		iy = true;	ix = j>1;	iz = k>1;
	}
	else if(zz->GetNy()>1)
	{
		n = zz->GetNx();	m = zz->GetNy();	l = 1;
		j = yy->GetNx()*yy->GetNy();	if(j>1 && j!=n*m)	return 0;	// wrong sizes
		k = xx->GetNx()*xx->GetNy();	if(k>1 && k!=n*m)	return 0;	// wrong sizes
		iz = true;	iy = j>1;	ix = k>1;
	}
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	long vx=long(xx->v(0)), vy=long(yy->v(0)), vz=long(zz->v(0));
	mglData *r;
	if(n*m*l>1)	// this is 2d or 3d data
	{
		mglDataV tx(n,m,l),ty(n,m,l),tz(n,m,l);
		if(!ix)	{	xx = &tx;	if(vx>=0)	tx.Fill(vx);	else tx.All();	}
		if(!iy)	{	yy = &ty;	if(vy>=0)	ty.Fill(vy);	else ty.All();	}
		if(!iz)	{	zz = &tz;	if(vz>=0)	tz.Fill(vz);	else tz.All();	}
		r=new mglData(n,m,l);
#pragma omp parallel for
		for(long i0=0;i0<n*m*l;i0++)
		{
			long x=long(0.5+xx->vthr(i0)), y=long(0.5+yy->vthr(i0)), z=long(0.5+zz->vthr(i0));
			r->a[i0] = (x>=0 && x<nx && y>=0 && y<ny && z>=0 && z<nz)?d->v(x,y,z):NAN;
		}
	}
	else	// this is 1d data -> try as normal SubData()
	{
		mglDataV tx(nx),ty(ny),tz(nz);	tx.All();	ty.All();	tz.All();
		if(xx->GetNx()>1 || vx>=0)	n=xx->GetNx();	else	{	n=nx;	xx = &tx;	}
		if(yy->GetNx()>1 || vy>=0)	m=yy->GetNx();	else	{	m=ny;	yy = &ty;	}
		if(zz->GetNx()>1 || vz>=0)	l=zz->GetNx();	else	{	l=nz;	zz = &tz;	}
		r=new mglData(n,m,l);
#pragma omp parallel for collapse(3)
		for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
		{
			long x=long(0.5+xx->v(i)), y=long(0.5+yy->v(j)), z=long(0.5+zz->v(k));
			r->a[i+n*(j+m*k)] = (x>=0 && x<nx && y>=0 && y<ny && z>=0 && z<nz)?d->v(x,y,z):NAN;
		}
		if(m==1)	{	r->ny=r->nz;	r->nz=1;	}// "squeeze" dimensions
		if(n==1)	{	r->nx=r->ny;	r->ny=r->nz;	r->nz=1;	r->NewId();}
	}
	return r;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_subdata(HCDT d, long xx,long yy,long zz)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz(), n=1,m=1,l=1;
	int dx=0,dy=0,dz=0;
	if(xx<0)	{	xx=0;	dx=1;	n=nx;	}
	if(yy<0)	{	yy=0;	dy=1;	m=ny;	}
	if(zz<0)	{	zz=0;	dz=1;	l=nz;	}
	mglData *r=new mglData(n,m,l);
	if(xx<nx && yy<ny && zz<nz)
#pragma omp parallel for collapse(3)
		for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			r->a[i+n*(j+m*k)] = d->v(xx+dx*i, yy+dy*j, zz+dz*k);
	else
#pragma omp parallel for
		for(long i=0;i<n*m*l;i++)	r->a[i] = NAN;
	if(m==1)	{	r->ny=r->nz;	r->nz=1;	}// "squeeze" dimensions
	if(n==1)	{	r->nx=r->ny;	r->ny=r->nz;	r->nz=1;	r->NewId();}
	return r;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_data_subdata_(uintptr_t *d, int *xx,int *yy,int *zz)
{	return uintptr_t(mgl_data_subdata(_DT_,*xx,*yy,*zz));	}
uintptr_t MGL_EXPORT mgl_data_subdata_ext_(uintptr_t *d, uintptr_t *xx, uintptr_t *yy, uintptr_t *zz)
{	return uintptr_t(mgl_data_subdata_ext(_DT_,_DA_(xx),_DA_(yy),_DA_(zz)));	}
//-----------------------------------------------------------------------------
static void *mgl_resize(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0]+0.1, ny=t->p[1]+0.1;
	mreal *b=t->a;
	const mreal *c=t->c;
	HCDT dat = (HCDT)(t->v);
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		long i=i0%nx, j=((i0/nx)%ny), k=i0/(nx*ny);
		b[i0] = dat->value(c[0]+i*c[1], c[2]+j*c[3], c[4]+k*c[5]);
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_resize_box(HCDT dat, long mx,long my,long mz, mreal x1,mreal x2, mreal y1,mreal y2, mreal z1,mreal z2)
{
	long nx = dat->GetNx(), ny = dat->GetNy(), nz = dat->GetNz();
	mx = mx<1 ? nx:mx;	my = my<1 ? ny:my;	mz = mz<1 ? nz:mz;
	mglData *r=new mglData(mx,my,mz);

	mreal par[6]={nx*x1,0,ny*y1,0,nz*z1,0};
	long nn[6]={mx,my,mz,nx,ny,nz};
	if(mx>1)	par[1] = (nx-1)*(x2-x1)/(mx-1);
	if(my>1)	par[3] = (ny-1)*(y2-y1)/(my-1);
	if(mz>1)	par[5] = (nz-1)*(z2-z1)/(mz-1);
	mglStartThread(mgl_resize,0,mx*my*mz,r->a,0,par,nn,dat);
	return r;
}
HMDT MGL_EXPORT mgl_data_resize(HCDT d, long mx,long my,long mz)
{	return mgl_data_resize_box(d, mx,my,mz,0,1,0,1,0,1);	}
uintptr_t MGL_EXPORT mgl_data_resize_(uintptr_t *d, int *mx,int *my,int *mz)
{	return uintptr_t(mgl_data_resize(_DT_,*mx,*my,*mz));	}
uintptr_t MGL_EXPORT mgl_data_resize_box_(uintptr_t *d, int *mx,int *my,int *mz, mreal *x1,mreal *x2, mreal *y1,mreal *y2, mreal *z1,mreal *z2)
{	return uintptr_t(mgl_data_resize_box(_DT_,*mx,*my,*mz,*x1,*x2,*y1,*y2,*z1,*z2));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_combine(HCDT d1, HCDT d2)
{
	long n1=d1->GetNy(),n2=d2->GetNx(),nx=d1->GetNx();
	if(d1->GetNz()>1 || (n1>1 && d2->GetNy()>1) || d2->GetNz()>1)	return 0;	// wrong dimensions
	mglData *r=new mglData;
	bool dim2=true;
	if(n1==1)	{	n1=n2;	n2=d2->GetNy();	dim2 = false;	}
	r->Create(nx,n1,n2);
	if(dim2)	n1*=nx;	else	{	n2*=n1;	n1=nx;	}
#pragma omp parallel for collapse(2)
	for(long j=0;j<n2;j++)	for(long i=0;i<n1;i++)
		r->a[i+n1*j] = d1->vthr(i)*d2->vthr(j);
	return r;
}
uintptr_t MGL_EXPORT mgl_data_combine_(uintptr_t *a, uintptr_t *b)
{	return uintptr_t(mgl_data_combine(_DA_(a),_DA_(b)));	}
//-----------------------------------------------------------------------------
static void *mgl_sum_z(void *par)
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
		b[i]=0;
		for(long j=0;j<nz;j++)	b[i] += a[i+nn*j];
		b[i] /= nz;
	}
	return 0;
}
static void *mgl_sum_y(void *par)
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
		long k = (i%nx)+nx*ny*(i/nx);	b[i]=0;
		for(long j=0;j<ny;j++)	b[i] += a[k+nx*j];
		b[i] /= ny;
	}
	return 0;
}
static void *mgl_sum_x(void *par)
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
		long k = i*nx;	b[i]=0;
		for(long j=0;j<nx;j++)	b[i] += a[j+k];
		b[i] /= nx;
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_sum(HCDT dat, const char *dir)
{
	if(!dir || *dir==0)	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nx*ny*nz];
	mreal *c = new mreal[nx*ny*nz];

	const mglData *d=dynamic_cast<const mglData *>(dat);
	if(d)	memcpy(c,d->a,nx*ny*nz*sizeof(mreal));
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	c[i]=dat->vthr(i);

	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_sum_z,0,nx*ny,b,c,0,p);
		memcpy(c,b,nx*ny*sizeof(mreal));	p[2] = 1;
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_sum_y,0,nx*p[2],b,c,0,p);
		memcpy(c,b,nx*p[2]*sizeof(mreal));	p[1] = p[2];	p[2] = 1;
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_sum_x,0,p[1]*p[2],b,c,0,p);
		p[0] = p[1];	p[1] = p[2];	p[2] = 1;
		memcpy(c,b,p[0]*p[1]*sizeof(mreal));
	}
	mglData *r=new mglData(p[0],p[1],p[2]);
	memcpy(r->a,c,p[0]*p[1]*p[2]*sizeof(mreal));
	delete []b;	delete []c;	return r;
}
uintptr_t MGL_EXPORT mgl_data_sum_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_data_sum(_DT_,s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
static void *mgl_max_z(void *par)
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
		b[i]=a[i];
		for(long j=1;j<nz;j++)	if(b[i]<a[i+nn*j]) b[i] = a[i+nn*j];
	}
	return 0;
}
static void *mgl_max_y(void *par)
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
		long k = (i%nx)+nx*ny*(i/nx);	b[i]=a[k];
		for(long j=1;j<ny;j++)	if(b[i]<a[k+nx*j])	b[i]=a[k+nx*j];
	}
	return 0;
}
static void *mgl_max_x(void *par)
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
		long k = i*nx;	b[i]=a[k];
		for(long j=1;j<nx;j++)	if(b[i]<a[j+k])	b[i]=a[j+k];
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_max_dir(HCDT dat, const char *dir)
{
	if(!dir || *dir==0)	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nx*ny*nz];
	mreal *c = new mreal[nx*ny*nz];

	const mglData *d=dynamic_cast<const mglData *>(dat);
	if(d)	memcpy(c,d->a,nx*ny*nz*sizeof(mreal));
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	c[i]=dat->vthr(i);

	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_max_z,0,nx*ny,b,c,0,p);
		memcpy(c,b,nx*ny*sizeof(mreal));	p[2] = 1;
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_max_y,0,nx*p[2],b,c,0,p);
		memcpy(c,b,nx*p[2]*sizeof(mreal));	p[1] = p[2];	p[2] = 1;
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_max_x,0,p[1]*p[2],b,c,0,p);
		p[0] = p[1];	p[1] = p[2];	p[2] = 1;
		memcpy(c,b,p[0]*p[1]*sizeof(mreal));
	}
	mglData *r=new mglData(p[0],p[1],p[2]);
	memcpy(r->a,c,p[0]*p[1]*p[2]*sizeof(mreal));
	delete []b;	delete []c;	return r;
}
uintptr_t MGL_EXPORT mgl_data_max_dir_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_data_max_dir(_DT_,s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
static void *mgl_min_z(void *par)
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
		b[i]=a[i];
		for(long j=1;j<nz;j++)	if(b[i]>a[i+nn*j]) b[i] = a[i+nn*j];
	}
	return 0;
}
static void *mgl_min_y(void *par)
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
		long k = (i%nx)+nx*ny*(i/nx);	b[i]=a[k];
		for(long j=1;j<ny;j++)	if(b[i]>a[k+nx*j])	b[i]=a[k+nx*j];
	}
	return 0;
}
static void *mgl_min_x(void *par)
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
		long k = i*nx;	b[i]=a[k];
		for(long j=1;j<nx;j++)	if(b[i]>a[j+k])	b[i]=a[j+k];
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_min_dir(HCDT dat, const char *dir)
{
	if(!dir || *dir==0)	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	long p[3]={nx,ny,nz};
	mreal *b = new mreal[nx*ny*nz];
	mreal *c = new mreal[nx*ny*nz];

	const mglData *d=dynamic_cast<const mglData *>(dat);
	if(d)	memcpy(c,d->a,nx*ny*nz*sizeof(mreal));
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	c[i]=dat->vthr(i);

	if(strchr(dir,'z') && nz>1)
	{
		mglStartThread(mgl_min_z,0,nx*ny,b,c,0,p);
		memcpy(c,b,nx*ny*sizeof(mreal));	p[2] = 1;
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThread(mgl_min_y,0,nx*p[2],b,c,0,p);
		memcpy(c,b,nx*p[2]*sizeof(mreal));	p[1] = p[2];	p[2] = 1;
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThread(mgl_min_x,0,p[1]*p[2],b,c,0,p);
		p[0] = p[1];	p[1] = p[2];	p[2] = 1;
		memcpy(c,b,p[0]*p[1]*sizeof(mreal));
	}
	mglData *r=new mglData(p[0],p[1],p[2]);
	memcpy(r->a,c,p[0]*p[1]*p[2]*sizeof(mreal));
	delete []b;	delete []c;	return r;
}
uintptr_t MGL_EXPORT mgl_data_min_dir_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_data_min_dir(_DT_,s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_minmax(HCDT dat)
{
	long n=dat->GetNx(), m=dat->GetNy(), l=dat->GetNz();
	std::vector<mreal> imax, jmax, kmax;
	HMDT res=NULL;
	if(n>3 && m==1 && l==1)
	{
		std::vector<mreal> imax;
		for(long i=1;i<n-1;i++)
		{
			mreal v = dat->v(i);
			if(v > dat->v(i-1) && v >= dat->v(i+1))	imax.push_back(i);
			if(v < dat->v(i-1) && v <= dat->v(i+1))	imax.push_back(i);
		}
		size_t nn = imax.size();
		if(nn>0)
		{	res = new mglData(nn);
			for(size_t i=0;i<nn;i++)	res->a[i] = imax[i]/(n-1.);	}
	}
	else if(n>3 && m>3 && l==1)
	{
		std::vector<mreal> imax, jmax;
		for(long j=1;j<m-1;j++)	for(long i=1;i<n-1;i++)
		{
			mreal v = dat->v(i,j);
			long di[] = {-1,0,1, -1,1, -1,0,1}, dj[] = {-1,-1, -1,0, 0,1,1,1}, vmax=0, vmin=0;
			for(int ii=0;ii<8;ii++)
			{
				mreal u = dat->v(i+di[ii],j+dj[ii]);
				if(mgl_isnan(u))	{	vmax=vmin=0;	break;	}
				if(v < u)	vmin++;
				if(v > u)	vmax++;
			}
			if(vmin>0 && vmax==0)	{	imax.push_back(i);	jmax.push_back(j);	}
			if(vmax>0 && vmin==0)	{	imax.push_back(i);	jmax.push_back(j);	}
		}
		size_t nn = imax.size();
		if(nn>0)
		{
			res = new mglData(2,nn);
			for(size_t i=0;i<nn;i++)
			{	res->a[2*i] = imax[i]/(n-1.);	res->a[2*i+1] = jmax[i]/(m-1.);	}
		}
	}
	else if(n>3 && m>3 && l>3)
	{
		std::vector<mreal> imax, jmax;
		for(long k=1;k<l-1;k++)	for(long j=1;j<m-1;j++)	for(long i=1;i<n-1;i++)
		{
			mreal v = dat->v(i,j,k);
			long di[] = {-1,0,1,-1,0,1,-1,0,1,		-1,0,1,-1,1,-1,0,1,	-1,0,1,-1,0,1,-1,0,1};
			long dj[] = {-1,-1,-1,0,0,0,1,1,1,		-1,-1,-1,0,0,1,1,1,	-1,-1,-1,0,0,0,1,1,1};
			long dk[] = {-1,-1,-1,-1,-1,-1,-1,-1,-1, 0,0,0,0,0,0,0,0,	1,1,1,1,1,1,1,1,1}, vmax=0, vmin=0;
			for(int ii=0;ii<26;ii++)
			{
				mreal u = dat->v(i+di[ii],j+dj[ii],k+dk[ii]);
				if(mgl_isnan(u))	{	vmax=vmin=0;	break;	}
				if(v < u)	vmin++;
				if(v > u)	vmax++;
			}
			if(vmin>0 && vmax==0)	{	imax.push_back(i);	jmax.push_back(j);	kmax.push_back(k);	}
			if(vmax>0 && vmin==0)	{	imax.push_back(i);	jmax.push_back(j);	kmax.push_back(k);	}
		}
		size_t nn = imax.size();
		if(nn>0)
		{	
			res = new mglData(3,nn);
			for(size_t i=0;i<nn;i++)
			{	res->a[3*i] = imax[i]/(n-1.);	res->a[3*i+1] = jmax[i]/(m-1.);	res->a[3*i+2] = kmax[i]/(l-1.);	}
		}
	}
	return res;
}
uintptr_t MGL_EXPORT mgl_data_minmax_(uintptr_t *d)
{	return uintptr_t(mgl_data_minmax(_DT_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_momentum(HCDT dat, char dir, const char *how)
{
	if(!how || !(*how) || !strchr("xyz",dir))	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	mglDataV x(nx,ny,nz, 0,1,'x');	x.Name(L"x");
	mglDataV y(nx,ny,nz, 0,1,'y');	y.Name(L"y");
	mglDataV z(nx,ny,nz, 0,1,'z');	z.Name(L"z");
	mglData u(dat);	u.Name(L"u");	// NOTE slow !!!
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);	list.push_back(&u);
	HMDT res=mglFormulaCalc(how,list), b=0;

	if(dir=='x')
	{
		b=new mglData(nx);
#pragma omp parallel for
		for(long i=0;i<nx;i++)
		{
			mreal i1=0,i0=0;
			for(long j=0;j<ny*nz;j++)
			{
				mreal u=dat->vthr(i+nx*j);
				i0 += u;	i1 += u*res->a[i+nx*j];
			}
			b->a[i] = i0>0 ? i1/i0 : 0;
		}
	}
	if(dir=='y')
	{
		b=new mglData(ny);
#pragma omp parallel for
		for(long i=0;i<ny;i++)
		{
			mreal i1=0,i0=0;
			for(long k=0;k<nz;k++)	for(long j=0;j<nx;j++)
			{
				mreal u=dat->v(j,i,k);
				i0 += u;	i1 += u*res->a[j+nx*(i+ny*k)];
			}
			b->a[i] = i0>0 ? i1/i0 : 0;
		}
	}
	if(dir=='z')
	{
		long nn=nx*ny;
		b=new mglData(nz);
#pragma omp parallel for
		for(long i=0;i<nz;i++)
		{
			mreal i1=0,i0=0;
			for(long j=0;j<nn;j++)
			{
				mreal u=dat->vthr(j+nn*i);
				i0 += u;	i1 += u*res->a[j+nn*i];
			}
			b->a[i] = i0>0 ? i1/i0 : 0;
		}
	}
	mgl_delete_data(res);	return b;
}
uintptr_t MGL_EXPORT mgl_data_momentum_(uintptr_t *d, char *dir, const char *how, int,int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_data_momentum(_DT_,*dir, s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_mul_dat(HMDT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	if(mz==1 && my==1 && mx==1)
	{
		mreal v=a->v(0);
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			d->a[i+nx*(j+ny*k)] *= v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
#pragma omp parallel for collapse(2)
		for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] *= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_mul_num(HMDT d, mreal a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] *= a;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_div_dat(HMDT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	if(mz==1 && my==1 && mx==1)
	{
		mreal v=a->v(0);
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			d->a[i+nx*(j+ny*k)] /= v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
#pragma omp parallel for collapse(2)
		for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] /= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_div_num(HMDT d, mreal a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] /= a;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_add_dat(HMDT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	if(mz==1 && my==1 && mx==1)
	{
		mreal v=a->v(0);
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			d->a[i+nx*(j+ny*k)] += v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
#pragma omp parallel for collapse(2)
		for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] += a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_add_num(HMDT d, mreal a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] += a;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_sub_dat(HMDT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	if(mz==1 && my==1 && mx==1)
	{
		mreal v=a->v(0);
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			d->a[i+nx*(j+ny*k)] -= v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
#pragma omp parallel for collapse(2)
		for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] -= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_sub_num(HMDT d, mreal a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] -= a;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_mul_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_data_mul_dat(_DT_, _DA_(b));	}
void MGL_EXPORT mgl_data_div_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_data_div_dat(_DT_, _DA_(b));	}
void MGL_EXPORT mgl_data_add_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_data_add_dat(_DT_, _DA_(b));	}
void MGL_EXPORT mgl_data_sub_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_data_sub_dat(_DT_, _DA_(b));	}
void MGL_EXPORT mgl_data_mul_num_(uintptr_t *d, mreal *b)		{	mgl_data_mul_num(_DT_, *b);	}
void MGL_EXPORT mgl_data_div_num_(uintptr_t *d, mreal *b)		{	mgl_data_div_num(_DT_, *b);	}
void MGL_EXPORT mgl_data_add_num_(uintptr_t *d, mreal *b)		{	mgl_data_add_num(_DT_, *b);	}
void MGL_EXPORT mgl_data_sub_num_(uintptr_t *d, mreal *b)		{	mgl_data_sub_num(_DT_, *b);	}
//-----------------------------------------------------------------------------
void static mgl_hist_p(mglThreadD *t,mreal *a)
{
	long n=t[0].p[0];
	memset(a,0,n*sizeof(mreal));
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=0;i<mglNumThr;i++)
	{
		mreal *b = t[i].a;
		for(long j=0;j<n;j++)	a[j] += b[j];
		delete []b;
	}
}
static void *mgl_hist_1(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nn=t->n, n = t->p[0];
	mreal *b=new mreal[n];
	memset(b,0,n*sizeof(mreal));
	HCDT a = (HCDT)(t->b), c = (HCDT)(t->c);
	const mreal *v=(const mreal *)t->v;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		long k = long(n*(a->vthr(i)-v[0])/(v[1]-v[0]));
		if(k>=0 && k<n)
#if !MGL_HAVE_PTHREAD
#pragma omp critical(hist)
#endif
			b[k] += c ? c->vthr(i):1.;
	}
	t->a = b;	return 0;
}
static void *mgl_hist_2(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nn=t->n, n = t->p[0];
	long ns=t->p[1], nx=t->p[2], ny=t->p[3];
	mreal *b=new mreal[n], d=1./ns;
	memset(b,0,n*sizeof(mreal));
	HCDT a = (HCDT)(t->b), c = (HCDT)(t->c);
	const mreal *v=(const mreal *)t->v;
	bool sp = n>0;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nn;i+=mglNumThr)
	{
		mreal x = d*(i%(nx*ns)), y = d*((i/(nx*ns))%(ny*ns)), z = d*(i/(nx*ny*ns*ns));
		mreal f = sp ? a->value(x,y,z) : a->linear(x,y,z), w=1;
		if(c)	w = sp ? c->value(x,y,z) : c->linear(x,y,z);
		if(mgl_isnan(f) || mgl_isnan(w))	continue;
		long k = long(n*(f-v[0])/(v[1]-v[0]));
		if(k>=0 && k<n)
#if !MGL_HAVE_PTHREAD
#pragma omp critical(hist)
#endif
			b[k] += w * d*d*d;
	}
	t->a = b;	return 0;
}
HMDT MGL_EXPORT mgl_data_hist(HCDT dat, long n, mreal v1, mreal v2, long nsub)
{
	if(n<2 || v1==v2)	return 0;
	mglData *b=new mglData(n);
	mreal v[2]={v1,v2};
	long nx=dat->GetNx(), ny=dat->GetNy(), nz=dat->GetNz();
	long ns=labs(nsub)+1, p[5]={n,ns,nx,ny,nz};
	if(nsub==0)	mglStartThread(mgl_hist_1,mgl_hist_p, nx*ny*nz, b->a,(const mreal *)dat,0,p,v);
	else	mglStartThread(mgl_hist_2,mgl_hist_p, nx*ny*nz*ns*ns*ns, b->a,(const mreal *)dat,0,p,v);
	return b;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_hist_w(HCDT dat, HCDT weight, long n, mreal v1, mreal v2, long nsub)
{
	if(n<2 || v1==v2)	return 0;
	mglData *b=new mglData(n);
	mreal v[2]={v1,v2};

	long nx=dat->GetNx(), ny=dat->GetNy(), nz=dat->GetNz();
	long ns=labs(nsub)+1, p[5]={n,ns,nx,ny,nz};
	if(nsub==0)	mglStartThread(mgl_hist_1,mgl_hist_p, nx*ny*nz, b->a,(const mreal *)dat,(const mreal *)weight,p,v);
	else	mglStartThread(mgl_hist_2,mgl_hist_p, nx*ny*nz*ns*ns*ns, b->a,(const mreal *)dat,(const mreal *)weight,p,v);
	return b;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_data_hist_(uintptr_t *d, int *n, mreal *v1, mreal *v2, int *nsub)
{	return uintptr_t(mgl_data_hist(_DT_,*n,*v1,*v2,*nsub));	}
uintptr_t MGL_EXPORT mgl_data_hist_w_(uintptr_t *d, uintptr_t *w, int *n, mreal *v1, mreal *v2, int *nsub)
{	return uintptr_t(mgl_data_hist_w(_DT_,_DA_(w),*n,*v1,*v2,*nsub));	}
//-----------------------------------------------------------------------------
long MGL_NO_EXPORT mgl_idx_var;
int MGL_LOCAL_PURE mgl_cmd_idx(const void *a, const void *b)
{
	mreal *aa = (mreal *)a, *bb = (mreal *)b;
	return (aa[mgl_idx_var]>bb[mgl_idx_var] ? 1:(aa[mgl_idx_var]<bb[mgl_idx_var]?-1:0) );
}
void MGL_EXPORT mgl_data_sort(HMDT dat, long idx, long idy)
{
	mglData *d = dynamic_cast<mglData *>(dat);
	if(!d || idx>=d->nx || idx<0)	return;
	bool single = (d->nz==1 || idy<0);
	if(idy<0 || idy>d->ny)	idy=0;
	mgl_idx_var = idx+d->nx*idy;	// NOTE: not thread safe!!!
	if(single)	qsort(d->a, d->ny*d->nz, d->nx*sizeof(mreal), mgl_cmd_idx);
	else		qsort(d->a, d->nz, d->ny*d->nx*sizeof(mreal), mgl_cmd_idx);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_sort_(uintptr_t *d, int *idx, int *idy)
{	mgl_data_sort(_DT_,*idx,*idy);	}
//-----------------------------------------------------------------------------
//
//	Find roots series
//
//
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
#include <gsl/gsl_multiroots.h>
struct mglRoots
{
	mreal *x, *f;
	size_t n;
	void (*func)(const mreal *x, mreal *f, void *par);
	void *par;
};
int static mgl_root(const gsl_vector *x, void *params, gsl_vector *f)
{
	mglRoots *p = (mglRoots *)params;
	for(size_t i=0;i<p->n;i++)	p->x[i] = gsl_vector_get (x, i);
	p->func(p->x,p->f,p->par);
	bool ok = true;
	for(size_t i=0;i<p->n;i++)
	{	gsl_vector_set (f, i, p->f[i]);
		if(mgl_isbad(p->f[i]))	ok=false;	}
	return ok?GSL_SUCCESS:GSL_FAILURE;
}
bool MGL_EXPORT mgl_find_roots(size_t n, void (*func)(const mreal *x, mreal *f, void *par), mreal *x0, void *par)
{
	for(size_t i=0;i<n;i++)	if(mgl_isbad(x0[i]))	return false;
	mglRoots pp;
	pp.x=x0;	pp.func=func;	pp.n=n;	pp.par=par;
	mreal *y = new mreal[n];	pp.f=y;

	gsl_multiroot_function f = {&mgl_root, n, &pp};
	gsl_vector *x = gsl_vector_alloc(n);
	for(size_t i=0;i<n;i++)	gsl_vector_set(x, i, x0[i]);

	gsl_multiroot_fsolver *s = gsl_multiroot_fsolver_alloc(gsl_multiroot_fsolver_hybrids, n);
	gsl_multiroot_fsolver_set(s, &f, x);

	int status = GSL_CONTINUE;
	for(size_t i=0;i<1000 && status==GSL_CONTINUE;i++)
	{
	  status = gsl_multiroot_fsolver_iterate(s);
	  if(status)	break;
	  status = gsl_multiroot_test_residual(s->f, 1e-7);
	}
	for(size_t i=0;i<n;i++)	x0[i] = gsl_vector_get(s->x, i);
	gsl_multiroot_fsolver_free(s);
	gsl_vector_free(x);	delete []y;
	return status==GSL_SUCCESS;
}
#else
bool MGL_EXPORT mgl_find_roots(size_t , void (*)(const mreal *, mreal *, void *), mreal *, void *)
{	return false;	}
#endif // MGL_HAVE_GSL
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_find_roots_txt(const char *func, const char *vars, HCDT ini)
{
	if(!vars || !(*vars) || !func || !ini)	return 0;
	mglEqTxT par;
	par.var=vars;	par.FillReal(func);
	size_t n = par.str.size();
	if(ini->GetNx()!=long(n))	return 0;
	mreal *xx = new mreal[n];
	mglData *res = new mglData(ini);
	for(long j=0;j<ini->GetNy()*ini->GetNz();j++)
	{
		for(size_t i=0;i<n;i++)	xx[i] = ini->vthr(i+n*j);
		bool ok=mgl_find_roots(n,mgl_txt_func,xx,&par);
		for(size_t i=0;i<n;i++)	res->a[i+n*j] = ok?xx[i]:NAN;
	}
	delete []xx;	return res;
}
uintptr_t MGL_EXPORT mgl_find_roots_txt_(const char *func, const char *vars, uintptr_t *ini,int l,int m)
{	char *s=new char[l+1];	memcpy(s,func,l);	s[l]=0;
	char *v=new char[m+1];	memcpy(v,vars,m);	v[m]=0;
	uintptr_t r = uintptr_t(mgl_find_roots_txt(s,v,_DA_(ini)));
	delete []s;	delete []v;	return r;	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_find_root(mreal (*func)(mreal x, void *par), mreal x0, void *par)
{
	mreal x1=x0+1e-2*(x0?x0:1), f0=func(x0,par), f1=func(x1,par), x, f;
	if(fabs(f0)<1e-7)	return x0;
	if(fabs(f1)<1e-7)	return x1;
	if(f0==f1)	return NAN;
	for(long i=0;i<20;i++)
	{
		x = x1-f1*(x1-x0)/(f1-f0);	f = func(x,par);
		if(fabs(f)<1e-7)	return x;
/*		if(fabs(f-f1)>0.5*mgl_min(fabs(f),fabs(f1)))	// TODO switch to bisection if slow
		{
			x = (x1+x0)/2;	f = func(x,par);
			if(fabs(f)<1e-7)	return x;
		}*/
		x0=x1;	f0=f1;	x1=x;	f1=f;	// new points
	}
	return NAN;	// no roots found
}
//-----------------------------------------------------------------------------
struct MGL_NO_EXPORT mglFuncV	{	mglFormula *eq;	char var;	};
mreal static mgl_funcv(mreal v, void *par)
{
	mglFuncV *f = (mglFuncV *)par;
	mreal var[MGL_VS];	memset(var,0,MGL_VS*sizeof(mreal));
	var[f->var-'a'] = v;
	return f->eq->Calc(var);
}
HMDT MGL_EXPORT mgl_data_roots(const char *func, HCDT ini, char var)
{
	if(!ini)	return 0;
	mglData *res = new mglData(ini);

	mglFormula eq(func);
	mglFuncV f;	f.eq = &eq;	f.var = var;
	long n = res->nx*res->ny*res->nz;
#pragma omp parallel for
	for(long i=0;i<n;i++)
		res->a[i] = mgl_find_root(mgl_funcv,res->a[i],&f);
	return res;
}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_find_root_txt(const char *func, mreal ini, char var)
{
	mglFormula eq(func);
	mglFuncV f;	f.eq = &eq;	f.var = var;
	return mgl_find_root(mgl_funcv,ini,&f);
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_data_roots_(const char *func, uintptr_t *ini, const char *var,int l,int)
{	char *s=new char[l+1];	memcpy(s,func,l);	s[l]=0;
	uintptr_t r = uintptr_t(mgl_data_roots(s,_DA_(ini),*var));
	delete []s;	return r;	}
mreal MGL_EXPORT mgl_find_root_txt_(const char *func, mreal *ini, const char *var,int l,int)
{	char *s=new char[l+1];	memcpy(s,func,l);	s[l]=0;
	mreal r = mgl_find_root_txt(s,*ini,*var);
	delete []s;	return r;	}
//-----------------------------------------------------------------------------
static void *mgl_pulse_z(void *par)
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
		long j0=0;	mreal m=a[i];
		for(long j=1;j<nz;j++)	// get maximum
		{	long i0=i+nn*j;
			if(m<a[i0])	{	m=a[i0];	j0=j;	}
		}
		if(j0>0 && j0<nz-1)
		{
			long i0=i+nn*j0;
			mreal A = (a[i0-nn]-2*a[i0]+a[i0+nn])/2;
			mreal B = (a[i0+nn]-a[i0-nn])/2;
			mreal C = a[i0] - B*B/(4*A);
			b[i]=C;	b[i+nn]=j0-B/(2*A);	b[i+2*nn]=sqrt(fabs(C/A));	C /= 2;
			mreal j1=NAN,j2=NAN;
			for(long j=j0;j<nz-1;j++)
			{	long i0 = i+nn*j;
				if((a[i0]-C)*(a[i0+nn]-C)<0)	j2 = j + (a[i0]-C)/(a[i0]-a[i0+nn]);
			}
			for(long j=j0;j>0;j--)
			{	long i0=i+nn*j;
				if((a[i0]-C)*(a[i0-nn]-C)<0)	j1 = j - (a[i0]-C)/(a[i0]-a[i0-nn]);
			}
			b[i+3*nn]=j2-j1;	b[i+4*nn]=0;
			if(j2>j1)	for(long j = j1;j<=j2;j++)	b[i+4*nn] += a[i+nn*j];
		}
		else	// maximum at the edges
		{	b[i]=m;	b[i+nn]=j0;	b[i+2*nn]=b[i+3*nn]=b[i+4*nn]=NAN;	}
	}
	return 0;
}
static void *mgl_pulse_y(void *par)
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
		long k = (i%nx)+nx*ny*(i/nx), j0=0;	mreal m=a[k];
		long ki = (i%nx)+5*nx*(i/nx);
		for(long j=1;j<ny;j++)	// get maximum
		{	long i0=k+nx*j;
			if(m<a[i0])	{	m=a[i0];	j0=j;	}
		}
		if(j0>0 && j0<ny-1)
		{
			long i0=k+nx*j0;
			mreal A = (a[i0-nx]-2*a[i0]+a[i0+nx])/2;
			mreal B = (a[i0+nx]-a[i0-nx])/2;
			mreal C = a[i0] - B*B/(4*A);
			b[ki]=C;	b[ki+nx]=j0-B/(2*A);	b[ki+2*nx]=sqrt(fabs(C/A));	C /= 2;
			mreal j1=NAN,j2=NAN;
			for(long j=j0;j<ny-1;j++)
			{	long i0 = k+nx*j;
				if((a[i0]-C)*(a[i0+nx]-C)<0)	j2 = j + (a[i0]-C)/(a[i0]-a[i0+nx]);
			}
			for(long j=j0;j>0;j--)
			{	long i0=k+nx*j;
				if((a[i0]-C)*(a[i0-nx]-C)<0)	j1 = j - (a[i0]-C)/(a[i0]-a[i0-nx]);
			}
			b[ki+3*nx]=j2-j1;	b[ki+4*nx]=0;
			if(j2>j1)	for(long j = j1;j<=j2;j++)	b[ki+4*nx] += a[k+nx*j];
		}
		else	// maximum at the edges
		{	b[ki]=m;	b[ki+nx]=j0;	b[ki+2*nx]=b[ki+3*nx]=b[ki+4*nx]=NAN;	}
	}
	return 0;
}
static void *mgl_pulse_x(void *par)
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
		long k = i*nx, j0=0;	mreal m=a[k];
		for(long j=1;j<nx;j++)	// get maximum
		{	long i0=j+k;
			if(m<a[i0])	{	m=a[i0];	j0=j;	}
		}
		if(j0>0 && j0<nx-1)
		{
			long i0=j0+k;
			mreal A = (a[i0-1]-2*a[i0]+a[i0+1])/2;
			mreal B = (a[i0+1]-a[i0-1])/2;
			mreal C = a[i0] - B*B/(4*A);
			b[5*i]=C;	b[5*i+1]=j0-B/(2*A);	b[5*i+2]=sqrt(fabs(C/A));	C /= 2;
			mreal j1=NAN,j2=NAN;
			for(long j=j0;j<nx-1;j++)
			{	long i0 = j+k;
				if((a[i0]-C)*(a[i0+1]-C)<0)	j2 = j + (a[i0]-C)/(a[i0]-a[i0+1]);
			}
			for(long j=j0;j>0;j--)
			{	long i0=j+k;
				if((a[i0]-C)*(a[i0-1]-C)<0)	j1 = j - (a[i0]-C)/(a[i0]-a[i0-1]);
			}
			b[5*i+3]=j2-j1;	b[5*i+4]=0;
			if(j2>j1)	for(long j = j1;j<=j2;j++)	b[5*i+4] += a[j+k];
		}
		else	// maximum at the edges
		{	b[5*i]=m;	b[5*i+1]=j0;	b[5*i+2]=b[5*i+3]=b[5*i+4]=NAN;	}
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_pulse(HCDT dat, char dir)
{
//	if(!dir || *dir==0)	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	long p[3]={nx,ny,nz};
	mreal *c = new mreal[nx*ny*nz], *b=0;

	const mglData *d=dynamic_cast<const mglData *>(dat);
	if(d)	memcpy(c,d->a,nx*ny*nz*sizeof(mreal));
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	c[i]=dat->vthr(i);

	if(dir=='z' && nz>1)
	{
		b = new mreal[nx*ny*5];
		mglStartThread(mgl_pulse_z,0,nx*ny,b,c,0,p);	p[2] = 5;
	}
	else if(dir=='y' && ny>1)
	{
		b = new mreal[5*nx*nz];
		mglStartThread(mgl_pulse_y,0,nx*p[2],b,c,0,p);	p[1] = 5;
	}
	else if(dir=='x' && nx>1)
	{
		b = new mreal[5*ny*nz];
		mglStartThread(mgl_pulse_x,0,p[1]*p[2],b,c,0,p);	p[0] = 5;
	}
	mglData *r=0;
	if(b)
	{
		r=new mglData(p[0],p[1],p[2]);
		memcpy(r->a,b,p[0]*p[1]*p[2]*sizeof(mreal));
		delete []b;
	}
	delete []c;	return r;
}
uintptr_t MGL_EXPORT mgl_data_pulse_(uintptr_t *d, const char *dir,int)
{	return uintptr_t(mgl_data_pulse(_DT_,dir[0]));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_section(HCDT dat, HCDT ids, char dir, mreal val)
{
	long di = 1, n = dat->GetNx();
	if(dir=='y')	{	di = dat->GetNx();	n = dat->GetNy();	}
	if(dir=='z')	{	di = dat->GetNx()*dat->GetNy();	n = dat->GetNz();	}
	// first collect position of key values
	std::vector<long> pos;	pos.push_back(0);
	if(mgl_isnan(val))	for(long i=1;i<n;i++)
	{
		if(mgl_isnan(dat->vthr(i*di)))	pos.push_back(i);
	}
	else	for(long i=0;i<n;i++)
	{
		if(dat->vthr(i*di)==val)	pos.push_back(i);
	}
	pos.push_back(n);	// add last point (size of data)
	// now collect required position from section and its lengths
	std::vector<long> ls, ps;
	long np = pos.size()-1, nl=0;
	if(np<1)	return NULL;	// nothing to do
	for(long i=0;i<ids->GetNN();i++)
	{
		long j = mgl_int(ids->vthr(i)+0.5);	j = j<0?np+j:j;
		if(j>=0 && j<np)
		{	long l = pos[j+1]-pos[j];	nl += l;
			ls.push_back(l);	ps.push_back(pos[j]);	}
	}
	if(nl==0)	return NULL;
	mglData *r=0;
	size_t ns = ps.size();
	if(dir=='y')
	{
		long nx=dat->GetNx(), nz=dat->GetNz(), sh=0;
		r = new mglData(nx,nl,nz);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<nz;k++)	for(long j=0;j<ls[s];j++)	for(long i=0;i<nx;i++)
				r->a[i+nx*(sh+j+nl*k)] = dat->v(i,pp+j,k);
			sh += ls[s];
		}
	}
	else if(dir=='x')
	{
		long ny=dat->GetNy(), nz=dat->GetNz(), sh=0;
		r = new mglData(nl,ny,nz);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<ls[s];i++)
				r->a[sh+i+nl*(j+ny*k)] = dat->v(pp+i,j,k);
			sh += ls[s];
		}
	}
	else if(dir=='z')
	{
		long nx=dat->GetNx(), ny=dat->GetNy(), sh=0;
		r = new mglData(nx,ny,nl);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<ls[s];k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
				r->a[i+nx*(j+ny*(sh+k))] = dat->v(i,j,pp+k);
			sh += ls[s];
		}
	}
	return r;
}
HMDT MGL_EXPORT mgl_data_section_val(HCDT dat, long id, char dir, mreal val)
{	mglData v;	v.a[0]=id;	return mgl_data_section(dat,&v,dir,val);	}
uintptr_t MGL_EXPORT mgl_data_section_(uintptr_t *d, uintptr_t *ids, const char *dir, mreal *val,int)
{	return uintptr_t(mgl_data_section(_DT_,_DA_(ids),dir[0],*val));	}
uintptr_t MGL_EXPORT mgl_data_section_val_(uintptr_t *d, int *id, const char *dir, mreal *val,int)
{	return uintptr_t(mgl_data_section_val(_DT_,*id,dir[0],*val));	}
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_closest(mreal *res, long n, long i0, long i1, HCDT a, HCDT b)
{
	mreal *rr = new mreal[n*n];
	for(long k0=0;k0<n;k0++)	for(long k1=0;k1<n;k1++)
		rr[k0+n*k1] = hypot(a->vthr(k0+i0)-a->vthr(k1+i1), b->vthr(k0+i0)-b->vthr(k1+i1));
	std::set<long> ids, idk;
	for(long k=0;k<n;k++)	{	ids.insert(k);	idk.insert(k);	}
	for(long k=0;k<n;k++)
	{
		long k0=-1,k1=-1;
		mreal rm = INFINITY;
		for(long kk=0;kk<n*n;kk++)
			if(rr[kk]<rm)	{	rm=rr[kk];	k0=kk%n;	k1=kk/n;	}
		if(k0>=0)
		{
			for(long i=0;i<n;i++)	rr[i+k1*n] = rr[k0+i*n] = NAN;
			long kk = long(0.5+res[k1+i1]);	res[k0+i0] = kk;
			ids.erase(kk);	idk.erase(k0);
		}
		else
		{
			long kk=*(ids.begin()), ik = *(idk.begin());
			res[ik+i0] = kk;
			ids.erase(kk);	idk.erase(ik);
		}
	}
	delete []rr;
}
HMDT MGL_EXPORT mgl_data_connect(HCDT a, HCDT b)
{
	int nx = a->GetNx(), ny = a->GetNy(), nz = a->GetNz();
	HMDT res = new mglData(nx,ny,nz);
	for(long j=0;j<ny*nz;j++)	for(long i=0;i<nx;i++)	res->a[i+nx*j] = i;
	for(long j=0;j<nz;j++)
	{
		if(j>0)
		{
			long i0 = nx*(ny-1+ny*j);
			mgl_closest(res->a, nx, i0, i0-nx*ny, a, b);
		}
		for(long i=ny-1;i>0;i--)	// TODO optimized for QHCU_dh case only!!!
		{
			long i0 = nx*(i+ny*j);
			mgl_closest(res->a, nx, i0-nx, i0, a, b);
		}
	}
	return res;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_data_connect_(uintptr_t *a, uintptr_t *b)
{	return uintptr_t(mgl_data_connect(_DA_(a),_DA_(b)));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_connect_r(HMDT a, HMDT b)
{
	int nx = a->GetNx(), ny = a->GetNy(), nz = a->GetNz();
	HMDT res = mgl_data_connect(a,b);
	mreal *buf = new mreal[2*nx];
	for(long j=0;j<ny*nz;j++)
	{
		long i0 = nx*j;
		memcpy(buf,a->a+i0,nx*sizeof(mreal));
		memcpy(buf+nx,b->a+i0,nx*sizeof(mreal));
		for(long i=0;i<nx;i++)
		{
			long i1 = res->a[i0+i];
			a->a[i0+i1] = buf[i];
			b->a[i0+i1] = buf[i+nx];
		}
	}
	delete []buf;	delete res;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_connect_r_(uintptr_t *a, uintptr_t *b)
{	mgl_data_connect_r(_DM_(a),_DM_(b));	}
//-----------------------------------------------------------------------------
