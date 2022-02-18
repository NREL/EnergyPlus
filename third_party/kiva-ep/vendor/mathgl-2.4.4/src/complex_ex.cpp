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
#include <ctype.h>
#include "mgl2/datac.h"
#include "mgl2/evalc.h"
#include "mgl2/thread.h"
#include "interp.hpp"
void MGL_NO_EXPORT mgl_txt_funcC(const mreal *x, mreal *dx, void *par);
HADT MGL_NO_EXPORT mglFormulaCalcC(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_trace(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	const mglDataC *dc = dynamic_cast<const mglDataC *>(d);
	mglDataC *r=new mglDataC(nx);
	if(dc)
	{
		if(ny>=nx && nz>=nx)
#pragma omp parallel for
			for(long i=0;i<nx;i++)	r->a[i] = dc->a[i+nx*(i+ny*i)];
		else if(ny>=nx)
#pragma omp parallel for
			for(long i=0;i<nx;i++)	r->a[i] = dc->a[i+nx*i];
		else
#pragma omp parallel for
			for(long i=0;i<nx;i++)	r->a[i] = dc->a[i];
	}
	else if(ny>=nx && nz>=nx)
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
uintptr_t MGL_EXPORT mgl_datac_trace_(uintptr_t *d)
{	return uintptr_t(mgl_datac_trace(_DC_));	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_subdata_ext(HCDT d, HCDT xx, HCDT yy, HCDT zz)
{
	if(!xx || !yy || !zz)
	{
		mglData tmp;	tmp.a[0]=-1;
		return mgl_datac_subdata_ext(d,xx?xx:&tmp,yy?yy:&tmp,zz?zz:&tmp);
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
	const mglDataC *dd = dynamic_cast<const mglDataC *>(d);
	mglDataC *r;
	if(n*m*l>1)	// this is 2d or 3d data
	{
		mglDataV tx(n,m,l),ty(n,m,l),tz(n,m,l);
		if(!ix)	{	xx = &tx;	if(vx>=0)	tx.Fill(vx);	else tx.All();	}
		if(!iy)	{	yy = &ty;	if(vy>=0)	ty.Fill(vy);	else ty.All();	}
		if(!iz)	{	zz = &tz;	if(vz>=0)	tz.Fill(vz);	else tz.All();	}
		r=new mglDataC(n,m,l);
		if(dd)
#pragma omp parallel for
			for(long i0=0;i0<n*m*l;i0++)
			{
				long x=long(0.5+xx->vthr(i0)), y=long(0.5+yy->vthr(i0)), z=long(0.5+zz->vthr(i0));
				r->a[i0] = (x>=0 && x<nx && y>=0 && y<ny && z>=0 && z<nz)?dd->a[x+nx*(y+ny*z)]:NAN;
			}
		else
#pragma omp parallel for
			for(long i0=0;i0<n*m*l;i0++)
			{
				long x=long(0.5+xx->vthr(i0)), y=long(0.5+yy->vthr(i0)), z=long(0.5+zz->vthr(i0));
				r->a[i0] = (x>=0 && x<nx && y>=0 && y<ny && z>=0 && z<nz)?d->v(x,y,z):NAN;
			}
	}
	else	// this is 1d data -> try as normal SubData()
	{
		mglDataV tx(nx),ty(ny),tz(nz);	tx.Fill(0,nx-1);	ty.Fill(0,ny-1);	tz.Fill(0,nz-1);
		if(xx->GetNx()>1 || vx>=0)	n=xx->GetNx();	else	{	n=nx;	xx = &tx;	}
		if(yy->GetNx()>1 || vy>=0)	m=yy->GetNx();	else	{	m=ny;	yy = &ty;	}
		if(zz->GetNx()>1 || vz>=0)	l=zz->GetNx();	else	{	l=nz;	zz = &tz;	}
		r=new mglDataC(n,m,l);
		if(dd)
#pragma omp parallel for collapse(3)
			for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			{
				long x=long(0.5+xx->v(i)), y=long(0.5+yy->v(j)), z=long(0.5+zz->v(k));
				r->a[i+n*(j+m*k)] = (x>=0 && x<nx && y>=0 && y<ny && z>=0 && z<nz)?dd->a[x+nx*(y+ny*z)]:NAN;
			}
		else
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
HADT MGL_EXPORT mgl_datac_subdata(HCDT d, long xx,long yy,long zz)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz(), n=1,m=1,l=1;
	int dx=0,dy=0,dz=0;
	if(xx<0)	{	xx=0;	dx=1;	n=nx;	}
	if(yy<0)	{	yy=0;	dy=1;	m=ny;	}
	if(zz<0)	{	zz=0;	dz=1;	l=nz;	}
	const mglDataC *dd = dynamic_cast<const mglDataC *>(d);
	mglDataC *r=new mglDataC(n,m,l);
	if(xx>=nx || yy>=ny || zz>=nz)
#pragma omp parallel for
		for(long i=0;i<n*m*l;i++)	r->a[i] = NAN;
	else if(dd)
#pragma omp parallel for collapse(3)
		for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			r->a[i+n*(j+m*k)] = dd->a[xx+dx*i + nx*(yy+dy*j + ny*(zz+dz*k))];
	else
#pragma omp parallel for collapse(3)
		for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			r->a[i+n*(j+m*k)] = d->v(xx+dx*i, yy+dy*j, zz+dz*k);
	if(m==1)	{	r->ny=r->nz;	r->nz=1;	}// "squeeze" dimensions
	if(n==1)	{	r->nx=r->ny;	r->ny=r->nz;	r->nz=1;	r->NewId();}
	return r;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_datac_subdata_(uintptr_t *d, int *xx,int *yy,int *zz)
{	return uintptr_t(mgl_datac_subdata(_DC_,*xx,*yy,*zz));	}
uintptr_t MGL_EXPORT mgl_datac_subdata_ext_(uintptr_t *d, uintptr_t *xx, uintptr_t *yy, uintptr_t *zz)
{	return uintptr_t(mgl_datac_subdata_ext(_DC_,_DA_(xx),_DA_(yy),_DA_(zz)));	}
//-----------------------------------------------------------------------------
static void *mgl_cresize(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	long nx=t->p[0]+0.1, ny=t->p[1]+0.1;
	long n1=t->p[3]+0.1,n2=t->p[4]+0.1,n3=t->p[5]+0.1;
	dual *b=t->a;
	const dual *a=t->b;
	const mreal *c=(const mreal *)t->v;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		mreal i=(i0%nx), j=((i0/nx)%ny), k=(i0/(nx*ny));
		b[i0] = mglSpline3Cs(a,n1,n2,n3, c[0]+i*c[1], c[2]+j*c[3], c[4]+k*c[5]);
	}
	return 0;
}
HADT MGL_EXPORT mgl_datac_resize_box(HCDT dat, long mx,long my,long mz, mreal x1,mreal x2, mreal y1,mreal y2, mreal z1,mreal z2)
{	// NOTE: only for mglDataC
	const mglDataC *d=dynamic_cast<const mglDataC *>(dat);
	if(!d)	return 0;
	long nx = d->nx-1, ny = d->ny-1, nz = d->nz-1;
	mx = mx<1 ? nx+1:mx;	my = my<1 ? ny+1:my;	mz = mz<1 ? nz+1:mz;
	mglDataC *r=new mglDataC(mx,my,mz);

	mreal par[6]={nx*x1,0,ny*y1,0,nz*z1,0};
	long nn[6]={mx,my,mz,nx+1,ny+1,nz+1};
	if(mx>1)	par[1] = nx*(x2-x1)/(mx-1);
	if(my>1)	par[3] = ny*(y2-y1)/(my-1);
	if(mz>1)	par[5] = nz*(z2-z1)/(mz-1);
	mglStartThreadC(mgl_cresize,0,mx*my*mz,r->a,d->a,0,nn,par);
	return r;
}
HADT MGL_EXPORT mgl_datac_resize(HCDT d, long mx,long my,long mz)
{	return mgl_datac_resize_box(d, mx,my,mz,0,1,0,1,0,1);	}
uintptr_t MGL_EXPORT mgl_datac_resize_(uintptr_t *d, int *mx,int *my,int *mz)
{	return uintptr_t(mgl_datac_resize(_DC_,*mx,*my,*mz));	}
uintptr_t MGL_EXPORT mgl_datac_resize_box_(uintptr_t *d, int *mx,int *my,int *mz, mreal *x1,mreal *x2, mreal *y1,mreal *y2, mreal *z1,mreal *z2)
{	return uintptr_t(mgl_datac_resize_box(_DC_,*mx,*my,*mz,*x1,*x2,*y1,*y2,*z1,*z2));	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_combine(HCDT d1, HCDT d2)
{
	long n1=d1->GetNy(),n2=d2->GetNx(),nx=d1->GetNx();
	if(d1->GetNz()>1 || (n1>1 && d2->GetNy()>1) || d2->GetNz()>1)	return 0;	// wrong dimensions
	mglDataC *r=new mglDataC;
	bool dim2=true;
	if(n1==1)	{	n1=n2;	n2=d2->GetNy();	dim2 = false;	}
	r->Create(nx,n1,n2);
	if(dim2)	n1*=nx;	else	{	n2*=n1;	n1=nx;	}

	const mglDataC *c1=dynamic_cast<const mglDataC *>(d1);
	const mglDataC *c2=dynamic_cast<const mglDataC *>(d2);
	if(c1 && c2)
#pragma omp parallel for collapse(2)
		for(long j=0;j<n2;j++)	for(long i=0;i<n1;i++)
			r->a[i+n1*j] = c1->a[i]*c2->a[j];
	else if(c1)
#pragma omp parallel for collapse(2)
		for(long j=0;j<n2;j++)	for(long i=0;i<n1;i++)
			r->a[i+n1*j] = c1->a[i]*d2->vthr(j);
	else if(c2)
#pragma omp parallel for collapse(2)
		for(long j=0;j<n2;j++)	for(long i=0;i<n1;i++)
			r->a[i+n1*j] = d1->vthr(i)*c2->a[j];
	else
#pragma omp parallel for collapse(2)
		for(long j=0;j<n2;j++)	for(long i=0;i<n1;i++)
			r->a[i+n1*j] = d1->vthr(i)*d2->vthr(j);
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_combine_(uintptr_t *a, uintptr_t *b)
{	return uintptr_t(mgl_datac_combine(_DA_(a),_DA_(b)));	}
//-----------------------------------------------------------------------------
static void *mgl_sumc_z(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	long nz=t->p[2], nn=t->n;
	dual *b=t->a;
	const dual *a=t->b;
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
static void *mgl_sumc_y(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	long nx=t->p[0], ny=t->p[1], nn=t->n;
	dual *b=t->a;
	const dual *a=t->b;
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
static void *mgl_sumc_x(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	long nx=t->p[0], nn=t->n;
	dual *b=t->a;
	const dual *a=t->b;
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
HADT MGL_EXPORT mgl_datac_sum(HCDT dat, const char *dir)
{
	if(!dir || *dir==0)	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	long p[3]={nx,ny,nz};
	dual *b = new dual[nx*ny*nz];
	dual *c = new dual[nx*ny*nz];

	const mglDataC *d=dynamic_cast<const mglDataC *>(dat);
	if(d)	memcpy(c,d->a,nx*ny*nz*sizeof(dual));
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	c[i]=dat->vthr(i);

	if(strchr(dir,'z') && nz>1)
	{
		mglStartThreadC(mgl_sumc_z,0,nx*ny,b,c,0,p);
		memcpy(c,b,nx*ny*sizeof(dual));	p[2] = 1;
	}
	if(strchr(dir,'y') && ny>1)
	{
		mglStartThreadC(mgl_sumc_y,0,nx*p[2],b,c,0,p);
		memcpy(c,b,nx*p[2]*sizeof(dual));	p[1] = p[2];	p[2] = 1;
	}
	if(strchr(dir,'x') && nx>1)
	{
		mglStartThreadC(mgl_sumc_x,0,p[1]*p[2],b,c,0,p);
		p[0] = p[1];	p[1] = p[2];	p[2] = 1;
		memcpy(c,b,p[0]*p[1]*sizeof(dual));
	}
	mglDataC *r=new mglDataC(p[0],p[1],p[2]);
	memcpy(r->a,c,p[0]*p[1]*p[2]*sizeof(dual));
	delete []b;	delete []c;	return r;
}
uintptr_t MGL_EXPORT mgl_datac_sum_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_datac_sum(_DC_,s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_momentum(HCDT dat, char dir, const char *how)
{
	if(!how || !(*how) || !strchr("xyz",dir))	return 0;
	long nx=dat->GetNx(),ny=dat->GetNy(),nz=dat->GetNz();
	mglDataV x(nx,ny,nz, 0,1,'x');	x.Name(L"x");
	mglDataV y(nx,ny,nz, 0,1,'y');	y.Name(L"y");
	mglDataV z(nx,ny,nz, 0,1,'z');	z.Name(L"z");
	mglDataC u(dat);	u.Name(L"u");	// NOTE slow !!!
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);	list.push_back(&u);
	HADT res=mglFormulaCalcC(how,list), b=0;

	if(dir=='x')
	{
		b=new mglDataC(nx);
#pragma omp parallel for
		for(long i=0;i<nx;i++)
		{
			dual i1=0,i0=0;
			for(long j=0;j<ny*nz;j++)
			{
				dual u=dat->vthr(i+nx*j);
				i0 += u;	i1 += u*res->a[i+nx*j];
			}
			b->a[i] = i0!=mreal(0) ? i1/i0 : 0;
		}
	}
	if(dir=='y')
	{
		b=new mglDataC(ny);
#pragma omp parallel for
		for(long i=0;i<ny;i++)
		{
			dual i1=0,i0=0;
			for(long k=0;k<nz;k++)	for(long j=0;j<nx;j++)
			{
				dual u=dat->v(j,i,k);
				i0 += u;	i1 += u*res->a[j+nx*(i+ny*k)];
			}
			b->a[i] = i0!=mreal(0) ? i1/i0 : 0;
		}
	}
	if(dir=='z')
	{
		long nn=nx*ny;
		b=new mglDataC(nz);
#pragma omp parallel for
		for(long i=0;i<nz;i++)
		{
			dual i1=0,i0=0;
			for(long j=0;j<nn;j++)
			{
				dual u=dat->vthr(j+nn*i);
				i0 += u;	i1 += u*res->a[j+nn*i];
			}
			b->a[i] = i0!=mreal(0) ? i1/i0 : 0;
		}
	}
	mgl_delete_datac(res);	return b;
}
uintptr_t MGL_EXPORT mgl_datac_momentum_(uintptr_t *d, char *dir, const char *how, int,int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	uintptr_t r=uintptr_t(mgl_datac_momentum(_DC_,*dir, s));	delete []s;	return r;	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_evaluate(HCDT dat, HCDT idat, HCDT jdat, HCDT kdat, int norm)
{
	if(!idat || (jdat && jdat->GetNN()!=idat->GetNN()) || (kdat && kdat->GetNN()!=idat->GetNN()))	return 0;
	const mglData *dd=dynamic_cast<const mglData *>(dat);
	const mglDataC *dc=dynamic_cast<const mglDataC *>(dat);
	long nx=dat->GetNx(), ny=dat->GetNy(), nz=dat->GetNz();
	mglDataC *r=new mglDataC(idat->GetNx(),idat->GetNy(),idat->GetNz());
	mreal dx = nx-1, dy = ny-1, dz = nz-1;
	if(!norm)	dx=dy=dz=1;
	if(dd)
#pragma omp parallel for
		for(long i=0;i<idat->GetNN();i++)
		{
			mreal x=dx*idat->vthr(i), y=jdat?dy*jdat->vthr(i):0, z=kdat?dz*kdat->vthr(i):0;
			r->a[i] = mgl_isnum(x*y*z)?mglSpline3st<mreal>(dd->a,nx,ny,nz, x,y,z):NAN;
		}
	else if(dc)
#pragma omp parallel for
		for(long i=0;i<idat->GetNN();i++)
		{
			mreal x=dx*idat->vthr(i), y=jdat?dy*jdat->vthr(i):0, z=kdat?dz*kdat->vthr(i):0;
			r->a[i] = mgl_isnum(x*y*z)?mglSpline3st<dual>(dc->a,nx,ny,nz, x,y,z):NAN;
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
uintptr_t MGL_EXPORT mgl_datac_evaluate_(uintptr_t *d, uintptr_t *idat, uintptr_t *jdat, uintptr_t *kdat, int *norm)
{	return uintptr_t(mgl_datac_evaluate(_DC_,_DA_(idat),_DA_(jdat),_DA_(kdat),*norm));	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_column(HCDT dat, const char *eq)
{
	std::vector<mglDataA*> list;
	const char *id = dat->GetColumnId();
	size_t len = strlen(id);
	for(size_t i=0;i<len;i++)
	{
		mglDataT *col = new mglDataT(*dat);
		col->SetInd(i,id[i]);
		list.push_back(col);
	}
	if(list.size()==0)	return 0;	// no named columns
	mglDataV *t = new mglDataV(dat->GetNy(),dat->GetNz());
	t->Name(L"#$mgl");	list.push_back(t);
	HADT r = mglFormulaCalcC(eq,list);
	for(size_t i=0;i<list.size();i++)	delete list[i];
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_column_(uintptr_t *d, const char *eq,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	uintptr_t r = uintptr_t(mgl_datac_column(_DC_,s));
	delete []s;	return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_mul_dat(HADT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	const mglDataC *c = dynamic_cast<const mglDataC*>(a);

	if(mz*my*mx==1)
	{
		dual v=c?c->a[0]:a->v(0);
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	d->a[i] += v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
		if(c)
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] *= c->a[i];
		else
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] *= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_mul_num(HADT d, mdual a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] *= dual(a);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_div_dat(HADT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	const mglDataC *c = dynamic_cast<const mglDataC*>(a);

	if(mz*my*mx==1)
	{
		dual v=c?c->a[0]:a->v(0);
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	d->a[i] /= v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
		if(c)
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] /= c->a[i];
		else
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] /= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_div_num(HADT d, mdual a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] /= dual(a);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_add_dat(HADT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	const mglDataC *c = dynamic_cast<const mglDataC*>(a);

	if(mz*my*mx==1)
	{
		dual v=c?c->a[0]:a->v(0);
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	d->a[i] += v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
		if(c)
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] += c->a[i];
		else
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] += a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_add_num(HADT d, mdual a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] += dual(a);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_sub_dat(HADT d, HCDT a)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long mx=a->GetNx(), my=a->GetNy(), mz=a->GetNz();
	const mglDataC *c = dynamic_cast<const mglDataC*>(a);

	if(mz*my*mx==1)
	{
		dual v=c?c->a[0]:a->v(0);
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	d->a[i] -= v;
	}
	else
	{
		long n=0, m=0;
		if(nz*ny*nx==mz*my*mx)	{	n=nx*ny*nz;	m=1;	}
		else if(ny*nx==my*mx)	{	n=nx*ny;	m=nz;	}
		else if(nx==mx)			{	n=nx;	m=ny*nz;	}
		if(c)
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] -= c->a[i];
		else
#pragma omp parallel for collapse(2)
			for(long k=0;k<m;k++)	for(long i=0;i<n;i++)	d->a[i+n*k] -= a->vthr(i);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_sub_num(HADT d, mdual a)
{
	long n=d->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	d->a[i] -= dual(a);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_mul_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_datac_mul_dat(_DC_, _DA_(b));	}
void MGL_EXPORT mgl_datac_div_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_datac_div_dat(_DC_, _DA_(b));	}
void MGL_EXPORT mgl_datac_add_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_datac_add_dat(_DC_, _DA_(b));	}
void MGL_EXPORT mgl_datac_sub_dat_(uintptr_t *d, uintptr_t *b)	{	mgl_datac_sub_dat(_DC_, _DA_(b));	}
void MGL_EXPORT mgl_datac_mul_num_(uintptr_t *d, mdual *b)		{	mgl_datac_mul_num(_DC_, *b);	}
void MGL_EXPORT mgl_datac_div_num_(uintptr_t *d, mdual *b)		{	mgl_datac_div_num(_DC_, *b);	}
void MGL_EXPORT mgl_datac_add_num_(uintptr_t *d, mdual *b)		{	mgl_datac_add_num(_DC_, *b);	}
void MGL_EXPORT mgl_datac_sub_num_(uintptr_t *d, mdual *b)		{	mgl_datac_sub_num(_DC_, *b);	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_section(HCDT dat, HCDT ids, char dir, mreal val)
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
	mglDataC *r=0;
	size_t ns = ps.size();
	if(dir=='y')
	{
		long nx=dat->GetNx(), nz=dat->GetNz(), sh=0;
		r = new mglDataC(nx,nl,nz);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<nz;k++)	for(long j=0;j<ls[s];j++)	for(long i=0;i<nx;i++)
				r->a[i+nx*(sh+j+nl*k)] = dat->vc(i,pp+j,k);
			sh += ls[s];
		}
	}
	else if(dir=='x')
	{
		long ny=dat->GetNy(), nz=dat->GetNz(), sh=0;
		r = new mglDataC(nl,ny,nz);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<ls[s];i++)
				r->a[sh+i+nl*(j+ny*k)] = dat->vc(pp+i,j,k);
			sh += ls[s];
		}
	}
	else if(dir=='z')
	{
		long nx=dat->GetNx(), ny=dat->GetNy(), sh=0;
		r = new mglDataC(nx,ny,nl);
		for(size_t s=0;s<ns;s++)
		{
			long pp = ps[s];
#pragma omp parallel for collapse(3)
			for(long k=0;k<ls[s];k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
				r->a[i+nx*(j+ny*(sh+k))] = dat->vc(i,j,pp+k);
			sh += ls[s];
		}
	}
	return r;
}
HADT MGL_EXPORT mgl_datac_section_val(HCDT dat, long id, char dir, mreal val)
{	mglData v;	v.a[0]=id;	return mgl_datac_section(dat,&v,dir,val);	}
uintptr_t MGL_EXPORT mgl_datac_section_(uintptr_t *d, uintptr_t *ids, const char *dir, mreal *val,int)
{	return uintptr_t(mgl_datac_section(_DT_,_DA_(ids),dir[0],*val));	}
uintptr_t MGL_EXPORT mgl_datac_section_val_(uintptr_t *d, int *id, const char *dir, mreal *val,int)
{	return uintptr_t(mgl_datac_section_val(_DT_,*id,dir[0],*val));	}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_find_roots_txt_c(const char *func, const char *vars, HCDT ini)
{
	if(!vars || !(*vars) || !func || !ini)	return 0;
	mglEqTxT par;
	par.var=vars;	par.FillCmplx(func);
	size_t n = par.str.size();
	if(ini->GetNx()!=long(n))	return 0;
	mreal *xx = new mreal[2*n];
	mglDataC *res = new mglDataC(ini);
	for(long j=0;j<ini->GetNy()*ini->GetNz();j++)
	{
		for(size_t i=0;i<n;i++)
		{	dual c = ini->vcthr(i+n*j);	xx[2*i] = real(c);	xx[2*i+1] = imag(c);	}
		bool ok=mgl_find_roots(2*n,mgl_txt_funcC,xx,&par);
		for(size_t i=0;i<n;i++)	res->a[i+n*j] = ok?dual(xx[2*i],xx[2*i+1]):NAN;
	}
	delete []xx;	return res;
}
uintptr_t MGL_EXPORT mgl_find_roots_txt_c_(const char *func, const char *vars, uintptr_t *ini,int l,int m)
{	char *s=new char[l+1];	memcpy(s,func,l);	s[l]=0;
	char *v=new char[m+1];	memcpy(v,vars,m);	v[m]=0;
	uintptr_t r = uintptr_t(mgl_find_roots_txt_c(s,v,_DA_(ini)));
	delete []s;	delete []v;	return r;	}
//-----------------------------------------------------------------------------
