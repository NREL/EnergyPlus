/***************************************************************************
 * fft.cpp is part of Math Graphic Library
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
#include "mgl2/datac.h"
#include "mgl2/data.h"
#include "mgl2/thread.h"
#if MGL_HAVE_GSL
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_dht.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_wavelet.h>
#endif
//-----------------------------------------------------------------------------
void MGL_EXPORT mglStartThreadT(void *(*func)(void *), long n, void *a, double *b, const void *v, void **w, const long *p, const void *re, const void *im)
{
	if(!func)	return;
#if MGL_HAVE_PTHREAD
	if(mglNumThr<1)	mgl_set_num_thr(0);
	if(mglNumThr>1)
	{
		pthread_t *tmp=new pthread_t[mglNumThr];
		mglThreadT *par=new mglThreadT[mglNumThr];
		for(long i=0;i<mglNumThr;i++)	// put parameters into the structure
		{	par[i].n=n;	par[i].a=a;	par[i].v=v;	par[i].w=w;	par[i].b=b;
			par[i].p=p;	par[i].re=re;	par[i].im=im;	par[i].id=i;	}
		for(long i=0;i<mglNumThr;i++)	pthread_create(tmp+i, 0, func, par+i);
		for(long i=0;i<mglNumThr;i++)	pthread_join(tmp[i], 0);
		delete []tmp;	delete []par;
	}
	else
#endif
	{
		mglNumThr = 1;
		mglThreadT par;
		par.n=n;	par.a=a;	par.b=b;	par.v=v;	par.w=w;
		par.p=p;	par.re=re;	par.im=im;	par.id=0;
		func(&par);
	}
}
//-----------------------------------------------------------------------------
struct mglFFTdata
{
	long wnx,wny,wnz;		// sizes for FFT
	long hnx,hny,hnz;		// sizes for Hankel
	void *wtx,*wty,*wtz;	// tables for FFT
	void *htx,*hty,*htz;	// tables for Hankel
	mglFFTdata()	{	memset(this,0,sizeof(mglFFTdata));	}
	~mglFFTdata()	{	Clear();	}
	void Clear()
	{
		if(wnx)	{	wnx=0;	mgl_fft_free(wtx,0,0);	}
		if(wny)	{	wny=0;	mgl_fft_free(wty,0,0);	}
		if(wnz)	{	wnz=0;	mgl_fft_free(wtz,0,0);	}
#if MGL_HAVE_GSL
		if(hnx)	{	hnx=0;	gsl_dht_free((gsl_dht*)htx);	}
		if(hny)	{	hny=0;	gsl_dht_free((gsl_dht*)hty);	}
		if(hnz)	{	hnz=0;	gsl_dht_free((gsl_dht*)htz);	}
#endif
	}
} mgl_fft_data;
void MGL_EXPORT mgl_clear_fft()	{	mgl_fft_data.Clear();	}
//-----------------------------------------------------------------------------
MGL_EXPORT void *mgl_fft_alloc_thr(long n)
{
#if MGL_HAVE_GSL
	return gsl_fft_complex_workspace_alloc(n);
#else
	return new double[2*n];
#endif
}
//-----------------------------------------------------------------------------
MGL_EXPORT void *mgl_fft_alloc(long n, void **space, long nthr)
{
	if(space && nthr>0)	for(long i=0;i<nthr;i++)	space[i] = mgl_fft_alloc_thr(n);
#if MGL_HAVE_GSL
	return gsl_fft_complex_wavetable_alloc(n);
#else
	double *c = new double[2*n*n];
#pragma omp parallel for collapse(2)
	for(long i=0;i<n;i++)	for(long j=0;j<n;j++)
	{	c[2*(i+n*j)]=cos(2*M_PI*i*j/n);	c[2*(i+n*j)+1]=-sin(2*M_PI*i*j/n);	}
	return c;
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fft_free_thr(void *ws)
{
#if MGL_HAVE_GSL
	if(ws)	gsl_fft_complex_workspace_free((gsl_fft_complex_workspace*)ws);
#else
	if(ws)	delete []((double*)ws);
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fft_free(void *wt, void **ws, long nthr)
{
	if(ws && nthr>0)	for(long i=0;i<nthr;i++)	mgl_fft_free_thr(ws[i]);
#if MGL_HAVE_GSL
	if(wt)	gsl_fft_complex_wavetable_free((gsl_fft_complex_wavetable*)wt);
#else
	if(wt)	delete []((double*)wt);
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fft(double *x, long s, long n, const void *wt, void *ws, int inv)
{
#if MGL_HAVE_GSL
	if(inv)	gsl_fft_complex_inverse(x, s, n, (const gsl_fft_complex_wavetable*)wt, (gsl_fft_complex_workspace*)ws);
	else	gsl_fft_complex_forward(x, s, n, (const gsl_fft_complex_wavetable*)wt, (gsl_fft_complex_workspace*)ws);
#else	// NOTE this is VERY slow!
	const double *c = (const double *)wt;
	double *d = (double *)ws, f = inv?1./n:1;
	memset(d,0,2*n*sizeof(double));
	if(inv)	for(long i=0;i<n;i++)	for(long j=0;j<n;j++)
	{
		long ii = 2*(i+n*j), jj = 2*j*s;
		d[2*i] 	+= x[jj]*c[ii]+x[jj+1]*c[ii+1];
		d[2*i+1]+= x[jj+1]*c[ii]-x[jj]*c[ii+1];
	}
	else	for(long i=0;i<n;i++)	for(long j=0;j<n;j++)
	{
		long ii = 2*(i+n*j), jj = 2*j*s;
		d[2*i] 	+= x[jj]*c[ii]-x[jj+1]*c[ii+1];
		d[2*i+1]+= x[jj+1]*c[ii]+x[jj]*c[ii+1];
	}
	for(long j=0;j<n;j++)
	{	long jj = 2*j*s;	x[jj] = d[2*j]*f;	x[jj+1] = d[2*j+1]*f;	}
#endif
}
//-----------------------------------------------------------------------------
static void* mgl_fftx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0];
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		void *w = mgl_fft_alloc_thr(nx);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
			mgl_fft(t->b+2*nx*i, 1, nx, t->v, w, t->p[3]);
		mgl_fft_free_thr(w);
	}
	return 0;
}
static void* mgl_ffty(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1];
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		void *w = mgl_fft_alloc_thr(ny);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
			mgl_fft(t->b+2*(i%nx)+2*nx*ny*(i/nx), nx, ny, t->v, w, t->p[3]);
		mgl_fft_free_thr(w);
	}
	return 0;
}
static void* mgl_fftz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1],nz=t->p[2];
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		void *w = mgl_fft_alloc_thr(nz);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
			mgl_fft(t->b+2*i, nx*ny, nz, t->v, w, t->p[3]);
		mgl_fft_free_thr(w);
	}
	return 0;
}
void MGL_EXPORT mgl_datac_fft(HADT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx = d->nx, ny = d->ny, nz = d->nz;
	void *wt=0;
	bool clear=false;
	long par[4]={nx,ny,nz,strchr(dir,'i')!=0};
#if MGL_USE_DOUBLE
	double *a = (double *)(d->a);
#else
	double *a = new double[2*nx*ny*nz];	// manually convert to double
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)
	{	a[2*i] = real(d->a[i]);	a[2*i+1] = imag(d->a[i]);	}
#endif
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.wnx==nx)	wt = mgl_fft_data.wtx;
		else	{	clear = true;	wt = mgl_fft_alloc(nx,0,0);	}
		mglStartThreadT(mgl_fftx,ny*nz,0,a,wt,0,par);
		if(mgl_fft_data.wnx==0)
		{	clear = false;	mgl_fft_data.wtx = wt;	mgl_fft_data.wnx=nx;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.wny==ny)	wt = mgl_fft_data.wty;
		else	{	clear = true;	wt = mgl_fft_alloc(ny,0,0);	}
		mglStartThreadT(mgl_ffty,nx*nz,0,a,wt,0,par);
		if(mgl_fft_data.wny==0)
		{	clear = false;	mgl_fft_data.wty = wt;	mgl_fft_data.wny=ny;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.wnz==nz)	wt = mgl_fft_data.wtz;
		else	{	clear = true;	wt = mgl_fft_alloc(nz,0,0);	}
		mglStartThreadT(mgl_fftz,nx*ny,0,a,wt,0,par);
		if(mgl_fft_data.wnz==0)
		{	clear = false;	mgl_fft_data.wtz = wt;	mgl_fft_data.wnz=nz;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
#if !MGL_USE_DOUBLE
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)	d->a[i] = dual(a[2*i], a[2*i+1]);
	delete []a;
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_fourier(HMDT re, HMDT im, const char *dir)
{
	if(!dir || *dir==0)	return;
	long nx = re->nx, ny = re->ny, nz = re->nz;
	if(nx*ny*nz != im->nx*im->ny*im->nz || dir[0]==0)	return;
	bool clear=false;
	void *wt=0;
	long par[4]={nx,ny,nz,strchr(dir,'i')!=0};
	double *a = new double[2*nx*ny*nz];
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)
	{	a[2*i] = re->a[i];	a[2*i+1] = im->a[i];	}
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.wnx==nx)	wt = mgl_fft_data.wtx;
		else	{	clear = true;	wt = mgl_fft_alloc(nx,0,0);	}
		mglStartThreadT(mgl_fftx,ny*nz,0,a,wt,0,par);
		if(mgl_fft_data.wnx==0)
		{	mgl_fft_data.wtx = wt;	clear = false;	mgl_fft_data.wnx=nx;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.wny==ny)	wt = mgl_fft_data.wty;
		else	{	clear = true;	wt = mgl_fft_alloc(ny,0,0);	}
		mglStartThreadT(mgl_ffty,nx*nz,0,a,wt,0,par);
		if(mgl_fft_data.wny==0)
		{	mgl_fft_data.wty = wt;	clear = false;	mgl_fft_data.wny=ny;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.wnz==nz)	wt = mgl_fft_data.wtz;
		else	{	clear = true;	wt = mgl_fft_alloc(nz,0,0);	}
		mglStartThreadT(mgl_fftz,nx*ny,0,a,wt,0,par);
		if(mgl_fft_data.wnz==0)
		{	mgl_fft_data.wtz = wt;	clear = false;	mgl_fft_data.wnz=nz;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)
	{	re->a[i] = a[2*i];	im->a[i] = a[2*i+1];	}
	delete []a;
}
//-----------------------------------------------------------------------------
static void* mgl_envx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0];
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b =	new double[2*nx];
		void *w = mgl_fft_alloc_thr(nx);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nx;j++)	{	b[2*j] = a[j+i*nx];	b[2*j+1] = 0;	}
			mgl_fft(b, 1, nx, t->v, w, false);
			for(long j=0;j<nx;j++)	{	b[j] *= 2.;	b[j+nx] = 0;	}
			mgl_fft(b, 1, nx, t->v, w, true);
			for(long j=0;j<nx;j++)	a[j+i*nx] = hypot(b[2*j], b[2*j+1]);
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
static void* mgl_envy(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1];
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b =	new double[2*ny];
		void *w = mgl_fft_alloc_thr(ny);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<ny;j++)	{	b[2*j] = a[(i%nx)+nx*(j+ny*(i/nx))];	b[2*j+1] = 0;	}
			mgl_fft(b, 1, ny, t->v, t->w[t->id], false);
			for(long j=0;j<ny;j++)	{	b[j] *= 2.;	b[j+ny] = 0;	}
			mgl_fft(b, 1, ny, t->v, t->w[t->id], true);
			for(long j=0;j<ny;j++)	a[(i%nx)+nx*(j+ny*(i/nx))] = hypot(b[2*j], b[2*j+1]);
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
static void* mgl_envz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1],nz=t->p[2],k=nx*ny;
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b =	new double[2*nz];
		void *w = mgl_fft_alloc_thr(nz);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nz;j++)	{	b[2*j] = a[j*k+i];	b[2*j+1] = 0;	}
			mgl_fft(b, 1, nz, t->v, t->w[t->id], false);
			for(long j=0;j<nz;j++)	{	b[j] *= 2.;	b[j+nz] = 0;	}
			mgl_fft(b, 1, nz, t->v, t->w[t->id], true);
			for(long j=0;j<nz;j++)	a[j*k+i] = hypot(b[2*j], b[2*j+1]);
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
void MGL_EXPORT mgl_data_envelop(HMDT d, char dir)
{
	long nx=d->nx,ny=d->ny,nz=d->nz,par[3]={nx,ny,nz};
	bool clear=false;
	void *wt=0;
	if(dir=='x' && nx>1)
	{
		if(mgl_fft_data.wnx==nx)	wt = mgl_fft_data.wtx;
		else	{	clear = true;	wt = mgl_fft_alloc(nx,0,0);	}
		mglStartThreadT(mgl_envx,ny*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wnx==0)
		{	mgl_fft_data.wtx = wt;	clear = false;	mgl_fft_data.wnx=nx;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(dir=='y' && ny>1)
	{
		if( mgl_fft_data.wny==ny)	wt = mgl_fft_data.wty;
		else	{	clear = true;	wt = mgl_fft_alloc(ny,0,0);	}
		mglStartThreadT(mgl_envy,nx*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wny==0)
		{	mgl_fft_data.wty = wt;	clear = false;	mgl_fft_data.wny=ny;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(dir=='z' && nz>1)
	{
		if(mgl_fft_data.wnz==nz)	wt = mgl_fft_data.wtz;
		else	{	clear = true;	wt = mgl_fft_alloc(nz,0,0);	}
		mglStartThreadT(mgl_envz,nx*ny,d->a,0,wt,0,par);
		if(mgl_fft_data.wnz==0)
		{	mgl_fft_data.wtz = wt;	clear = false;	mgl_fft_data.wnz=nz;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_envelop(HADT c, char dir)
{
	mglData re(c->nx, c->ny, c->nz), im(c->nx, c->ny, c->nz);
	long n = c->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	{	re.a[i]=real(c->a[i]);	im.a[i]=imag(c->a[i]);	}
	mgl_data_envelop(&re, dir);
	mgl_data_envelop(&im, dir);
#pragma omp parallel for
	for(long i=0;i<n;i++)	c->a[i] = dual(re.a[i], im.a[i]);
}
//-----------------------------------------------------------------------------
static void* mgl_stfa1(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long mx=t->p[0],mz=t->p[2],dn=t->p[3],dd=dn/2,ny=t->p[4];
	mreal *d = (mreal*)t->a;
	HCDT re = (HCDT)t->re, im = (HCDT)t->im;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *a = new double[4*dn], ff;
		void *w = mgl_fft_alloc_thr(2*dn);
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%mx, j = ii/mx, i0;
			for(long k=0;k<2*dn;k++)
			{
				i0 = k-dd+j*dn;		ff = 1;
				if(i0<0)	i0=0;	else if(i0>=ny)	i0=ny-1;
				if(k<dd)
				{	ff = 0.5*(k-dd/2.)/dd;		ff=0.5+ff*(3-ff*ff);	}
				else if(k>=dn+dd)
				{	ff = 0.5*(k-3.5*dd)/dd;	ff=0.5-ff*(3-ff*ff);	}
				a[2*k] = re->v(i,i0)*ff;	a[2*k+1] = im->v(i,i0)*ff;
			}
			mgl_fft(a, 1, 2*dn, t->v, w, false);
			for(long k=0;k<dd;k++)
			{
				i0 = i+mx*(j+mz*k);
				d[i0+mx*mz*dd] = hypot(a[4*k],a[4*k+1])/dn;
				d[i0] = hypot(a[4*k+2*dn],a[4*k+2*dn+1])/dn;
			}
		}
		mgl_fft_free_thr(w);	delete []a;
	}
	return 0;
}
static void* mgl_stfa2(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long mx=t->p[0],my=t->p[1],dn=t->p[3],dd=dn/2,nx=t->p[4];
	mreal *d = (mreal*)t->a;
	HCDT re = (HCDT)t->re, im = (HCDT)t->im;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *a = new double[4*dn], ff;
		void *w = mgl_fft_alloc_thr(2*dn);
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%my, j = ii/my, i0;
			for(long k=0;k<2*dn;k++)
			{
				i0 = k-dd+i*dn;		ff = 1;
				if(i0<0)	i0=0;	else if(i0>=nx)	i0=nx-1;
				if(k<dd)
				{	ff = 0.5*(k-dd/2.)/dd;	ff=0.5+ff*(3-ff*ff);	}
				else if(k>=3*dd)
				{	ff = 0.5*(k-3.5*dd)/dd;	ff=0.5-ff*(3-ff*ff);	}
				a[2*k] = re->v(i0,j)*ff;	a[2*k+1] = im->v(i0,j)*ff;
			}
			mgl_fft(a, 1, 2*dn, t->v, w, false);
			for(long k=0;k<dd;k++)
			{
				i0 = i+my*(k+mx*j);
				d[i0+dd*my] = hypot(a[4*k],a[4*k+1])/dn;
				d[i0] = hypot(a[4*k+2*dn],a[4*k+2*dn+1])/dn;
			}
		}
		mgl_fft_free_thr(w);	delete []a;
	}
	return 0;
}
HMDT MGL_EXPORT mgl_data_stfa(HCDT re, HCDT im, long dn, char dir)
{
	if(dn<2)	return 0;
	dn = 2*(dn/2);
	long nx = re->GetNx(), ny = re->GetNy();
	if(nx*ny!=im->GetNx()*im->GetNy())	return 0;
	void *wt = mgl_fft_alloc(2*dn,0,0);
	long mx,my,mz;
	mglData *d=new mglData;
	if(dir=='y')
	{
		mx = nx;	my = dn;	mz = ny/dn;
		mgl_data_create(d, mx, mz, my);
		long par[5]={mx,my,mz,dn,ny};
		mglStartThreadT(mgl_stfa1,mx*mz,d->a,0,wt,0,par,re,im);
	}
	else
	{
		mx = dn;	my = nx/dn;	mz = ny;
		mgl_data_create(d, my, mx, mz);
		long par[5]={mx,my,mz,dn,nx};
		mglStartThreadT(mgl_stfa2,my*mz,d->a,0,wt,0,par,re,im);
	}
	mgl_fft_free(wt,0,0);
	return d;
}
//-----------------------------------------------------------------------------
static void* mgl_sinx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0];
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nx], f=sqrt(2./nx);
		void *w = mgl_fft_alloc_thr(nx);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long k = i*nx;	memset(b,0,2*nx*sizeof(double));
			for(long j=1;j<nx;j++)	b[2*j]=sin(M_PI*j/nx)*(a[j+k]+a[nx-j+k])+(a[j+k]-a[nx-j+k])*0.5;
			mgl_fft(b,1,nx,t->v,w,false);
			a[k]=0;	a[k+1]=b[0]*f/2;	// fill sinfft
			for(long j=1;j<nx/2;j++)
			{
				a[k+2*j] = -b[2*j+1]*f;
				a[k+2*j+1] = a[k+2*j-1]+b[2*j]*f;
			}
			if(nx%2)	a[nx-1] = -b[nx]*f;
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
static void* mgl_siny(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1];
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*ny], f=sqrt(2./ny);
		void *w = mgl_fft_alloc_thr(ny);
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%nx, k = ii/nx;	memset(b,0,2*ny*sizeof(double));
			for(long j=1;j<ny;j++)	b[2*j]=sin(M_PI*j/ny)*(a[i+nx*(ny*k+j)]+a[i+nx*(ny*k+ny-j)])+(a[i+nx*(ny*k+j)]-a[i+nx*(ny*k+ny-j)])*0.5;
			mgl_fft(b,1,ny,t->v,w,false);
			a[i+nx*ny*k]=0;	a[i+nx*(ny*k+1)]=b[0]*f/2;	// fill sinfft
			for(long j=1;j<ny/2;j++)
			{
				a[i+nx*(ny*k+2*j)] = -b[2*j+1]*f;
				a[i+nx*(ny*k+2*j+1)] = a[i+nx*(ny*k+2*j-1)]+b[2*j]*f;
			}
			if(ny%2)	a[i+nx*(ny*k+ny-1)] = -b[ny]*f;
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
static void* mgl_sinz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1],nz=t->p[2],k=nx*ny;
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nz], f=sqrt(2./nz);
		void *w = mgl_fft_alloc_thr(nz);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			memset(b,0,2*nz*sizeof(double));
			for(long j=1;j<nz;j++)	b[2*j]=sin(M_PI*j/nz)*(a[i+k*j]+a[i+k*(nz-j)])+(a[i+k*j]-a[i+k*(nz-j)])*0.5;
			mgl_fft(b,1,nz,t->v,w,false);
			a[i]=0;	a[i+k]=b[0]*f/2;	// fill sinfft
			for(long j=1;j<nz/2;j++)
			{
				a[i+k*2*j] = -b[2*j+1]*f;
				a[i+k*(2*j+1)] = a[i+k*(2*j-1)]+b[2*j]*f;
			}
			if(nz%2)	a[i+k*nz-k] = -b[nz]*f;
		}
		mgl_fft_free_thr(w);	delete []b;
	}
	return 0;
}
void MGL_EXPORT mgl_data_sinfft(HMDT d, const char *dir)	// use DST-1
{
	if(!dir || *dir==0)	return;
	bool clear=false;
	void *wt=0;
	long nx=d->nx, ny=d->ny, nz=d->nz, par[3]={nx,ny,nz};
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.wnx==nx)	wt = mgl_fft_data.wtx;
		else	{	clear = true;	wt = mgl_fft_alloc(nx,0,0);	}
		mglStartThreadT(mgl_sinx,ny*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wnx==0)
		{	mgl_fft_data.wtx = wt;	clear = false;	mgl_fft_data.wnx=nx;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.wny==ny)	wt = mgl_fft_data.wty;
		else	{	clear = true;	wt = mgl_fft_alloc(ny,0,0);	}
		mglStartThreadT(mgl_siny,nx*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wny==0)
		{	mgl_fft_data.wty = wt;	clear = false;	mgl_fft_data.wny=ny;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.wnz==nz)	wt = mgl_fft_data.wtz;
		else	{	clear = true;	wt = mgl_fft_alloc(nz,0,0);	}
		mglStartThreadT(mgl_sinz,nx*ny,d->a,0,wt,0,par);
		if(mgl_fft_data.wnz==0)
		{	mgl_fft_data.wtz = wt;	clear = false;	mgl_fft_data.wnz=nz;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_sinfft(HADT c, const char *dir)
{
	if(!dir || *dir==0)	return;
	mglData re(c->nx, c->ny, c->nz), im(c->nx, c->ny, c->nz);
	long n = c->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	{	re.a[i]=real(c->a[i]);	im.a[i]=imag(c->a[i]);	}
	mgl_data_sinfft(&re, dir);
	mgl_data_sinfft(&im, dir);
#pragma omp parallel for
	for(long i=0;i<n;i++)	c->a[i] = dual(re.a[i], im.a[i]);
}
//-----------------------------------------------------------------------------
static void* mgl_cosx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],nn=nx-1;
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nx], f=sqrt(2./nn);
		void *w = mgl_fft_alloc_thr(nn);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			long k = i*nx;	memset(b,0,2*nx*sizeof(double));
			for(long j=0;j<nn;j++)	b[2*j]=(a[j+k]+a[nn-j+k])*0.5-sin(M_PI*j/nn)*(a[j+k]-a[nn-j+k]);
			mgl_fft(b,1,nn,t->v,w,false);
			double f1=0.5*(a[k]-a[nn+k]), s=-1;
			a[nn+k]=0.5*(a[k]+a[nn+k]*((nn%2)?-1:1));
			for(long j=1;j<nn;j++)
			{
				f1 += a[j+k]*cos(M_PI*j/nn);
				a[nn+k] += a[j+k]*s;	s = -s;
			}
			a[k]=b[0]*f;	a[1+k]=f1*f;	a[nn+k]*=f;	// fill cosfft
			for(long j=1;j<nn/2;j++)
			{
				a[2*j+k] = b[2*j]*f;
				a[2*j+1+k] = a[2*j-1+k]-b[2*j+1]*f;
			}
			if(nn%2)	a[nn-1+k] = b[nn-1]*f;
		}
		mgl_fft_free_thr(w);
		delete []b;
	}
	return 0;
}
static void* mgl_cosy(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1],nn=ny-1;
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*ny], f=sqrt(2./nn);
		void *w = mgl_fft_alloc_thr(nn);
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%nx, k = ii/nx;	memset(b,0,2*ny*sizeof(double));
			for(long j=0;j<nn;j++)	b[2*j]=(a[i+nx*(ny*k+j)]+a[i+nx*(ny*k+nn-j)])*0.5-sin(M_PI*j/nn)*(a[i+nx*(ny*k+j)]-a[i+nx*(ny*k+nn-j)]);
			mgl_fft(b,1,nn,t->v,w,false);
			double f1=0.5*(a[i+nx*ny*k]-a[i+nx*(ny*k+nn)]), s=-1;
			a[i+nx*(ny*k+nn)]=0.5*(a[i+nx*ny*k]+a[i+nx*(ny*k+nn)]*((nn%2)?-1:1));
			for(long j=1;j<nn;j++)
			{
				f1 += a[i+nx*(ny*k+j)]*cos(M_PI*j/nn);
				a[i+nx*(ny*k+nn)] += a[i+nx*(ny*k+j)]*s;	s = -s;
			}
			a[i+nx*ny*k]=b[0]*f;	a[i+nx*(ny*k+1)]=f1*f;	a[i+nx*(ny*k+nn)]*=f;	// fill cosfft
			for(long j=1;j<nn/2;j++)
			{
				a[i+nx*(ny*k+2*j)] = b[2*j]*f;
				a[i+nx*(ny*k+2*j+1)] = a[i+nx*(ny*k+2*j-1)]-b[2*j+1]*f;
			}
			if(nn%2)	a[i+nx*(ny*k+nn-1)] = b[nn-1]*f;
		}
		mgl_fft_free_thr(w);
		delete []b;
	}
	return 0;
}
static void* mgl_cosz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1],nz=t->p[2],k=nx*ny,nn=nz-1;
	mreal *a = (mreal*)t->a;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nz], f=sqrt(2./nn);
		void *w = mgl_fft_alloc_thr(nn);
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			memset(b,0,2*nz*sizeof(double));
			for(long j=0;j<nn;j++)	b[2*j]=(a[i+k*j]+a[i+k*(nn-j)])*0.5-sin(M_PI*j/nn)*(a[i+k*j]-a[i+k*(nn-j)]);
			mgl_fft(b,1,nn,t->v,w,false);
			double f1=0.5*(a[i]-a[i+k*nn]), s=-1;
			a[i+k*nn]=0.5*(a[i]+a[i+k*nn]*((nn%2)?-1:1));
			for(long j=1;j<nn;j++)
			{
				f1 += a[i+k*j]*cos(M_PI*j/nn);
				a[i+k*nn] += a[i+k*j]*s;	s = -s;
			}
			a[i]=b[0]*f;	a[i+k]=f1*f;	a[i+k*nn]*=f;	// fill cosfft
			for(long j=1;j<nn/2;j++)
			{
				a[i+k*2*j] = b[2*j]*f;
				a[i+k*2*j+k] = a[i+k*2*j-k]-b[2*j+1]*f;
			}
			if(nn%2)	a[i+k*nn-k] = b[nn-1]*f;
		}
		mgl_fft_free_thr(w);
		delete []b;
	}
	return 0;
}
void MGL_EXPORT mgl_data_cosfft(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	bool clear=false;
	void *wt=0;
	long nx=d->nx, ny=d->ny, nz=d->nz, par[3]={nx,ny,nz};
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.wnx==nx-1)	wt = mgl_fft_data.wtx;
		else	{	clear = true;	wt = mgl_fft_alloc(nx-1,0,0);	}
		mglStartThreadT(mgl_cosx,ny*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wnx==0)
		{	mgl_fft_data.wtx = wt;	clear = false;	mgl_fft_data.wnx=nx-1;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.wny==ny-1)	wt = mgl_fft_data.wty;
		else	{	clear = true;	wt = mgl_fft_alloc(ny-1,0,0);	}
		mglStartThreadT(mgl_cosy,nx*nz,d->a,0,wt,0,par);
		if(mgl_fft_data.wny==0)
		{	mgl_fft_data.wty = wt;	clear = false;	mgl_fft_data.wny=ny-1;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.wnz==nz-1)	wt = mgl_fft_data.wtz;
		else	{	clear = true;	wt = mgl_fft_alloc(nz-1,0,0);	}
		mglStartThreadT(mgl_cosz,nx*ny,d->a,0,wt,0,par);
		if(mgl_fft_data.wnz==0)
		{	mgl_fft_data.wtz = wt;	clear = false;	mgl_fft_data.wnz=nz-1;	}
		if(clear)	{	mgl_fft_free(wt,0,0);	clear = false;	}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_cosfft(HADT c, const char *dir)
{
	if(!dir || *dir==0)	return;
	mglData re(c->nx, c->ny, c->nz), im(c->nx, c->ny, c->nz);
	long n = c->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	{	re.a[i]=real(c->a[i]);	im.a[i]=imag(c->a[i]);	}
	mgl_data_cosfft(&re, dir);
	mgl_data_cosfft(&im, dir);
#pragma omp parallel for
	for(long i=0;i<n;i++)	c->a[i] = dual(re.a[i], im.a[i]);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_transform_a(HCDT am, HCDT ph, const char *tr)
{
	long nx = am->GetNx(), ny = am->GetNy(), nz = am->GetNz();
	if(nx*ny*nz != ph->GetNN() || !tr || tr[0]==0)	return 0;
	mglData re(nx,ny,nz), im(nx,ny,nz);
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)
	{
		mreal a=am->vthr(i), p=ph->vthr(i);
		re.a[i] = a*cos(p);	im.a[i] = a*sin(p);
	}
	return mgl_transform(&re, &im, tr);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_transform(HCDT re, HCDT im, const char *tr)
{
	if(!tr || *tr==0)	return 0;
	long nx = re->GetNx(), ny = re->GetNy(), nz = re->GetNz();
	if(nx*ny*nz != im->GetNN() || tr[0]==0)	return 0;
	mglData rr(re),ii(im);
	if(strchr(tr,'i') && strchr(tr,'f'))	// general case
	{
		if(tr[0]=='f')	mgl_data_fourier(&rr,&ii,"x");
		if(tr[0]=='i')	mgl_data_fourier(&rr,&ii,"xi");
		if(tr[1]=='f')	mgl_data_fourier(&rr,&ii,"y");
		if(tr[1]=='i')	mgl_data_fourier(&rr,&ii,"yi");
		if(tr[2]=='f')	mgl_data_fourier(&rr,&ii,"z");
		if(tr[2]=='i')	mgl_data_fourier(&rr,&ii,"zi");
	}
	else if(strchr(tr,'f'))	// do Fourier only once for speeding up
	{
		char str[4] = "   ";
		if(tr[0]=='f')	str[0]='x';
		if(tr[1]=='f')	str[1]='y';
		if(tr[2]=='f')	str[2]='z';
		mgl_data_fourier(&rr,&ii,str);
	}
	else if(strchr(tr,'i'))	// do Fourier only once for speeding up
	{
		char str[5] = "   i";
		if(tr[0]=='i')	str[0]='x';
		if(tr[1]=='i')	str[1]='y';
		if(tr[2]=='i')	str[2]='z';
		mgl_data_fourier(&rr,&ii,str);
	}
	else if(strchr(tr,'s'))	// do Fourier only once for speeding up
	{
		if(tr[0]=='s')	{	rr.SinFFT("x");	ii.SinFFT("x");	}
		if(tr[1]=='s')	{	rr.SinFFT("y");	ii.SinFFT("y");	}
		if(tr[2]=='s')	{	rr.SinFFT("z");	ii.SinFFT("z");	}
	}
	else if(strchr(tr,'c'))	// do Fourier only once for speeding up
	{
		if(tr[0]=='c')	{	rr.CosFFT("x");	ii.CosFFT("x");	}
		if(tr[1]=='c')	{	rr.CosFFT("y");	ii.CosFFT("y");	}
		if(tr[2]=='c')	{	rr.CosFFT("z");	ii.CosFFT("z");	}
	}
	else if(strchr(tr,'h'))	// do Fourier only once for speeding up
	{
		if(tr[0]=='h')	{	rr.Hankel("x");	ii.Hankel("x");	}
		if(tr[1]=='h')	{	rr.Hankel("y");	ii.Hankel("y");	}
		if(tr[2]=='h')	{	rr.Hankel("z");	ii.Hankel("z");	}
	}
	mglData *d = new mglData(nx, ny, nz);
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)	d->a[i] = hypot(rr.a[i],ii.a[i]);
	return d;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_transform_a_(uintptr_t *am, uintptr_t *ph, const char *tr, int l)
{	char *s=new char[l+1];	memcpy(s,tr,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_transform_a(_DA_(am),_DA_(ph),s));
	delete []s;		return res;	}
uintptr_t MGL_EXPORT mgl_transform_(uintptr_t *re, uintptr_t *im, const char *tr, int l)
{	char *s=new char[l+1];	memcpy(s,tr,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_transform(_DA_(re),_DA_(im),s));
	delete []s;		return res;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_envelop_(uintptr_t *d, const char *dir, int)
{	mgl_data_envelop(_DT_,*dir);	}
void MGL_EXPORT mgl_datac_envelop_(uintptr_t *d, const char *dir, int)
{	mgl_datac_envelop(_DC_,*dir);	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
static void* mgl_chnkx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0];
	dual *a = (dual*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(nx+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[3*nx];
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nx;j++)	b[j] = real(a[j+nx*i]);
			gsl_dht_apply(dht,b,b+nx);
			for(long j=0;j<nx;j++)	b[j] = imag(a[j+nx*i]);
			gsl_dht_apply(dht,b,b+2*nx);
			for(long j=0;j<nx;j++)	a[j+nx*i] = dual(b[j+nx]*mm,b[j+2*nx]*mm);
		}
		delete []b;
	}
	return 0;
}
static void* mgl_chnky(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1];
	dual *a = (dual*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(ny+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[3*ny];
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%nx, k = ii/nx;
			for(long j=0;j<ny;j++)	b[j] = real(a[i+nx*(j+ny*k)]);
			gsl_dht_apply(dht,b,b+ny);
			for(long j=0;j<ny;j++)	b[j] = imag(a[i+nx*(j+ny*k)]);
			gsl_dht_apply(dht,b,b+2*ny);
			for(long j=0;j<ny;j++)	a[i+nx*(j+ny*k)] = dual(b[j+ny]*mm,b[j+2*ny]*mm);
		}
		delete []b;
	}
	return 0;
}
static void* mgl_chnkz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long k=t->p[0]*t->p[1],nz=t->p[2];
	dual *a = (dual*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(nz+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[3*nz];
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nz;j++)	b[j] = real(a[i+j*k]);
			gsl_dht_apply(dht,b,b+nz);
			for(long j=0;j<nz;j++)	b[j] = imag(a[i+j*k]);
			gsl_dht_apply(dht,b,b+2*nz);
			for(long j=0;j<nz;j++)	a[i+j*k] = dual(b[j+nz]*mm,b[j+2*nz]*mm);
		}
		delete []b;
	}
	return 0;
}
void MGL_EXPORT mgl_datac_hankel(HADT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	gsl_dht *dht=0;
	bool clear = false;
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long par[3]={nx,ny,nz};
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.hnx==nx)	dht = (gsl_dht *)mgl_fft_data.htx;
		else	{	dht = gsl_dht_new(nx,0,1);	clear = true;	}
		mglStartThreadT(mgl_chnkx,ny*nz,d->a,0,dht,0,par);
		if(mgl_fft_data.hnx==0)
		{	mgl_fft_data.htx = dht;	clear = false;	mgl_fft_data.hnx=nx;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.hny==ny)	dht = (gsl_dht *)mgl_fft_data.hty;
		else	{	dht = gsl_dht_new(ny,0,1);	clear = true;	}
		mglStartThreadT(mgl_chnky,nx*nz,d->a,0,dht,0,par);
		if(mgl_fft_data.hny==0)
		{	mgl_fft_data.hty = dht;	clear = false;	mgl_fft_data.hny=ny;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.hnz==nz)	dht = (gsl_dht *)mgl_fft_data.htz;
		else	{	dht = gsl_dht_new(nz,0,1);	clear = true;	}
		mglStartThreadT(mgl_chnkz,nx*ny,d->a,0,dht,0,par);
		if(mgl_fft_data.hnz==0)
		{	mgl_fft_data.htz = dht;	clear = false;	mgl_fft_data.hnz=nz;	}
	}
	if(clear)	gsl_dht_free(dht);
}
#else
void MGL_EXPORT mgl_datac_hankel(HADT , const char *){}
#endif
void MGL_EXPORT mgl_datac_hankel_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_datac_hankel(_DC_,s);	delete []s;	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
static void* mgl_hnkx(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0];
	mreal *a = (mreal*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(nx+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nx];
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nx;j++)	b[j] = a[j+nx*i];
			gsl_dht_apply(dht,b,b+nx);
			for(long j=0;j<nx;j++)	a[j+nx*i] = b[j+nx]*mm;
		}
		delete []b;
	}
	return 0;
}
static void* mgl_hnky(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long nx=t->p[0],ny=t->p[1];
	mreal *a = (mreal*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(ny+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*ny];
#pragma omp for nowait
		for(long ii=t->id;ii<t->n;ii+=mglNumThr)
		{
			long i = ii%nx, k = ii/nx;
			for(long j=0;j<ny;j++)	b[j] = a[i+nx*(j+ny*k)];
			gsl_dht_apply(dht,b,b+ny);
			for(long j=0;j<ny;j++)a[i+nx*(j+ny*k)] = b[j+ny]*mm;
		}
		delete []b;
	}
	return 0;
}
static void* mgl_hnkz(void *par)
{
	mglThreadT *t=(mglThreadT *)par;
	long k=t->p[0]*t->p[1],nz=t->p[2];
	mreal *a = (mreal*)t->a;
	const gsl_dht *dht = (const gsl_dht*)t->v;
	double mm = gsl_sf_bessel_zero_J0(nz+1);

#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	{
		double *b = new double[2*nz];
#pragma omp for nowait
		for(long i=t->id;i<t->n;i+=mglNumThr)
		{
			for(long j=0;j<nz;j++)	b[j] = a[i+j*k];
			gsl_dht_apply(dht,b,b+nz);
			for(long j=0;j<nz;j++)	a[i+j*k] = b[j+nz]*mm;
		}
		delete []b;
	}
	return 0;
}
void MGL_EXPORT mgl_data_hankel(HMDT d, const char *dir)
{
	if(!dir || *dir==0)	return;
	bool clear = false;
	gsl_dht *dht=0;
	long nx=d->nx, ny=d->ny, nz=d->nz;
	long par[3]={nx,ny,nz};
	if(strchr(dir,'x') && nx>1)
	{
		if(mgl_fft_data.hnx==nx)	dht = (gsl_dht *)mgl_fft_data.htx;
		else	{	dht = gsl_dht_new(nx,0,1);	clear = true;	}
		mglStartThreadT(mgl_hnkx,ny*nz,d->a,0,dht,0,par);
		if(mgl_fft_data.hnx==0)
		{	mgl_fft_data.htx = dht;	clear = false;	mgl_fft_data.hnx=nx;	}
	}
	if(strchr(dir,'y') && ny>1)
	{
		if(mgl_fft_data.hny==ny)	dht = (gsl_dht *)mgl_fft_data.hty;
		else	{	dht = gsl_dht_new(ny,0,1);	clear = true;	}
		mglStartThreadT(mgl_hnky,nx*nz,d->a,0,dht,0,par);
		if(mgl_fft_data.hny==0)
		{	mgl_fft_data.hty = dht;	clear = false;	mgl_fft_data.hny=ny;	}
	}
	if(strchr(dir,'z') && nz>1)
	{
		if(mgl_fft_data.hnz==nz)	dht = (gsl_dht *)mgl_fft_data.htz;
		else	{	dht = gsl_dht_new(nz,0,1);	clear = true;	}
		mglStartThreadT(mgl_hnkz,nx*ny,d->a,0,dht,0,par);
		if(mgl_fft_data.hnz==0)
		{	mgl_fft_data.htz = dht;	clear = false;	mgl_fft_data.hnz=nz;	}
	}
	if(clear)	gsl_dht_free(dht);
}
#else
void MGL_EXPORT mgl_data_hankel(HMDT , const char *){}
#endif
void MGL_EXPORT mgl_data_hankel_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_hankel(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_fill_sample(HMDT d, const char *how)
{
	if(!how || *how==0)	return;
	bool kk = mglchr(how,'k');
	long n=d->nx,dn=1;
	mreal *aa=d->a;
	if(mglchr(how,'y'))	{	n=d->ny;	dn=d->nx;	}
	if(mglchr(how,'z'))	{	n=d->nz;	dn=d->nx*d->ny;	}
	if(mglchr(how,'h'))	// Hankel
	{
#if MGL_HAVE_GSL
		gsl_dht *dht = gsl_dht_new(n,0,1);
#pragma omp parallel for
		for(long i=0;i<n;i++)
			aa[i*dn] = kk ? gsl_dht_k_sample(dht, i) : gsl_dht_x_sample(dht, i);
		gsl_dht_free(dht);
#endif
	}
	else	// Fourier
	{
		if(kk)	for(long i=0;i<n;i++)	aa[i*dn] = M_PI*(i<n/2 ? i:i-n);
		else	for(long i=0;i<n;i++)	aa[i*dn] = mreal(2*i-n)/n;
	}
#pragma omp parallel for
	for(long i=0;i<d->GetNN();i++)	aa[i] = aa[((i%(n*dn))/dn)*dn];
}
void MGL_EXPORT mgl_data_fill_sample_(uintptr_t *d, const char *how,int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	mgl_data_fill_sample(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_fft_(uintptr_t *d, const char *dir, int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_datac_fft(_DC_,s);	delete []s;	}
void MGL_EXPORT mgl_data_fourier_(uintptr_t *re, uintptr_t *im, const char *dir, int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_fourier(_DM_(re),_DM_(im),s);	delete []s;	}
uintptr_t MGL_EXPORT mgl_data_stfa_(uintptr_t *re, uintptr_t *im, int *dn, char *dir, int)
{	return uintptr_t(mgl_data_stfa(_DA_(re),_DA_(im),*dn,*dir));	}
void MGL_EXPORT mgl_data_cosfft_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_cosfft(_DT_,s);	delete []s;	}
void MGL_EXPORT mgl_data_sinfft_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_sinfft(_DT_,s);	delete []s;	}
void MGL_EXPORT mgl_datac_cosfft_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_datac_cosfft(_DC_,s);	delete []s;	}
void MGL_EXPORT mgl_datac_sinfft_(uintptr_t *d, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_datac_sinfft(_DC_,s);	delete []s;	}
//-----------------------------------------------------------------------------
/*static void* mgl_cor(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	dual *a = t->a;
	const dual *b = t->b;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel
#endif
	for(long i=t->id;i<t->n;i+=mglNumThr)	a[i] *= conj(b[i]);
	return 0;
}*/
HADT MGL_EXPORT mgl_datac_correl(HCDT d1, HCDT d2, const char *dir)
{
	if(!dir || *dir==0)	return 0;
	if(d2==NULL)	d2=d1;
	long nx = d1->GetNx(), ny = d1->GetNy(), nz = d1->GetNz();
	if(nx*ny*nz!=d2->GetNN())	return 0;
	std::string dirs;
	if(strchr(dir,'x') && nx>1)	dirs += 'x';
	if(strchr(dir,'y') && ny>1)	dirs += 'y';
	if(strchr(dir,'z') && nz>1)	dirs += 'z';
	if(dirs.empty())	return 0;
	mglDataC *a = new mglDataC(d1), *b=a;	a->FFT(dirs.c_str());
	if(d1!=d2)
	{	b = new mglDataC(d2);	b->FFT(dirs.c_str());	}
//	mglStartThreadC(mgl_cor,0,nx*ny*nz,a->a,b->a);	// TODO: sth strange
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)	a->a[i] *= conj(b->a[i]);
	dirs += 'i';	a->FFT(dirs.c_str());
	if(d1!=d2)	delete b;
	return a;
}
uintptr_t MGL_EXPORT mgl_datac_correl_(uintptr_t *d1, uintptr_t *d2, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_datac_correl(_DA_(d1),_DA_(d2),s));
	delete []s;		return res;	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_correl(HCDT d1, HCDT d2, const char *dir)
{
	HADT a = mgl_datac_correl(d1,d2,dir);	// NOTE: this is not so effective but straightforward way
	if(!a)	return 0;
	const long nx = d1->GetNx(), ny = d1->GetNy(), nz = d1->GetNz();
	mglData *res = new mglData(nx,ny,nz);
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)	res->a[i] = real(a->a[i]);
	delete a;	return res;
}
uintptr_t MGL_EXPORT mgl_data_correl_(uintptr_t *d1, uintptr_t *d2, const char *dir,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_data_correl(_DA_(d1),_DA_(d2),s));
	delete []s;		return res;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_wavelet(HMDT dat, const char *how, int k)
{
#if MGL_HAVE_GSL
	gsl_wavelet *w=0;
	if(mglchr(how,'d'))	w = gsl_wavelet_alloc(gsl_wavelet_daubechies, k);
	else if(mglchr(how,'D'))	w = gsl_wavelet_alloc(gsl_wavelet_daubechies_centered, k);
	else if(mglchr(how,'h'))	w = gsl_wavelet_alloc(gsl_wavelet_haar, k);
	else if(mglchr(how,'H'))	w = gsl_wavelet_alloc(gsl_wavelet_haar_centered, k);
	else if(mglchr(how,'b'))	w = gsl_wavelet_alloc(gsl_wavelet_bspline, k);
	else if(mglchr(how,'B'))	w = gsl_wavelet_alloc(gsl_wavelet_bspline_centered, k);
	if(!w)	return;

	double *a;
#if MGL_USE_DOUBLE
	a = dat->a;
#else
	long nn = dat->GetNN();
	a = new double[nn];
#pragma omp parallel for
	for(long i=0;i<nn;i++)	a[i] = dat->a[i];
#endif
	if(mglchr(how,'x'))
#pragma omp parallel
	{
		long n = dat->nx;
		gsl_wavelet_workspace *work = gsl_wavelet_workspace_alloc(n);
		if(mglchr(how,'i'))
#pragma omp for
			for(long i=0;i<dat->ny*dat->nz;i++)
				gsl_wavelet_transform_inverse(w, a+i*n, 1, n, work);
		else
#pragma omp for
			for(long i=0;i<dat->ny*dat->nz;i++)
				gsl_wavelet_transform_forward(w, a+i*n, 1, n, work);
		gsl_wavelet_workspace_free(work);
	}
	if(mglchr(how,'y'))
#pragma omp parallel
	{
		long n = dat->ny, s = dat->nx;
		gsl_wavelet_workspace *work = gsl_wavelet_workspace_alloc(n);
		if(mglchr(how,'i'))
#pragma omp for collapse(2)
			for(long j=0;j<dat->nz;j++)	for(long i=0;i<dat->nx;i++)
				gsl_wavelet_transform_inverse(w, a+i+n*s*j, s, n, work);
		else
#pragma omp for collapse(2)
			for(long j=0;j<dat->nz;j++)	for(long i=0;i<dat->nx;i++)
				gsl_wavelet_transform_forward(w, a+i+n*s*j, s, n, work);
		gsl_wavelet_workspace_free(work);
	}
	if(mglchr(how,'z'))
#pragma omp parallel
	{
		long n = dat->nz, s = dat->nx*dat->ny;
		gsl_wavelet_workspace *work = gsl_wavelet_workspace_alloc(n);
		if(mglchr(how,'i'))
#pragma omp for
			for(long i=0;i<dat->nx*dat->ny;i++)
				gsl_wavelet_transform_inverse(w, a+i, s, n, work);
		else
#pragma omp for
			for(long i=0;i<dat->nx*dat->ny;i++)
				gsl_wavelet_transform_forward(w, a+i, s, n, work);
		gsl_wavelet_workspace_free(work);
	}
#if !MGL_USE_DOUBLE
#pragma omp parallel for
	for(long i=0;i<nn;i++)	dat->a[i] = a[i];
	delete []a;
#endif
	gsl_wavelet_free (w);
#endif
}
void MGL_EXPORT mgl_data_wavelet_(uintptr_t *d, const char *dir, int *k,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_data_wavelet(_DT_,s,*k);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_wavelet(HADT c, const char *how, int k)
{
	mglData re(c->nx, c->ny, c->nz), im(c->nx, c->ny, c->nz);
	long n = c->GetNN();
#pragma omp parallel for
	for(long i=0;i<n;i++)	{	re.a[i]=real(c->a[i]);	im.a[i]=imag(c->a[i]);	}
	mgl_data_wavelet(&re, how, k);
	mgl_data_wavelet(&im, how, k);
#pragma omp parallel for
	for(long i=0;i<n;i++)	c->a[i] = dual(re.a[i], im.a[i]);
}
void MGL_EXPORT mgl_datac_wavelet_(uintptr_t *d, const char *dir, int *k,int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	mgl_datac_wavelet(_DC_,s,*k);	delete []s;	}
//-----------------------------------------------------------------------------
