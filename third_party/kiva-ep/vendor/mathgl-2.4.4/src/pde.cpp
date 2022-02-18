/***************************************************************************
 * pde.cpp is part of Math Graphic Library
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
#include "mgl2/data.h"
#include "mgl2/datac.h"
#include "mgl2/eval.h"
#include "mgl2/thread.h"
#include "mgl2/base.h"
#include "interp.hpp"
const double GAMMA=0.1;	///< value for damping
HADT MGL_NO_EXPORT mglFormulaCalcC(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
//
//		Advanced PDE series in 2D case
//
//-----------------------------------------------------------------------------
void static mgl_operator_exp(long n, const dual *h, dual *a, dual *f)
{
	memset(f,0,2*n*sizeof(dual));
	const long i1=n/2, i2=3*n/2-1;
#pragma omp parallel for
	for(long j=0;j<n;j++)
	{
		long jp = (j+1)%n;
		dual h1=h[n*j]*dual(0,1),		g1=(h1+h[n*jp]*dual(0,1))/mreal(2);
		dual h2=h[n-1+n*j]*dual(0,1),	g2=(h2+h[n-1+n*jp]*dual(0,1))/mreal(2);
		mreal k1=M_PI*2*j/n, k2 = M_PI*(2*j+1)/n;
		for(long i=0;i<i1;i++)
		{
			f[2*j] += a[i]*exp(h1+dual(0,i*k1));
			f[2*j+1] += a[i]*exp(g1+dual(0,i*k2));
		}
		for(long i=i1;i<i2;i++)
		{
			dual hh = h[i-i1+n*j];
			f[2*j] += a[i]*exp(hh*dual(0,1)+dual(0,i*k1));
			f[2*j+1] += a[i]*exp((hh+h[i-i1+n*jp])*dual(0,0.5)+dual(0,i*k2));
		}
		for(long i=i2;i<2*n;i++)
		{
			f[2*j] += a[i]*exp(h2+dual(0,i*k1));
			f[2*j+1] += a[i]*exp(g2+dual(0,i*k2));
		}
	}
	memset(a,0,2*n*sizeof(dual));
#pragma omp parallel for
	for(long i=0;i<2*n;i++)
	{
		long ii=i-i1;
		if(ii<0)	ii=0;
		if(ii>n-1)	ii=n-1;
		double kk=M_PI*2*i/n;
		for(long j=0;j<n;j++)
		{
			dual h1 = h[ii+n*j], g1 = (h1+h[ii+n*((j+1)%n)])*dual(0,0.5);
			a[i] += f[2*j]*exp(h1*dual(0,1)-dual(0,kk*j));
			a[i] += f[2*j+1]*exp(g1-dual(0,kk*(j+0.5)));
		}
	}
}
//-----------------------------------------------------------------------------
void static mgl_operator_lin(long n, mreal *h, dual *a, dual *f, dual *g, dual *o, const dual *iexp)
{
	memset(f,0,2*n*sizeof(dual));
	memset(g,0,2*n*sizeof(dual));
	const long i1=n/2, i2=3*n/2-1;
#pragma omp parallel for
	for(long j=0;j<n;j++)
	{
		long jp = (j+1)%n;
		mreal h1=tanh(h[n*j]), g1=(h1+tanh(h[n*jp]))/2;
		mreal h2=tanh(h[n-1+n*j]), g2=(h2+tanh(h[n-1+n*jp]))/2;
		const dual *ie1=iexp+4*n*j, *ie2=iexp+2*n*(2*j+1);
		for(long i=0;i<i1;i++)
		{
			dual e1=ie1[i], e2=ie2[i];
			f[2*j] += a[i]*h1*e1;	f[2*j+1] += a[i]*g1*e2;
			g[2*j] += a[i]*e1;		g[2*j+1] += a[i]*e2;
		}
		for(long i=i1;i<i2;i++)
		{
			mreal hh = tanh(h[i-i1+n*j]);
			mreal gg = (hh+tanh(h[i-i1+n*jp]))/2;
			dual e1=ie1[i], e2=ie2[i];
			f[2*j] += a[i]*hh*e1;	f[2*j+1] += a[i]*gg*e2;
			g[2*j] += a[i]*e1;		g[2*j+1] += a[i]*e2;
		}
		for(long i=i2;i<2*n;i++)
		{
			dual e1=ie1[i], e2=ie2[i];
			f[2*j] += a[i]*h2*e1;	f[2*j+1] += a[i]*g2*e2;
			g[2*j] += a[i]*e1;		g[2*j+1] += a[i]*e2;
		}
	}
	memset(o,0,2*n*sizeof(dual));
#pragma omp parallel for
	for(long i=0;i<2*n;i++)
	{
		long ii=i-i1;
		if(ii<0)	ii=0;
		if(ii>n-1)	ii=n-1;
		const dual *ie1=iexp+2*n*i;
//		double kk=M_PI*2*i/n;
		for(long j=0;j<n;j++)
		{
			mreal h1 = tanh(h[ii+n*j]);
			mreal g1 = (h1+tanh(h[ii+n*((j+1)%n)]))/2;
			dual e1=conj(ie1[2*j]), e2=conj(ie1[2*j+1]);
			o[i] += f[2*j]*e1 + f[2*j+1]*e2;
			o[i] += g[2*j]*h1*e1 + g[2*j+1]*g1*e2;
		}
	}
}
//-----------------------------------------------------------------------------
HADT static mgl_apde_calc_ham(HMDT hs, bool old, const char *func, std::vector<mglDataA*> list, const mreal dd)
{
	HADT ham = mglFormulaCalcC(func, list);	mgl_datac_mul_num(ham,dd);
	const long nx = ham->nx;
	if(old)
	{
		mreal hh = ham->Imag().Minimal();
		if(hh>0)	hh=0;
#pragma omp parallel for
		for(long i=0;i<nx*nx;i++)
		{
			hs->a[i] = sqrt(imag(ham->a[i])-hh);	// non-additive term
			ham->a[i] = dual(real(ham->a[i]),hh);	// additive terms
		}
	}
	else
	{
		mglData xIm(nx), pIm(nx);
#pragma omp parallel for
		for(long i=0;i<nx;i++)	// first find minimal values along x and p
		{
			dual *ax=ham->a+i, *ay=ham->a+i*nx;
			mreal mx=imag(ax[0]), my=imag(ay[0]);
			for(long j=1;j<nx;j++)	my = (my<imag(ay[j]))?my:imag(ay[j]);
			for(long j=1;j<nx;j++)	mx = (mx<imag(ax[j*nx]))?mx:imag(ax[j*nx]);
			xIm.a[i] = mx;	pIm.a[i]=my;
		}
		mreal mIm=xIm.a[0];	mreal *aa=xIm.a;	// global minimum
		for(long j=1;j<nx;j++)	mIm = (mIm<aa[j])?mIm:aa[j];
#pragma omp parallel for collapse(2)
		for(long j=0;j<nx;j++)	for(long i=0;i<nx;i++)
		{
			mreal hh = xIm.a[i]+pIm.a[j]-mIm;
			long i0=i+nx*j;
			hs->a[i0] = sqrt(fabs(imag(ham->a[i0])-hh));	// non-additive term. NOTE: fabs() guarantee absence of negative values due to rounding error
			ham->a[i0] = dual(real(ham->a[i0]),hh);	// additive terms
		}
	}
	return ham;
}
//-----------------------------------------------------------------------------
// Solve equation dx/dy = func(p,x,y,|u|)[u] where p=d/dx. There are no assumptions about form of func().
HADT MGL_EXPORT mgl_pde_adv_c(HMGL gr, const char *func, HCDT ini_re, HCDT ini_im, mreal dt, mreal k0, const char *opt)
{
	mreal gamma = gr->SaveState(opt);	if(mgl_isnan(gamma))	gamma = 20;
	const mglPoint &Min=gr->Min, &Max=gr->Max;
	const long nx=ini_re->GetNx(), nt = long((Max.y-Min.y)/dt)+1;
	if(nx<2 || nt<2 || Max.x==Min.x){	gr->SetWarn(mglWarnLow,"PDE");	return 0;	}	// Too small data
	if(ini_im->GetNx() != nx)		{	gr->SetWarn(mglWarnDim,"PDE");	return 0;	}	// Wrong dimensions

	mglDataC *res=new mglDataC(nx, nt);
	mglData hIm(nx,nx);	// for advanced damping calculation
	mglDataC u(nx);	u.Name(L"u");
	mglDataV x(nx,nx), y(nx,nx), r(nx,nx);
	mglDataW p(nx,nx);	p.Name(L"p");
	bool old = func[0]==';';	if(old)	func=func+1;
	x.Name(L"x");	y.Name(L"y");	r.Name(L"#$mgl");
	const mreal dp = 2*M_PI/(Max.x-Min.x), dd = k0*dt/2;
	x.Fill(Min.x,Max.x,'x');	p.Freq(dp/k0,'y');
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&p);	list.push_back(&r);	list.push_back(&u);

	dual *a = new dual[2*nx];	memset(a,0,2*nx*sizeof(dual));	// Add "damping" area
	dual *f = new dual[6*nx], *g=f+2*nx, *s=f+4*nx;
#pragma omp parallel for
	for(long i=0;i<nx;i++)	// Initial conditions
		a[i+nx/2] = dual(ini_re->v(i), ini_im->v(i));
	double *dmp = new double[2*nx];	memset(dmp,0,2*nx*sizeof(double));
#pragma omp parallel for
	for(long i=0;i<2*nx;i++)	// dumping
	{
		if(i<nx/2)		dmp[i] += gamma*mgl_ipow((nx/2-i)/mreal(nx/2),2);
		if(i>3*nx/2)	dmp[i] += gamma*mgl_ipow((i-3*nx/2-1)/mreal(nx/2),2);
	}
	bool have_y = mglchr(func,'y');
	HADT ham;
	if(!have_y)		ham = mgl_apde_calc_ham(&hIm, old, func, list, dd);
	dual *iexp = new dual[4*nx*nx];
#pragma omp parallel for collapse(2)
	for(long j=0;j<2*nx;j++)	for(long i=0;i<2*nx;i++)
		iexp[i+2*nx*j] = exp(dual(0,(M_PI*i*j)/nx));
	for(long k=0;k<nt;k++)
	{
		memcpy(u.a,a+nx/2,nx*sizeof(dual));
		memcpy(res->a+k*nx,a+nx/2,nx*sizeof(dual));
		if(have_y)
		{	y.Fill(k*dt);	ham = mgl_apde_calc_ham(&hIm, old, func, list, dd);	}
		mgl_operator_exp(nx,ham->a,a,f);
		mgl_operator_lin(nx,hIm.a,a,f,g,s,iexp);
		mgl_operator_lin(nx,hIm.a,s,f,g,s,iexp);
#pragma omp parallel for
		for(long i=0;i<2*nx;i++)
			a[i] = (a[i]-s[i]/mreal(8*nx*nx))*mreal(exp(-dmp[i]*dt)/2/nx);
		if(have_y)	delete ham;
	}
	delete []a;	delete []f;	delete []dmp;	delete []iexp;
	if(!have_y)	delete ham;
	gr->LoadState();	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_pde_adv(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0, const char *opt)
{
	HADT res = mgl_pde_adv_c(gr,ham,ini_re,ini_im,dz,k0,opt);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_pde_adv_c_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t res = uintptr_t(mgl_pde_adv_c(_GR_, s, _DA_(ini_re), _DA_(ini_im), *dz, *k0, o));
	delete []o;	delete []s;	return res;	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_pde_adv_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t res = uintptr_t(mgl_pde_adv(_GR_, s, _DA_(ini_re), _DA_(ini_im), *dz, *k0, o));
	delete []o;	delete []s;	return res;	}
//-----------------------------------------------------------------------------
//
//		Simplified PDE series
//
//-----------------------------------------------------------------------------
struct mgl_pde_ham
{
	dual *a,*hxy,*hxv,*huv,*huy;
	const char *eqs;
	long nx,ny;
	double xx,yy,xs,ys,dx,dy,dq,dp,zz;
	double dd;
};
void static mgl_pde_hprep(const mgl_pde_ham *f)
{
	const long nx = f->nx, ny = f->ny;
	mglDataV x(nx,ny), y(nx,ny), z, r(nx,ny);
	mglDataW p(nx,ny), q(nx,ny);
	x.Name(L"x");	y.Name(L"y");	p.Name(L"p");	q.Name(L"q");	r.Name(L"#$mgl");
	z.Name(L"z");	z.Fill(f->zz);
	dual dd(0,f->dd);
	mglData u(nx,ny);	u.Name(L"u");
#pragma omp parallel for
	for(long i=0;i<nx*ny;i++)	u.a[i] = abs(f->a[i]);
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);
	list.push_back(&p);	list.push_back(&q);	list.push_back(&u);

	x.Fill(f->xx,f->xx+f->dx*(nx-1),'x');	p.Freq(0,'x');
	y.Fill(f->yy,f->yy+f->dy*(ny-1),'y');	q.Freq(0,'y');
	HADT res = mglFormulaCalcC(f->eqs, list);
#pragma omp parallel for
	for(long i=0;i<nx*ny;i++)	f->hxy[i] = res->a[i]*dd;
	delete res;
	if(ny>2)
	{
		x.Fill(f->xs);	p.Freq(f->dp,'x');
		res = mglFormulaCalcC(f->eqs, list);
#pragma omp parallel for
		for(long i=0;i<nx*ny;i++)	f->huy[i] = res->a[i]*dd;
		delete res;
	}
	x.Fill(f->xs);	p.Freq(f->dp,'x');
	y.Fill(f->ys);	q.Freq(f->dq,'y');
	res = mglFormulaCalcC(f->eqs, list);
#pragma omp parallel for
	for(long i=0;i<nx*ny;i++)	f->huv[i] = res->a[i]*dd;
	delete res;
	if(ny>2)
	{
		x.Fill(f->xx,f->xx+f->dx*(nx-1),'x');	p.Freq(0,'x');
		res = mglFormulaCalcC(f->eqs, list);
#pragma omp parallel for
		for(long i=0;i<nx*ny;i++)	f->hxv[i] = res->a[i]*dd;
		delete res;
	}
}
//-----------------------------------------------------------------------------
// Solve equation dx/dz = func(p,q,x,y,z,|u|)[u] where p=d/dx, q=d/dy. At this moment simplified form of ham is supported: ham = f(p,q,z) + g(x,y,z,'u'), where variable 'u'=|u| (for allowing solve nonlinear problems). You may specify imaginary part like ham = p^2 + 1i*x*(x>0).
HADT MGL_EXPORT mgl_pde_solve_c(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0, const char *opt)
{
	mreal gamma = gr->SaveState(opt);	if(mgl_isnan(gamma))	gamma = GAMMA;
	mglPoint Min=gr->Min, Max=gr->Max;
	long nx=ini_re->GetNx(), ny=ini_re->GetNy(), nz = long((Max.z-Min.z)/dz)+1;
	if(nx<2 || nz<2 || Max.x==Min.x)			// Too small data
	{	gr->SetWarn(mglWarnLow,"PDE");	return 0;	}
	if(ini_im->GetNx()*ini_im->GetNy() != nx*ny)// Wrong dimensions
	{	gr->SetWarn(mglWarnDim,"PDE");	return 0;	}
	mglDataC *res=new mglDataC(nz, nx, ny);

	dual *a = new dual[4*nx*ny], hh0;	// Add "damping" area
	dual *hxy = new dual[4*nx*ny], *hxv = new dual[4*nx*ny];
	dual *huy = new dual[4*nx*ny], *huv = new dual[4*nx*ny];
	dual *hx = new dual[2*nx], *hv = new dual[2*ny];
	dual *hy = new dual[2*ny], *hu = new dual[2*nx];
	double *dmp = new double[4*nx*ny];
	memset(a,0,4*nx*ny*sizeof(dual));
	memset(dmp,0,4*nx*ny*sizeof(double));
#pragma omp parallel for collapse(2)
	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)	// Initial conditions
	{
		long i0 = i+nx/2+2*nx*(j+ny/2);
		a[i0] = dual(ini_re->v(i,j), ini_im->v(i,j));
		res->a[nz*(i+nx*j)] = a[i0];
	}
#pragma omp parallel for collapse(2)
	for(long j=0;j<2*ny;j++)	for(long i=0;i<2*nx;i++)	// step 1
	{
		long i0 = i+2*nx*j;
		if(i<nx/2)		dmp[i0] += gamma*mgl_ipow((nx/2-i)/(nx/2.),2);
		if(i>3*nx/2)	dmp[i0] += gamma*mgl_ipow((i-3*nx/2-1)/(nx/2.),2);
		if(j<ny/2)		dmp[i0] += gamma*mgl_ipow((ny/2-j)/(ny/2.),2);
		if(j>3*ny/2)	dmp[i0] += gamma*mgl_ipow((j-3*ny/2-1)/(ny/2.),2);
	}
	mreal dx = (Max.x-Min.x)/(nx-1), dy = ny>1?(Max.y-Min.y)/(ny-1):0;
	mreal dp = M_PI/(Max.x-Min.x)/k0, dq = M_PI/(Max.y-Min.y)/k0;
	mreal xs=(Min.x+Max.x)/2, ys=(Min.y+Max.y)/2;
	double dd = k0*dz;

	mgl_pde_ham tmp;tmp.eqs = ham;
	tmp.nx = 2*nx;	tmp.ny = 2*ny;	tmp.dd = dd;	tmp.a=a;
	tmp.hxy=hxy;	tmp.hxv=hxv;	tmp.huy=huy;	tmp.huv=huv;
	tmp.xx = Min.x-dx*(nx/2);	tmp.xs = xs;	tmp.dx = dx;	tmp.dp = dp;
	tmp.yy = Min.y-dy*(ny/2);	tmp.ys = ys;	tmp.dy = dy;	tmp.dq = dq;

	// prepare fft. NOTE: slow procedures due to unknown nx, ny.
	void *wtx = mgl_fft_alloc(2*nx,0,0);
	void *wty = mgl_fft_alloc(2*ny,0,0);
	for(long k=1;k<nz;k++)
	{
		if(gr->NeedStop())	break;
		tmp.zz = Min.z+dz*k;
		memset(hxy,0,4*nx*ny*sizeof(dual));	memset(hxv,0,4*nx*ny*sizeof(dual));
		memset(huv,0,4*nx*ny*sizeof(dual));	memset(huy,0,4*nx*ny*sizeof(dual));
		mgl_pde_hprep(&tmp);
		for(long i=0;i<2*nx;i++)	{	hx[i] = hxv[i];			hu[i] = huv[i];		}
		for(long j=0;j<2*ny;j++)	{	hy[j] = huy[2*nx*j];	hv[j] = huv[2*nx*j];}
		// rearrange arrays
		hh0=hu[0];
		if(ny>1)
#pragma omp parallel for collapse(2)
		 for(long j=0;j<2*ny;j++)	for(long i=0;i<2*nx;i++)
			{
				long i0 = i+2*nx*j;	huv[i0] -= hh0;
				hxv[i0] -= hx[i]+hv[j]-hh0;
				huy[i0] -= hu[i]+hy[j]-hh0;
			}
		else
#pragma omp parallel for
			for(long i=0;i<4*nx*ny;i++)	huv[i] -= hh0;
		// solve equation
		if(ny>1)
#pragma omp parallel
		{
			void *wsx = mgl_fft_alloc_thr(2*nx), *wsy = mgl_fft_alloc_thr(2*ny);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(hxy[i]-double(dmp[i]*dz));
#pragma omp for
			for(long i=0;i<2*ny;i++)	mgl_fft((double *)(a+i*2*nx), 1, 2*nx, wtx, wsx, false);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(huy[i]);
#pragma omp for
			for(long i=0;i<2*nx;i++)	mgl_fft((double *)(a+i), 2*nx, 2*ny, wty, wsy, false);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(huv[i]);
#pragma omp for
			for(long i=0;i<2*ny;i++)	mgl_fft((double *)(a+2*i*nx), 1, 2*nx, wtx, wsx, true);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(hxv[i]);
#pragma omp for
			for(long i=0;i<2*nx;i++)	mgl_fft((double *)(a+i), 2*nx, 2*ny, wty, wsy, true);
			mgl_fft_free_thr(wsx);	mgl_fft_free_thr(wsy);
		}
		else
#pragma omp parallel
		{
			void *wsx = mgl_fft_alloc_thr(2*nx);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(hxy[i]-double(dmp[i]*dz));
#pragma omp for
			for(long i=0;i<2*ny;i++)	mgl_fft((double *)(a+i*2*nx), 1, 2*nx, wtx, wsx, false);
#pragma omp for
			for(long i=0;i<4*nx*ny;i++)	a[i] *= exp(huv[i]);
#pragma omp for
			for(long i=0;i<2*ny;i++)	mgl_fft((double *)(a+2*i*nx), 1, 2*nx, wtx, wsx, true);
			mgl_fft_free_thr(wsx);
		}
#pragma omp parallel for collapse(2)
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)	// save result
			res->a[k+nz*(i+nx*j)] = a[i+nx/2+2*nx*(j+ny/2)];
	}
	mgl_fft_free(wtx,0,0);	mgl_fft_free(wty,0,0);
	delete []a;		delete []dmp;
	delete []hxy;	delete []hxv;	delete []huy;	delete []huv;
	delete []hx;	delete []hy;	delete []hu;	delete []hv;
	gr->LoadState();
	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_pde_solve(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0, const char *opt)
{
	HADT res = mgl_pde_solve_c(gr,ham,ini_re,ini_im,dz,k0,opt);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_pde_solve_c_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t res = uintptr_t(mgl_pde_solve_c(_GR_, s, _DA_(ini_re), _DA_(ini_im), *dz, *k0, o));
	delete []o;	delete []s;	return res;	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_pde_solve_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t res = uintptr_t(mgl_pde_solve(_GR_, s, _DA_(ini_re), _DA_(ini_im), *dz, *k0, o));
	delete []o;	delete []s;	return res;	}
//-----------------------------------------------------------------------------
//
//		ODE series
//
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_txt_func(const mreal *x, mreal *dx, void *par)
{
	mglEqTxT *p=(mglEqTxT *)par;
	mreal vars[MGL_VS];
	size_t n = p->str.size();
	for(size_t i=0;i<n;i++)
	{	char ch = p->var[i];	if(ch>='a' && ch<='z')	vars[ch-'a']=x[i];	}
#pragma omp parallel for
	for(long i=0;i<long(n);i++)
		dx[i] = mgl_expr_eval_v(p->eqR[i], vars);
}
HMDT MGL_EXPORT mgl_ode_solve_str(const char *func, const char *var, HCDT x0, mreal dt, mreal tmax)
{
	if(!var || !(*var) || !func)	return 0;
	mglEqTxT par;
	par.var=var;	par.FillReal(func);
	size_t n = par.str.size();
	mreal *xx = new mreal[n];
	for(size_t i=0;i<n;i++)	xx[i] = x0?x0->vthr(i):0;
	HMDT res = mgl_ode_solve_ex(mgl_txt_func,n,xx,dt,tmax,&par,NULL);
	delete []xx;	return res;
}
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_txt_funcC(const mreal *x, mreal *dx, void *par)
{
	mglEqTxT *p=(mglEqTxT *)par;
	mdual vars[MGL_VS];
	size_t n = p->str.size();
	for(size_t i=0;i<n;i++)
	{	char ch = p->var[i];	if(ch>='a' && ch<='z')	vars[ch-'a']=dual(x[2*i],x[2*i+1]);	}
#pragma omp parallel for
	for(long i=0;i<long(n);i++)
	{
		dual r = mgl_cexpr_eval_v(p->eqC[i], vars);
		dx[2*i] = real(r);	dx[2*i+1] = imag(r);
	}
}
HADT MGL_EXPORT mgl_ode_solve_str_c(const char *func, const char *var, HCDT x0, mreal dt, mreal tmax)
{
	if(!var || !(*var) || !func)	return 0;
	mglEqTxT par;	par.var=var;
	par.var=var;	par.FillCmplx(func);
	size_t n = par.str.size();
	mreal *xx = new mreal[2*n];
	const mglDataC *c = dynamic_cast<const mglDataC *>(x0);
	for(size_t i=0;i<n;i++)
	{
		if(c)	{	xx[2*i]=real(c->a[i]);	xx[2*i+1]=imag(c->a[i]);	}
		else	{	xx[2*i] = x0?x0->vthr(i):0;	xx[2*i+1]=0;	}
	}
	HMDT res = mgl_ode_solve_ex(mgl_txt_funcC,2*n,xx,dt,tmax,&par,NULL);
	delete []xx;
	const long nn=n, nt=res->ny;
	mglDataC *out = new mglDataC(nn, nt);
#pragma omp parallel for
	for(long i=0;i<nt*nn;i++)	out->a[i] = dual(res->a[2*i],res->a[2*i+1]);
	delete res;	return out;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_ode_solve(void (*func)(const mreal *x, mreal *dx, void *par), int n, const mreal *x0, mreal dt, mreal tmax, void *par)
{	return mgl_ode_solve_ex(func,n,x0,dt,tmax,par,0);	}
HMDT MGL_EXPORT mgl_ode_solve_ex(void (*func)(const mreal *x, mreal *dx, void *par), int n, const mreal *x0, mreal dt, mreal tmax, void *par, void (*bord)(mreal *x, const mreal *xp, void *par))
{
	if(tmax<dt)	return 0;	// nothing to do
	const long nt = int(tmax/dt+0.5)+1;
	mglData *res=new mglData(n,nt);
	mreal *x=new mreal[n], *k1=new mreal[n], *k2=new mreal[n], *k3=new mreal[n], *v=new mreal[n], hh=dt/2;
	// initial conditions
	for(long i=0;i<n;i++)	x[i] = res->a[i] = x0[i];
	// Runge Kutta scheme of 4th order
	bool good=true;
	long k;
	for(k=1;k<nt && good;k++)
	{
		func(x,k1,par);
		for(long i=0;i<n;i++)	v[i] = x[i]+k1[i]*hh;
		func(v,k2,par);
		for(long i=0;i<n;i++)	v[i] = x[i]+k2[i]*hh;
		func(v,k3,par);
		for(long i=0;i<n;i++)	{	v[i] = x[i]+k3[i]*dt;	k3[i] += k2[i];	}
		func(v,k2,par);
		for(long i=0;i<n;i++)	x[i] += (k1[i]+k2[i]+2*k3[i])*dt/6;
		if(bord)	bord(x,res->a+n*(k-1),par);
		for(long i=0;i<n;i++)
		{	res->a[i+n*k] = x[i];	if(mgl_isbad(x[i]))	good=false;	}
	}
	delete []x;	delete []k1;	delete []k2;	delete []k3;	delete []v;
	res->Crop(0,k,'y');
	return res;
}
//-----------------------------------------------------------------------------
//
//		Common functions for quasioptical calculations
//
//-----------------------------------------------------------------------------
void static mgl_ray3d(const mreal *in, mreal *out, void *par)
{
	mglFormula *eqs = (mglFormula *)par;
	const char *v="xyzpqvt";
	mreal var[MGL_VS];	memset(var,0,MGL_VS*sizeof(mreal));
	for(int i=0;i<7;i++)	var[v[i]-'a'] = in[i];
	out[0] = eqs->CalcD(var,'p');	out[3] = -eqs->CalcD(var,'x');
	out[1] = eqs->CalcD(var,'q');	out[4] = -eqs->CalcD(var,'y');
	out[2] = eqs->CalcD(var,'v');	out[5] = -eqs->CalcD(var,'z');
	out[7] = eqs->CalcD(var,'i');	out[6] = 1;
}
// Solve GO ray equation like dr/dt = d ham/dp, dp/dt = -d ham/dr where ham = ham(x,y,z,p,q,v,t) and px=p, py=q, pz=v. The starting point (at t=0) is r0, p0. Result is array of {x,y,z,p,q,v,t}
HMDT MGL_EXPORT mgl_ray_trace(const char *ham, mreal x0, mreal y0, mreal z0, mreal px, mreal py, mreal pz, mreal dt, mreal tmax)
{
	mglFormula eqs(ham);
	mreal in[8]={x0,y0,z0,px,py,pz,0,0};
	HMDT res = mgl_ode_solve(mgl_ray3d,8,in,dt,tmax,&eqs);
	mgl_data_set_id(res,"xyzpqvti");
	return res;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_ray_trace_(const char *ham, mreal *x0, mreal *y0, mreal *z0, mreal *px, mreal *py, mreal *pz, mreal *dt, mreal *tmax,int l)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_ray_trace(s, *x0,*y0,*z0, *px,*py,*pz, *dt,*tmax));
	delete []s;	return res;	}
//-----------------------------------------------------------------------------
struct mgl_ap
{
	double x0,y0,z0,x1,y1,z1,x2,y2,z2;	// vectors {l, g1, g2}
	double t1,t2,ch,q1,q2,pt,dt,d1,d2;	// theta_{1,2}, chi, q_{1,2}, p_t, dtau, dq_{1,2}
	mgl_ap()	{	memset(this,0,sizeof(mgl_ap));	}
};
//-----------------------------------------------------------------------------
void static mgl_init_ra(long n, int n7, const mreal *r, mgl_ap *ra)	// prepare some intermediate data for QO (3d case)
{
	double tt = hypot(r[n7]-r[0], r[n7+1]-r[1]);
	if(tt)
	{
		ra[0].x1 = (r[n7+1]-r[1])/tt;
		ra[0].y1 = (r[0]-r[n7])/tt;
		ra[0].z1 = 0;
	}
	else	{	ra[0].x1 = ra[0].y1 = 0;	ra[0].z1 = 1;	}
	ra[0].x0 = r[n7] - r[0];	ra[0].y0 = r[n7+1] - r[1];	ra[0].z0 = r[n7+2] - r[2];
	tt = sqrt(ra[0].x0*ra[0].x0 + ra[0].y0*ra[0].y0 + ra[0].z0*ra[0].z0);
	ra[0].x0 /= tt;	ra[0].y0 /= tt;	ra[0].z0 /= tt;
	ra[0].x2 = ra[0].y1*ra[0].z0 - ra[0].y0*ra[0].z1;	// vector g_2
	ra[0].y2 = ra[0].z1*ra[0].x0 - ra[0].z0*ra[0].x1;
	ra[0].z2 = ra[0].x1*ra[0].y0 - ra[0].x0*ra[0].y1;
	for(long i=1;i<n;i++)	// NOTE: no parallel due to dependence on prev point!
	{
		mgl_ap *ri=ra+i, *rp=ra+i-1;
		const mreal *rr = r+n7*i;
		ri->dt = rr[6] - rr[6-n7];
		ri->x0 = rr[0] - rr[-n7];	// NOTE: very rough formulas
		ri->y0 = rr[1] - rr[1-n7];	// for corresponding with dt one
		ri->z0 = rr[2] - rr[2-n7];	// for corresponding with dt one
		double ch = sqrt(ri->x0*ri->x0 + ri->y0*ri->y0 + ri->z0*ri->z0);
		ri->x0 /= ch;	ri->y0 /= ch;	ri->z0 /= ch;
		ri->ch = ch/ri->dt;
		ri->pt = rr[3]*ri->x0 + rr[4]*ri->y0 + rr[5]*ri->z0;
		ri->q1 = rr[3]*ri->x1 + rr[4]*ri->y1 + rr[5]*ri->z1;
		ri->q2 = rr[3]*ri->x2 + rr[4]*ri->y2 + rr[5]*ri->z2;
		// NOTE previous point is used here!
		tt = ri->x0*rp->x1 + ri->y0*rp->y1 + ri->z0*rp->z1;
		ri->x1 = rp->x1 - tt*ri->x0;	// vector g_1
		ri->y1 = rp->y1 - tt*ri->y0;
		ri->z1 = rp->z1 - tt*ri->z0;
		ri->t1 = tt/ch;
		tt = sqrt(ri->x1*ri->x1 + ri->y1*ri->y1 + ri->z1*ri->z1);
		ri->x1 /= tt;	ri->y1 /= tt;	ri->z1 /= tt;	// norm for reducing numeric error
		ri->x2 = ri->y1*ri->z0 - ri->y0*ri->z1;	// vector g_2
		ri->y2 = ri->z1*ri->x0 - ri->z0*ri->x1;
		ri->z2 = ri->x1*ri->y0 - ri->x0*ri->y1;
		tt = ri->x0*rp->x2 + ri->y0*rp->y2 + ri->z0*rp->z2;
		ri->t2 = tt/ch;
		ri->d1 = (ri->q1-rp->q1)/ch;
		ri->d2 = (ri->q2-rp->q2)/ch;
	}
	memcpy(ra,ra+1,sizeof(mgl_ap));	// setup zero point
	ra[0].pt = r[3]*ra[0].x0 + r[4]*ra[0].y0 + r[5]*ra[0].z0;
	ra[0].q1 = r[3]*ra[0].x1 + r[4]*ra[0].y1 + r[5]*ra[0].z1;
	ra[0].q2 = r[3]*ra[0].x2 + r[4]*ra[0].y2 + r[5]*ra[0].z2;
}
//-----------------------------------------------------------------------------
//
//		QO2d series
//
//-----------------------------------------------------------------------------
struct mgl_qo2d_ham
{
	dual *hx, *hu, *a, h0;
	double *dmp, dr, dk;
	mreal *r;
	mgl_ap *ra;
	mdual (*ham)(mreal u, mreal x, mreal y, mreal px, mreal py, void *par);
	void *par;
};
//-----------------------------------------------------------------------------
static void *mgl_qo2d_hprep(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	mgl_qo2d_ham *f = (mgl_qo2d_ham *)t->v;
	mgl_ap *ra = f->ra;

	const mreal *r = f->r;
	const long nx=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=t->id;i<nx;i+=mglNumThr)
	{
		// x terms
		mreal x1 = (2*i-nx+1)*f->dr, hh = 1 - ra->t1*x1;
		hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		mreal tt = (ra->pt + ra->d1*x1)/hh - ra->pt;
		dual tmp = f->ham(abs(f->a[i]), r[0]+ra->x1*x1, r[1]+ra->y1*x1, r[3]+ra->x0*tt, r[4]+ra->y0*tt, f->par);
		f->hx[i] = tmp - f->h0/2.;
		// u-y terms
		x1 = f->dk/2*(i<nx/2 ? i:i-nx);
		tmp = f->ham(0, r[0], r[1], r[3]+ra->x1*x1, r[4]+ra->y1*x1, f->par);
		f->hu[i] = tmp - f->h0/2.;

		if(imag(f->hx[i])>0)	f->hx[i] = f->hx[i].real();
		if(imag(f->hu[i])>0)	f->hu[i] = f->hu[i].real();
		// add boundary conditions for x-direction
		f->hx[i] -= dual(0,f->dmp[i]);
	}
	return 0;
}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_qo2d_func_c(mdual (*ham)(mreal u, mreal x, mreal y, mreal px, mreal py, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy)
{
	const mglData *ray=dynamic_cast<const mglData *>(ray_dat);	// NOTE: Ray must be mglData!
	if(!ray)	return 0;
	const long nx=ini_re->GetNx(), nt=ray->ny, n7=ray->nx;
	if(nx<2 || ini_im->GetNx()!=nx || nt<2)	return 0;
	mglDataC *res=new mglDataC(nx,nt,1);

	dual *a=new dual[2*nx], *hu=new dual[2*nx],  *hx=new dual[2*nx];
	double *dmp=new double[2*nx];
	mgl_ap *ra = new mgl_ap[nt];	mgl_init_ra(nt, n7, ray->a, ra);	// ray

	mreal dr = r/(nx-1), dk = M_PI*(nx-1)/(k0*r*nx);
	memset(dmp,0,2*nx*sizeof(double));
	for(long i=0;i<nx/2;i++)	// prepare damping
	{
		mreal x1 = (nx/2-i)/(nx/2.);
		dmp[2*nx-1-i] = dmp[i] = 30*GAMMA*x1*x1/k0;
	}
	for(long i=0;i<nx;i++)	a[i+nx/2] = dual(ini_re->v(i),ini_im->v(i));	// init
	void *wsx, *wtx = mgl_fft_alloc(2*nx,&wsx,1);
	if(xx && yy)	{	xx->Create(nx,nt);	yy->Create(nx,nt);	}

	mgl_qo2d_ham tmp;	// parameters for Hamiltonian calculation
	tmp.hx=hx;	tmp.hu=hu;	tmp.dmp=dmp;	tmp.par=par;
	tmp.dr=dr;	tmp.dk=dk;	tmp.ham=ham;	tmp.a=a;
	// start calculation
	for(long k=0;k<nt;k++)
	{
		for(long i=0;i<nx;i++)	// "save"
			res->a[i+k*nx]=a[i+nx/2]*sqrt(ra[0].ch/ra[k].ch);
		if(xx && yy)	for(long i=0;i<nx;i++)	// prepare xx, yy
		{
			mreal x1 = (2*i-nx+1)*dr;
			xx->a[i+k*nx] = ray->a[n7*k] + ra[k].x1*x1;	// new coordinates
			yy->a[i+k*nx] = ray->a[n7*k+1] + ra[k].y1*x1;
		}
		tmp.r=ray->a+n7*k;	tmp.ra=ra+k;
		mreal hh = ra[k].pt*(1/sqrt(sqrt(1.041))-1);	// 0.041=0.45^4 -- minimal value of h
		tmp.h0 = ham(0, tmp.r[0], tmp.r[1], tmp.r[3]+ra[k].x0*hh, tmp.r[4]+ra[k].x0*hh, par);
		mglStartThread(mgl_qo2d_hprep,0,2*nx,0,0,0,0,&tmp);
		// Step for field
		dual dt = dual(0, -ra[k].dt*k0);
		for(long i=0;i<2*nx;i++)	a[i] *= exp(hx[i]*dt);
		mgl_fft((double *)a, 1, 2*nx, wtx, wsx, false);
		for(long i=0;i<2*nx;i++)	a[i] *= exp(hu[i]*dt);
		mgl_fft((double *)a, 1, 2*nx, wtx, wsx, true);

/*		// Calculate B1			// TODO make more general scheme later!!!
		hh = ra[k].pt*(1/sqrt(sqrt(1.041))-1);
		var['x'-'a'] = ray->a[n7*k];	// new coordiantes
		var['y'-'a'] = ray->a[n7*k+1];
		var['p'-'a'] = ray->a[n7*k+3] + ra[k].x0*hh;	// new momentums
		var['q'-'a'] = ray->a[n7*k+4] + ra[k].y0*hh;
		tt = h.CalcD(var,'p')*ra[k].x1 + h.CalcD(var,'q')*ra[k].y1;
		var['x'-'a'] = ray->a[n7*k] + ra[k].x1*dr;	// new coordiantes
		var['y'-'a'] = ray->a[n7*k+1] + ra[k].y1*dr;
		hh = 1 - ra[k].t1*dr;	hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		hh = (ra[k].ch*ra[k].pt + ra[k].d1*dr)/(hh*ra[k].ch) - ra[k].pt;
		var['p'-'a'] = ray->a[n7*k+3] + ra[k].x0*hh;	// new momentums
		var['q'-'a'] = ray->a[n7*k+4] + ra[k].y0*hh;
		B1 = h.CalcD(var,'p')*ra[k].x1 + h.CalcD(var,'q')*ra[k].y1;
		B1 = (B1-tt)/dr;
		double a1=0, a2=0;
		for(i=0;i<2*nx;i++)	a1 += norm(a[i]);
		hx[0] = hx[2*nx-1] = 0.;
		for(i=1;i<2*nx-1;i++)	hx[i] = (B1*ra[k].dt*(i-nx))*(a[i+1]-a[i-1]);
		for(i=0;i<2*nx;i++)	{	a[i] += hx[i];	a2 += norm(a[i]);	}
		a1 = sqrt(a1/a2);
		for(i=0;i<2*nx;i++)	a[i] *= a1;*/
	}
	mgl_fft_free(wtx,&wsx,1);
	delete []a;		delete []hu;	delete []hx;	delete []ra;	delete []dmp;
	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_qo2d_func(mdual (*ham)(mreal u, mreal x, mreal y, mreal px, mreal py, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy)
{
	HADT res = mgl_qo2d_func_c(ham,par,ini_re,ini_im,ray_dat,r,k0,xx,yy);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
mdual static mgl_ham2d(mreal u, mreal x, mreal y, mreal px, mreal py, void *par)
{
	mglFormula *h = (mglFormula *)par;
	mreal var[MGL_VS];	memset(var,0,MGL_VS*sizeof(mreal));
	var['x'-'a'] = x;	var['y'-'a'] = y;	var['u'-'a'] = u;
	var['p'-'a'] = px;	var['q'-'a'] = py;
	return mdual(h->Calc(var), -h->CalcD(var,'i'));
}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_qo2d_solve_c(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy)
{
	mglFormula h(ham);
	return mgl_qo2d_func_c(mgl_ham2d, &h, ini_re, ini_im, ray_dat, r, k0, xx, yy);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_qo2d_solve(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy)
{
	HADT res = mgl_qo2d_solve_c(ham,ini_re,ini_im,ray_dat,r,k0,xx,yy);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_qo2d_solve_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, int l)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_qo2d_solve(s, _DA_(ini_re), _DA_(ini_im), _DA_(ray), *r, *k0, _DM_(xx), _DM_(yy)));
	delete []s;	return res;	}
//-----------------------------------------------------------------------------
//
//		QO3d series
//
//-----------------------------------------------------------------------------
struct mgl_qo3d_ham
{
	dual *hxy, *huv, *hxv, *huy, *a;
	dual *hx, *hy, *hu, *hv, h0;
	double *dmp, dr, dk;
	mreal *r;
	mgl_ap *ra;
	mdual (*ham)(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par);
	void *par;
};
//-----------------------------------------------------------------------------
static void *mgl_qo3d_hprep(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	mgl_qo3d_ham *f = (mgl_qo3d_ham *)t->v;
	mgl_ap *ra = f->ra;
	const mreal *r = f->r;
	const long nx=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long ii=t->id;ii<nx*nx;ii+=mglNumThr)
	{
		long i = ii%nx, j = ii/nx;
		// x-y terms
		mreal x1 = (2*i-nx+1)*f->dr, x2 = (2*j-nx+1)*f->dr, hh = 1-ra->t1*x1-ra->t2*x2;
		hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		mreal tt = (ra->pt + ra->d1*x1 + ra->d2*x2)/hh - ra->pt;
		f->hxy[ii] = f->ham(abs(f->a[i]), r[0]+ra->x1*x1+ra->x2*x2, r[1]+ra->y1*x1+ra->y2*x2, r[2]+ra->z1*x1+ra->z2*x2, r[3]+ra->x0*tt, r[4]+ra->y0*tt, r[5]+ra->z0*tt, f->par);
		// x-v terms
		x1 = (2*i-nx+1)*f->dr;	x2 = f->dk/2*(j<nx/2 ? j:j-nx);	hh = 1-ra->t1*x1;
		hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		tt = (ra->pt + ra->d1*x1)/hh - ra->pt;
		f->hxv[ii] = f->ham(0, r[0]+ra->x1*x1, r[1]+ra->y1*x1, r[2]+ra->z1*x1, r[3]+ra->x0*tt+ra->x2*x2, r[4]+ra->y0*tt+ra->y2*x2, r[5]+ra->z0*tt+ra->z2*x2, f->par);
		// u-y terms
		x1 = f->dk/2*(i<nx/2 ? i:i-nx);	x2 = (2*j-nx+1)*f->dr;	hh = 1-ra->t2*x2;
		hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		tt = (ra->pt + ra->d2*x2)/hh - ra->pt;
		f->huy[ii] = f->ham(0, r[0]+ra->x2*x2, r[1]+ra->y2*x2, r[2]+ra->z2*x2, r[3]+ra->x1*x1+ra->x0*tt, r[4]+ra->y1*x1+ra->y0*tt, r[5]+ra->z1*x1+ra->z0*tt, f->par);
		// u-y terms
		x1 = f->dk/2*(i<nx/2 ? i:i-nx);	x2 = f->dk/2*(j<nx/2 ? j:j-nx);
		f->huv[ii] = f->ham(0, r[0], r[1], r[2], r[3]+ra->x1*x1+ra->x2*x2, r[4]+ra->y1*x1+ra->y2*x2, r[5]+ra->z1*x1+ra->z2*x2, f->par);
	}
	return 0;
}
//-----------------------------------------------------------------------------
static void *mgl_qo3d_post(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	mgl_qo3d_ham *f = (mgl_qo3d_ham *)t->v;
	const long nx=t->n;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long ii=t->id;ii<nx*nx;ii+=mglNumThr)
	{
		long i = ii%nx, j = ii/nx;
		f->hxy[ii] -= (f->hx[i]+f->hy[j]-f->h0/2.)/2.;
		if(imag(f->hxy[ii])>0)	f->hxy[ii] = f->hxy[ii].real();
		f->hxv[ii] -= (f->hx[i]+f->hv[j]-f->h0/2.)/2.;
		if(imag(f->hxv[ii])>0)	f->hxv[ii] = f->hxv[ii].real();
		f->huy[ii] -= (f->hu[i]+f->hy[j]-f->h0/2.)/2.;
		if(imag(f->huy[ii])>0)	f->huy[ii] = f->huy[ii].real();
		f->huv[ii] -= (f->hu[i]+f->hv[j]-f->h0/2.)/2.;
		if(imag(f->huv[ii])>0)	f->huv[ii] = f->huv[ii].real();
		// add boundary conditions for x-direction
		f->hxy[ii] -= dual(0,f->dmp[ii]);
	}
	return 0;
}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_qo3d_func_c(mdual (*ham)(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz)
{
	const mglData *ray=dynamic_cast<const mglData *>(ray_dat);	// NOTE: Ray must be mglData!
	if(!ray)	return 0;
	const long nx=ini_re->GetNx(), nt=ray->ny, n7=ray->nx;	// NOTE: only square grids are supported now (for simplicity)
	if(nx<2 || ini_re->GetNx()!=nx || ini_im->GetNx()*ini_im->GetNy()!=nx*nx || nt<2)	return 0;
	mglDataC *res=new mglDataC(nx,nx,nt);

	dual *a=new dual[4*nx*nx], *huv=new dual[4*nx*nx],  *hxy=new dual[4*nx*nx], *huy=new dual[4*nx*nx],  *hxv=new dual[4*nx*nx];
	dual *hu=new dual[2*nx],  *hx=new dual[2*nx], *hy=new dual[2*nx],  *hv=new dual[2*nx];
	double *dmp=new double[4*nx*nx];
	mgl_ap *ra = new mgl_ap[nt];
	mgl_init_ra(nt, n7, ray->a, ra);	// prepare ray

	double dr = r/(nx-1), dk = M_PI*(nx-1)/(k0*r*nx);
	memset(dmp,0,4*nx*nx*sizeof(double));
#pragma omp parallel for collapse(2)
	for(long i=0;i<nx/2;i++)	for(long j=0;j<nx/2;j++)	// prepare damping
	{
		double x1 = (nx/2-i)/(nx/2.), x2 = (nx/2-j)/(nx/2.);
		dmp[2*nx-1-i] = dmp[i] = 30*GAMMA*x1*x1/k0;
		dmp[(2*nx-1-j)*2*nx] += 30*GAMMA*x2*x2/k0;
		dmp[j*2*nx] += 30*GAMMA*x2*x2/k0;
	}
#pragma omp parallel for collapse(2)
	for(long i=0;i<nx;i++)	for(long j=0;j<nx;j++)	// init
		a[i+nx/2+2*nx*(j+nx/2)] = dual(ini_re->v(i,j),ini_im->v(i,j));
	void *wtx = mgl_fft_alloc(2*nx,0,0);
	if(xx && yy && zz)	{	xx->Create(nx,nx,nt);	yy->Create(nx,nx,nt);	zz->Create(nx,nx,nt);	}

	mgl_qo3d_ham tmp;	// parameters for Hamiltonian calculation
	tmp.hxy=hxy;	tmp.hx=hx;	tmp.huv=huv;	tmp.hu=hu;
	tmp.huy=huy;	tmp.hy=hy;	tmp.hxv=hxv;	tmp.hv=hv;
	tmp.dmp=dmp;	tmp.par=par;
	tmp.dr=dr;	tmp.dk=dk;	tmp.ham=ham;	tmp.a=a;
	// start calculation
	for(long k=0;k<nt;k++)
	{
#pragma omp parallel for collapse(2)
		for(long i=0;i<nx;i++)	for(long j=0;j<nx;j++)	// "save"
			res->a[i+nx*(j+k*nx)]=a[i+nx/2+2*nx*(j+nx/2)]*sqrt(ra[0].ch/ra[k].ch);
		if(xx && yy && zz)
#pragma omp parallel for collapse(2)
			for(long i=0;i<nx;i++)	for(long j=0;j<nx;j++)	// prepare xx, yy, zz
			{
				mreal x1 = (2*i-nx+1)*dr, x2 = (2*j-nx+1)*dr;
				xx->a[i+nx*(j+k*nx)] = ray->a[n7*k] + ra[k].x1*x1 + ra[k].x2*x2;	// new coordinates
				yy->a[i+nx*(j+k*nx)] = ray->a[n7*k+1] + ra[k].y1*x1 + ra[k].y2*x2;
				zz->a[i+nx*(j+k*nx)] = ray->a[n7*k+2] + ra[k].z1*x1 + ra[k].z2*x2;
			}
		tmp.r=ray->a+n7*k;	tmp.ra=ra+k;
		mglStartThread(mgl_qo3d_hprep,0,2*nx,0,0,0,0,&tmp);	tmp.h0 = huv[0];
		for(long i=0;i<2*nx;i++)	// fill intermediate arrays
		{
			tmp.hx[i] = hxv[i];	tmp.hy[i] = huy[i*2*nx];
			tmp.hv[i] = huv[i];	tmp.hu[i] = huv[i*2*nx];
		}
		mglStartThread(mgl_qo3d_post,0,2*nx,0,0,0,0,&tmp);
		// Step for field
		dual dt = dual(0, -ra[k].dt*k0);
#pragma omp parallel
		{
			void *wsx = mgl_fft_alloc_thr(2*nx);
#pragma omp for
			for(long i=0;i<4*nx*nx;i++)	a[i] *= exp(hxy[i]*dt);		// x-y
#pragma omp for
			for(long i=0;i<2*nx;i++)	// x->u
				mgl_fft((double *)(a+i*2*nx), 1, 2*nx, wtx, wsx, false);
#pragma omp for
			for(long i=0;i<4*nx*nx;i++)	a[i] *= exp(huy[i]*dt);		// u-y
#pragma omp for
			for(long i=0;i<2*nx;i++)	// y->v
				mgl_fft((double *)(a+i), 2*nx, 2*nx, wtx, wsx, false);
#pragma omp for
			for(long i=0;i<4*nx*nx;i++)	a[i] *= exp(huv[i]*dt);		// u-v
#pragma omp for
			for(long i=0;i<2*nx;i++)	// u->x
				mgl_fft((double *)(a+i*2*nx), 1, 2*nx, wtx, wsx, true);
#pragma omp for
			for(long i=0;i<4*nx*nx;i++)	a[i] *= exp(hxv[i]*dt);		// x-v
#pragma omp for
			for(long i=0;i<2*nx;i++)	// v->y
				mgl_fft((double *)(a+i), 2*nx, 2*nx, wtx, wsx, true);
			mgl_fft_free_thr(wsx);
		}

/*		// Calculate B1			// TODO make more general scheme later!!!
		hh = ra[k].pt*(1/sqrt(sqrt(1.041))-1);
		var['x'-'a'] = ray->a[n7*k];	// new coordiantes
		var['y'-'a'] = ray->a[n7*k+1];
		var['p'-'a'] = ray->a[n7*k+3] + ra[k].x0*hh;	// new momentums
		var['q'-'a'] = ray->a[n7*k+4] + ra[k].y0*hh;
		tt = h.CalcD(var,'p')*ra[k].x1 + h.CalcD(var,'q')*ra[k].y1;
		var['x'-'a'] = ray->a[n7*k] + ra[k].x1*dr;	// new coordiantes
		var['y'-'a'] = ray->a[n7*k+1] + ra[k].y1*dr;
		hh = 1 - ra[k].t1*dr;	hh = sqrt(sqrt(0.041+hh*hh*hh*hh));
		hh = (ra[k].ch*ra[k].pt + ra[k].d1*dr)/(hh*ra[k].ch) - ra[k].pt;
		var['p'-'a'] = ray->a[n7*k+3] + ra[k].x0*hh;	// new momentums
		var['q'-'a'] = ray->a[n7*k+4] + ra[k].y0*hh;
		B1 = h.CalcD(var,'p')*ra[k].x1 + h.CalcD(var,'q')*ra[k].y1;
		B1 = (B1-tt)/dr;
		double a1=0, a2=0;
		for(i=0;i<2*nx;i++)	a1 += norm(a[i]);
		hx[0] = hx[2*nx-1] = 0.;
		for(i=1;i<2*nx-1;i++)	hx[i] = (B1*ra[k].dt*(i-nx))*(a[i+1]-a[i-1]);
		for(i=0;i<2*nx;i++)	{	a[i] += hx[i];	a2 += norm(a[i]);	}
		a1 = sqrt(a1/a2);
		for(i=0;i<2*nx;i++)	a[i] *= a1;*/
	}
	mgl_fft_free(wtx,0,0);
	delete []a;		delete []ra;	delete []dmp;
	delete []huv;	delete []hxy;	delete []hxv;	delete []huy;
	delete []hu;	delete []hx;	delete []hv;	delete []hy;
	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_qo3d_func(mdual (*ham)(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz)
{
	HADT res = mgl_qo3d_func_c(ham,par,ini_re,ini_im,ray_dat,r,k0,xx,yy,zz);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
mdual static mgl_ham3d(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par)
{
	mglFormula *h = (mglFormula *)par;
	mreal var[MGL_VS];	memset(var,0,MGL_VS*sizeof(mreal));
	var['x'-'a'] = x;	var['y'-'a'] = y;	var['z'-'a'] = z;	var['u'-'a'] = u;
	var['p'-'a'] = px;	var['q'-'a'] = py;	var['v'-'a'] = pz;
	return mdual(h->Calc(var), -h->CalcD(var,'i'));
}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_qo3d_solve_c(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz)
{
	mglFormula h(ham);
	return mgl_qo3d_func_c(mgl_ham3d, &h, ini_re, ini_im, ray_dat, r, k0, xx, yy, zz);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_qo3d_solve(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray_dat, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz)
{
	HADT res = mgl_qo3d_solve_c(ham,ini_re,ini_im,ray_dat,r,k0,xx,yy,zz);
	HMDT out = mgl_datac_abs(res);	delete res;	return out;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_qo3d_solve_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, uintptr_t* zz, int l)
{	char *s=new char[l+1];	memcpy(s,ham,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_qo3d_solve(s, _DA_(ini_re), _DA_(ini_im), _DA_(ray), *r, *k0, _DM_(xx), _DM_(yy), _DM_(zz)));
	delete []s;	return res;	}
//-----------------------------------------------------------------------------
//
//		mglJacobian series
//
//-----------------------------------------------------------------------------
static void *mgl_jacob2(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	const long nx=t->p[0], ny=t->p[1];
	mreal *r=t->a;
	const mreal *x=t->b, *y=t->c;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		long i=i0%nx, j=i0/nx;
		long ip = i<nx-1 ? 1:0, jp = j<ny-1 ? nx:0;
		long im = i>0 ? -1:0, jm = j>0 ? -nx:0;
		r[i0] = (x[i0+ip]-x[i0+im])*(y[i0+jp]-y[i0+jm]) -
				(y[i0+ip]-y[i0+im])*(x[i0+jp]-x[i0+jm]);
		r[i0] *= mreal((nx-1)*(ny-1)) / mreal((ip-im)*(jp-jm));
	}
	return 0;
}
HMDT MGL_EXPORT mgl_jacobian_2d(HCDT x, HCDT y)
{
	const long nx = x->GetNx(), ny=x->GetNy();
	if(nx!=y->GetNx() || ny!=y->GetNy() || nx<2 || ny<2)	return	0;
	mglData *r=new mglData(nx,ny,1);
	const mglData *xx=dynamic_cast<const mglData *>(x);
	const mglData *yy=dynamic_cast<const mglData *>(y);
	if(xx && yy)
	{
		long p[2]={nx,ny};
		mglStartThread(mgl_jacob2,0,nx*ny,r->a,xx->a,yy->a,p);
	}
	else	// slow variant
	{
#pragma omp parallel for collapse(2)
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long im = i>0 ? i-1:i, ip = i<nx-1 ? i+1:i;
			long jm = j>0 ? j-1:j, jp = j<ny-1 ? j+1:j;
			r->a[i+nx*j] = (x->v(ip,j)-x->v(im,j))*(y->v(i,jp)-y->v(i,jm)) -
						(y->v(ip,j)-y->v(im,j))*(x->v(i,jp)-x->v(i,jm));
			r->a[i+nx*j] *= mreal((nx-1)*(ny-1)) / mreal((ip-im)*(jp-jm));
		}
	}
	return r;
}
//-----------------------------------------------------------------------------
static void *mgl_jacob3(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	const long nx=t->p[0], ny=t->p[1], nz=t->p[2];
	mreal *r=t->a;
	const mreal *x=t->b, *y=t->c, *z=t->d;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		long i=i0%nx, j=(i0/nx)%ny, k=i0/(nx*ny);
		long ip = i<nx-1 ? 1:0, jp = j<ny-1 ? nx:0, kp = k<nz-1 ? nx*ny:0;
		long im = i>0 ? -1:0, jm = j>0 ? -nx:0, km = k>0 ? -nx*ny:0;
		r[i0] = (x[i0+ip]-x[i0+im])*(y[i0+jp]-y[i0+jm])*(z[i0+kp]-z[i0+km]) -
				(x[i0+ip]-x[i0+im])*(y[i0+kp]-y[i0+km])*(z[i0+jp]-z[i0+jm]) -
				(x[i0+jp]-x[i0+jm])*(y[i0+ip]-y[i0+im])*(z[i0+kp]-z[i0+km]) +
				(x[i0+jp]-x[i0+jm])*(y[i0+kp]-y[i0+km])*(z[i0+ip]-z[i0+im]) +
				(x[i0+kp]-x[i0+km])*(y[i0+ip]-y[i0+im])*(z[i0+jp]-z[i0+jm]) -
				(x[i0+kp]-x[i0+km])*(y[i0+jp]-y[i0+jm])*(z[i0+ip]-z[i0+im]);
		r[i0] *= mreal((nx-1)*(ny-1)*(nz-1)) / mreal((ip-im)*(jp-jm)*(kp-km));
	}
	return 0;
}
HMDT MGL_EXPORT mgl_jacobian_3d(HCDT x, HCDT y, HCDT z)
{
	const long nx = x->GetNx(), ny=x->GetNy(), nz=x->GetNz(), nn = nx*ny*nz;
	if(nx<2 || ny<2 || nz<2)	return 0;
	if(nn!=y->GetNN() || nn!=z->GetNN())	return 0;
	mglData *r=new mglData(nx,ny,nz);
	const mglData *xx=dynamic_cast<const mglData *>(x);
	const mglData *yy=dynamic_cast<const mglData *>(y);
	const mglData *zz=dynamic_cast<const mglData *>(z);
	if(xx && yy && zz)
	{
		long p[3]={nx,ny,nz};
		mglStartThread(mgl_jacob3,0,nx*ny*nz,r->a,xx->a,yy->a,p,0,zz->a);
	}
	else	// slow variant
	{
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long im = i>0 ? i-1:i, ip = i<nx-1 ? i+1:i;
			long jm = j>0 ? j-1:j, jp = j<ny-1 ? j+1:j;
			long km = k>0 ? k-1:k, kp = k<nz-1 ? k+1:k;
			long i0 = i+nx*(j+ny*k);
			r->a[i0] = (x->v(ip,j,k)-x->v(im,j,k))*(y->v(i,jp,k)-y->v(i,jm,k))*(z->v(i,j,kp)-z->v(i,j,km)) -
					(x->v(ip,j,k)-x->v(im,j,k))*(y->v(i,j,kp)-y->v(i,j,km))*(z->v(i,jp,k)-z->v(i,jm,k)) -
					(x->v(i,jp,k)-x->v(i,jm,k))*(y->v(ip,j,k)-y->v(im,j,k))*(z->v(i,j,kp)-z->v(i,j,km)) +
					(x->v(i,jp,k)-x->v(i,jm,k))*(y->v(i,j,kp)-y->v(i,j,km))*(z->v(ip,j,k)-z->v(im,j,k)) +
					(x->v(i,j,kp)-x->v(i,j,km))*(y->v(ip,j,k)-y->v(im,j,k))*(z->v(i,jp,k)-z->v(i,jm,k)) -
					(x->v(i,j,kp)-x->v(i,j,km))*(y->v(i,jp,k)-y->v(i,jm,k))*(z->v(ip,j,k)-z->v(im,j,k));
			r->a[i0] *= mreal((nx-1)*(ny-1)*(nz-1)) / mreal((ip-im)*(jp-jm)*(kp-km));
		}

	}
	return r;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_jacobian_2d_(uintptr_t* x, uintptr_t* y)
{	return uintptr_t(mgl_jacobian_2d(_DA_(x), _DA_(y)));	}
uintptr_t MGL_EXPORT mgl_jacobian_3d_(uintptr_t* x, uintptr_t* y, uintptr_t* z)
{	return uintptr_t(mgl_jacobian_3d(_DA_(x), _DA_(y), _DA_(z)));	}
//-----------------------------------------------------------------------------
//
//	Progonka
//
//-----------------------------------------------------------------------------
void static mgl_progonka_sr(HCDT A, HCDT B, HCDT C, HCDT D, mreal *dat, long n, long id, long i0, long di, bool difr)
{
	mreal *aa=dat, *bb=dat+n, *uu=dat+2*n;
	mreal b0=B->vthr(i0), c0=C->vthr(i0), d0=D->vthr(id);
	if(difr)	d0 = (2.-b0)*d0-c0*D->vthr(id+di);
	aa[0] = -c0/b0;	bb[0] = d0/b0;
	for(long i=1;i<n;i++)
	{
		long ii=i0+di*i, dd=id+di*i, tt = id+di*((i+1)%n);
		mreal a=A->vthr(ii), b=B->vthr(ii), c=C->vthr(ii);
		mreal d=difr?-a*D->vthr(dd-di)+(2.-b)*D->vthr(dd)-c*D->vthr(tt):D->vthr(dd);
		aa[i] = -c/(b+a*aa[i-1]);
		bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
	}
	uu[n-1] = bb[n-1];
	for(long i=n-2;i>=0;i--)	uu[i] = bb[i]+aa[i]*uu[i+1];
}
void static mgl_progonka_pr(HCDT A, HCDT B, HCDT C, HCDT D, mreal *dat, long n, long id, long i0, long di, bool difr)
{
	mreal *aa=dat, *bb=dat+n, *gg=dat+2*n, *uu=dat+3*n;
	mreal a0=A->vthr(i0), b0=B->vthr(i0), c0=C->vthr(i0), d0=D->vthr(id);
	if(difr)	d0 = -a0*D->vthr(id+di*(n-1))+(2.-b0)*d0-c0*D->vthr(id+di);
	aa[0] =-c0/b0;	bb[0] = d0/b0;	gg[0] =-a0/b0;
	for(long i=1;i<n;i++)
	{
		long ii=i0+di*i, il=id+di*((i+1)%n), dd=id+di*i;
		mreal a=A->vthr(ii), b=B->vthr(ii), c=C->vthr(ii);
		mreal d=difr?-a*D->vthr(dd-di)+(2.-b)*D->vthr(dd)-c*D->vthr(il):D->vthr(dd);
		aa[i] = -c/(b+a*aa[i-1]);
		bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		gg[i] = -a*gg[i-1]/(b+a*aa[i-1]);
	}
	mreal P=bb[n-1]/(1.-gg[n-1]), Q=aa[n-1]/(1.-gg[n-1]);
	aa[n-1] = Q;	bb[n-1] = P;
	for(long i=n-2;i>=0;i--)
	{
		bb[i] += aa[i]*bb[i+1]+gg[i]*P;
		aa[i] = aa[i]*aa[i+1]+gg[i]*Q;
	}
	mreal u0 = bb[0]/(1.-aa[0]);
	for(long i=0;i<n;i++)	uu[i]=bb[i]+aa[i]*u0;
}
void static mgl_progonka_hr(HCDT A, HCDT B, HCDT C, HCDT D, mreal *dat, long n, long id, long i0, bool difr)
{
	mreal *aa=dat, *bb=dat+n, *uu=dat+n*n;
	mreal b0=B->vthr(i0), c0=C->vthr(i0), d0=D->vthr(id);
	uu[0] = d0/b0*(difr?(2.-b0):1.);
	b0=B->vthr(i0+n*n-1);	d0=D->vthr(id+n*n-1);
	uu[n*n-1] = d0/b0*(difr?(2.-b0):1.);
	long di = n-1, i1 = i0+n*(n-1), d1 = id+n*(n-1);
	// suppose the square grid!
	for(long j=1;j<n;j++)
	{
		// first bottom-left triangle
		b0=B->vthr(i0+j);	c0=C->vthr(i0+j);	d0=D->vthr(id+j);
		if(difr)	d0 = (2.-b0)*d0-c0*D->vthr(id+j+di);
		aa[0] = -c0/b0;	bb[0] = d0/b0;
		for(long i=1;i<=j;i++)
		{
			long ii=i0+j+di*i, dd=id+j+di*i;
			mreal a=A->vthr(ii),b=B->vthr(ii),c=C->vthr(ii);
			mreal d=difr?-a*D->vthr(dd-di)+(2.-b)*D->vthr(dd)-c*D->vthr(dd+di):D->vthr(dd);
			aa[i] = -c/(b+a*aa[i-1]);
			bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		}
		uu[j+di*(j-1)] = bb[j];
		for(long i=j-1;i>=0;i--)
			uu[j+di*i] = bb[i]+aa[i]*uu[j+di*i+di];
		// next top-right triangle
		long j1=n-1-j;
		b0=B->vthr(i1+j1);	c0=C->vthr(i1+j1);	d0=D->vthr(d1+j1);
		if(difr)	d0 = (2.-b0)*d0-c0*D->vthr(d1+j1-di);
		aa[0] = -c0/b0;	bb[0] = d0/b0;
		for(long i=1;i<=j;i++)
		{
			long ii=i1+j1-di*i, dd=d1+j1-di*i;
			mreal a=A->vthr(ii),b=B->vthr(ii),c=C->vthr(ii);
			mreal d=difr?-a*D->vthr(dd+di)+(2.-b)*D->vthr(dd)-c*D->vthr(dd-di):D->vthr(dd);
			aa[i] = -c/(b+a*aa[i-1]);
			bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		}
		uu[j1+n*(n-1)-di*(j-1)] = bb[j];
		for(long i=j-1;i>=0;i--)
			uu[j1+n*(n-1)-di*i] = bb[i]+aa[i]*uu[j1+n*(n-1)-di*i-di];
	}
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_tridmat(HCDT A, HCDT B, HCDT C, HCDT D, const char *how)
{
	const long nx=D->GetNx(),ny=D->GetNy(),nz=D->GetNz();
	const long nn=nx*ny*nz, np=nx*ny, na=A->GetNN();
	if(B->GetNN()!=na || C->GetNN()!=na)	return 0;
	mglData *r = new mglData(nx,ny,nz);
	bool per = mglchr(how,'c');
	bool difr = mglchr(how,'d');
	if(mglchr(how,'x') && (na==nn || na==np || na==nx))
#pragma omp parallel
	{
		mglData T(nx,4);	mreal *uu=T.a+(per?3:2)*nx;
#pragma omp for collapse(2)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)
		{
			long i0=0, i1=nx*(j+ny*k);
			if(na==nn)	i0=nx*(j+ny*k);	else if(na==np)	i0=nx*j;
			if(per)	mgl_progonka_pr(A,B,C,D,T.a,nx,i1,i0,1,difr);
			else	mgl_progonka_sr(A,B,C,D,T.a,nx,i1,i0,1,difr);
			i0 = nx*(j+ny*k);
			for(long i=0;i<nx;i++)	r->a[i+i0] = uu[i];
		}
	}
	else if(mglchr(how,'y') && (na==nn || na==np || na==ny))
#pragma omp parallel
	{
		mglData T(ny,4);	mreal *uu=T.a+(per?3:2)*ny;
#pragma omp for collapse(2)
		for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)
		{
			long i0=0, i1 = i+np*k;
			if(na==nn)	i0=i+np*k;	else if(na==np)	i0=i;
			if(per)	mgl_progonka_pr(A,B,C,D,T.a,ny,i1,i0,nx,difr);
			else	mgl_progonka_sr(A,B,C,D,T.a,ny,i1,i0,nx,difr);
			i0 = i+np*k;
			for(long j=0;j<ny;j++)	r->a[j*nx+i0] = uu[j];
		}
	}
	else if(mglchr(how,'z') && (na==nn || na==nz))
#pragma omp parallel
	{
		mglData T(nz,4);	mreal *uu=T.a+(per?3:2)*nz;
#pragma omp for collapse(2)
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = na==nn?i+nx*j:0, i1 = i+nx*j;
			if(per)	mgl_progonka_pr(A,B,C,D,T.a,nz,i1,i0,np,difr);
			else	mgl_progonka_sr(A,B,C,D,T.a,nz,i1,i0,np,difr);
			i0 = i+nx*j;
			for(long k=0;k<nz;k++)	r->a[k*np+i0] = uu[k];
		}
	}
	else if(mglchr(how,'h') && ny==nx && (na==nn || na==np) && nx>1)
#pragma omp parallel
	{
		mglData T(np,2);
#pragma omp for
		for(long k=0;k<nz;k++)
		{
			mgl_progonka_hr(A,B,C,D,T.a,nx,k*np,na==nn ? k*np:0,difr);
			memcpy(r->a+k*np, T.a+np, np*sizeof(mreal));
		}
	}
	else	{	delete r;	r=0;	}
	return r;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_data_tridmat_(uintptr_t *A, uintptr_t *B, uintptr_t *C, uintptr_t *D, const char *how, int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	uintptr_t r = uintptr_t(mgl_data_tridmat(_DA_(A),_DA_(B),_DA_(C),_DA_(D),s));
	delete []s;	return r;
}
//-----------------------------------------------------------------------------
void static mgl_progonka_sc(HCDT A, HCDT B, HCDT C, HCDT D, dual *dat, long n, long id, long i0, long di, bool difr)
{
	dual *aa=dat, *bb=dat+n, *uu=dat+2*n;
	dual b0=B->vcthr(i0), c0=C->vcthr(i0), d0=D->vcthr(id);
	if(difr)	d0 = (mreal(2)-b0)*d0-c0*D->vcthr(id+di);
	aa[0] = -c0/b0;	bb[0] = d0/b0;
	for(long i=1;i<n;i++)
	{
		long ii=i0+di*i, dd=id+di*i, tt = id+di*((i+1)%n);
		dual a=A->vcthr(ii), b=B->vcthr(ii), c=C->vcthr(ii);
		dual d=difr?-a*D->vcthr(dd-di)+(mreal(2)-b)*D->vcthr(dd)-c*D->vcthr(tt):D->vcthr(dd);
		aa[i] = -c/(b+a*aa[i-1]);
		bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
	}
	uu[n-1] = bb[n-1];
	for(long i=n-2;i>=0;i--)	uu[i] = bb[i]+aa[i]*uu[i+1];
}
void static mgl_progonka_pc(HCDT A, HCDT B, HCDT C, HCDT D, dual *dat, long n, long id, long i0, long di, bool difr)
{
	dual *aa=dat, *bb=dat+n, *gg=dat+2*n, *uu=dat+3*n;
	dual a0=A->vcthr(i0), b0=B->vcthr(i0), c0=C->vcthr(i0), d0=D->vcthr(id);
	if(difr)	d0 = -a0*D->vcthr(id+di*(n-1))+(mreal(2)-b0)*d0-c0*D->vcthr(id+di);
	aa[0] =-c0/b0;	bb[0] = d0/b0;	gg[0] =-a0/b0;
	for(long i=1;i<n;i++)
	{
		long ii=i0+di*i, il=id+di*((i+1)%n), dd=id+di*i;
		dual a=A->vcthr(ii), b=B->vcthr(ii), c=C->vcthr(ii);
		dual d=difr?-a*D->vcthr(dd-di)+(mreal(2)-b)*D->vcthr(dd)-c*D->vcthr(il):D->vcthr(dd);
		aa[i] = -c/(b+a*aa[i-1]);
		bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		gg[i] = -a*gg[i-1]/(b+a*aa[i-1]);
	}
	dual P=bb[n-1]/(mreal(1)-gg[n-1]), Q=aa[n-1]/(mreal(1)-gg[n-1]);
	aa[n-1] = Q;	bb[n-1] = P;
	for(long i=n-2;i>=0;i--)
	{
		bb[i] += aa[i]*bb[i+1]+gg[i]*P;
		aa[i] = aa[i]*aa[i+1]+gg[i]*Q;
	}
	dual u0 = bb[0]/(mreal(1)-aa[0]);
	for(long i=0;i<n;i++)	uu[i]=bb[i]+aa[i]*u0;
}
void static mgl_progonka_hc(HCDT A, HCDT B, HCDT C, HCDT D, dual *dat, long n, long id, long i0, bool difr)
{
	dual *aa=dat, *bb=dat+n, *uu=dat+n*n;
	dual b0=B->vcthr(i0), c0=C->vcthr(i0), d0=D->vcthr(id);
	uu[0] = d0/b0*(difr?(mreal(2)-b0):mreal(1));
	b0=B->vcthr(i0+n*n-1);	d0=D->vcthr(id+n*n-1);
	uu[n*n-1] = d0/b0*(difr?(mreal(2)-b0):mreal(1));
	long di = n-1, i1 = i0+n*(n-1), d1 = id+n*(n-1);
	// suppose the square grid!
	for(long j=1;j<n;j++)
	{
		// first bottom-left triangle
		b0=B->vcthr(i0+j);	c0=C->vcthr(i0+j);	d0=D->vcthr(id+j);
		if(difr)	d0 = (mreal(2)-b0)*d0-c0*D->vcthr(id+j+di);
		aa[0] = -c0/b0;	bb[0] = d0/b0;
		for(long i=1;i<=j;i++)
		{
			long ii=i0+j+di*i, dd=id+j+di*i;
			dual a=A->vcthr(ii),b=B->vcthr(ii),c=C->vcthr(ii);
			dual d=difr?-a*D->vcthr(dd-di)+(mreal(2)-b)*D->vcthr(dd)-c*D->vcthr(dd+di):D->vcthr(dd);
			aa[i] = -c/(b+a*aa[i-1]);
			bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		}
		uu[j+di*(j-1)] = bb[j];
		for(long i=j-1;i>=0;i--)
			uu[j+di*i] = bb[i]+aa[i]*uu[j+di*i+di];
		// next top-right triangle
		long j1=n-1-j;
		b0=B->vcthr(i1+j1);	c0=C->vcthr(i1+j1);	d0=D->vcthr(d1+j1);
		if(difr)	d0 = (mreal(2)-b0)*d0-c0*D->vcthr(d1+j1-di);
		aa[0] = -c0/b0;	bb[0] = d0/b0;
		for(long i=1;i<=j;i++)
		{
			long ii=i1+j1-di*i, dd=d1+j1-di*i;
			dual a=A->vcthr(ii),b=B->vcthr(ii),c=C->vcthr(ii);
			dual d=difr?-a*D->vcthr(dd+di)+(mreal(2)-b)*D->vcthr(dd)-c*D->vcthr(dd-di):D->vcthr(dd);
			aa[i] = -c/(b+a*aa[i-1]);
			bb[i] = (d-a*bb[i-1])/(b+a*aa[i-1]);
		}
		uu[j1+n*(n-1)-di*(j-1)] = bb[j];
		for(long i=j-1;i>=0;i--)
			uu[j1+n*(n-1)-di*i] = bb[i]+aa[i]*uu[j1+n*(n-1)-di*i-di];
	}
}
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_datac_tridmat(HCDT A, HCDT B, HCDT C, HCDT D, const char *how)
{
	const long nx=D->GetNx(),ny=D->GetNy(),nz=D->GetNz();
	const long nn=nx*ny*nz, np=nx*ny, na=A->GetNN();
	if(B->GetNN()!=na || C->GetNN()!=na)	return 0;
	mglDataC *r = new mglDataC(nx,ny,nz);
	bool per = mglchr(how,'c');
	bool difr = mglchr(how,'d');
	if(mglchr(how,'x') && (na==nn || na==np || na==nx))
#pragma omp parallel
	{
		mglDataC T(nx,4);	dual *uu=T.a+(per?3:2)*nx;
#pragma omp for collapse(2)
		for(long k=0;k<nz;k++)	for(long j=0;j<ny;j++)
		{
			long i0=0, i1=nx*(j+ny*k);
			if(na==nn)	i0=i1;	else if(na==np)	i0=nx*j;
			if(per)	mgl_progonka_pc(A,B,C,D,T.a,nx,i1,i0,1,difr);
			else	mgl_progonka_sc(A,B,C,D,T.a,nx,i1,i0,1,difr);
			for(long i=0;i<nx;i++)	r->a[i+i1] = uu[i];
		}
	}
	else if(mglchr(how,'y') && (na==nn || na==np || na==ny))
#pragma omp parallel
	{
		mglDataC T(ny,4);	dual *uu=T.a+(per?3:2)*ny;
#pragma omp for collapse(2)
		for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)
		{
			long i0=0, i1 = i+np*k;
			if(na==nn)	i0=i1;	else if(na==np)	i0=i;
			if(per)	mgl_progonka_pc(A,B,C,D,T.a,ny,i1,i0,nx,difr);
			else	mgl_progonka_sc(A,B,C,D,T.a,ny,i1,i0,nx,difr);
			i0 = i+np*k;
			for(long j=0;j<ny;j++)	r->a[j*nx+i0] = uu[j];
		}
	}
	else if(mglchr(how,'z') && (na==nn || na==nz))
#pragma omp parallel
	{
		mglDataC T(nz,4);	dual *uu=T.a+(per?3:2)*nz;
#pragma omp for collapse(2)
		for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
		{
			long i0 = na==nn?i+nx*j:0, i1 = i+nx*j;
			if(per)	mgl_progonka_pc(A,B,C,D,T.a,nz,i1,i0,np,difr);
			else	mgl_progonka_sc(A,B,C,D,T.a,nz,i1,i0,np,difr);
			for(long k=0;k<nz;k++)	r->a[k*np+i1] = uu[k];
		}
	}
	else if(mglchr(how,'h') && ny==nx && (na==nn || na==np) && nx>1)
#pragma omp parallel
	{
		mglDataC T(np,2);
#pragma omp for
		for(long k=0;k<nz;k++)
		{
			mgl_progonka_hc(A,B,C,D,T.a,nx,k*np, na==nn ? k*np:0,difr);
			memcpy(r->a+k*np, T.a+np, np*sizeof(dual));
		}
	}
	else	{	delete r;	r=0;	}
	return r;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_datac_tridmat_(uintptr_t *A, uintptr_t *B, uintptr_t *C, uintptr_t *D, const char *how, int l)
{	char *s=new char[l+1];	memcpy(s,how,l);	s[l]=0;
	uintptr_t r = uintptr_t(mgl_datac_tridmat(_DA_(A),_DA_(B),_DA_(C),_DA_(D),s));
	delete []s;	return r;
}
//-----------------------------------------------------------------------------
