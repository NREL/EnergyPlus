/***************************************************************************
 * fit.cpp is part of Math Graphic Library
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
#include "mgl2/fit.h"
#include "mgl2/prim.h"
#include "mgl2/eval.h"
#include "mgl2/data.h"
#include "mgl2/base.h"

#if MGL_HAVE_GSL
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_blas.h>
#endif
HMDT MGL_NO_EXPORT mglFormulaCalc(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
int mglFitPnts=100;		///< Number of output points in fitting
char mglFitRes[1024];	///< Last fitted formula
mreal mglFitChi=NAN;	///< Chi value for last fitted formula
mglData mglFitCovar;	///< Covar matrix for lat fitted formula
//-----------------------------------------------------------------------------
mreal MGL_EXPORT_PURE mgl_get_fit_chi()		{	return mglFitChi;	}
mreal MGL_EXPORT_PURE mgl_get_fit_chi_()	{	return mglFitChi;	}
//-----------------------------------------------------------------------------
HCDT MGL_EXPORT_CONST mgl_get_fit_covar()	{	return &mglFitCovar;	}
uintptr_t MGL_EXPORT_CONST mgl_get_fit_covar_()	{	return (uintptr_t)&mglFitCovar;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_puts_fit(HMGL gr, double x, double y, double z, const char *pre, const char *font, double size)
{
	long n = strlen(mglFitRes)+(pre?strlen(pre):0)+1;
	char *buf = new char[n];
	if(pre)	snprintf(buf,n,"%s%s",pre,mglFitRes);
	else	mgl_strncpy(buf,mglFitRes,n);
	buf[n-1]=0;	mgl_puts(gr,x,y,z,buf,font,size);
	delete []buf;
}
void MGL_EXPORT mgl_puts_fit_(uintptr_t* gr, mreal *x, mreal *y, mreal *z, const char *prefix, const char *font, mreal *size, int l, int n)
{
	char *s=new char[l+1];	memcpy(s,prefix,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,font,n);	d[n]=0;
	mgl_puts_fit(_GR_, *x,*y,*z, s, d, *size);
	delete []s;		delete []d;
}
//-----------------------------------------------------------------------------
/// Structure for keeping data and precompiled fitted formula
struct mglFitData
{
	long n;				///< number of points
	mglDataA *x,*y,*z;	///< x, y, z values
	mreal *a;			///< function values
	mreal *s;			///< value dispersions (sigma)
	const char *eq;		///< approximation formula
	int m;				///< number of variables
	const char *var;	///< variables for fitting
};
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
int	mgl_fit__f (const gsl_vector *x, void *data, gsl_vector *f)
{
	mglFitData *fd = (mglFitData *)data;
	mglDataV *var = new mglDataV[fd->m];
	std::vector<mglDataA*> list;
	for(long i=0;i<fd->m;i++)
	{	var[i].s = fd->var[i];	var[i].Fill(gsl_vector_get(x,i));	list.push_back(var+i);	}
	if(fd->x)	list.push_back(fd->x);
	if(fd->y)	list.push_back(fd->y);
	if(fd->z)	list.push_back(fd->z);
	HMDT res = mglFormulaCalc(fd->eq, list);
#pragma omp parallel for
	for(long i=0;i<fd->n;i++)
	{
		mreal aa = fd->a[i], ss = fd->s[i];
		if(mgl_isnum(aa) && ss==ss && ss!=0)
			gsl_vector_set (f, i, (res->a[i] - aa)/ss);
		else	gsl_vector_set (f, i, 0);
	}
	delete []var;	mgl_delete_data(res);
	return GSL_SUCCESS;
}
//-----------------------------------------------------------------------------
int static mgl_fit__df (const gsl_vector * x, void *data, gsl_matrix * J)
{
	mglFitData *fd = (mglFitData *)data;
	mglDataV *var = new mglDataV[fd->m];
	std::vector<mglDataA*> list;
	for(long i=0;i<fd->m;i++)
	{	var[i].s = fd->var[i];	var[i].Fill(gsl_vector_get(x,i));	list.push_back(var+i);	}
	if(fd->x)	list.push_back(fd->x);
	if(fd->y)	list.push_back(fd->y);
	if(fd->z)	list.push_back(fd->z);
	HMDT res = mglFormulaCalc(fd->eq, list);
	const mreal eps = 1e-5;
	for(long j=0;j<fd->m;j++)
	{
		var[j].Fill(gsl_vector_get(x,j)+eps);
		HMDT dif = mglFormulaCalc(fd->eq, list);
		var[j].Fill(gsl_vector_get(x,j));
#pragma omp parallel for
		for(long i=0;i<fd->n;i++)
		{
			mreal aa = fd->a[i], ss = fd->s[i];
			if(mgl_isnum(aa) && ss==ss && ss!=0)
				gsl_matrix_set (J, i, j, (dif->a[i]-res->a[i])/(eps*ss));
			else	gsl_matrix_set (J, i, j, 0);
		}
		mgl_delete_data(dif);
	}
	delete []var;	mgl_delete_data(res);
	return GSL_SUCCESS;
}
//-----------------------------------------------------------------------------
int static mgl_fit__fdf (const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J)
{
	mglFitData *fd = (mglFitData *)data;
	mglDataV *var = new mglDataV[fd->m];
	std::vector<mglDataA*> list;
	for(long i=0;i<fd->m;i++)
	{	var[i].s = fd->var[i];	var[i].Fill(gsl_vector_get(x,i));	list.push_back(var+i);	}
	if(fd->x)	list.push_back(fd->x);
	if(fd->y)	list.push_back(fd->y);
	if(fd->z)	list.push_back(fd->z);
	HMDT res = mglFormulaCalc(fd->eq, list);
#pragma omp parallel for
	for(long i=0;i<fd->n;i++)
	{
		mreal aa = fd->a[i], ss = fd->s[i];
		if(mgl_isnum(aa) && ss==ss && ss!=0)
			gsl_vector_set (f, i, (res->a[i] - aa)/ss);
		else	gsl_vector_set (f, i, 0);
	}
	const mreal eps = 1e-5;
	for(long j=0;j<fd->m;j++)
	{
		var[j].Fill(gsl_vector_get(x,j)+eps);
		HMDT dif = mglFormulaCalc(fd->eq, list);
		var[j].Fill(gsl_vector_get(x,j));
#pragma omp parallel for
		for(long i=0;i<fd->n;i++)
		{
			mreal aa = fd->a[i], ss = fd->s[i];
			if(mgl_isnum(aa) && ss==ss && ss!=0)
				gsl_matrix_set (J, i, j, (dif->a[i]-res->a[i])/(eps*ss));
			else	gsl_matrix_set (J, i, j, 0);
		}
		mgl_delete_data(dif);
	}
	delete []var;	mgl_delete_data(res);
	return GSL_SUCCESS;
}
#endif
//-----------------------------------------------------------------------------
/// GSL based fitting procedure for formula/arguments specified by string
mreal static mgl_fit_base(mglFitData &fd, mreal *ini)
{
#if MGL_HAVE_GSL
	long m=fd.m,n=fd.n,iter=0;
	if(n<1 || ini==0)	return -1;
	// setup data
	double *x_init = new double[fd.m];
	for(long i=0;i<m;i++)	x_init[i] = ini[i];
	// setup fitting
	gsl_vector_view vx = gsl_vector_view_array(x_init, m);
	const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
	gsl_multifit_fdfsolver *s = gsl_multifit_fdfsolver_alloc(T, n, m);
	gsl_multifit_function_fdf f;
	f.f = mgl_fit__f;		f.df = mgl_fit__df;
	f.fdf = mgl_fit__fdf;	f.n = n;	f.p = m;
	f.params = &fd;
	gsl_multifit_fdfsolver_set(s, &f, &vx.vector);
	int status;	// start fitting
	do
	{
		iter++;
		status = gsl_multifit_fdfsolver_iterate(s);
		if ( status )	break;
		status = gsl_multifit_test_delta (s->dx, s->x, 1e-4, 1e-4 );
	}
	while ( status == GSL_CONTINUE && iter < 500 );

	gsl_matrix *covar = gsl_matrix_alloc(m, m);
#ifdef MGL_HAVE_GSL2
	gsl_matrix *J = gsl_matrix_alloc(s->fdf->n, s->fdf->p);
	gsl_multifit_fdfsolver_jac(s, J);
	gsl_multifit_covar (J, 0.0, covar);
	gsl_matrix_free (J);
#else
	gsl_multifit_covar(s->J, 0.0, covar);
#endif
	mglFitCovar.Set(covar);
	gsl_matrix_free(covar);

	mreal res = gsl_blas_dnrm2(s->f);
	for(long i=0;i<m;i++)	ini[i] = gsl_vector_get(s->x, i);
	// free memory
	gsl_multifit_fdfsolver_free(s);
	delete []x_init;
	return res;
#else
	return 0.0;
#endif
}
//-----------------------------------------------------------------------------
void mglPrepareFitEq(mglBase *gr,mreal chi, const char *eq, const char *var, mreal *par)
{
	char buf[32]="";
	mglFitChi = chi;
	snprintf(mglFitRes,1024,"chi=%g",chi);	mglFitRes[1023]=0;
	size_t i,k,len=strlen(var);
	for(i=0;i<len;i++)
	{
		snprintf(buf,32,", %c=%g",var[i],par[i]);
		buf[31]=0;	strcat(mglFitRes,buf);
	}
	gr->SetWarn(-1,mglFitRes);

	memset(mglFitRes, 0, 1024);	//mglFitRes[0] = 0;
	len=strlen(eq);
	for(i=k=0;i<len;i++)
	{
		const char *c = strchr(var,eq[i]);
		if(c && (i==0 || !isalnum(eq[i-1])) && (i==len-1 || !isalnum(eq[i+1])))
		{
			snprintf(buf,32,"%g",par[c-var]);
			buf[31]=0;	strcat(mglFitRes+k, buf);	k+=strlen(buf);
		}
		else	{	mglFitRes[k] = eq[i];	k++;	}
	}
	mglFitRes[k]=0;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_1(HMGL gr, HCDT y, const char *eq, const char *var, HMDT ini, const char *opt)
{
	gr->SaveState(opt);
	mglData x(y->GetNx());	x.Fill(gr->Min.x, gr->Max.x);
	mglData s(y);		s.Fill(1,1);
	return mgl_fit_xys(gr,&x,y,&s,eq,var,ini,0);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_2(HMGL gr, HCDT z, const char *eq, const char *var, HMDT ini, const char *opt)
{
	gr->SaveState(opt);
	mglData x(z->GetNx());	x.Fill(gr->Min.x, gr->Max.x);
	mglData y(z->GetNy());	y.Fill(gr->Min.y, gr->Max.y);
	mglData s(z);		s.Fill(1,1);
	return mgl_fit_xyzs(gr,&x,&y,z,&s,eq,var,ini,0);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_3(HMGL gr, HCDT a, const char *eq, const char *var, HMDT ini, const char *opt)
{
	gr->SaveState(opt);
	mglData x(a->GetNx());	x.Fill(gr->Min.x, gr->Max.x);
	mglData y(a->GetNy());	y.Fill(gr->Min.y, gr->Max.y);
	mglData z(a->GetNz());	z.Fill(gr->Min.z, gr->Max.z);
	mglData s(a);		s.Fill(1,1);
	return mgl_fit_xyzas(gr,&x,&y,&z,a,&s,eq,var,ini,0);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xy(HMGL gr, HCDT x, HCDT y, const char *eq, const char *var, HMDT ini, const char *opt)
{
	mglData s(y);	s.Fill(1,1);
	return mgl_fit_xys(gr,x,y,&s,eq,var,ini,opt);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *eq, const char *var, HMDT ini, const char *opt)
{
	mglData s(z);	s.Fill(1,1);
	return mgl_fit_xyzs(gr,x,y,z,&s,eq,var,ini,opt);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xyza(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *eq, const char *var, HMDT ini, const char *opt)
{
	mglData s(a);	s.Fill(1,1);
	return mgl_fit_xyzas(gr,x,y,z,a,&s,eq,var,ini,opt);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_ys(HMGL gr, HCDT y, HCDT s, const char *eq, const char *var, HMDT ini, const char *opt)
{
	gr->SaveState(opt);
	mglData x(y->GetNx());	x.Fill(gr->Min.x, gr->Max.x);
	return mgl_fit_xys(gr,&x,y,s,eq,var,ini,0);
}
//-----------------------------------------------------------------------------
void static mgl_fill_fit(HMGL gr, mglData &fit, mglData &in, mglFitData &fd, const char *var, long nx, long ny, long nz, long k)
{
	mglDataV *vv = new mglDataV[fd.m];
	std::vector<mglDataA*> list;
	for(long i=0;i<fd.m;i++)
	{	vv[i].s = var[i];	vv[i].Fill(in.a[i]);	list.push_back(vv+i);	}
	mglDataV x(nx,ny,nz, gr->Min.x,gr->Max.x,'x');	x.Name(L"x");	list.push_back(&x);
	mglDataV y(nx,ny,nz, gr->Min.y,gr->Max.y,'y');	y.Name(L"y");	list.push_back(&y);
	mglDataV z(nx,ny,nz, gr->Min.z,gr->Max.z,'z');	z.Name(L"z");	list.push_back(&z);
	HMDT res = mglFormulaCalc(fd.eq, list);
	long nn = nx*ny*nz;
	memcpy(fit.a+k*nn,res->a,nn*sizeof(mreal));
	delete []vv;	mgl_delete_data(res);
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xys(HMGL gr, HCDT xx, HCDT yy, HCDT ss, const char *eq, const char *var, HMDT ini, const char *opt)
{
	long m = yy->GetNx();
	mreal rr = gr->SaveState(opt);
	long nn = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	if(xx->GetNx()!=m)
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(m<2)
	{	gr->SetWarn(mglWarnLow,"Fit[S]");	return 0;	}
	if(ss->GetNN() != yy->GetNN())
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(!var || *var==0)
	{	gr->SetWarn(mglWarnNull,"Fit[S]");	return 0;	}

	mglData x(xx), y(yy), s(ss);	x.Name(L"x");
	long mm = yy->GetNy()*yy->GetNz();
#pragma omp parallel for
	for(long i=0;i<m;i++)	if(mgl_isnan(x.a[i]))
		for(long j=0;j<mm;j++)	y.a[i+m*j] = NAN;
	mglFitData fd;
	fd.n = m;	fd.x = &x;		fd.y = 0;
	fd.z = 0;	fd.a = y.a;		fd.s = s.a;
	fd.eq = eq;	fd.var = var;	fd.m = strlen(var);
	mglData in(fd.m), *fit=new mglData(nn, yy->GetNy(), yy->GetNz());
	mreal res=-1;
	mglDataR xc(x);
	for(long i=0;i<yy->GetNy()*yy->GetNz();i++)
	{
		if(ini && ini->nx>=fd.m)	in.Set(ini->a,fd.m);
		else in.Fill(0.,0);
		xc.SetInd(i%x.ny, L"x");
		fd.a = y.a+i*m;		fd.x = &xc;	//x.a+(i%x.ny)*m;
		fd.s = s.a+i*m;
		res = mgl_fit_base(fd,in.a);
		mgl_fill_fit(gr,*fit,in,fd,var,nn,1,1,i);
		if(ini && ini->nx>=fd.m)	memcpy(ini->a,in.a,fd.m*sizeof(mreal));
	}
	mglPrepareFitEq(gr,res,eq,var,in.a);
	gr->LoadState();	return fit;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xyzs(HMGL gr, HCDT xx, HCDT yy, HCDT zz, HCDT ss, const char *eq, const char *var, HMDT ini, const char *opt)
{
	long m=zz->GetNx(),n=zz->GetNy();
	mreal rr = gr->SaveState(opt);
	long nn = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	if(xx->GetNx()!=m)
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(ss->GetNN() != zz->GetNN())
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(yy->GetNx()!=n && (xx->GetNy()!=n || yy->GetNx()!=m || yy->GetNy()!=n))
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(m<2|| n<2)
	{	gr->SetWarn(mglWarnLow,"Fit[S]");	return 0;	}
	if(!var || *var==0)
	{	gr->SetWarn(mglWarnNull,"Fit[S]");	return 0;	}

	mglData x(m, n), y(m, n), z(zz), s(ss);	x.Name(L"x");	y.Name(L"y");
	long nz = zz->GetNz(), mm = n*m;
#pragma omp parallel for collapse(2)
	for(long j=0;j<n;j++)	for(long i=0;i<m;i++)
	{
		long i0 = i+m*j;
		x.a[i0] = GetX(xx,i,j,0).x;
		y.a[i0] = GetY(yy,i,j,0).x;
		if(mgl_isnan(x.a[i0]) || mgl_isnan(y.a[i0]))
			for(long k=0;k<nz;k++)	z.a[i0+mm*k] = NAN;
	}
	mglFitData fd;
	fd.n = m*n;	fd.x = &x;	fd.y = &y;
	fd.z = 0;	fd.a = z.a;	fd.s = s.a;
	fd.eq = eq;	fd.var=var;	fd.m = strlen(var);

	mglData in(fd.m), *fit=new mglData(nn, nn, zz->GetNz());
	mreal res = -1;
	for(long i=0;i<nz;i++)
	{
		if(ini && ini->nx>=fd.m)	in.Set(ini->a,fd.m);
		else in.Fill(0.,0);
		fd.a = z.a+i*m*n;		fd.s = s.a+i*m*n;
		res = mgl_fit_base(fd,in.a);
		mgl_fill_fit(gr,*fit,in,fd,var,nn,nn,1,i);
		if(ini && ini->nx>=fd.m)	memcpy(ini->a,in.a,fd.m*sizeof(mreal));
	}
	mglPrepareFitEq(gr,res, eq,var,in.a);
	gr->LoadState();	return fit;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_fit_xyzas(HMGL gr, HCDT xx, HCDT yy, HCDT zz, HCDT aa, HCDT ss, const char *eq, const char *var, HMDT ini, const char *opt)
{
	long m=aa->GetNx(), n=aa->GetNy(), l=aa->GetNz(), i = n*m*l;
	mreal rr = gr->SaveState(opt);
	long nn = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	if(m<2 || n<2 || l<2)
	{	gr->SetWarn(mglWarnLow,"Fit[S]");	return 0;	}
	if(ss->GetNN() != i)
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	bool both = xx->GetNN()==i && yy->GetNN()==i && zz->GetNN()==i;
	if(!(both || (xx->GetNx()==m && yy->GetNx()==n && zz->GetNx()==l)))
	{	gr->SetWarn(mglWarnDim,"Fit[S]");	return 0;	}
	if(!var || *var==0)
	{	gr->SetWarn(mglWarnNull,"Fit[S]");	return 0;	}

	mglData x(m,n,l), y(m,n,l), z(m,n,l), a(aa), s(ss);
	x.Name(L"x");	y.Name(L"y");	z.Name(L"z");
#pragma omp parallel for collapse(3)
	for(long k=0;k<l;k++)	for(long j=0;j<n;j++)	for(long i=0;i<m;i++)
	{
		long i0 = i+m*(j+n*k);
		x.a[i0] = GetX(xx,i,j,k).x;
		y.a[i0] = GetY(yy,i,j,k).x;
		z.a[i0] = GetZ(zz,i,j,k).x;
		if(mgl_isnan(x.a[i0]) || mgl_isnan(y.a[i0]) || mgl_isnan(z.a[i0]))	a.a[i0] = NAN;
	}
	mglFitData fd;
	fd.n = m*n*l;	fd.x = &x;	fd.y = &y;
	fd.z = &z;		fd.a = a.a;	fd.s = s.a;
	fd.eq = eq;		fd.var=var;	fd.m = strlen(var);
	mglData in(fd.m), *fit=new mglData(nn, nn, nn);
	mreal res = -1;

	if(ini && ini->nx>=fd.m)	in.Set(ini->a,fd.m);
	else in.Fill(0.,0);
	res = mgl_fit_base(fd,in.a);
	mgl_fill_fit(gr,*fit,in,fd,var,nn,nn,nn,0);
	if(ini && ini->nx>=fd.m)	memcpy(ini->a,in.a,fd.m*sizeof(mreal));

	mglPrepareFitEq(gr,res, eq,var,in.a);
	gr->LoadState();	return fit;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_hist_x(HMGL gr, HCDT x, HCDT a, const char *opt)
{
	long nn=a->GetNN();
	if(nn!=x->GetNN())
	{	gr->SetWarn(mglWarnDim,"Hist");	return 0;	}
	mreal rr = gr->SaveState(opt);
	long n = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	mglData *res = new mglData(n);

	mreal vx = n/(gr->Max.x-gr->Min.x);
	for(long i=0;i<nn;i++)
	{
		long j1 = long((x->vthr(i)-gr->Min.x)*vx);
		if(j1>=0 && j1<n)	res->a[j1] += a->vthr(i);
	}
	gr->LoadState();	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_hist_xy(HMGL gr, HCDT x, HCDT y, HCDT a, const char *opt)
{
	long nn=a->GetNN();
	if(nn!=x->GetNN() || nn!=y->GetNN())
	{	gr->SetWarn(mglWarnDim,"Hist");	return 0;	}
	mreal rr = gr->SaveState(opt);
	long n = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	mglData *res = new mglData(n, n);
	mreal vx = n/(gr->Max.x-gr->Min.x);
	mreal vy = n/(gr->Max.y-gr->Min.y);
	for(long i=0;i<nn;i++)
	{
		long j1 = long((x->vthr(i)-gr->Min.x)*vx);
		long j2 = long((y->vthr(i)-gr->Min.y)*vy);
		if(j1>=0 && j1<n && j2>=0 && j2<n)	res->a[j1+n*j2] += a->vthr(i);
	}
	gr->LoadState();	return res;
}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_hist_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *opt)
{
	long nn=a->GetNN();
	if(nn!=x->GetNN() || nn!=y->GetNN() || nn!=z->GetNN())
	{	gr->SetWarn(mglWarnDim,"Hist");	return 0;	}
	mreal rr = gr->SaveState(opt);
	long n = (mgl_isnan(rr) || rr<=0) ? mglFitPnts:long(rr+0.5);
	mglData *res = new mglData(n, n, n);
	mreal vx = n/(gr->Max.x-gr->Min.x), vy = n/(gr->Max.y-gr->Min.y), vz = n/(gr->Max.z-gr->Min.z);
	for(long i=0;i<nn;i++)
	{
		long j1 = long((x->vthr(i)-gr->Min.x)*vx);
		long j2 = long((y->vthr(i)-gr->Min.y)*vy);
		long j3 = long((z->vthr(i)-gr->Min.z)*vz);
		if(j1>=0 && j1<n && j2>=0 && j2<n && j3>=0 && j3<n)
			res->a[j1+n*(j2+n*j3)] += a->vthr(i);
	}
	gr->LoadState();	return res;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_hist_x_(uintptr_t* gr, uintptr_t* x, uintptr_t* a, const char *opt, int lo)
{	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_hist_x(_GR_, _DA_(x), _DA_(a), o);
	delete []o;	return r;	}
uintptr_t MGL_EXPORT mgl_hist_xy_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* a, const char *opt, int lo)
{	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_hist_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), o);
	delete []o;	return r;	}
uintptr_t MGL_EXPORT mgl_hist_xyz_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, const char *opt, int lo)
{	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_hist_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), o);
	delete []o;	return r;	}
//-----------------------------------------------------------------------------
MGL_EXPORT_CONST const char *mgl_get_fit(HMGL )	{	return mglFitRes;	}
int MGL_EXPORT mgl_get_fit_(uintptr_t *gr, char *out, int len)
{
	const char *res = mgl_get_fit(_GR_);
	if(out)	mgl_strncpy(out,res,len);
	return strlen(res);
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_fit_1_(uintptr_t* gr, uintptr_t* y, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_1(_GR_, _DA_(y), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_2_(uintptr_t* gr, uintptr_t* z, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_2(_GR_, _DA_(z), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_3_(uintptr_t* gr, uintptr_t* a, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_3(_GR_, _DA_(a), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xy_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xy(_GR_, _DA_(x), _DA_(y), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xyz_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xyza_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xyza(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_ys_(uintptr_t* gr, uintptr_t* y, uintptr_t* ss, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_ys(_GR_, _DA_(y), _DA_(ss), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xys_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* ss, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xys(_GR_, _DA_(x), _DA_(y), _DA_(ss), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xyzs_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* ss, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xyzs(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ss), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
uintptr_t MGL_EXPORT mgl_fit_xyzas_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, uintptr_t* ss, const char *eq, const char *var, uintptr_t *ini, const char *opt, int l, int n, int lo)
{
	char *s=new char[l+1];	memcpy(s,eq,l);		s[l]=0;
	char *d=new char[n+1];	memcpy(d,var,n);	d[n]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	uintptr_t r = (uintptr_t)mgl_fit_xyzas(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(ss), s, d, _DM_(ini), o);
	delete []o;	delete []s;	delete []d;	return r;
}
//-----------------------------------------------------------------------------
