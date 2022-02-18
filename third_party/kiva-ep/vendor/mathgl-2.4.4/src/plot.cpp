/***************************************************************************
 * plot.cpp is part of Math Graphic Library
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
#include <algorithm>
#include "mgl2/plot.h"
#include "mgl2/eval.h"
#include "mgl2/data.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//	Plot by formulas series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fplot(HMGL gr, const char *eqY, const char *pen, const char *opt)
{
	if(eqY==0 || eqY[0]==0)	return;		// nothing to plot
	double r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 100:long(r+0.5);
	long nm = gr->FaceNum?gr->FaceNum*n:10000, nd = gr->FaceNum?gr->FaceNum*10:1000;

	mglDataS x, y;
	x.dat.reserve(nm);	y.dat.reserve(nm);

	mglFormula *eq = new mglFormula(eqY);
	// initial data filling
	x.clear();	y.clear();
	if(gr->Min.x>0 && gr->Max.x>100*gr->Min.x)
	{
		double d = log(2*gr->Max.x/gr->Min.x)/(n-1);
		for(long i=0;i<n;i++)
		{	double xx = 2*gr->Max.x*exp(d*i)/(2*gr->Max.x/gr->Min.x+exp(d*i));
			x.dat.push_back(xx);	y.dat.push_back(eq->Calc(xx));	}
	}
	else if(gr->Max.x<0 && gr->Min.x<100*gr->Max.x)
	{
		double d = log(2*gr->Min.x/gr->Max.x)/(n-1);
		for(long i=0;i<n;i++)
		{	double xx = 2*gr->Min.x*exp(d*i)/(2*gr->Min.x/gr->Max.x+exp(d*i));
			x.dat.push_back(xx);	y.dat.push_back(eq->Calc(xx));	}
	}
	else
	{
		double d = (gr->Max.x - gr->Min.x)/(n-1.);
		for(long i=0;i<n;i++)
		{	double xx = gr->Min.x + i*d;
			x.dat.push_back(xx);	y.dat.push_back(eq->Calc(xx));	}
	}

	bool check=true;
	double ym=fabs(gr->Max.y - gr->Min.y)/nd;
	while(check && long(x.dat.size())<nm)
	{
		if(gr->NeedStop())	{	delete eq;	return;	}
		check = false;
		for(long i=1;i<long(x.size());i++)
		{
			double xs=(x[i]+x[i-1])/2;
			double ys=(y[i]+y[i-1])/2, yr=eq->Calc(xs);
			if(fabs(yr-ys)>ym)	// bad approximation here
			{
				x.dat.insert(x.dat.begin()+i,xs);
				y.dat.insert(y.dat.begin()+i,yr);
				check = true;	i++;
			}
		}
	}
	delete eq;	mgl_plot_xy(gr,&x,&y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fplot_xyz(HMGL gr, const char *eqX, const char *eqY, const char *eqZ, const char *pen, const char *opt)
{
	double r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 100:long(r+0.5);
	long nm = gr->FaceNum?gr->FaceNum*n:10000, nd = gr->FaceNum?gr->FaceNum*10:1000;

	mglDataS x, y, z, t;
	x.dat.reserve(nm);	y.dat.reserve(nm);
	z.dat.reserve(nm);	t.dat.reserve(nm);
	mglFormula *ex, *ey, *ez;
	ex = new mglFormula(eqX ? eqX : "0");
	ey = new mglFormula(eqY ? eqY : "0");
	ez = new mglFormula(eqZ ? eqZ : "0");
	t.clear();	x.clear();	y.clear();	z.clear();
	for(long i=0;i<n;i++)	// initial data filling
	{
		double tt = i/(n-1.);	t.push_back(tt);
		x.push_back(ex->Calc(0,0,t[i]));
		y.push_back(ey->Calc(0,0,t[i]));
		z.push_back(ez->Calc(0,0,t[i]));
	}

	bool check=true;
	double xm=fabs(gr->Max.x-gr->Min.x)/nd, ym=fabs(gr->Max.y-gr->Min.y)/nd, zm=fabs(gr->Max.z-gr->Min.z)/nd;
	while(check && long(x.dat.size())<nm)
	{
		if(gr->NeedStop())	{	delete ex;	delete ey;	delete ez;	return;	}
		check = false;
		for(long i=1;i<long(t.size());i++)
		{
			double ts=(t[i]+t[i-1])/2;
			double xs=(x[i]+x[i-1])/2, xr=ex->Calc(0,0,ts);
			double ys=(y[i]+y[i-1])/2, yr=ey->Calc(0,0,ts);
			double zs=(z[i]+z[i-1])/2, zr=ez->Calc(0,0,ts);
			if(fabs(xr-xs)>xm || fabs(yr-ys)>ym || fabs(zr-zs)>zm)	// bad approximation here
			{
				t.dat.insert(t.dat.begin()+i,ts);
				x.dat.insert(x.dat.begin()+i,xr);
				y.dat.insert(y.dat.begin()+i,yr);
				z.dat.insert(z.dat.begin()+i,zr);
				check = true;	i++;
			}
		}
	}
	delete ex;	delete ey;	delete ez;
	mgl_plot_xyz(gr,&x,&y,&z,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fplot_(uintptr_t *gr, const char *fy, const char *stl, const char *opt, int ly, int ls, int lo)
{	char *s=new char[ly+1];	memcpy(s,fy,ly);	s[ly]=0;
	char *p=new char[ls+1];	memcpy(p,stl,ls);	p[ls]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_fplot(_GR_, s, p, o);
	delete []s;		delete []p;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fplot_xyz_(uintptr_t *gr, const char *fx, const char *fy, const char *fz, const char *stl, const char *opt, int lx, int ly, int lz, int ls, int lo)
{	char *sx=new char[lx+1];	memcpy(sx,fx,lx);	sx[lx]=0;
	char *sy=new char[ly+1];	memcpy(sy,fy,ly);	sy[ly]=0;
	char *sz=new char[lz+1];	memcpy(sz,fz,lz);	sz[lz]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	char *p=new char[ls+1];		memcpy(p,stl,ls);	p[ls]=0;
	mgl_fplot_xyz(_GR_, sx, sy, sz, p, o);
	delete []sx;	delete []sy;	delete []sz;	delete []p;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Radar series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_radar(HMGL gr, HCDT a, const char *pen, const char *opt)
{
	long n = a->GetNx(), ny=a->GetNy();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Radar");	return;	}
	mglData x(n+1,ny), y(n+1,ny);
	double m=a->Minimal(), r=gr->SaveState(opt);
	if(mgl_isnan(r) || r<0)	r = m<0 ? -m:0;
	double *co=new double[2*n];
	for(long i=0;i<n;i++)	{	co[i]=cos(2*i*M_PI/n);	co[i+n]=sin(2*i*M_PI/n);	}
	for(long j=0;j<ny;j++)
	{
		for(long i=0;i<n;i++)
		{
			double v = a->v(i,j);
			x.a[i+(n+1)*j] = (r+v)*co[i];
			y.a[i+(n+1)*j] = (r+v)*co[i+n];
		}
		x.a[n+(n+1)*j] = r+a->v(0,j);	y.a[n+(n+1)*j] = 0;
	}
	mgl_plot_xy(gr,&x,&y,pen,0);
	if(mglchr(pen,'#'))	// draw "grid"
	{
		m = 1.1*(a->Maximal()+r);
		x.Create(2);	y.Create(2);
		for(long i=0;i<n;i++)
		{
			x.a[1]=m*co[i];		y.a[1]=m*co[i+n];
			mgl_plot_xy(gr,&x,&y,"k",0);
		}
		if(r>0)
		{
			x.Create(101);	y.Create(101);
			for(long i=0;i<91;i++)
			{	x.a[i]=r*mgl_cos[(4*i)%360];	y.a[i]=r*mgl_cos[(270+4*i)%360];	}
			mgl_plot_xy(gr,&x,&y,"k",0);
		}
	}
	delete []co;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_radar_(uintptr_t *gr, uintptr_t *a, const char *pen, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_radar(_GR_, _DA_(a),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Candle series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle_xyv(HMGL gr, HCDT x, HCDT v1, HCDT v2, HCDT y1, HCDT y2, const char *pen, const char *opt)
{
	long n=v1->GetNx(),pal,nx=x->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Candle");	return;	}
	if(nx<n || v2->GetNx()!=n)	{	gr->SetWarn(mglWarnDim,"Candle");	return;	}
	bool d1=false,d2=false;
	if(!y1)	{	y1 = new mglData(n);	d1=true;	((mglData *)y1)->Fill(NAN,NAN);	}
	if(!y2)	{	y2 = new mglData(n);	d2=true;	((mglData *)y2)->Fill(NAN,NAN);	}
	if(y1->GetNx()!=n || y2->GetNx()!=n)
	{	if(d1)	delete y1;	if(d2)	delete y2;
		gr->SetWarn(mglWarnDim,"Candle");	return;	}
	static int cgid=1;	gr->StartGroup("Candle",cgid++);
	gr->SaveState(opt);	gr->SetPenPal(pen,&pal);
	long kq = gr->AllocPnts(8*n);
	bool sh = mglchr(pen,'!');
	bool wire = mglchr(pen,'#');

	double dv=nx>n?1:0;
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv =-1;
	double zm = gr->AdjustZMin();
	double c1=gr->NextColor(pal), c2=c1;
	bool col2 = (gr->GetNumPal(pal)==2 && !sh);
	if(col2)	c2 = gr->NextColor(pal);
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{
		double m1=v1->v(i),	m2 = v2->v(i),	xx = x->v(i);
		double d = i<nx-1 ? x->v(i+1)-xx : xx-x->v(i-1), c;
		double x1 = xx + d/2*(dv-gr->BarWidth);
		double x2 = x1 + gr->BarWidth*d;	xx = (x1+x2)/2;
		if(sh)	c = gr->NextColor(pal,i);
		else if(wire)	c = (i>0 && m2>v2->v(i-1))?c2:c1;
		else	c = (m1>m2)?c1:c2;
		long iq = kq+8*i;
		gr->AddPntQ(iq,mglPoint(xx,y1->v(i),zm),c);
		gr->AddPntQ(iq+1,mglPoint(xx,m1,zm),c);
		gr->AddPntQ(iq+2,mglPoint(xx,y2->v(i),zm),c);
		gr->AddPntQ(iq+3,mglPoint(xx,m2,zm),c);
		gr->AddPntQ(iq+4,mglPoint(x1,m1,zm),c);
		gr->AddPntQ(iq+5,mglPoint(x2,m1,zm),c);
		gr->AddPntQ(iq+6,mglPoint(x1,m2,zm),c);
		gr->AddPntQ(iq+7,mglPoint(x2,m2,zm),c);
	}
	for(long i=0;i<n;i++)
	{
		long iq = kq+8*i;
		gr->line_plot(iq,  iq+1);	gr->line_plot(iq+2,iq+3);
		gr->line_plot(iq+4,iq+5);	gr->line_plot(iq+4,iq+6);
		gr->line_plot(iq+7,iq+5);	gr->line_plot(iq+7,iq+6);
		if(v1->v(i)>v2->v(i) || (col2 && !wire))	gr->quad_plot(iq+4,iq+5,iq+6,iq+7);
	}
	if(d1)	delete y1;
	if(d2)	delete y2;
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle_yv(HMGL gr, HCDT v1, HCDT v2, HCDT y1, HCDT y2, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(v1->GetNx()+1);
	x.Fill(gr->Min.x,gr->Max.x);
	mgl_candle_xyv(gr,&x,v1,v2,y1,y2,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle(HMGL gr, HCDT v1, HCDT y1, HCDT y2, const char *pen, const char *opt)
{
	mglData v2(v1);
	v2.Roll('x',1);	v2.a[0]=NAN;
	mgl_candle_yv(gr,v1,&v2,y1,y2,pen,opt);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle_xyv_(uintptr_t *gr, uintptr_t *x, uintptr_t *v1, uintptr_t *v2, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_candle_xyv(_GR_,_DA_(x),_DA_(v1),_DA_(v2),_DA_(y1),_DA_(y2),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle_yv_(uintptr_t *gr, uintptr_t *v1, uintptr_t *v2, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_candle_yv(_GR_,_DA_(v1),_DA_(v2),_DA_(y1),_DA_(y2),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_candle_(uintptr_t *gr, uintptr_t *y, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_candle(_GR_,_DA_(y),_DA_(y1),_DA_(y2),s,o);
	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Plot series
//
//-----------------------------------------------------------------------------
struct mglPointA {	mglPoint p;	bool orig;	mglPointA(const mglPoint &pp, bool o) : p(pp), orig(o) {}	};
std::vector<mglPointA> static mgl_pnt_prepare(const mglPoint &p1, const mglPoint &p2, HCDT xx, HCDT yy, HCDT zz, HCDT cc)
{
	std::vector<mglPointA> out;
	long n = xx->GetNx();
	mglPoint p(xx->v(0),yy->v(0),zz->v(0),cc?cc->v(0):0);
	if(p>p1 && p<p2)	out.push_back(mglPointA(p,true));
	else	out.push_back(mglPointA(mglPoint(NAN),true));
	for(long i=1;i<n;i++)
	{
		mglPoint q(xx->v(i),yy->v(i),zz->v(i),cc?cc->v(i):0);
		double x1,x2,y1,y2,z1,z2,t;
		x1=mgl_d(p1.x, p.x, q.x);	x2=mgl_d(p2.x, p.x, q.x);	if(x2<x1)	{	t=x1;	x1=x2;	x2=t;	}
		y1=mgl_d(p1.y, p.y, q.y);	y2=mgl_d(p2.y, p.y, q.y);	if(y2<y1)	{	t=y1;	y1=y2;	y2=t;	}
		z1=mgl_d(p1.z, p.z, q.z);	z2=mgl_d(p2.z, p.z, q.z);	if(z2<z1)	{	t=z1;	z1=z2;	z2=t;	}
		double d1 = mgl_isnum(x1)?x1:0, d2 = mgl_isnum(x2)?x2:1;
		if(y1>d1)	d1=y1;
		if(y2<d2)	d2=y2;
		if(z1>d1)	d1=z1;
		if(z2<d2)	d2=z2;
		if(d1>0 && d1<1)	out.push_back(mglPointA(p+d1*(q-p),false));
		if(d2>0 && d2<1)	out.push_back(mglPointA(p+d2*(q-p),false));
		if(d1<1 && d2>=1)	out.push_back(mglPointA(q,true));
		else if(i==n-1)	out.push_back(mglPointA(mglPoint(NAN),true));
		p = q;
	}
	return out;
}
std::vector<mglPointA> static mgl_pnt_copy(HCDT xx, HCDT yy, HCDT zz, HCDT cc)
{
	std::vector<mglPointA> out;
	long n = xx->GetNx();
	for(long i=0;i<n;i++)
		out.push_back(mglPointA(mglPoint(xx->v(i),yy->v(i),zz->v(i),cc?cc->v(i):0),true));
	return out;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark(HMGL gr, double x, double y, double z,const char *mark);
void MGL_EXPORT mgl_plot_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	static int cgid=1;
	long n=y->GetNx(),pal;
	if(n<2 && !mgl_check_dim0(gr,x,y,z,0,"Plot"))
	{
		gr->StartGroup("Plot",cgid++);
		gr->SaveState(opt);

		char mk = gr->SetPenPal(pen);
		if(mk)
		{
			long k = gr->AddPnt(mglPoint(x->v(0),y->v(0),z->v(0)),gr->CDef,mglPoint(NAN),-1,3);
			gr->mark_plot(k,mk,gr->GetPenWidth()); 	gr->AddActive(k);
		}
		gr->EndGroup(); return;
	}
	if(mgl_check_dim1(gr,x,y,z,0,"Plot"))	return;

	gr->StartGroup("Plot",cgid++);
	gr->SaveState(opt);
	long m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	bool sh = mglchr(pen,'!'), orig = !mglchr(pen,'a');

	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n, dx = n>d?n/d:1;
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;
		gr->NextColor(pal);
		mglDataR xx(x,mx), yy(y,my), zz(z,mz);
		const std::vector<mglPointA> &pp = orig ? mgl_pnt_copy(&xx, &yy, &zz, 0) :
			mgl_pnt_prepare(gr->Min, gr->Max, &xx, &yy, &zz, 0);
		size_t num = pp.size();
		long kq = gr->AllocPnts(num);
#pragma omp parallel for
		for(msize i=0;i<num;i++)
		{	double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+i, pp[i].p, c);	}
		if(mk)	for(size_t i=0;i<num;i+=dx)
			if(pp[i].orig)	gr->mark_plot(kq+i, mk);
		if(num>1)
		{
			gr->arrow_plot(kq,kq+1,gr->Arrow1);
			gr->arrow_plot(kq+num-1,kq+num-2,gr->Arrow2);
		}
		gr->curve_plot(num, kq);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_plot_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_plot_xyz(gr,x,y,&z,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_plot(HMGL gr, HCDT y, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Plot");	return;	}
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_plot_xyz(gr,&x,y,&z,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_plot_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_plot_xyz(_GR_, _DA_(x),_DA_(y),_DA_(z),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_plot_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_plot_xy(_GR_, _DA_(x),_DA_(y),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_plot_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_plot(_GR_, _DA_(y),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Tens series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, const char *pen, const char *opt)
{
	long m,n=y->GetNx(), pal;
	if(mgl_check_dim1(gr,x,y,z,0,"Tens"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Tens",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	char mk=gr->SetPenPal(pen, &pal);	gr->Reserve(2*n*m);
	long ss=gr->AddTexture(pen);
	bool orig = !mglchr(pen,'a');

	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n, dx = n>d?n/d:1;
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		long mz = j<z->GetNy() ? j:0, mc = j<c->GetNy() ? j:0;
		mglDataR xx(x,mx), yy(y,my), zz(z,mz), cc(c,mc);
		const std::vector<mglPointA> &pp = orig ? mgl_pnt_copy(&xx, &yy, &zz, &cc) :
			mgl_pnt_prepare(gr->Min, gr->Max, &xx, &yy, &zz, &cc);

		size_t num = pp.size();
		long kq = gr->AllocPnts(num);
#pragma omp parallel for
		for(msize i=0;i<num;i++)
		{	gr->AddPntQ(kq+i,pp[i].p, gr->GetC(ss,pp[i].p.c));	}
		if(mk)	for(size_t i=0;i<num;i+=dx)
			if(pp[i].orig)	gr->mark_plot(kq+i, mk);
		if(num>1)
		{
			gr->arrow_plot(kq,kq+1,gr->Arrow1);
			gr->arrow_plot(kq+num-1,kq+num-2,gr->Arrow2);
		}
		gr->curve_plot(num, kq);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens_xy(HMGL gr, HCDT x, HCDT y, HCDT c, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_tens_xyz(gr,x,y,&z,c,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens(HMGL gr, HCDT y, HCDT c, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Tens");	return;	}
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_tens_xyz(gr,&x,y,&z,c,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tens_xyz(_GR_, _DA_(x),_DA_(y),_DA_(z),_DA_(c),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *c, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tens_xy(_GR_, _DA_(x),_DA_(y),_DA_(c),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tens_(uintptr_t *gr, uintptr_t *y, uintptr_t *c, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tens(_GR_, _DA_(y),_DA_(c),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Area series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	long n=y->GetNx(),m,pal;
	if(mgl_check_dim1(gr,x,y,z,0,"Area"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Area3",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	bool sh = mglchr(pen,'!'), wire = mglchr(pen,'#'), orig = !mglchr(pen,'a');

	double z0=gr->GetOrgZ('x');
	gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;

		mglDataR xx(x,mx), yy(y,my), zz(z,mz);
		std::vector<mglPointA> pp = orig ? mgl_pnt_copy(&xx, &yy, &zz, 0) :
			mgl_pnt_prepare(gr->Min, gr->Max, &xx, &yy, &zz, 0);
		size_t np = pp.size();
		mglPoint nn(pp[0].p.y-pp[1].p.y, pp[1].p.x-pp[0].p.x);
		long kq = gr->AllocPnts(2*np);
#pragma omp parallel for
		for(msize i=0;i<np;i++)
		{
			double cc=gr->NextColor(pal,i);
			if(i>0 && i<np-1)	{	nn.x=(pp[i-1].p.y-pp[i+1].p.y)/2;	nn.y=(pp[i+1].p.x-pp[i-1].p.x)/2;	}
			else if(i==np-1)	{	nn.x=pp[np-2].p.y-pp[np-1].p.y;	nn.y=pp[np-1].p.x-pp[np-2].p.x;	}
			bool r1 = gr->AddPntQ(kq+2*i,pp[i].p, sh?cc:c1,nn,-1,27);	pp[i].p.z = z0;
			bool r2 = gr->AddPntQ(kq+2*i+1,pp[i].p, sh?cc:c2,nn,-1,27);
			if(!r1 && !r2)	{	gr->DisablePnt(kq+2*i);	gr->DisablePnt(kq+2*i+1);	}
		}
		for(size_t i=1;i<np;i++)
		{
			long iq = kq+2*i;
			if(gr->SamePnt(iq,iq-2) || gr->SamePnt(iq+1,iq-1))	continue;
			if(wire)
			{
				gr->line_plot(iq,iq+1);	gr->line_plot(iq-1,iq+1);
				gr->line_plot(iq,iq-2);	gr->line_plot(iq-1,iq-2);
			}
			else	gr->quad_plot(iq,iq+1,iq-2,iq-1);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	long n=y->GetNx(),m=y->GetNy(),pal;
	if(mgl_check_dim1(gr,x,y,0,0,"Area"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Curve",cgid++);
	double zm = gr->AdjustZMin();
	double y0=gr->GetOrgY('x');
	mglPoint nn(0,0,1);
	bool sh = mglchr(pen,'!'), wire = mglchr(pen,'#'), orig = !mglchr(pen,'a');

	gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2=gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		double z0 = zm + (m-1-j)*(gr->Max.z-zm)/m;

		mglDataR xx(x,mx), yy(y,my);	mglDataV zz(n,1,1,z0);
		std::vector<mglPointA> pp = orig ? mgl_pnt_copy(&xx, &yy, &zz, 0) :
			mgl_pnt_prepare(gr->Min, gr->Max, &xx, &yy, &zz, 0);
		size_t np = pp.size();
		long kq = gr->AllocPnts(2*np);
#pragma omp parallel for
		for(msize i=0;i<np;i++)
		{
			double cc=gr->NextColor(pal,i);
			bool r1 = gr->AddPntQ(kq+2*i,pp[i].p, sh?cc:c1,nn,-1,27);	pp[i].p.y = y0;
			bool r2 = gr->AddPntQ(kq+2*i+1,pp[i].p, sh?cc:c2,nn,-1,27);
			if(!r1 && !r2)	{	gr->DisablePnt(kq+2*i);	gr->DisablePnt(kq+2*i+1);	}
		}
		if(wire)	gr->line_plot(kq,kq+1);
		for(size_t i=1;i<np;i++)
		{
			long iq = kq+2*i;
			if(gr->SamePnt(iq,iq-2) || gr->SamePnt(iq+1,iq-1))	continue;
			if(wire)
			{
				gr->line_plot(iq,iq+1);	gr->line_plot(iq-1,iq+1);
				gr->line_plot(iq,iq-2);
			}
			else	gr->quad_plot(iq,iq+1,iq-2,iq-1);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area(HMGL gr, HCDT y, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx());	x.Fill(gr->Min.x,gr->Max.x);
	mgl_area_xy(gr,&x,y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_area_xyz(_GR_, _DA_(x),_DA_(y),_DA_(z),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_area_xy(_GR_, _DA_(x),_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_area_(uintptr_t *gr, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_area(_GR_, _DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Region series
//
//-----------------------------------------------------------------------------
struct mglPointB {	mglPoint p1, p2;	bool orig;
	mglPointB(const mglPoint &pp1, const mglPoint &pp2, bool o) : p1(pp1), p2(pp2), orig(o) {}	};
std::vector<mglPointB> static mgl_pnt_prepare(const mglPoint &a1, const mglPoint &a2, HCDT xx1, HCDT yy1, HCDT zz1, HCDT xx2, HCDT yy2, HCDT zz2)
{
	std::vector<mglPointB> out;
	long n = xx1->GetNx();
	mglPoint p1(xx1->v(0),yy1->v(0),zz1->v(0)), p2(xx2->v(0),yy2->v(0),zz2->v(0));
	if(p1>a1 && p1<a2 && p2>a1 && p2<a2)	out.push_back(mglPointB(p1,p2,true));
	else	out.push_back(mglPointB(mglPoint(NAN),mglPoint(NAN),true));
	for(long i=1;i<n;i++)
	{
		mglPoint q1(xx1->v(i),yy1->v(i),zz1->v(i)), q2(xx2->v(i),yy2->v(i),zz2->v(i));
		double x1,x2,y1,y2,z1,z2,t;
		x1=mgl_d(a1.x, p1.x, q1.x);	x2=mgl_d(a2.x, p1.x, q1.x);	if(x2<x1)	{	t=x1;	x1=x2;	x2=t;	}
		y1=mgl_d(a1.y, p1.y, q1.y);	y2=mgl_d(a2.y, p1.y, q1.y);	if(y2<y1)	{	t=y1;	y1=y2;	y2=t;	}
		z1=mgl_d(a1.z, p1.z, q1.z);	z2=mgl_d(a2.z, p1.z, q1.z);	if(z2<z1)	{	t=z1;	z1=z2;	z2=t;	}
		double d11 = mgl_isnum(x1)?x1:0, d12 = mgl_isnum(x2)?x2:1;
		if(y1>d11)	d11=y1;
		if(y2<d12)	d12=y2;
		if(z1>d11)	d11=z1;
		if(z2<d12)	d12=z2;
		x1=mgl_d(a1.x, p2.x, q2.x);	x2=mgl_d(a2.x, p2.x, q2.x);	if(x2<x1)	{	t=x1;	x1=x2;	x2=t;	}
		y1=mgl_d(a1.y, p2.y, q2.y);	y2=mgl_d(a2.y, p2.y, q2.y);	if(y2<y1)	{	t=y1;	y1=y2;	y2=t;	}
		z1=mgl_d(a1.z, p2.z, q2.z);	z2=mgl_d(a2.z, p2.z, q2.z);	if(z2<z1)	{	t=z1;	z1=z2;	z2=t;	}
		double d21 = mgl_isnum(x1)?x1:0, d22 = mgl_isnum(x2)?x2:1;
		if(y1>d21)	d21=y1;
		if(y2<d22)	d22=y2;
		if(z1>d21)	d21=z1;
		if(z2<d22)	d22=z2;

		std::vector<double> dd;
		if(d11>0 && d11<1)	dd.push_back(d11);
		if(d21>0 && d21<1)	dd.push_back(d21);
		if(d12>0 && d12<1)	dd.push_back(d12);
		if(d22>0 && d22<1)	dd.push_back(d22);
		// now add all intersections to be sure
		x1=mgl_d(0, p2.x-p1.x, q2.x-q1.x);	if(x1>0 && x1<1)	dd.push_back(x1);
		y1=mgl_d(0, p2.y-p1.y, q2.y-q1.y);	if(y1>0 && y1<1)	dd.push_back(y1);
		z1=mgl_d(0, p2.z-p1.z, q2.z-q1.z);	if(z1>0 && z1<1)	dd.push_back(z1);
		std::sort(dd.begin(),dd.end());
		for(size_t j=0;j<dd.size();j++)
			out.push_back(mglPointB(p1+dd[j]*(q1-p1), p2+dd[j]*(q2-p2), false));
		if((d11<1 && d12>=1) || (d21<1 && d22>=1))	out.push_back(mglPointB(q1,q2,true));
		else if(i==n-1)	out.push_back(mglPointB(mglPoint(NAN),mglPoint(NAN),true));
		p1 = q1;	p2 = q2;
	}
	return out;
}
std::vector<mglPointB> static mgl_pnt_copy(HCDT xx1, HCDT yy1, HCDT zz1, HCDT xx2, HCDT yy2, HCDT zz2)
{
	std::vector<mglPointB> out;
	long n = xx1->GetNx();
	for(long i=0;i<n;i++)
		out.push_back(mglPointB(mglPoint(xx1->v(i),yy1->v(i),zz1->v(i)), mglPoint(xx2->v(i),yy2->v(i),zz2->v(i)), true));
	return out;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region_3d(HMGL gr, HCDT x1, HCDT y1, HCDT z1, HCDT x2, HCDT y2, HCDT z2, const char *pen, const char *opt)
{
	long n=y1->GetNx(), m, pal;
	if(mgl_check_dim1(gr,x1,y1,z1,0,"Region"))	return;
	if(mgl_check_dim1(gr,x1,x2,y2,z2,"Region"))	return;
	m = x1->GetNy() > y1->GetNy() ? x1->GetNy() : y1->GetNy();
	m = (z1 && z1->GetNy() > m) ? z1->GetNy() : m;
	bool zhave = z1 && z2;
	if(x1->GetNy()!=x2->GetNy() || y1->GetNy()!=y2->GetNy())
	{	gr->SetWarn(mglWarnDim,"Region");	return;	}
	if(zhave && z1->GetNy()!=z2->GetNy())
	{	gr->SetWarn(mglWarnDim,"Region");	return;	}

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Region",cgid++);
	mglPoint nn(0,0,1);
	double zm = gr->AdjustZMin();
//	bool inside = (mglchr(pen,'i'));	// NOTE: check if 'i' is free (used here for inside flag)
	bool sh = mglchr(pen,'!'), wire = mglchr(pen,'#'), orig = !mglchr(pen,'a');

	gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2=gr->NextColor(pal);
		long mx = j<x1->GetNy() ? j:0, my = j<y1->GetNy() ? j:0;
		long mz = (zhave && j<z1->GetNy()) ? j:0;
		double z0 = zm + (m-1-j)*(gr->Max.z-zm)/m;

		mglDataR xx1(x1,mx), yy1(y1,my), xx2(x2,mx), yy2(y2,my);
		mglDataV zz0(n,1,1,z0);
		std::vector<mglPointB> pp;
		if(zhave)
		{
			mglDataR zz1(z1,mz), zz2(z2,mz);
			pp = orig ? mgl_pnt_copy(&xx1, &yy1, &zz1, &xx2, &yy2, &zz2) :
				mgl_pnt_prepare(gr->Min, gr->Max, &xx1, &yy1, &zz1, &xx2, &yy2, &zz2);
		}
		else
		{
			pp = orig ? mgl_pnt_copy(&xx1, &yy1, &zz0, &xx2, &yy2, &zz0) :
				mgl_pnt_prepare(gr->Min, gr->Max, &xx1, &yy1, &zz0, &xx2, &yy2, &zz0);
		}

		size_t np = pp.size();
		long kq = gr->AllocPnts(2*np);
#pragma omp parallel for
		for(msize i=0;i<np;i++)
		{
			double cc=gr->NextColor(pal,i);
			bool r1 = gr->AddPntQ(kq+2*i,pp[i].p1, sh?cc:c1,nn,-1,27);
			bool r2 = gr->AddPntQ(kq+2*i+1,pp[i].p2, sh?cc:c2,nn,-1,27);
			if(!r1 && !r2)	{	gr->DisablePnt(kq+2*i);	gr->DisablePnt(kq+2*i+1);	}
		}
		if(wire)	gr->line_plot(kq,kq+1);
		for(size_t i=1;i<np;i++)
		{
			long iq = kq+2*i;
			if(gr->SamePnt(iq,iq-2) || gr->SamePnt(iq+1,iq-1))	continue;
			if(wire)
			{
				gr->line_plot(iq,iq+1);	gr->line_plot(iq-1,iq+1);
				gr->line_plot(iq,iq-2);
			}
			else	gr->quad_plot(iq,iq+1,iq-2,iq-1);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region_xy(HMGL gr, HCDT x, HCDT y1, HCDT y2, const char *pen, const char *opt)
{
	long n=y1->GetNx(), m=y1->GetNy(), pal;
	if(mgl_check_dim1(gr,x,y1,y2,0,"Region"))	return;
	if(y2->GetNy()!=m)	{	gr->SetWarn(mglWarnDim,"Region");	return;	}

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Region",cgid++);
	mglPoint nn(0,0,1);
	double zm = gr->AdjustZMin();
	bool inside = mglchr(pen,'i');	// NOTE: check if 'i' is free (used here for inside flag)
	bool sh = mglchr(pen,'!'), wire = mglchr(pen,'#'), orig = !mglchr(pen,'a');

	gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2=gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0;
		double z0 = zm + (m-1-j)*(gr->Max.z-zm)/m;

		mglDataR xx(x,mx), yy1(y1,j), yy2(y2,j);
		mglDataV zz0(n,1,1,z0);
		std::vector<mglPointB> pp = orig ? mgl_pnt_copy(&xx, &yy1, &zz0, &xx, &yy2, &zz0) :
			mgl_pnt_prepare(gr->Min, gr->Max, &xx, &yy1, &zz0, &xx, &yy2, &zz0);

		size_t np = pp.size();
		long kq = gr->AllocPnts(2*np);
#pragma omp parallel for
		for(msize i=0;i<np;i++)
		{
			double cc=gr->NextColor(pal,i);
			bool r1 = gr->AddPntQ(kq+2*i,pp[i].p1, sh?cc:c1,nn,-1,27);
			bool r2 = gr->AddPntQ(kq+2*i+1,pp[i].p2, sh?cc:c2,nn,-1,27);
			if(!r1 && !r2)	{	gr->DisablePnt(kq+2*i);	gr->DisablePnt(kq+2*i+1);	}
		}
		if(wire)	gr->line_plot(kq,kq+1);
		for(size_t i=1;i<np;i++)
		{
			long iq = kq+2*i;
			if(gr->SamePnt(iq,iq-2) || gr->SamePnt(iq+1,iq-1))	continue;
			if(wire)
			{
				gr->line_plot(iq,iq+1);
				gr->line_plot(iq-1,iq+1);
				gr->line_plot(iq,iq-2);
			}
			else if(!inside)	gr->quad_plot(iq,iq+1,iq-2,iq-1);
			else
			{
				const mglPointB &a=pp[i-1], &b=pp[i];
				if(a.p1.y<=a.p2.y && b.p1.y<=b.p2.y)
					gr->quad_plot(iq,iq+1,iq-2,iq-1);
				else if(a.p1.y<=a.p2.y)
				{
					double cc=gr->NextColor(pal,i);
					double dd = (a.p1.y-a.p2.y)/(b.p2.y-b.p1.y-a.p2.y+a.p1.y);
					long ns = gr->AddPnt(b.p1*dd+a.p1*(1-dd), sh?cc:c1,nn,-1,27);
					gr->trig_plot(iq-2,iq-1,ns);
				}
				else if(b.p1.y<=b.p2.y)
				{
					double cc=gr->NextColor(pal,i);
					double dd = (a.p1.y-a.p2.y)/(b.p2.y-b.p1.y-a.p2.y+a.p1.y);
					long ns = gr->AddPnt(b.p1*dd+a.p1*(1-dd), sh?cc:c1,nn,-1,27);
					gr->trig_plot(iq,iq+1,ns);
				}
			}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region(HMGL gr, HCDT y1, HCDT y2, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y1->GetNx());	x.Fill(gr->Min.x, gr->Max.x);
	mgl_region_xy(gr,&x,y1,y2,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region_3d_(uintptr_t *gr, uintptr_t *x1, uintptr_t *y1, uintptr_t *z1, uintptr_t *x2, uintptr_t *y2, uintptr_t *z2, const char *pen, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_region_3d(_GR_, _DA_(x1),_DA_(y1),_DA_(z1),_DA_(x2),_DA_(y2),_DA_(z2),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_region_xy(_GR_, _DA_(x),_DA_(y1),_DA_(y2),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_region_(uintptr_t *gr, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt, int l, int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_region(_GR_, _DA_(y1),_DA_(y2),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Step series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	long m,n=y->GetNx(), pal;
	if(mgl_check_dim1(gr,x,y,z,0,"Step"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Step3",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	bool sh = mglchr(pen,'!');

	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n, dx = n>d?n/d:1;
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;
		gr->NextColor(pal);
		long kq = gr->AllocPnts(2*n);	gr->SetPntOff(kq);
		gr->AddPntQ(kq+1,mglPoint(x->v(0,mx), y->v(0,my), z->v(0,mz)));
		if(mk)	gr->mark_plot(kq+1,mk);
#pragma omp parallel for
		for(long i=1;i<n;i++)
		{
			mglPoint p(x->v(i,mx), y->v(i,my), z->v(i-1,mz));
			double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+2*i,p,c);
			p.z = z->v(i,mz);	gr->AddPntQ(kq+2*i+1,p,c);
		}
		for(long i=1;i<n;i++)
		{
			long iq = kq+2*i;
			gr->line_plot(iq,iq-1);	gr->line_plot(iq,iq+1);
			if(mk && i%dx==0)	gr->mark_plot(iq+1,mk);
		}
		gr->arrow_plot(kq+1,kq+2,gr->Arrow2);
		gr->arrow_plot(kq+2*n-1,kq+2*n-2,gr->Arrow2);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	long m,n=y->GetNx(), pal;
	if(mgl_check_dim1(gr,x,y,0,0,"Step",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Step",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	bool sh = mglchr(pen,'!');
	bool same = x->GetNx()==n;

	double zVal =gr->AdjustZMin();
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n, dx = n>d?n/d:1;
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		gr->NextColor(pal);

		long kq = gr->AllocPnts(2*n);	gr->SetPntOff(kq);
		gr->AddPntQ(kq+1,mglPoint(x->v(0,mx), y->v(0,my), zVal));
		if(same && mk)	gr->mark_plot(kq+1,mk);
#pragma omp parallel for
		for(long i=1;i<n;i++)
		{
			mglPoint p(x->v(i,mx), y->v(i-1,my), zVal);
			double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+2*i,p,c);
			p.y = y->v(i,my);	gr->AddPntQ(kq+2*i+1,p,c);
		}
		for(long i=1;i<n;i++)
		{
			long iq = kq+2*i;
			gr->line_plot(iq,iq-1);	gr->line_plot(iq,iq+1);
			if(same && mk && i%dx==0)	gr->mark_plot(iq+1,mk);
		}
		gr->arrow_plot(kq+1,kq+2,gr->Arrow2);
		gr->arrow_plot(kq+2*n-1,kq+2*n-2,gr->Arrow2);
		if(!same && mk)
		{
			kq = gr->AllocPnts(1+(n-1)/dx);
#pragma omp parallel for
			for(long i=0;i<n;i+=dx)
			{
				mglPoint p((x->v(i,mx)+x->v(i+1,mx))/2, y->v(i,my), zVal);
				double c = sh ? gr->NextColor(pal,i):gr->CDef;
				gr->AddPntQ(kq+i,p,c);
			}
			for(long i=0;i<n;i+=dx)	gr->mark_plot(kq+i,mk);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step(HMGL gr, HCDT y,	const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx());	x.Fill(gr->Min.x,gr->Max.x);
	mgl_step_xy(gr,&x,y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_step_xyz(_GR_, _DA_(x),_DA_(y),_DA_(z),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_step_xy(_GR_, _DA_(x),_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_step_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_step(_GR_, _DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Stem series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	long m,n=y->GetNx(), pal;
	if(mgl_check_dim0(gr,x,y,z,0,"Stem"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Stem3",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	bool sh = mglchr(pen,'!');

	double z0=gr->GetOrgZ('x');
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;
		gr->NextColor(pal);
		long kq = gr->AllocPnts(2*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+2*i,mglPoint(x->v(i,mx), y->v(i,my), z->v(i,mz)),c);
			gr->AddPntQ(kq+2*i+1,mglPoint(x->v(i,mx), y->v(i,my), z0),c);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+2*i;	gr->line_plot(iq,iq+1);
			if(mk)	gr->mark_plot(iq,mk);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	long m,n=y->GetNx(), pal;
	if(mgl_check_dim0(gr,x,y,0,0,"Stem"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Stem",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	bool sh = mglchr(pen,'!');

	double zVal = gr->AdjustZMin(), y0=gr->GetOrgY('x');
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(2*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		gr->NextColor(pal);
		long kq = gr->AllocPnts(2*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+2*i,mglPoint(x->v(i,mx), y->v(i,my), zVal),c);
			gr->AddPntQ(kq+2*i+1,mglPoint(x->v(i,mx), y0, zVal),c);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+2*i;	gr->line_plot(iq,iq+1);
			if(mk)	gr->mark_plot(iq,mk);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem(HMGL gr, HCDT y,	const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx());	x.Fill(gr->Min.x,gr->Max.x);
	mgl_stem_xy(gr,&x,y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_stem_xyz(_GR_,_DA_(x),_DA_(y),_DA_(z),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_stem_xy(_GR_,_DA_(x),_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stem_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_stem(_GR_,_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Bars series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	long m,n=z->GetNx(), pal,nx=x->GetNx(),ny=y->GetNx();
	if(mgl_check_dim1(gr,x,z,y,0,"Bars",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Bars3",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	m = z->GetNy() > m ? z->GetNy() : m;
	const bool sh = mglchr(pen,'!');

	bool wire = mglchr(pen,'#'), fixed = mglchr(pen,'F');
	bool above = mglchr(pen,'a'), fall = mglchr(pen,'f');
	if(above)	fall = false;
	double *dd=new double[n], *zp=0, dv=nx>n?1:0;
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv = -1;
	memset(dd,0,n*sizeof(double));
	if(fall)	zp = new double[n];

	double dc=INFINITY;
	if(fixed)	for(long j=0;j<m;j++)
	{
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		for(long i=0;i<n-1;i++)
		{
			double cx = hypot(x->v(i+1,mx)-x->v(i,mx), y->v(i+1,my)-y->v(i,my));
			if(cx<dc)	dc=cx;
		}
	}
	if(dc==0)	fixed=false;	// NOTE: disable fixed width if it is zero

	gr->SetPenPal(pen,&pal);
	gr->Reserve(4*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		const long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;
		const double z0 = gr->GetOrgZ('x');
		if(fall)
		{	zp[0]=z0;	for(long i=0;i<n-1;i++)	zp[i+1] = zp[i]+z->v(i,mz);	}

		const long kq = gr->AllocPnts(4*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double vv = x->v(i,mx), dx = i<nx-1 ? x->v(i+1,mx)-vv : vv-x->v(i-1,mx), dy, zz;
			double x1 = vv + dx/2*(dv-gr->BarWidth), x2 = x1 + gr->BarWidth*dx;
			vv = y->v(i,my);	dy = i<ny-1 ? y->v(i+1,my)-vv : vv-y->v(i-1,my);
			if(fixed)
			{	double ff = dc/hypot(dx,dy);	dx *= ff;	dy *= ff;	}
			double y1 = vv + dy/2*(dv-gr->BarWidth), y2 = y1 + gr->BarWidth*dy;
			vv = zz = z->v(i,mz);
			double zt = z0;
			if(!above)
			{
				x2 = (x2-x1)/m;		x1 += j*x2;		x2 += x1;
				y2 = (y2-y1)/m;		y1 += j*y2;		y2 += y1;
				if(fall)	{	zt = zp[i];	zz += zp[i];	}
			}
			else
			{	zt = gr->GetOrgZ('x') + dd[i];	dd[i] += zz;	zz += zt;	}

			double c = !sh ? (vv<0 ? c1 : c2) : gr->NextColor(pal,i);
			mglPoint nn(-y->dvx(i,my),x->dvx(i,mx));
			gr->AddPntQ(kq+4*i,mglPoint(x1,y1,zz),c,nn);
			gr->AddPntQ(kq+4*i+1,mglPoint(x1,y1,zt),c,nn);
			gr->AddPntQ(kq+4*i+2,mglPoint(x2,y2,zt),c,nn);
			gr->AddPntQ(kq+4*i+3,mglPoint(x2,y2,zz),c,nn);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+4*i;
			if(wire)
			{	gr->line_plot(iq,iq+1);		gr->line_plot(iq,iq+3);
				gr->line_plot(iq+2,iq+1);	gr->line_plot(iq+2,iq+3);	}
			else	gr->quad_plot(iq,iq+1,iq+3,iq+2);
		}
	}
	gr->EndGroup();	delete []dd;	if(fall)	delete []zp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	long m,n=y->GetNx(),nx=x->GetNx(),pal;
	if(mgl_check_dim1(gr,x,y,0,0,"Bars",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Bars",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	bool sh = mglchr(pen,'!');

	bool wire = mglchr(pen,'#'), fixed = mglchr(pen,'F');
	bool above = mglchr(pen,'a'), fall = mglchr(pen,'f');
	if(above)	fall = false;
	double *dd=new double[n], dv=nx>n?1:0, *yp=0;
	memset(dd,0,n*sizeof(double));
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv = -1;
	const double zm = gr->AdjustZMin();
	if(fall)	yp = new double[n];

	double dx=INFINITY;
	if(fixed)
	{
		long nn=x->GetNy();
		for(long j=0;j<nn;j++)	for(long i=0;i<n-1;i++)
		{
			double cx = fabs(x->v(i+1,j)-x->v(i,j));
			if(cx<dx)	dx=cx;
		}
	}
	if(dx==0)	fixed=false;	// NOTE: disable fixed width if it is zero

	gr->SetPenPal(pen,&pal);
	gr->Reserve(4*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		const long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		const double y0 = gr->GetOrgY('x');
		if(fall)
		{	yp[0]=y0;	for(long i=0;i<n-1;i++)	yp[i+1] = yp[i]+y->v(i,my);	}

		const long kq = gr->AllocPnts(4*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double vv = x->v(i,mx), d = i<nx-1 ? x->v(i+1,mx)-vv : vv-x->v(i-1,mx), yy;
			if(fixed)	d = dx;
			double x1 = vv + d/2*(dv-gr->BarWidth), x2 = x1 + gr->BarWidth*d;
			vv = yy = y->v(i,my);
			double yt = y0;
			if(!above)
			{
				x2 = (x2-x1)/m;		x1 += j*x2;		x2 += x1;
				if(fall)	{	yt = yp[i];	yy += yp[i];	}
			}
			else
			{	yt = y0 + dd[i];	dd[i] += yy;	yy += yt;	}

			double c = !sh ? (vv<0 ? c1 : c2) : gr->NextColor(pal,i);
			gr->AddPntQ(kq+4*i,mglPoint(x1,yy,zm),c);
			gr->AddPntQ(kq+4*i+1,mglPoint(x1,yt,zm),c);
			gr->AddPntQ(kq+4*i+2,mglPoint(x2,yt,zm),c);
			gr->AddPntQ(kq+4*i+3,mglPoint(x2,yy,zm),c);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+4*i;
			if(wire)
			{	gr->line_plot(iq,iq+1);		gr->line_plot(iq,iq+3);
				gr->line_plot(iq+2,iq+1);	gr->line_plot(iq+2,iq+3);	}
			else	gr->quad_plot(iq,iq+1,iq+3,iq+2);
		}
	}
	gr->EndGroup();	delete []dd;	if(fall)	delete []yp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars(HMGL gr, HCDT y, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx()+1);	x.Fill(gr->Min.x,gr->Max.x);
	mgl_bars_xy(gr,&x,y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_bars_xyz(_GR_,_DA_(x),_DA_(y),_DA_(z),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_bars_xy(_GR_,_DA_(x),_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_bars_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_bars(_GR_,_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//		Barh series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_barh_yx(HMGL gr, HCDT y, HCDT v, const char *pen, const char *opt)
{
	long m,n=v->GetNx(),ny=y->GetNx(),pal;
	if(mgl_check_dim1(gr,y,v,0,0,"Barh",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Barh",cgid++);
	m = y->GetNy() > v->GetNy() ? y->GetNy() : v->GetNy();
	bool sh = mglchr(pen,'!');

	bool wire = mglchr(pen,'#'), fixed = mglchr(pen,'F');
	bool above = mglchr(pen,'a'), fall = mglchr(pen,'f');
	if(above)	fall = false;
	double *dd=new double[n], *xp=0,dv=ny>n?1:0;
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv = -1;
	const double zm = gr->AdjustZMin();
	memset(dd,0,n*sizeof(double));
	if(fall)	xp = new double[n];

	double dy=INFINITY;
	if(fixed)
	{
		long nn=y->GetNy();
		for(long j=0;j<nn;j++)	for(long i=0;i<n-1;i++)
		{
			double cx = fabs(y->v(i+1,j)-y->v(i,j));
			if(cx<dy)	dy=cx;
		}
	}
	if(dy==0)	fixed=false;	// NOTE: disable fixed width if it is zero

	gr->SetPenPal(pen,&pal);
	gr->Reserve(4*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		const long mx = j<v->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		const double x0 = gr->GetOrgX('y');
		if(fall)
		{	xp[0]=x0;	for(long i=0;i<n-1;i++)	xp[i+1] = xp[i]+v->v(i,mx);	}

		const long kq = gr->AllocPnts(4*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double vv = y->v(i,my), d = i<ny-1 ? y->v(i+1,my)-vv : vv-y->v(i-1,my), xx;
			if(fixed)	d = dy;
			double y1 = vv + d/2*(dv-gr->BarWidth), y2 = y1 + gr->BarWidth*d;
			vv = xx = v->v(i,mx);
			double xt = x0;
			if(!above)
			{
				y2 = (y2-y1)/m;		y1 += j*y2;		y2 += y1;
				if(fall)	{	xt = xp[i];	xx += xp[i];	}
			}
			else
			{	xt = x0 + dd[i];	dd[i] += xx;	xx += xt;	}

			double c = !sh ? (vv<0 ? c1 : c2) : gr->NextColor(pal,i);
			gr->AddPntQ(kq+4*i,mglPoint(xx,y1,zm),c);
			gr->AddPntQ(kq+4*i+1,mglPoint(xx,y2,zm),c);
			gr->AddPntQ(kq+4*i+2,mglPoint(xt,y2,zm),c);
			gr->AddPntQ(kq+4*i+3,mglPoint(xt,y1,zm),c);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+4*i;
			if(wire)
			{	gr->line_plot(iq,iq+1);		gr->line_plot(iq,iq+3);
				gr->line_plot(iq+2,iq+1);	gr->line_plot(iq+2,iq+3);	}
			else	gr->quad_plot(iq,iq+1,iq+3,iq+2);
		}
	}
	gr->EndGroup();	delete []dd;	if(fall)	delete []xp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_barh(HMGL gr, HCDT v,	const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV y(v->GetNx()+1);	y.Fill(gr->Min.y,gr->Max.y);
	mgl_barh_yx(gr,&y,v,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_barh_yx_(uintptr_t *gr, uintptr_t *y, uintptr_t *v, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_barh_yx(_GR_,_DA_(y),_DA_(v),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_barh_(uintptr_t *gr, uintptr_t *v,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_barh(_GR_,_DA_(v),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	OHLC series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ohlc_x(HMGL gr, HCDT x, HCDT open, HCDT high, HCDT low, HCDT close, const char *pen, const char *opt)
{
	long n=open->GetNx(), nx=x->GetNx(), m=open->GetNy(), mx;
	if(nx<n || n*m!=high->GetNx()*high->GetNy() || n*m!=low->GetNx()*low->GetNy() || n*m!=close->GetNx()*close->GetNy())
	{	gr->SetWarn(mglWarnDim,"OHLC");	return;	}
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("OHLC",cgid++);
	double dv=nx>n?1:0;
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv = -1;
	double zVal = gr->AdjustZMin();
	bool sh = mglchr(pen,'!');

	long pal;
	gr->SetPenPal(pen,&pal);	gr->Reserve(6*n*m);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		mx = j<x->GetNy() ? j:0;

		const long kq = gr->AllocPnts(6*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			double dd,vv,x1,x2;
			vv = x->v(i,mx);	dd = i<nx-1 ? x->v(i+1)-vv : vv-x->v(i-1);
			x1 = vv + dd/2*(dv-gr->BarWidth);	x2 = x1 + gr->BarWidth*dd;
			x2 = (x2-x1)/m;		x1 += j*x2;		x2 += x1;	vv = (x2+x1)/2;

			dd = close->v(i,j);
			double c = !sh? ((i==0 || dd>=close->v(i-1,j)) ? c1:c2) : gr->NextColor(pal,i);
			gr->AddPntQ(kq+6*i,mglPoint(vv,dd,zVal),c);
			gr->AddPntQ(kq+6*i+1,mglPoint(x2,dd,zVal),c);
			dd = open->v(i,j);
			gr->AddPntQ(kq+6*i+2,mglPoint(x1,dd,zVal),c);
			gr->AddPntQ(kq+6*i+3,mglPoint(vv,dd,zVal),c);
			gr->AddPntQ(kq+6*i+4,mglPoint(vv,low->v(i,j),zVal),c);
			gr->AddPntQ(kq+6*i+5,mglPoint(vv,high->v(i,j),zVal),c);
		}
		for(long i=0;i<n;i++)
		{
			long iq = kq+6*i;
			gr->line_plot(iq,iq+1);
			gr->line_plot(iq+2,iq+3);
			gr->line_plot(iq+4,iq+5);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ohlc(HMGL gr, HCDT open, HCDT high, HCDT low, HCDT close, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(open->GetNx()+1);	x.Fill(gr->Min.x,gr->Max.x);
	mgl_ohlc_x(gr,&x,open,high,low,close,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ohlc_x_(uintptr_t *gr, uintptr_t *x, uintptr_t *open, uintptr_t *high, uintptr_t *low, uintptr_t *close, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_ohlc_x(_GR_,_DA_(x),_DA_(open),_DA_(high),_DA_(low),_DA_(close),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ohlc_(uintptr_t *gr, uintptr_t *open, uintptr_t *high, uintptr_t *low, uintptr_t *close, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_ohlc(_GR_,_DA_(open),_DA_(high),_DA_(low),_DA_(close),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	BoxPlot series
//
//-----------------------------------------------------------------------------
double sgn(double a);
int static mgl_cmp_flt(const void *a, const void *b)
{
	const double *aa = (const double *)a;
	const double *bb = (const double *)b;
	return int(sgn(*aa-*bb));
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxplot_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	long n=y->GetNx(), m=y->GetNy(), nx=x->GetNx();
	if(nx<n || nx<2 || m<2)	{	gr->SetWarn(mglWarnDim,"BoxPlot");	return;	}
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("BoxPlot",cgid++);
	double *b = new double[5*n], dv=nx>n?1:0;
	if(mglchr(pen,'<'))	dv = 1;
	if(mglchr(pen,'^'))	dv = 0;
	if(mglchr(pen,'>'))	dv = -1;
	double zVal = gr->AdjustZMin();
	bool sh = mglchr(pen,'!');
	double *d = new double[m];
	for(long i=0;i<n;i++)	// find quartiles by itself
	{
		long mm=0,k;
		for(long j=0;j<m;j++)
		{
			double vv = y->v(i,j);
			if(mgl_isnum(vv))	{	d[mm]=vv;	mm++;	}
		}
		qsort(d, mm, sizeof(double), mgl_cmp_flt);
		b[i] = d[0];	b[i+4*n] = d[mm-1];		k = mm/4;
		b[i+n] = (mm%4) ? d[k] : (d[k]+d[k-1])/2.;
		b[i+2*n] = (mm%2) ? d[mm/2] : (d[mm/2]+d[mm/2-1])/2.;
		b[i+3*n] = (mm%4) ? d[mm-k-1] : (d[mm-k-1]+d[mm-k])/2.;
	}
	delete []d;

	long pal;
	gr->SetPenPal(pen,&pal);	gr->NextColor(pal);

	const long kq = gr->AllocPnts(18*n);
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{
		const double vv = x->v(i);
		const double dd = i<nx-1 ? x->v(i+1)-vv : vv-x->v(i-1);
		const double x1 = vv + dd/2*(dv-gr->BarWidth);
		const double x2 = x1 + gr->BarWidth*dd;
		const double c = sh ? gr->NextColor(pal,i):gr->CDef;
		const long iq = kq+18*i;

		gr->AddPntQ(iq,mglPoint(x1,b[i],zVal),c);	// horizontal lines
		gr->AddPntQ(iq+1,mglPoint(x2,b[i],zVal),c);
		gr->AddPntQ(iq+2,mglPoint(x1,b[i+n],zVal),c);
		gr->AddPntQ(iq+3,mglPoint(x2,b[i+n],zVal),c);
		gr->AddPntQ(iq+4,mglPoint(x1,b[i+2*n],zVal),c);
		gr->AddPntQ(iq+5,mglPoint(x2,b[i+2*n],zVal),c);
		gr->AddPntQ(iq+6,mglPoint(x1,b[i+3*n],zVal),c);
		gr->AddPntQ(iq+7,mglPoint(x2,b[i+3*n],zVal),c);
		gr->AddPntQ(iq+8,mglPoint(x1,b[i+4*n],zVal),c);
		gr->AddPntQ(iq+9,mglPoint(x2,b[i+4*n],zVal),c);

		gr->AddPntQ(iq+10,mglPoint(x1,b[i+n],zVal),c);	//vertical lines
		gr->AddPntQ(iq+11,mglPoint(x1,b[i+3*n],zVal),c);
		gr->AddPntQ(iq+12,mglPoint(x2,b[i+n],zVal),c);
		gr->AddPntQ(iq+13,mglPoint(x2,b[i+3*n],zVal),c);
		gr->AddPntQ(iq+14,mglPoint((x1+x2)/2,b[i],zVal),c);
		gr->AddPntQ(iq+15,mglPoint((x1+x2)/2,b[i+n],zVal),c);
		gr->AddPntQ(iq+16,mglPoint((x1+x2)/2,b[i+3*n],zVal),c);
		gr->AddPntQ(iq+17,mglPoint((x1+x2)/2,b[i+4*n],zVal),c);
	}
	for(long i=0;i<n;i++)	for(long j=0;j<9;j++)
		gr->line_plot(kq+18*i+2*j, kq+18*i+2*j+1);
	delete []b;	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxplot(HMGL gr, HCDT y, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx()+1);	x.Fill(gr->Min.x,gr->Max.x);
	mgl_boxplot_xy(gr,&x,y,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxplot_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_boxplot_xy(_GR_,_DA_(x),_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxplot_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_boxplot(_GR_,_DA_(y),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//		Error series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error_exy(HMGL gr, HCDT x, HCDT y, HCDT ex, HCDT ey, const char *pen, const char *opt)
{
	long m,n=ey->GetNx(),pal;
	if(mgl_check_dim0(gr,x,y,ey,ex,"Error"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Error",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	m = ex->GetNy() > m ? ex->GetNy() : m;
	m = ey->GetNy() > m ? ey->GetNy() : m;
	bool sh = mglchr(pen,'!');

	bool ma = mglchr(pen,'@');
	char mk = gr->SetPenPal(pen,&pal);
	double zVal=gr->AdjustZMin();
	gr->Reserve(5*n*m);
	if(ma && (mk==0 || !strchr("PXsSdD+xoOC",mk) ))	mk = 'S';
	gr->ResetMask();
	mglPoint q(NAN,NAN);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		long m1 = j<ex->GetNy() ? j:0,m2 = j<ey->GetNy() ? j:0;
		gr->NextColor(pal);
		if(ma)
		{
			if(strchr("PXsS",mk))	// boundary of square
			{
				const long kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx-ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx+ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy-vf, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{
					long iq = kq+4*i;
					gr->line_plot(iq,iq+1);		gr->line_plot(iq,iq+2);
					gr->line_plot(iq+3,iq+1);	gr->line_plot(iq+3,iq+2);
				}
			}
			if(strchr("dD",mk))
			{
				const long kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)	// boundary of rhomb
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{
					long iq = kq+4*i;
					gr->line_plot(iq,iq+1);		gr->line_plot(iq+1,iq+2);
					gr->line_plot(iq+2,iq+3);	gr->line_plot(iq+3,iq);
				}
			}
			if(strchr("oOC",mk))
			{
				const long kq = gr->AllocPnts(40*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)	// circle
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					for(long k=0;k<40;k++)
						gr->AddPntQ(kq+40*i+k,mglPoint(vx+ve*mgl_cos[(18*k)%360],
								vy+vf*mgl_cos[(270+18*k)%360], zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)	// circle
				{
					long iq = kq+40*i;	gr->line_plot(iq+39,iq);
					for(long k=1;k<40;k++)	gr->line_plot(iq+k-1,iq+k);
				}
			}
			long kq;
			switch(mk)
			{
			case 'P':	case '+':
				kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{	long iq = kq+4*i;
					gr->line_plot(iq,iq+2);	gr->line_plot(iq+1,iq+3);
				}	break;
			case 'X':	case 'x':
				kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx-ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx+ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy-vf, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{	long iq = kq+4*i;
					gr->line_plot(iq,iq+3);	gr->line_plot(iq+1,iq+2);
				}	break;
			case 'S':
				kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx-ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx+ve, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy-vf, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{	long iq = kq+4*i;	gr->quad_plot(iq,iq+1,iq+2,iq+3);	}
				break;
			case 'D':
				kq = gr->AllocPnts(4*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					gr->AddPntQ(kq+4*i,mglPoint(vx, vy+vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+1,mglPoint(vx-ve, vy, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+2,mglPoint(vx, vy-vf, zVal),c,q,-1,27);
					gr->AddPntQ(kq+4*i+3,mglPoint(vx+ve, vy, zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)
				{	long iq = kq+4*i;	gr->quad_plot(iq,iq+3,iq+1,iq+2);	}
				break;
			case 'O':
				kq = gr->AllocPnts(41*n);
#pragma omp parallel for
				for(long i=0;i<n;i++)	// circle
				{
					double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
					double c = sh ? gr->NextColor(pal,i):gr->CDef;
					long iq = kq+41*i+1;
					gr->AddPntQ(iq-1,mglPoint(vx,vy,zVal),c);
					for(long k=0;k<40;k++)
						gr->AddPntQ(iq+k,mglPoint(vx+ve*mgl_cos[(18*k)%360],
								vy+vf*mgl_cos[(270+18*k)%360], zVal),c,q,-1,27);
				}
				for(long i=0;i<n;i++)	// circle
				{
					long iq = kq+41*i+1;	gr->trig_plot(iq-1,iq+39,iq);
					for(long k=1;k<40;k++)	gr->trig_plot(iq-1,iq+k-1,iq+k);
				}	break;
			case 'C':
				for(long i=0;i<n;i++)
				{
					gr->mark_plot(gr->AddPnt(mglPoint(x->v(i,mx),y->v(i,my),zVal),-1,q,-1,3), '.');
					if(sh)	gr->NextColor(pal);
				}
			}
		}
		else
		{
			const long nq = mk?5:4, kq = gr->AllocPnts(nq*n);
#pragma omp parallel for
			for(long i=0;i<n;i++)
			{
				double vx=x->v(i,mx), ve=ex->v(i,m1), vy=y->v(i,my), vf=ey->v(i,m2);
				double c = sh ? gr->NextColor(pal,i):gr->CDef;
				long iq = kq + nq*i;
				gr->AddPntQ(iq,mglPoint(vx, vy+vf, zVal),c,q,-1,27);
				gr->AddPntQ(iq+1,mglPoint(vx, vy-vf, zVal),c,q,-1,27);
				gr->AddPntQ(iq+2,mglPoint(vx+ve, vy, zVal),-1,q,c,27);
				gr->AddPntQ(iq+3,mglPoint(vx-ve, vy, zVal),-1,q,c,27);
				if(mk)	gr->AddPntQ(iq+4,mglPoint(vx,vy,zVal),c);
			}
			for(long i=0;i<n;i++)
			{
				long iq = kq + nq*i;
				if(mk)	gr->mark_plot(iq+4, mk);
				gr->line_plot(iq,iq+1);			gr->line_plot(iq+2,iq+3);
				gr->arrow_plot(iq,iq+1,'I');	gr->arrow_plot(iq+1,iq,'I');
				gr->arrow_plot(iq+2,iq+3,'I');	gr->arrow_plot(iq+3,iq+2,'I');
			}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error_xy(HMGL gr, HCDT x, HCDT y, HCDT ey, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV ex(y->GetNx());	ex.Fill(NAN);
	mgl_error_exy(gr,x,y,&ex,ey,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error(HMGL gr, HCDT y, HCDT ey, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx()), ex(y->GetNx());
	x.Fill(gr->Min.x,gr->Max.x);	ex.Fill(NAN);
	mgl_error_exy(gr,&x,y,&ex,ey,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error_(uintptr_t *gr, uintptr_t *y, uintptr_t *ey, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_error(_GR_,_DA_(y),_DA_(ey),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ey, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_error_xy(_GR_,_DA_(x),_DA_(y),_DA_(ey),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_error_exy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ex, uintptr_t *ey, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_error_exy(_GR_,_DA_(x),_DA_(y),_DA_(ex),_DA_(ey),s,o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//		Chart series
//
//-----------------------------------------------------------------------------
void face_plot(mglBase *gr, mglPoint o, mglPoint d1, mglPoint d2, double c, bool wire)
{
	const int num=10;
	mglPoint nn=d1^d2;
	d1 = d1/num;	d2 = d2/num;
	const long n=num+1, kq = gr->AllocPnts(n*n);
#pragma omp parallel for
	for(long j=0;j<n;j++)	for(long i=0;i<n;i++)
		gr->AddPntQ(kq+i+n*j,o+d1*i+d2*j,c,nn);
	for(long j=0;j<num;j++)	for(long i=0;i<num;i++)
	{	long ii = kq+i+n*j;	gr->quad_plot(ii,ii+1,ii+n,ii+n+1);	}
	if(wire)
	{
		gr->SetPenPal("k-");
		const long jq = gr->AllocPnts(4*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			gr->CopyNtoC(jq+4*i,kq+i,gr->CDef);
			gr->CopyNtoC(jq+4*i+1,kq+n*i,gr->CDef);
			gr->CopyNtoC(jq+4*i+2,kq+n*n-1-i,gr->CDef);
			gr->CopyNtoC(jq+4*i+3,kq+n*n-1-n*i,gr->CDef);
		}
		for(long i=0;i<num;i++)
		{
			long jj = jq+4*i;
			gr->line_plot(jj+4,jj);
			gr->line_plot(jj+5,jj+1);
			gr->line_plot(jj+6,jj+2);
			gr->line_plot(jj+7,jj+3);
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_chart(HMGL gr, HCDT a, const char *cols, const char *opt)
{
	if(a->Minimal()<0)	{	gr->SetWarn(mglWarnNeg,"Chart");	return;	}
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Chart",cgid++);
	bool wire = mglchr(cols,'#');	// draw edges
	long n=a->GetNx(),i,j=0,len=cols?long(strlen(cols)):0;
	if(cols)	for(i=0;i<len;i++)
		if(strchr(MGL_COLORS,cols[i]) || cols[i]==' ')	j++;
	if(j==0)	{	cols = MGL_DEF_PAL;	len=long(strlen(cols));	}
	double *c = new double[len+1],cc;
	long nc=0;			// number of colors
	for(i=0;i<len;i++)
		if(strchr(MGL_COLORS,cols[i]) || cols[i]==' ')
		{	c[nc]=gr->AddTexture(cols[i]);	nc++;	}
	// NOTE: nc>0 since j>0 or MGL_DEF_PAL is not empty

	double dy = (gr->Max.y-gr->Min.y)/a->GetNy(), dx, ss, cs, x1, y1, dz=gr->Max.z-gr->Min.z, vv;
	mglPoint d1,d2,o;

	for(j=0;j<a->GetNy();j++)
	{
		if(gr->NeedStop())	break;
		y1 = gr->Min.y + dy*j;
		for(i=0,ss=0;i<n;i++)	ss += a->v(i,j);
		if(ss==0)	continue;
		for(cs=0,i=0;i<n;i++)
		{
			vv = a->v(i,j);	dx = vv/ss;	cc = c[i%nc];
			if(dx==0)	continue;
			x1 = gr->Min.x + (gr->Max.x-gr->Min.x)*cs/ss;	dx *= (gr->Max.x-gr->Min.x);
			if(cc>=0)
			{
				face_plot(gr,mglPoint(x1,y1,gr->Min.z),mglPoint(dx,0,0),mglPoint(0,0,dz),cc,wire);
				face_plot(gr,mglPoint(x1,y1,gr->Min.z),mglPoint(dx,0,0),mglPoint(0,dy,0),cc,wire);
				face_plot(gr,mglPoint(x1,y1,gr->Min.z),mglPoint(0,dy,0),mglPoint(0,0,dz),cc,wire);

				face_plot(gr,mglPoint(x1+dx,y1+dy,gr->Max.z),mglPoint(-dx,0,0),mglPoint(0,0,-dz),cc,wire);
				face_plot(gr,mglPoint(x1+dx,y1+dy,gr->Max.z),mglPoint(-dx,0,0),mglPoint(0,-dy,0),cc,wire);
				face_plot(gr,mglPoint(x1+dx,y1+dy,gr->Max.z),mglPoint(0,-dy,0),mglPoint(0,0,-dz),cc,wire);
			}
			cs += vv;
		}
	}
	gr->EndGroup();	delete []c;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_chart_(uintptr_t *gr, uintptr_t *a, const char *col, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,col,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_chart(_GR_, _DA_(a), s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Mark series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *pen, const char *opt)
{
	long m,n=y->GetNx(),pal;
	if(mgl_check_dim0(gr,x,y,z,r,"Mark"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Mark",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	m = z->GetNy() > m ? z->GetNy() : m;
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(n*m);
	if(mk==0)	mk='.';
	bool sh = mglchr(pen,'!');

	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n, dx = n>d?n/d:1;
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		long mz = j<z->GetNy() ? j:0, mr = j<r->GetNy() ? j:0;
		const long kq = gr->AllocPnts(n);
#pragma omp parallel for
		for(long i=0;i<n;i+=dx)
		{
			double c = sh ? gr->NextColor(pal,i):gr->CDef;
			gr->AddPntQ(kq+i,mglPoint(x->v(i,mx),y->v(i,my),z->v(i,mz)),c);
		}
		for(long i=0;i<n;i+=dx)		gr->mark_plot(kq+i, mk, fabs(r->v(i,mr)));
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_xy(HMGL gr, HCDT x, HCDT y, HCDT r, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_mark_xyz(gr,x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_y(HMGL gr, HCDT y, HCDT r, const char *pen, const char *opt)
{
	long n=y->GetNx();
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_mark_xyz(gr,&x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_mark_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(r),s,o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_mark_xy(_GR_, _DA_(x), _DA_(y), _DA_(r),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mark_y_(uintptr_t *gr, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_mark_y(_GR_,_DA_(y),_DA_(r),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Tube series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyzr(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *pen, const char *opt)
{
	long m,n=y->GetNx(),pal;
	if(mgl_check_dim1(gr,x,y,z,r,"Tube"))	return;

	mreal rnum = gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Tube",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	m = z->GetNy() > m ? z->GetNy() : m;
	m = r->GetNy() > m ? r->GetNy() : m;
	bool sh = mglchr(pen,'!');
	bool wire = mglchr(pen,'#');

	int num = rnum>2?rnum:!(gr->GetQuality()&3)?13:25;
	gr->SetPenPal(pen,&pal);	gr->Reserve(n*m*num);
	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		gr->NextColor(pal);
		const long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		const long mz = j<z->GetNy() ? j:0, mr = j<r->GetNy() ? j:0;
		const long kq = gr->AllocPnts(n*num);
#pragma omp parallel for collapse(2)
		for(long i=0;i<n;i++)	for(long k=0;k<num;k++)
		{
			const mglPoint l(x->dvx(i,mx),y->dvx(i,my),z->dvx(i,mz));
			mglPoint t(!l);		t.Normalize();
			mglPoint u(t^l);	u.Normalize();
			const mglPoint q(x->v(i,mx),y->v(i,my),z->v(i,mz));
			const double rr=r->v(i,mr), dr=r->dvx(i,mr);
			const double c = sh ? gr->NextColor(pal,i):gr->CDef;

			const int kk = k*360/(num-1);
			const float  co = mgl_cos[kk%360], si = mgl_cos[(270+kk)%360];
			const mglPoint p(q + t*(rr*co) + u*(rr*si));
			const mglPoint d((t*si - u*co)^(l + t*(dr*co) + u*(dr*si)));
			gr->AddPntQ(kq+num*i+k, p,c,wire?mglPoint(NAN,NAN):d,-1,3);
		}
		if(!wire)	for(long i=1;i<n;i++)	for(long k=1;k<num;k++)
		{	long jj=kq+num*i+k;	gr->quad_plot(jj,jj-1,jj-num,jj-num-1);	}
		if(wire)
		{
			for(long i=1;i<n;i++)	for(long k=0;k<num;k+=4)
			{	long jj=kq+num*i+k;	gr->line_plot(jj,jj-num);	}
			for(long i=0;i<n;i++)	for(long k=1;k<num;k++)
			{	long jj=kq+num*i+k;	gr->line_plot(jj,jj-1);		}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyr(HMGL gr, HCDT x, HCDT y, HCDT r, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_tube_xyzr(gr,x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_r(HMGL gr, HCDT y, HCDT r, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Tube");	return;	}
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_tube_xyzr(gr,&x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube(HMGL gr, HCDT y, double rr, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Tube");	return;	}
	gr->SaveState(opt);
	mglDataV x(n), r(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);
	r.Fill(rr);	z.Fill(gr->AdjustZMin());
	mgl_tube_xyzr(gr,&x,y,&z,&r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xy(HMGL gr, HCDT x, HCDT y, double rr, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Tube");	return;	}
	gr->SaveState(opt);
	mglDataV r(n), z(n);
	r.Fill(rr);	z.Fill(gr->AdjustZMin());
	mgl_tube_xyzr(gr,x,y,&z,&r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, double rr, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV r(y->GetNx());	r.Fill(rr);
	mgl_tube_xyzr(gr,x,y,z,&r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyzr_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube_xyzr(_GR_,_DA_(x),_DA_(y),_DA_(z), _DA_(r),s,o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyr_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube_xyr(_GR_,_DA_(x),_DA_(y),_DA_(r),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_r_(uintptr_t *gr, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube_r(_GR_,_DA_(y),_DA_(r),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, mreal *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube_xyz(_GR_,_DA_(x),_DA_(y),_DA_(z),*r,s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, mreal *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube_xy(_GR_,_DA_(x),_DA_(y),*r,s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tube_(uintptr_t *gr, uintptr_t *y, mreal *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tube(_GR_,_DA_(y),*r,s,o);
	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Tape series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt)
{
	long m,n=y->GetNx(),pal;
	if(mgl_check_dim1(gr,x,y,z,0,"Tape"))	return;

	static int cgid=1;	gr->StartGroup("Tape",cgid++);
	double rr = gr->SaveState(opt);
	if(rr==0 || mgl_isnan(rr))	rr = mgl_norm(gr->Max-gr->Min)*gr->BarWidth/25;
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();	m = z->GetNy() > m ? z->GetNy() : m;
	gr->SetPenPal(pen,&pal);	gr->Reserve(4*n*m);
	mglPoint qn(NAN,NAN);
	bool sh = mglchr(pen,'!'), xo = mglchr(pen,'x'), zo = mglchr(pen,'z'), wire = mglchr(pen,'#');
	if(!xo && !zo)	xo = zo = true;
	int nv = xo && zo ? 4:2;

	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		double c1=gr->NextColor(pal), c2=c1;
		if(gr->GetNumPal(pal)==2*m && !sh)	c2 = gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0, mz = j<z->GetNy() ? j:0;
		const long kq = gr->AllocPnts(nv*n);
// TODO: use AddPntQ() -- problem is vector q1
		// initial values for normales
		mglPoint p2(x->v(0,mx), y->v(0,my), z->v(0,mz));
		mglPoint l(x->v(1,mx)-p2.x, y->v(1,my)-p2.y, z->v(1,mz)-p2.z);	l /= mgl_norm(l);
		mglPoint q1(-l.y,l.x,0);
		double ll = mgl_norm(q1);
		if(ll)	q1 /= ll;	else	q1.Set(0,1,0);
		mglPoint q2(q1^l);
		if(xo&&zo)
		{
			gr->AddPntQ(kq,p2,c1,q2,-1,3);		gr->AddPntQ(kq+1,p2+rr*q1,c1,q2,-1,3);
			gr->AddPntQ(kq+2,p2,c2,q1,-1,3);	gr->AddPntQ(kq+3,p2+rr*q2,c2,q1,-1,3);
		}
		else if(xo)	{	gr->AddPntQ(kq,p2,c1,q2,-1,3);	gr->AddPntQ(kq+1,p2+rr*q1,c1,q2,-1,3);	}
		else		{	gr->AddPntQ(kq,p2,c2,q1,-1,3);	gr->AddPntQ(kq+1,p2+rr*q2,c2,q1,-1,3);	}
		for(long i=1;i<n;i++)
		{
			mglPoint p1 = p2;
			p2.Set(x->v(i,mx), y->v(i,my), z->v(i,mz));
			l = p2-p1;		l /= mgl_norm(l);
			q1 -= l*(l*q1);	q1/= mgl_norm(q1);
			q2 = (q1^l);	// NOTE: not thread safe!!!

			if(sh)	c2=c1=gr->NextColor(pal,i);	// NOTE: not thread safe
			long iq = kq+nv*i;
			if(xo&&zo)
			{
				gr->AddPntQ(iq,p2,c1,q2,-1,3);		gr->AddPntQ(iq+1,p2+rr*q1,c1,q2,-1,3);
				gr->AddPntQ(iq+2,p2,c2,q1,-1,3);	gr->AddPntQ(iq+3,p2+rr*q2,c2,q1,-1,3);
			}
			else if(xo)	{	gr->AddPntQ(iq,p2,c1,q2,-1,3);	gr->AddPntQ(iq+1,p2+rr*q1,c1,q2,-1,3);	}
			else		{	gr->AddPntQ(iq,p2,c2,q1,-1,3);	gr->AddPntQ(iq+1,p2+rr*q2,c2,q1,-1,3);	}
		}
		if(wire)
		{
			if(xo&&zo)	for(long i=1;i<n;i++)
			{	long iq = kq+nv*i+1;	gr->line_plot(iq-nv,iq);	gr->line_plot(iq-nv+2,iq+2);	}
			else	for(long i=1;i<n;i++)
			{	long iq = kq+nv*i+1;	gr->line_plot(iq-nv,iq);	}
		}
		else
		{
			if(xo&&zo)	for(long i=1;i<n;i++)
			{	long iq = kq+nv*i;
				gr->quad_plot(iq,iq+1,iq-nv,iq-nv+1);	gr->quad_plot(iq+2,iq+3,iq-nv+2,iq-nv+3);	}
			else	for(long i=1;i<n;i++)
			{	long iq = kq+nv*i;	gr->quad_plot(iq,iq+1,iq-nv,iq-nv+1);	}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape_xy(HMGL gr, HCDT x, HCDT y, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_tape_xyz(gr,x,y,&z,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape(HMGL gr, HCDT y, const char *pen, const char *opt)
{
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,"Plot");	return;	}
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_tape_xyz(gr,&x,y,&z,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tape_xyz(_GR_, _DA_(x),_DA_(y),_DA_(z),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tape_xy(_GR_, _DA_(x),_DA_(y),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tape_(uintptr_t *gr, uintptr_t *y,	const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tape(_GR_, _DA_(y),s,o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Pmap series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *pen, const char *opt)
{
	long m,n=y->GetNx(),pal;
	if(mgl_check_dim0(gr,x,y,z,r,"Mark"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Mark",cgid++);
	m = x->GetNy() > y->GetNy() ? x->GetNy() : y->GetNy();
	m = z->GetNy() > m ? z->GetNy() : m;
	char mk=gr->SetPenPal(pen,&pal);	gr->Reserve(n*m);
	if(mk==0)	mk='.';

	for(long j=0;j<m;j++)
	{
		if(gr->NeedStop())	break;
		gr->NextColor(pal);
		long mx = j<x->GetNy() ? j:0, my = j<y->GetNy() ? j:0;
		long mz = j<z->GetNy() ? j:0, mr = j<r->GetNy() ? j:0;
		for(long i=0;i<n-1;i++)	// NOTE: AddPntQ() is useless due to rare points
		{
			double r1=r->v(i,mr), r2 = r->v(i+1,mr);
			if(r1==0)	gr->mark_plot(gr->AddPnt(mglPoint(x->v(i,mx),y->v(i,my),z->v(i,mz))), mk);
			if(r1*r2<0)
			{
				double d = r1/(r1-r2);
				mglPoint p(x->v(i,mx)*(1-d)+x->v(i+1,mx)*d, y->v(i,my)*(1-d)+y->v(i+1,my)*d, z->v(i,mz)*(1-d)+d*z->v(i+1,mz));
				gr->mark_plot(gr->AddPnt(p), mk);
			}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap_xy(HMGL gr, HCDT x, HCDT y, HCDT r, const char *pen, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_pmap_xyz(gr,x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap(HMGL gr, HCDT y, HCDT r, const char *pen, const char *opt)
{
	long n=y->GetNx();
	gr->SaveState(opt);
	mglDataV x(n), z(n);
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_pmap_xyz(gr,&x,y,&z,r,pen,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_pmap_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(r),s,o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_pmap_xy(_GR_, _DA_(x), _DA_(y), _DA_(r),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pmap_(uintptr_t *gr, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_pmap(_GR_,_DA_(y),_DA_(r),s,o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
