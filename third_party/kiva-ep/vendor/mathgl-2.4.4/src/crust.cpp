/***************************************************************************
 * crust.cpp is part of Math Graphic Library
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
#include <float.h>
#include <math.h>
#include <list>
#include <limits>
#include "mgl2/other.h"
#include "mgl2/data.h"
#include "mgl2/thread.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//	TriPlot series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	long n = x->GetNN(), m = nums->GetNy();
	if(mgl_check_trig(gr,nums,x,y,z,a,"TriPlot"))	return;

	long ss=gr->AddTexture(sch);
	gr->SaveState(opt);	gr->SetPenPal("-");
	static int cgid=1;	gr->StartGroup("TriPlot",cgid++);

	bool wire = mglchr(sch,'#');
	long nc = a->GetNN();
	if(nc!=n && nc>=m)	// colors per triangle
	{
		mglPoint p1,p2,p3,q;
		long kq = gr->AllocPnts(m*3);
#pragma omp parallel for
		for(long i=0;i<m;i++)
		{
			if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0)
			{
				long k1 = long(nums->v(0,i)+0.5);
				mglPoint p1(x->v(k1), y->v(k1), z->v(k1));
				long k2 = long(nums->v(1,i)+0.5);
				mglPoint p2(x->v(k2), y->v(k2), z->v(k2));
				long k3 = long(nums->v(2,i)+0.5);
				mglPoint p3(x->v(k3), y->v(k3), z->v(k3));
				mglPoint q(wire ? mglPoint(NAN,NAN) : (p2-p1) ^ (p3-p1));
				mreal cc = a->v(i);
				gr->AddPntQ(kq+3*i,p1,gr->GetC(ss,cc),q);
				gr->AddPntQ(kq+3*i+1,p2,gr->GetC(ss,cc),q);
				gr->AddPntQ(kq+3*i+2,p3,gr->GetC(ss,cc),q);
			}
			else
			{	gr->SetPntOff(kq+3*i);	gr->SetPntOff(kq+3*i+1);	gr->SetPntOff(kq+3*i+2);	}
		}
		if(wire)	for(long i=0;i<m;i++)
		{
			gr->line_plot(kq+3*i,kq+3*i+1);
			gr->line_plot(kq+3*i+1,kq+3*i+2);
			gr->line_plot(kq+3*i+2,kq+3*i);
		}
		else	for(long i=0;i<m;i++)	gr->trig_plot(kq+3*i,kq+3*i+1,kq+3*i+2);
	}
	else if(nc>=n)		// colors per point
	{
		mglPoint *pp = new mglPoint[n];
		for(long i=0;i<m;i++)	if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0)	// add averaged normales
		{
			long k1 = long(nums->v(0,i)+0.5);
			long k2 = long(nums->v(1,i)+0.5);
			long k3 = long(nums->v(2,i)+0.5);
			if(!wire)
			{
				mglPoint q(mglPoint(x->v(k2)-x->v(k1), y->v(k2)-y->v(k1), z->v(k2)-z->v(k1)) ^
					mglPoint(x->v(k3)-x->v(k1), y->v(k3)-y->v(k1), z->v(k3)-z->v(k1)));
				q.Normalize();
				// try be sure that in the same direction ...
				if(q.z<0)	q *= -1;
				pp[k1] += q;	pp[k2] += q;	pp[k3] += q;
			}
			else	pp[k1]=pp[k2]=pp[k3]=mglPoint(NAN,NAN);
		}
		long kq = gr->AllocPnts(n);
#pragma omp parallel for
		for(long i=0;i<n;i++)	// add points
			gr->AddPntQ(kq+i, mglPoint(x->v(i), y->v(i), z->v(i)), gr->GetC(ss,a->v(i)), pp[i]);
		for(long i=0;i<m;i++)	if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0)	// draw triangles
		{
			long k1 = long(nums->v(0,i)+0.5);
			long k2 = long(nums->v(1,i)+0.5);
			long k3 = long(nums->v(2,i)+0.5);
			if(wire)
			{
				gr->line_plot(kq+k1,kq+k2);	gr->line_plot(kq+k1,kq+k3);
				gr->line_plot(kq+k3,kq+k2);
			}
			else	gr->trig_plot(kq+k1,kq+k2,kq+k3);
		}
		delete []pp;
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xyz(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_triplot_xyzc(gr,nums,x,y,z,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xy(HMGL gr, HCDT nums, HCDT x, HCDT y, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglData z(x->GetNN());
	mreal zm = gr->AdjustZMin();	z.Fill(zm,zm);
	mgl_triplot_xyzc(gr,nums,x,y,&z,&z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_triplot_xyzc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xyz_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_triplot_xyz(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_triplot_xy_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_triplot_xy(_GR_, _DA_(nums), _DA_(x), _DA_(y), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	QuadPlot series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	long n = x->GetNN(), m = nums->GetNy();
	if(mgl_check_trig(gr,nums,x,y,z,a,"QuadPlot",4))	return;

	long ss=gr->AddTexture(sch);
	gr->SaveState(opt);	gr->SetPenPal("-");
	static int cgid=1;	gr->StartGroup("QuadPlot",cgid++);
	mglPoint p1,p2,p3,p4;

	long nc = a->GetNN();
	bool wire = mglchr(sch,'#');
	if(nc!=n && nc>=m)	// colors per triangle
	{
		long kq = gr->AllocPnts(m*4);
#pragma omp parallel for
		for(long i=0;i<m;i++)
		{
			if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0 && nums->v(3,i)>=0)
			{
				long k1 = long(nums->v(0,i)+0.5);
				p1.Set(x->v(k1), y->v(k1), z->v(k1));
				long k2 = long(nums->v(1,i)+0.5);
				p2.Set(x->v(k2), y->v(k2), z->v(k2));
				long k3 = long(nums->v(2,i)+0.5);
				p3.Set(x->v(k3), y->v(k3), z->v(k3));
				long k4 = long(nums->v(3,i)+0.5);
				p4.Set(x->v(k4), y->v(k4), z->v(k4));
				mglPoint q = wire ? mglPoint(NAN,NAN):(p2-p1) ^ (p3-p1);
				mreal cc = a->v(i);
				gr->AddPntQ(kq+4*i,p1,gr->GetC(ss,cc),q);
				gr->AddPntQ(kq+4*i+1,p2,gr->GetC(ss,cc),q);
				gr->AddPntQ(kq+4*i+2,p3,gr->GetC(ss,cc),q);
				gr->AddPntQ(kq+4*i+3,p4,gr->GetC(ss,cc),q);
			}
			else
			{	gr->SetPntOff(kq+4*i);		gr->SetPntOff(kq+4*i+1);
				gr->SetPntOff(kq+4*i+1);	gr->SetPntOff(kq+4*i+3);	}
		}
		if(wire)	for(long i=0;i<m;i++)
		{
			gr->line_plot(kq+3*i,kq+3*i+1);
			gr->line_plot(kq+3*i+1,kq+3*i+2);
			gr->line_plot(kq+3*i+2,kq+3*i);
		}
		else	for(long i=0;i<m;i++)
			gr->quad_plot(kq+4*i,kq+4*i+1,kq+4*i+2,kq+4*i+3);

	}
	else if(nc>=n)		// colors per point
	{
		long *kk = new long[n];
		mglPoint *pp = new mglPoint[n];
		for(long i=0;i<m;i++)	if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0 && nums->v(3,i)>=0)
		{	// add averaged normales
			long k1 = long(nums->v(0,i)+0.5);
			p1.Set(x->v(k1), y->v(k1), z->v(k1));
			long k2 = long(nums->v(1,i)+0.5);
			p2.Set(x->v(k2), y->v(k2), z->v(k2));
			long k3 = long(nums->v(2,i)+0.5);
			p3.Set(x->v(k3), y->v(k3), z->v(k3));
			long k4 = long(nums->v(3,i)+0.5);
			p4.Set(x->v(k4), y->v(k4), z->v(k4));

			if(wire)	pp[k1]=pp[k2]=pp[k3]=pp[k4]=mglPoint(NAN,NAN);
			else
			{
				mglPoint q1 = (p2-p1) ^ (p3-p1);	if(q1.z<0) q1*=-1;
				mglPoint q2 = (p2-p4) ^ (p3-p4);	if(q2.z<0) q2*=-1;
				mglPoint q3 = (p1-p2) ^ (p4-p2);	if(q3.z<0) q3*=-1;
				mglPoint q4 = (p1-p4) ^ (p4-p3);	if(q4.z<0) q4*=-1;
				pp[k1] += q1;	pp[k2] += q2;	pp[k3] += q3;	pp[k4] += q4;
			}
		}
		long kq = gr->AllocPnts(n);
#pragma omp parallel for
		for(long i=0;i<n;i++)	// add points
			gr->AddPntQ(kq+i, mglPoint(x->v(i), y->v(i), z->v(i)),gr->GetC(ss,a->v(i)), pp[i]);
		for(long i=0;i<m;i++)	if(nums->v(0,i)>=0 && nums->v(1,i)>=0 && nums->v(2,i)>=0 && nums->v(3,i)>=0)
		{	// draw quads
			long k1 = long(nums->v(0,i)+0.5);
			long k2 = long(nums->v(1,i)+0.5);
			long k3 = long(nums->v(2,i)+0.5);
			long k4 = long(nums->v(3,i)+0.5);
			if(wire)
			{
				gr->line_plot(kq+k1,kq+k2);	gr->line_plot(kq+k1,kq+k3);
				gr->line_plot(kq+k4,kq+k2);	gr->line_plot(kq+k4,kq+k3);
			}
			else	gr->quad_plot(kq+k1,kq+k2,kq+k3,kq+k4);
		}
		delete []kk;	delete []pp;
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xyz(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_quadplot_xyzc(gr,nums,x,y,z,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xy(HMGL gr, HCDT nums, HCDT x, HCDT y, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglData z(x->GetNN());	z.Fill(gr->Min.z,gr->Min.z);
	mgl_quadplot_xyzc(gr,nums,x,y,&z,&z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_quadplot_xyzc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xyz_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_quadplot_xyzc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(z), s, o);
	delete []o;	delete []s;}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_quadplot_xy_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_quadplot_xy(_GR_, _DA_(nums), _DA_(x), _DA_(y), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	TriCont series
//
//-----------------------------------------------------------------------------
#include "cont.hpp"
//-----------------------------------------------------------------------------
std::vector<mglSegment> MGL_NO_EXPORT mgl_tri_lines(mreal val, HCDT nums, HCDT a, HCDT x, HCDT y, HCDT z)
{
	long n = x->GetNN(), m = nums->GetNy();
	std::vector<mglSegment> lines;
	for(long i=0;i<m;i++)
	{
		long k1 = long(nums->v(0,i)+0.5), k2 = long(nums->v(1,i)+0.5), k3 = long(nums->v(2,i)+0.5);
		if(k1<0 || k1>=n || k2<0 || k2>=n || k3<0 || k3>=n)	continue;
		mreal v1 = a->v(k1), v2 = a->v(k2), v3 = a->v(k3);
		mreal d1 = mgl_d(val,v1,v2), d2 = mgl_d(val,v1,v3), d3 = mgl_d(val,v2,v3);
		mglSegment line;
		if(d1>=0 && d1<=1 && d2>=0 && d2<=1)
		{
			line.p1.Set(x->v(k1)*(1-d1)+x->v(k2)*d1, y->v(k1)*(1-d1)+y->v(k2)*d1, z->v(k1)*(1-d1)+z->v(k2)*d1);
			line.p2.Set(x->v(k1)*(1-d2)+x->v(k3)*d2, y->v(k1)*(1-d2)+y->v(k3)*d2, z->v(k1)*(1-d2)+z->v(k3)*d2);
		}
		else if(d1>=0 && d1<=1 && d3>=0 && d3<=1)
		{
			line.p1.Set(x->v(k1)*(1-d1)+x->v(k2)*d1, y->v(k1)*(1-d1)+y->v(k2)*d1, z->v(k1)*(1-d1)+z->v(k2)*d1);
			line.p2.Set(x->v(k2)*(1-d3)+x->v(k3)*d3, y->v(k2)*(1-d3)+y->v(k3)*d3, z->v(k2)*(1-d3)+z->v(k3)*d3);
		}
		else if(d3>=0 && d3<=1 && d2>=0 && d2<=1)
		{
			line.p1.Set(x->v(k1)*(1-d2)+x->v(k3)*d2, y->v(k1)*(1-d2)+y->v(k3)*d2, z->v(k1)*(1-d2)+z->v(k3)*d2);
			line.p2.Set(x->v(k2)*(1-d3)+x->v(k3)*d3, y->v(k2)*(1-d3)+y->v(k3)*d3, z->v(k2)*(1-d3)+z->v(k3)*d3);
		}
		if(line.p1!=line.p2)	lines.push_back(line);
	}
	return lines;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyzcv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mglDataV zz(x->GetNN());
	if(!z)	z = &zz;
	if(mgl_check_trig(gr,nums,x,y,z,a,"TriCont"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("TriCont",cgid++);
	int text=0;
	if(mglchr(sch,'t'))	text=1;
	if(mglchr(sch,'T'))	text=2;
	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	long s=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	for(long k=0;k<v->GetNx();k++)
	{
		mreal v0 = v->v(k);		zz.Fill(fixed ? gr->Min.z : v0);
		mgl_draw_curvs(gr,v0,gr->GetC(s,v0),text,mgl_get_curvs(gr,mgl_tri_lines(v0,nums,a,x,y,fixed?&zz:z)));
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 7:long(r+0.5);
	mglData v(n);
	for(long i=0;i<n;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(n+1);
	mgl_tricont_xyzcv(gr,&v,nums,x,y,z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_tricont_xyzc(gr,nums,x,y,0,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xycv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_tricont_xyzcv(gr,v,nums,x,y,0,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyzcv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricont_xyzcv(_GR_, _DA_(v), _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xycv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricont_xycv(_GR_, _DA_(v), _DA_(nums), _DA_(x), _DA_(y), _DA_(z), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricont_xyzc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricont_xyc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricont_xyc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	TriContV series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyzcv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mglDataV zz(x->GetNN());
	if(!z)	z = &zz;
	if(mgl_check_trig(gr,nums,x,y,z,a,"TriContV"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("TriContV",cgid++);
	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	long s=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	for(long k=0;k<v->GetNx();k++)
	{
		mreal v0 = v->v(k);		zz.Fill(fixed ? gr->Min.z : v0);
		mreal dv = (gr->Max.c-gr->Min.c)/8, c = gr->GetC(s,v0);
		if(k>0)	dv = v->v(k-1)-v->v(k);
		else if(k<v->GetNx()-1)	dv = v->v(k)-v->v(k+1);
		if(fixed)	dv=-dv;

		const std::vector<mglSegment> curvs = mgl_get_curvs(gr,mgl_tri_lines(v0,nums,a,x,y,fixed?&zz:z));
		for(size_t i=0;i<curvs.size();i++)
		{
			const std::list<mglPoint> &pp=curvs[i].pp;
			long f2=-1,g2=-1;
			for(std::list<mglPoint>::const_iterator it=pp.begin(); it != pp.end(); ++it)
			{
				mglPoint p=*it,q(p.y,-p.x);
				long f1 = f2;	f2 = gr->AddPnt(p,c,q);	p.z+=dv;
				long g1 = g2;	g2 = gr->AddPnt(p,c,q);
				gr->quad_plot(f1,g1,f2,g2);
			}
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 7:long(r+0.5);
	mglData v(n);
	for(long i=0;i<n;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(n+1);
	mgl_tricontv_xyzcv(gr,&v,nums,x,y,z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_tricontv_xyzc(gr,nums,x,y,0,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xycv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_tricontv_xyzcv(gr,v,nums,x,y,0,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyzcv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricontv_xyzcv(_GR_, _DA_(v), _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xycv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricontv_xycv(_GR_, _DA_(v), _DA_(nums), _DA_(x), _DA_(y), _DA_(z), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricontv_xyzc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tricontv_xyc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_tricontv_xyc(_GR_, _DA_(nums), _DA_(x), _DA_(y), _DA_(z), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Dots series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots_ca(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt)
{
	long n = x->GetNN(), d, k=1;
	if(x->GetNz()>1) 	k=3;		else if(x->GetNy()>1)	k=2;

	if(y->GetNN()!=n || z->GetNN()!=n || c->GetNN()!=n || (a && a->GetNN()!=n))
	{	gr->SetWarn(mglWarnDim,"Dots");	return;	}
	gr->SaveState(opt);

	d = gr->MeshNum>0 ? mgl_ipow(gr->MeshNum+1,k) : n;
	d = n>d ? n/d:1;

	static int cgid=1;	gr->StartGroup("Dots",cgid++);
	char mk=gr->SetPenPal(sch);
	long ss=gr->AddTexture(sch);
	if(mk==0)	mk='.';
	gr->Reserve(n);

	long kq = gr->AllocPnts(n);
#pragma omp parallel for
	for(long i=0;i<n;i+=d)
	{
		mglPoint p(x->vthr(i),y->vthr(i),z->vthr(i));
		gr->AddPntQ(kq+i,p,gr->GetC(ss,c->vthr(i)),mglPoint(NAN),a?gr->GetA(a->vthr(i)):-1);
	}
	for(long i=0;i<n;i+=d)	gr->mark_plot(kq+i, mk);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots_a(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{	mgl_dots_ca(gr, x, y, z, z, a, sch, opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_dots_ca(gr, x, y, z, z, NULL, sch, opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_dots(_GR_, _DA_(x),_DA_(y),_DA_(z),s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots_a_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_dots_a(_GR_, _DA_(x),_DA_(y),_DA_(z),_DA_(a),s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dots_ca_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_dots_ca(_GR_, _DA_(x),_DA_(y),_DA_(z),_DA_(c),_DA_(a),s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	mglTriangulation
//
//-----------------------------------------------------------------------------
long MGL_NO_EXPORT mgl_crust(long n,mglPoint *pp,long **nn,mreal ff);
HMDT MGL_EXPORT mgl_triangulation_3d(HCDT x, HCDT y, HCDT z)
{
	mglData *nums=0;
	long n = x->GetNN(), m;
	if(y->GetNN()!=n || z->GetNN()!=n)	return nums;
	mglPoint *pp = new mglPoint[n];
	long *nn=0;
	for(long i=0;i<n;i++)	pp[i].Set(x->v(i), y->v(i), z->v(i));
	m = mgl_crust(n,pp,&nn,0);

	if(m>0)
	{
		nums=new mglData(3,m);
		for(long i=0;i<3*m;i++)	nums->a[i]=nn[i];
	}
	delete []pp;	free(nn);	return nums;
}
//-----------------------------------------------------------------------------
#include "s_hull/s_hull_pro.h"
HMDT MGL_EXPORT mgl_triangulation_2d(HCDT x, HCDT y)
{
	mglData *nums=0;
	long n = x->GetNN();
	if(y->GetNN()!=n)	return nums;
	// use s-hull here
	std::vector<Shx> pts;
	Shx pt;

	double x1=mglInf, x2=-mglInf, y1=mglInf, y2=-mglInf;
	for(long i=0;i<n;i++)
	{
		mreal xx=x->vthr(i), yy = y->vthr(i);
		if(xx<x1)	x1=xx;
		if(xx>x2)	x2=xx;
		if(yy<y1)	y1=yy;
		if(yy>y2)	y2=yy;
	}
	const double dx=x2-x1, dy=y2-y1;
	if(dx==0 || dy==0)	return nums;
	for(long i=0;i<n;i++)	// Filter NaNs and Infs
	{
		pt.r = (x->vthr(i)-x1)/dx;	pt.c = (y->vthr(i)-y1)/dy;
		if(mgl_isbad(pt.r) || mgl_isbad(pt.c))	continue;
		pt.id = i;    pts.push_back(pt);
	}

	std::vector<Triad> triads;
	static const double float_eps = std::numeric_limits<float>::epsilon();
	Dupex grid_step(float_eps, float_eps);
	const size_t original_size = pts.size();

	if(pts.size() >= 3u && 0. < grid_step.r && 0. < grid_step.c) {
		std::vector<long> out;
		de_duplicate(pts, out, grid_step);

		if (pts.size() >= 3u && s_hull_pro(pts, triads) < 0) {
			// Error occured. It may be caused by degenerated dataset. Well, let's try to increment rounding grid step.
			// Why 4? Why not. There are no particular reasons for this.
			grid_step.r *= 4.;
			grid_step.c *= 4.;

			out.clear();
			triads.clear();

			de_duplicate(pts, out, grid_step);

			if (pts.size() >= 3u && s_hull_pro(pts, triads) < 0) {
				// Last try. Let's assume uniform points distribution and use range / sqrt(pts.size()) * 2 as epsilon.
				// It removes a 3/4 of points in optimal case but in the worst case it merges all points to the one.
				const double density = 1. + floor(0.5 + std::sqrt(static_cast<double>(pts.size())));
				grid_step.r = grid_step.c = 2/density;

				out.clear();
				de_duplicate(pts, out, grid_step);

				triads.clear();
				s_hull_pro(pts, triads);
			}
		}
	}

	if (triads.empty()) {
		mgl_set_global_warn(_("Cannot triangulate this set!"));
	} else if(original_size > pts.size()) {
		mgl_set_global_warn(_("There are duplicated or indistinguishably adjacent points for triangulation."));
	}

	long m = triads.size();
	nums=new mglData(3,m);
	for(long i=0;i<m;i++)
	{
		nums->a[3*i]   = triads[i].a;
		nums->a[3*i+1] = triads[i].b;
		nums->a[3*i+2] = triads[i].c;
	}
	return nums;
}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_triangulation_3d_(uintptr_t *x, uintptr_t *y, uintptr_t *z)
{	return uintptr_t(mgl_triangulation_3d(_DA_(x),_DA_(y),_DA_(z)));	}
uintptr_t MGL_EXPORT mgl_triangulation_2d_(uintptr_t *x, uintptr_t *y)
{	return uintptr_t(mgl_triangulation_2d(_DA_(x),_DA_(y)));	}
//-----------------------------------------------------------------------------
//
//	DataGrid
//
//-----------------------------------------------------------------------------
static void *mgl_grid_t(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	long nx=t->p[0],ny=t->p[1];
	mreal *b=t->a;
	const mreal *x=t->b, *y=t->c, *d=t->d;
	HCDT zdat = (HCDT) t->v;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)	if(d[3*i0]>=0 && d[3*i0+1]>=0 && d[3*i0+2]>=0)
	{
		long k1 = long(d[3*i0]+0.5), k2 = long(d[3*i0+1]+0.5), k3 = long(d[3*i0+2]+0.5);
		mreal dxu,dxv,dyu,dyv;
		mreal z1=zdat->vthr(k1), z2=zdat->vthr(k2), z3=zdat->vthr(k3);
		mglPoint d1(x[k2]-x[k1],y[k2]-y[k1],z2-z1), d2(x[k3]-x[k1],y[k3]-y[k1],z3-z1), p;

		dxu = d2.x*d1.y - d1.x*d2.y;
		if(fabs(dxu)<1e-5) continue; // points lies on the same line
		dyv =-d1.x/dxu; dxv = d1.y/dxu;
		dyu = d2.x/dxu; dxu =-d2.y/dxu;

		long x1,y1,x2,y2;
		x1 = long(mgl_min(mgl_min(x[k1],x[k2]),x[k3])); // bounding box
		y1 = long(mgl_min(mgl_min(y[k1],y[k2]),y[k3]));
		x2 = long(mgl_max(mgl_max(x[k1],x[k2]),x[k3]));
		y2 = long(mgl_max(mgl_max(y[k1],y[k2]),y[k3]));
		x1 = x1>0 ? x1:0; x2 = x2<nx ? x2:nx-1;
		y1 = y1>0 ? y1:0; y2 = y2<ny ? y2:ny-1;
		if((x1>x2) | (y1>y2)) continue;

		mreal x0 = x[k1], y0 = y[k1];
		for(long i=x1;i<=x2;i++) for(long j=y1;j<=y2;j++)
		{
			mreal xx = (i-x0), yy = (j-y0);
			mreal u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;
			if((u<0) | (v<0) | (u+v>1)) continue;
			b[i+nx*j] = z1 + d1.z*u + d2.z*v;
		}
	}
	return 0;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_grid_xy(HMDT d, HCDT xdat, HCDT ydat, HCDT zdat, mreal x1, mreal x2, mreal y1, mreal y2)
{ // NOTE: only for mglData
	const mglData *x = dynamic_cast<const mglData *>(xdat);
	const mglData *y = dynamic_cast<const mglData *>(ydat);
	long n=xdat->GetNN();
	if((n<3) || (ydat->GetNN()!=n) || (zdat->GetNN()!=n))	return;

	mglData *nums = mgl_triangulation_2d(xdat,ydat);
	if(!nums)	return;
	if(nums->nx<3)	{	delete nums;	return;	}
	long nn = nums->ny, par[3]={d->nx,d->ny,d->nz};
	mreal xx[4]={x1,(d->nx-1)/(x2-x1), y1,(d->ny-1)/(y2-y1)};

	mreal *xc=new mreal[n], *yc=new mreal[n];
	if(x && y)
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{	xc[i]=xx[1]*(x->a[i]-xx[0]);	yc[i]=xx[3]*(y->a[i]-xx[2]);	}
	else
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{	xc[i]=xx[1]*(xdat->vthr(i)-xx[0]);	yc[i]=xx[3]*(ydat->vthr(i)-xx[2]);	}
	long tmp = d->nx*d->ny*d->nz;
#pragma omp parallel for
	for(long i=0;i<tmp;i++) d->a[i] = NAN;

	mglStartThread(mgl_grid_t,0,nn,d->a,xc,yc,par,zdat,nums->a);
	delete nums;	delete []xc;	delete []yc;
}
void MGL_EXPORT mgl_data_grid_xy_(uintptr_t *d, uintptr_t *x, uintptr_t *y, uintptr_t *z, mreal *x1, mreal *x2, mreal *y1, mreal *y2)
{	mgl_data_grid_xy(_DT_,_DA_(x),_DA_(y),_DA_(z),*x1,*x2,*y1,*y2);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_grid(HMGL gr, HMDT d, HCDT xdat, HCDT ydat, HCDT zdat, const char *opt)
{
	gr->SaveState(opt);
	mgl_data_grid_xy(d,xdat,ydat,zdat,gr->Min.x,gr->Max.x,gr->Min.y,gr->Max.y);
	gr->LoadState();
}
void MGL_EXPORT mgl_data_grid_(uintptr_t *gr, uintptr_t *d, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *opt,int lo)
{	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_data_grid(_GR_,_DT_,_DA_(x),_DA_(y),_DA_(z),o);	delete []o;	}
//-----------------------------------------------------------------------------
//
//	Crust series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_crust(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	if(y->GetNN()!=x->GetNN() || z->GetNN()!=x->GetNN())
	{	gr->SetWarn(mglWarnDim,"Crust");	return;	}
	HMDT nums = mgl_triangulation_3d(x, y, z);
	mgl_triplot_xyzc(gr,nums,x,y,z,z,sch,opt);
	mgl_delete_data(nums);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_crust_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_crust(_GR_, _DA_(x),_DA_(y),_DA_(z),s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
long static mgl_insert_trig(long i1,long i2,long i3,long **n)
{
	static long Cur=0,Max=0;
	if(i1<0 || i2<0 || i3<0)	return Cur;
	if(*n==0)
	{
		Max = 1024;		Cur = 0;
		*n = (long *)malloc(Max*3*sizeof(long));
		memset(*n,0,Max*3*sizeof(long));
	}
	if(Cur>=Max)
	{
		Max += 1024;
		*n = (long *)realloc(*n,Max*3*sizeof(long));
		memset(*n+3*(Max-1024),0,3*1024*sizeof(long));
	}
	long *nn;
	if(i1>i3)	{	long k1=i1;	i1=i3;	i3=k1;	}	// simple sorting
	if(i1>i2)	{	long k1=i1;	i1=i2;	i2=k1;	}
	if(i2>i3)	{	long k1=i2;	i2=i3;	i3=k1;	}
	for(long i=0;i<Cur;i++)	// check if it is unique
	{
		nn = *n + 3*i;
		if(nn[0]==i1 && nn[1]==i2 && nn[2]==i3)	return Cur;
	}
	nn = *n + 3*Cur;
	nn[0]=i1;	nn[1]=i2;	nn[2]=i3;
	Cur++;	return Cur;
}
//-----------------------------------------------------------------------------
long static mgl_get_next(long k1,long n,long *,long *set,mglPoint *qq)
{
	long i,j=-1;
	mreal r,rm=FLT_MAX;
	for(i=0;i<n;i++)
	{
		if(i==k1 || set[i]>0)	continue;
		r = mgl_anorm(qq[i]-qq[k1]);
		if(r<rm)	{	rm=r;	j=i;	}
	}
	return j;
}
//-----------------------------------------------------------------------------
long MGL_NO_EXPORT mgl_crust(long n,mglPoint *pp,long **nn,mreal ff)
{	// TODO: update to normal algorithm
	mreal rs=0;
	if(ff<=0)	ff=2;
	for(long i=0;i<n;i++)
	{
		mreal rm = FLT_MAX;
		for(long j=0;j<n;j++)
		{
			if(i==j)	continue;
			mreal r = mgl_anorm(pp[i]-pp[j]);
			if(rm>r)	rm = r;
		}
		rs += sqrt(rm);
	}
	rs *= ff/n;	rs = rs*rs;		// "average" distance
	const int nnum=100;
	long *ind, *set;	// indexes of "close" points, flag that it was added and its number
	mglPoint *qq;	// normalized point coordinates
	ind = new long[nnum];	set = new long[nnum];	qq = new mglPoint[nnum];
	long k1,k2,k3,m=0;
	for(long i=0;i<n;i++)	// now the triangles will be found
	{
		memset(set,0,nnum*sizeof(long));
		long ii=0;
		for(long j=0;j<n;j++)	// find close vertexes
		{
			mreal r = mgl_anorm(pp[i]-pp[j]);
			if(r<=rs && j!=i)	{	ind[ii] = j;	ii++;	if(ii==99)	break;}
		}
		if(ii<3)	continue;	// nothing to do
		for(long j=0;j<ii;j++)
		{
			k1 = j;	k2 = ind[j];	k3 = i;
			qq[k1] = pp[k2] - pp[k3];
			qq[k1] /= qq[k1].norm();
		}
		k1 = 0;
		while((k2=mgl_get_next(k1,ii,ind,set,qq))>0)
		{
			set[k1]=1;
			mgl_insert_trig(i,ind[k1],ind[k2],nn);
			k1 = k2;
		}
		m = mgl_insert_trig(i,ind[k1],ind[0],nn);
	}
	delete []set;	delete []ind;	delete []qq;	return m;
}
//-----------------------------------------------------------------------------
