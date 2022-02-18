/***************************************************************************
 * vect.cpp is part of Math Graphic Library
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
#include "mgl2/prim.h"


#include "mgl2/vect.h"
#include "mgl2/eval.h"
#include "mgl2/data.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//	Traj series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_traj_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	long n=ax->GetNx(),pal;
	if(mgl_check_dim1(gr,x,z,y,ax,"Traj"))	return;
	if(mgl_check_dim1(gr,ax,az,ay,0,"Traj"))	return;

	double len=gr->SaveState(opt);	if(mgl_isnan(len))	len = 0;
	static int cgid=1;	gr->StartGroup("Traj",cgid++);
	double fact = gr->size_opt<0 ? -gr->size_opt:1;

	// find maximum
	long m = x->GetNy()>y->GetNy() ? x->GetNy():y->GetNy();
	long i = ax->GetNy()>ay->GetNy() ? ax->GetNy():ay->GetNy();
	long j = z->GetNy()>az->GetNy() ? z->GetNy():az->GetNy();
	if(i>m)	m=i;
	if(j>m)	m=j;
	double asize = gr->GetArrowSize();
	gr->SetPenPal(sch,&pal);	gr->Reserve(4*n*m);

	for(long j=0;j<m;j++) // start prepare arrows
	{
		if(gr->NeedStop())	break;
		gr->NextColor(pal);
		long nx = j<x->GetNy() ? j:0, ny = j<y->GetNy() ? j:0, nz = j<z->GetNy() ? j:0;
		long mx = j<ax->GetNy() ? j:0,my = j<ay->GetNy() ? j:0,mz = j<az->GetNy() ? j:0;
		const long kq = gr->AllocPnts(2*n);
#pragma omp parallel for
		for(long i=0;i<n;i++)
		{
			mglPoint p1(x->v(i,nx), y->v(i,ny), z->v(i,nz));
			mglPoint p2(ax->v(i,mx),ay->v(i,my),az->v(i,mz));
			double dd = p2.norm();
			if(len==0)
			{
				double dx,dy,dz;
				if(i<n-1)
				{	dx=x->v(i+1,nx)-p1.x;	dy=y->v(i+1,ny)-p1.y;	dz=z->v(i+1,nz)-p1.z;	}
				else
				{	dx=p1.x-x->v(i-1,nx);	dy=p1.y-y->v(i-1,ny);	dz=p1.z-z->v(i-1,nz);	}
				dd = dd ? sqrt(dx*dx+dy*dy+dz*dz)/dd : 0;
			}
			else dd = len;
			gr->AddPntQ(kq+2*i,p1);
			gr->AddPntQ(kq+2*i+1,p1+(fact*dd)*p2,-1,mglPoint(NAN),-1,2);
		}
		for(long i=0;i<n;i++)	gr->vect_plot(kq+2*i, kq+2*i+1, asize);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_traj_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(x->GetNx()), az(x->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_traj_xyz(gr,x,y,&z,ax,ay,&az,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_traj_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_traj_xy(_GR_, _DA_(x), _DA_(y), _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_traj_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_traj_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Vect series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	long n=ax->GetNx(),m=ax->GetNy(),l=ax->GetNz();
	if(mgl_check_dim2(gr,x,y,ax,ay,"Vect"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Vect",cgid++);
	bool dot = mglchr(sch,'.');
	bool fix = mglchr(sch,'f'), end = mglchr(sch,'>');
	bool beg = mglchr(sch,'<'), grd = mglchr(sch,'=');
	double fact = gr->size_opt<0 ? -gr->size_opt:1;

	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	double zVal = gr->Min.z, asize = gr->GetArrowSize();

	long tx=1,ty=1;
	if(gr->MeshNum>1)	{	tx=(n-1)/(gr->MeshNum-1);	ty=(m-1)/(gr->MeshNum-1);	}
	if(tx<1)	tx=1;
	if(ty<1)	ty=1;

	double xm=0,cm=0,ca=0;
	double dm=(fabs(gr->Max.c)+fabs(gr->Min.c))*1e-5;
	// use whole array for determining maximal vectors length
#pragma omp parallel
	{
		double xm1=0,cm1=0,xx,c1,c2;
#pragma omp for nowait collapse(3) reduction(+:ca)
		for(long k=0;k<l;k++)	for(long j=0;j<m;j+=ty)	for(long i=0;i<n;i+=tx)
		{
			mglPoint d(GetX(x,i,j,k).x, GetY(y,i,j,k).x),p1;
			mglPoint v(ax->v(i,j,k),ay->v(i,j,k));
			c1 = v.norm();	xm1 = xm1<c1 ? c1:xm1;	// handle NAN values
			p1 = i<n-1 ? mglPoint(GetX(x,i+tx,j,k).x, GetY(y,i+tx,j,k).x)-d : d-mglPoint(GetX(x,i-tx,j,k).x, GetY(y,i-tx,j,k).x);
			c1 = fabs(v*p1);	xx = p1.norm();	c1 *= xx?1/(xx*xx):0;
			p1 = j<m-1 ? mglPoint(GetX(x,i,j+ty,k).x, GetY(y,i,j+ty,k).x)-d : d-mglPoint(GetX(x,i,j-ty,k).x, GetY(y,i,j-ty,k).x);
			c2 = fabs(v*p1);	xx = p1.norm();	c2 *= xx?1/(xx*xx):0;
			c1 = c1<c2 ? c2:c1;	ca+=c1;	cm1 = cm1<c1 ? c1:cm1;
		}
#pragma omp critical(max_vec)
		{cm = cm<cm1 ? cm1:cm;	xm = xm<xm1 ? xm1:xm;}
	}
	ca /= (n*m*l)/(tx*ty);
	xm = xm?1./xm:0;	cm = cm?fact/cm:0;

	for(long k=0;k<l;k++)
	{
		if(gr->NeedStop())	break;
		if(ax->GetNz()>1)	zVal = gr->Min.z+k*(gr->Max.z-gr->Min.z)/(ax->GetNz()-1);
		const long ni = 1+((n-1)/tx), nj = 1+((m-1)/ty);
		const long kq = gr->AllocPnts(2*ni*nj);
#pragma omp parallel for collapse(2)
		for(long j=0;j<nj;j++)	for(long i=0;i<ni;i++)
		{
			mglPoint d(GetX(x,i*tx,j*ty,k).x, GetY(y,i*tx,j*ty,k).x, zVal),p1,p2;
			mglPoint v(ax->v(i*tx,j*ty,k),ay->v(i*tx,j*ty,k));
			double dd = v.norm(), c1, c2;
			v *= cm*(fix?(dd>dm ? 1./dd : 0) : xm);

			if(end)		{	p1 = d-v;	p2 = d;	}
			else if(beg){	p1 = d;	p2 = d+v;	}
			else		{	p1=d-v/2.;	p2=d+v/2.;	}
			if(grd)	{	c1=gr->GetC(ss,dd*xm-0.5,false);	c2=gr->GetC(ss,dd*xm,false);}
			else	c1 = c2 = gr->GetC(ss,dd*xm,false);
			long iq = kq+2*(i+ni*j);
			bool r1=gr->AddPntQ(iq,p1,c1);
			bool r2=gr->AddPntQ(iq+1,p2,c2);
			if(!r1 && r2)	gr->AddPntQ(iq,p1,c1,mglPoint(NAN),-1,2);
			if(!r2 && r1)	gr->AddPntQ(iq+1,p2,c2,mglPoint(NAN),-1,2);
		}
		if(dot)	for(long i=0;i<ni*nj;i++)
		{	long iq=kq+2*i;	gr->line_plot(iq,iq+1);	gr->mark_plot(iq,'.');	}
		else	for(long i=0;i<ni*nj;i++)
		{	long iq=kq+2*i;	gr->vect_plot(iq,iq+1,asize);	}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_2d(HMGL gr, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_vect_xy(gr,&x,&y,ax,ay,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect_xy(_GR_, _DA_(x), _DA_(y), _DA_(ax), _DA_(ay), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_2d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect_2d(_GR_, _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Vect3 series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	long n=ax->GetNx(),m=ax->GetNy(),l=ax->GetNz();
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"Vect_3d"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Vect_3d",cgid++);
	bool dot = mglchr(sch,'.'), fix = mglchr(sch,'f');
	bool end = mglchr(sch,'>'), beg = mglchr(sch,'<');
	bool grd = mglchr(sch,'=');
	double fact = gr->size_opt<0 ? -gr->size_opt:1;

	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	gr->Reserve(2*n*m*l);
	long tx=1,ty=1,tz=1;
	if(gr->MeshNum>1)
	{	tx=(n-1)/(gr->MeshNum-1);	ty=(m-1)/(gr->MeshNum-1);	tz=(l-1)/(gr->MeshNum-1);}
	if(tx<1)	tx=1;
	if(ty<1)	ty=1;
	if(tz<1)	tz=1;

	double xm=0,cm=0,ca=0, asize = gr->GetArrowSize();
	double dm=(fabs(gr->Max.c)+fabs(gr->Min.c))*1e-5;
	// use whole array for determining maximal vectors length
#pragma omp parallel
	{
		double c1,c2,c3, xm1=0,cm1=0,xx;
#pragma omp for nowait collapse(3) reduction(+:ca)
		for(long k=0;k<l;k+=tz)	for(long i=0;i<n;i+=tx)	for(long j=0;j<m;j+=ty)
		{
			mglPoint d(GetX(x,i,j,k).x, GetY(y,i,j,k).x, GetZ(z,i,j,k).x);
			mglPoint v(ax->v(i,j,k),ay->v(i,j,k),az->v(i,j,k)),p1;
			c1 = v.norm();	xm1 = xm1<c1 ? c1:xm1;	// handle NAN values
			p1 = i<n-1 ? mglPoint(GetX(x,i+tx,j,k).x, GetY(y,i+tx,j,k).x, GetZ(z,i+tx,j,k).x)-d : d-mglPoint(GetX(x,i-tx,j,k).x, GetY(y,i-tx,j,k).x, GetZ(z,i-tx,j,k).x);
			c1 = fabs(v*p1);	xx = p1.norm();	c1 *= xx?1/(xx*xx):0;
			p1 = j<m-1 ? mglPoint(GetX(x,i,j+ty,k).x, GetY(y,i,j+ty,k).x, GetZ(z,i,j+ty,k).x)-d : d-mglPoint(GetX(x,i,j-ty,k).x, GetY(y,i,j-ty,k).x, GetZ(z,i,j-ty,k).x);
			c2 = fabs(v*p1);	xx = p1.norm();	c2 *= xx?1/(xx*xx):0;
			p1 = k<l-1 ? mglPoint(GetX(x,i,j,k+tz).x, GetY(y,i,j,k+tz).x, GetZ(z,i,j,k+tz).x)-d : d-mglPoint(GetX(x,i,j,k-tz).x, GetY(y,i,j,k-tz).x, GetZ(z,i,j,k-tz).x);
			c3 = fabs(v*p1);	xx = p1.norm();	c3 *= xx?1/(xx*xx):0;
			c1 = c1<c2 ? c2:c1;	c1 = c1<c3 ? c3:c1;
			ca+=c1;	cm1 = cm1<c1 ? c1:cm1;
		}
#pragma omp critical(max_vec)
		{cm = cm<cm1 ? cm1:cm;	xm = xm<xm1 ? xm1:xm;}
	}
	ca /= double(n*m*l)/double(tx*ty*tz);
	xm = xm?1./xm:0;	cm = cm?fact/cm:0;

	const long ni = 1+((n-1)/tx), nj = 1+((m-1)/ty), nk = 1+((l-1)/tz);
	const long kq = gr->AllocPnts(2*ni*nj*nk);
#pragma omp parallel for collapse(3)
	for(long k=0;k<nk;k++)	for(long j=0;j<nj;j++)	for(long i=0;i<ni;i++)
	{
		long ii=i*tx,ij=j*ty,ik=k*tz;
		mglPoint d(GetX(x,ii,ij,ik).x, GetY(y,ii,ij,ik).x, GetZ(z,ii,ij,ik).x);
		mglPoint v(ax->v(ii,ij,ik),ay->v(ii,ij,ik),az->v(ii,ij,ik)), p1, p2;
		double dd = v.norm(),c1,c2;
		v *= cm*(fix?(dd>dm ? 1./dd : 0) : xm);

		if(end)		{	p1 = d-v;	p2 = d;	}
		else if(beg){	p1 = d;	p2 = d+v;	}
		else		{	p1=d-v/2.;	p2=d+v/2.;	}
		if(grd)	{	c1=gr->GetC(ss,dd*xm-0.5,false);	c2=gr->GetC(ss,dd*xm,false);	}
		else	c1 = c2 = gr->GetC(ss,dd*xm,false);
		long iq = kq+2*(i+ni*(j+nj*k));
		bool r1=gr->AddPntQ(iq,p1,c1);
		bool r2=gr->AddPntQ(iq+1,p2,c2);
		if(!r1 && r2)	gr->AddPntQ(iq,p1,c1,mglPoint(NAN),-1,2);
		if(!r2 && r1)	gr->AddPntQ(iq+1,p2,c2,mglPoint(NAN),-1,2);
	}
	if(dot)	for(long i=0;i<ni*nj*nk;i++)
	{	long iq=kq+2*i;	gr->line_plot(iq,iq+1);	gr->mark_plot(iq,'.');	}
	else	for(long i=0;i<ni*nj*nk;i++)
	{	long iq=kq+2*i;	gr->vect_plot(iq,iq+1,asize);	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_3d(HMGL gr, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()), z(ax->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_vect_xyz(gr,&x,&y,&z,ax,ay,az,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect_3d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect_3d(_GR_, _DA_(ax), _DA_(ay), _DA_(az), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Vect3 series
//
//-----------------------------------------------------------------------------
struct _mgl_vec_slice	{	mglData x,y,z,ax,ay,az;	};
//-----------------------------------------------------------------------------
void static mgl_get_slice(_mgl_vec_slice &s, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, char dir, mreal d, bool both)
{
	long n=ax->GetNx(),m=ax->GetNy(),l=ax->GetNz(), nx=1,ny=1,p;

	if(dir=='x')	{	nx = m;	ny = l;	if(d<0)	d = n/2.;	}
	if(dir=='y')	{	nx = n;	ny = l;	if(d<0)	d = m/2.;	}
	if(dir=='z')	{	nx = n;	ny = m;	if(d<0)	d = l/2.;	}
	s.x.Create(nx,ny);	s.y.Create(nx,ny);	s.z.Create(nx,ny);
	s.ax.Create(nx,ny);	s.ay.Create(nx,ny);	s.az.Create(nx,ny);
	p = long(d);	d -= p;
	if(dir=='x' && p>=n-1)	{	d+=p-n+2;	p=n-2;	}
	if(dir=='y' && p>=m-1)	{	d+=p-m+2.;	p=m-2;	}
	if(dir=='z' && p>=l-1)	{	d+=p-l+2;	p=l-2;	}

	if(both)
	{
		if(dir=='x')
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;
				s.x.a[i0] = x->v(p,i,j)*(1-d) + x->v(p+1,i,j)*d;
				s.y.a[i0] = y->v(p,i,j)*(1-d) + y->v(p+1,i,j)*d;
				s.z.a[i0] = z->v(p,i,j)*(1-d) + z->v(p+1,i,j)*d;
				s.ax.a[i0] = ax->v(p,i,j)*(1-d) + ax->v(p+1,i,j)*d;
				s.ay.a[i0] = ay->v(p,i,j)*(1-d) + ay->v(p+1,i,j)*d;
				s.az.a[i0] = az->v(p,i,j)*(1-d) + az->v(p+1,i,j)*d;
			}
		if(dir=='y')
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;
				s.x.a[i0] = x->v(i,p,j)*(1-d) + x->v(i,p+1,j)*d;
				s.y.a[i0] = y->v(i,p,j)*(1-d) + y->v(i,p+1,j)*d;
				s.z.a[i0] = z->v(i,p,j)*(1-d) + z->v(i,p+1,j)*d;
				s.ax.a[i0] = ax->v(i,p,j)*(1-d) + ax->v(i,p+1,j)*d;
				s.ay.a[i0] = ay->v(i,p,j)*(1-d) + ay->v(i,p+1,j)*d;
				s.az.a[i0] = az->v(i,p,j)*(1-d) + az->v(i,p+1,j)*d;
			}
		if(dir=='z')
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;
				s.x.a[i0] = x->v(i,j,p)*(1-d) + x->v(i,j,p+1)*d;
				s.y.a[i0] = y->v(i,j,p)*(1-d) + y->v(i,j,p+1)*d;
				s.z.a[i0] = z->v(i,j,p)*(1-d) + z->v(i,j,p+1)*d;
				s.ax.a[i0] = ax->v(i,j,p)*(1-d) + ax->v(i,j,p+1)*d;
				s.ay.a[i0] = ay->v(i,j,p)*(1-d) + ay->v(i,j,p+1)*d;
				s.az.a[i0] = az->v(i,j,p)*(1-d) + az->v(i,j,p+1)*d;
			}
	}
	else	// x, y, z -- vectors
	{
		if(dir=='x')
		{
			mreal v = x->v(p)*(1-d)+x->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.x.a[i0] = v;
				s.y.a[i0] = y->v(i);	s.z.a[i0] = z->v(j);
				s.ax.a[i0] = ax->v(p,i,j)*(1-d) + ax->v(p+1,i,j)*d;
				s.ay.a[i0] = ay->v(p,i,j)*(1-d) + ay->v(p+1,i,j)*d;
				s.az.a[i0] = az->v(p,i,j)*(1-d) + az->v(p+1,i,j)*d;
			}
		}
		if(dir=='y')
		{
			mreal v = y->v(p)*(1-d)+y->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.y.a[i0] = v;
				s.x.a[i0] = x->v(i);	s.z.a[i0] = z->v(j);
				s.ax.a[i0] = ax->v(i,p,j)*(1-d) + ax->v(i,p+1,j)*d;
				s.ay.a[i0] = ay->v(i,p,j)*(1-d) + ay->v(i,p+1,j)*d;
				s.az.a[i0] = az->v(i,p,j)*(1-d) + az->v(i,p+1,j)*d;
			}
		}
		if(dir=='z')
		{
			mreal v = z->v(p)*(1-d)+z->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.z.a[i0] = v;
				s.x.a[i0] = x->v(i);	s.y.a[i0] = y->v(j);
				s.ax.a[i0] = ax->v(i,j,p)*(1-d) + ax->v(i,j,p+1)*d;
				s.ay.a[i0] = ay->v(i,j,p)*(1-d) + ay->v(i,j,p+1)*d;
				s.az.a[i0] = az->v(i,j,p)*(1-d) + az->v(i,j,p+1)*d;
			}
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, double sVal, const char *opt)
{
	bool both = mgl_isboth(x,y,z,ax);
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"Vect3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Vect3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	bool dot = mglchr(sch,'.'), fix = mglchr(sch,'f');
	bool end = mglchr(sch,'>'), beg = mglchr(sch,'<');
	bool grd = mglchr(sch,'=');
	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	double fact = gr->size_opt<0 ? -gr->size_opt:1;

	_mgl_vec_slice s;
	mgl_get_slice(s,x,y,z,ax,ay,az,dir,sVal,both);

	long n=s.ax.nx,m=s.ax.ny, tx=1,ty=1;
	if(gr->MeshNum>1)	{	tx=(n-1)/(gr->MeshNum-1);	ty=(m-1)/(gr->MeshNum-1);	}
	if(tx<1)	tx=1;
	if(ty<1)	ty=1;
	double xm=0,cm=0,ca=0, asize = gr->GetArrowSize();
	double dm=(fabs(gr->Max.c)+fabs(gr->Min.c))*1e-5;
	// use whole array for determining maximal vectors length
	mglPoint d=(gr->Max-gr->Min)/mglPoint(1./ax->GetNx(),1./ax->GetNy(),1./ax->GetNz());

	long tn=ty*n;
#pragma omp parallel
	{
		double xm1=0,cm1=0, xx,yy,zz, c1,c2;
		mglPoint p1, p2, v;
#pragma omp for nowait collapse(2) reduction(+:ca)
		for(long i=0;i<n;i+=tx)	for(long j=0;j<m;j+=ty)
		{
			long i0 = i+n*j;
			xx = s.x.a[i0];	yy = s.y.a[i0];	zz = s.z.a[i0];
			p1 = i<n-1 ? mglPoint(s.x.a[i0+tx]-xx, s.y.a[i0+tx]-yy, s.z.a[i0+tx]-zz) : mglPoint(xx-s.x.a[i0-tx], yy-s.y.a[i0-tx], zz-s.z.a[i0-tx]);
			p2 = j<m-1 ? mglPoint(s.x.a[i0+tn]-xx, s.y.a[i0+tn]-yy, s.z.a[i0+tn]-zz) : mglPoint(xx-s.x.a[i0-tn], yy-s.y.a[i0-tn], zz-s.z.a[i0-tn]);
			v.Set(s.ax.a[i0], s.ay.a[i0], s.az.a[i0]);
			c1 = v.norm();	xm1 = xm1<c1 ? c1:xm1;	// handle NAN values
			yy = fabs(v*d);	xx = d.norm();	yy *= xx?1/(xx*xx):0;
			c1 = fabs(v*p1);	xx = p1.norm();	c1 *= xx?1/(xx*xx):0;
			c2 = fabs(v*p2);	xx = p2.norm();	c2 *= xx?1/(xx*xx):0;
			c1 = c1<c2 ? c2:c1;	c1 = c1<yy ? yy:c1;
			ca+=c1;	cm1 = cm1<c1 ? c1:cm1;
		}
#pragma omp critical(max_vec)
		{cm = cm<cm1 ? cm1:cm;	xm = xm<xm1 ? xm1:xm;}
	}
	ca /= mreal(n*m)/mreal(tx*ty);
	xm = xm?1./xm:0;	cm = cm?fact/cm:0;

	const long ni = 1+((n-1)/tx), nj = 1+((m-1)/ty);
	const long kq = gr->AllocPnts(2*ni*nj);
#pragma omp parallel for collapse(2)
	for(long j=0;j<nj;j++)	for(long i=0;i<ni;i++)
	{
		long i0 = i*tx+n*j*ty;
		mglPoint d(s.x.a[i0], s.y.a[i0], s.z.a[i0]), p1, p2;
		mglPoint v(s.ax.a[i0], s.ay.a[i0], s.az.a[i0]);
		double dd = v.norm(),c1,c2;
		v *= cm*(fix?(dd>dm ? 1./dd : 0) : xm);

		if(end)		{	p1 = d-v;	p2 = d;	}
		else if(beg){	p1 = d;	p2 = d+v;	}
		else		{	p1=d-v/2.;	p2=d+v/2.;	}
		if(grd)	{	c1=gr->GetC(ss,dd*xm-0.5,false);	c2=gr->GetC(ss,dd*xm,false);}
		else	c1 = c2 = gr->GetC(ss,dd*xm,false);

		long iq = kq+2*(i+ni*j);
		bool r1=gr->AddPntQ(iq,p1,c1);
		bool r2=gr->AddPntQ(iq+1,p2,c2);
		if(!r1 && r2)	gr->AddPntQ(iq,p1,c1,mglPoint(NAN),-1,2);
		if(!r2 && r1)	gr->AddPntQ(iq+1,p2,c2,mglPoint(NAN),-1,2);
	}
	if(dot)	for(long i=0;i<ni*nj;i++)
	{	long iq=kq+2*i;	gr->line_plot(iq,iq+1);	gr->mark_plot(iq,'.');	}
	else	for(long i=0;i<ni*nj;i++)
	{	long iq=kq+2*i;	gr->vect_plot(iq,iq+1,asize);	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect3(HMGL gr, HCDT ax, HCDT ay, HCDT az, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()),z(ax->GetNz());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_vect3_xyz(gr,&x,&y,&z,ax,ay,az,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, *sVal, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_vect3_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_vect3(_GR_, _DA_(ax), _DA_(ay), _DA_(az), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Flow 2d series
//
//-----------------------------------------------------------------------------
void static flow(mglBase *gr, double zVal, double u, double v, HCDT x, HCDT y, HCDT ax, HCDT ay, long ss, bool vv)
{
	long n=100*(ax->GetNx()+ax->GetNy());
	bool nboth = x->GetNx()*x->GetNy()!=ax->GetNx()*ax->GetNy() || y->GetNx()*y->GetNy()!=ax->GetNx()*ax->GetNy();

	mglPoint *pp = new mglPoint[n], dp;
	mglPoint dx(1/fabs(gr->Max.x-gr->Min.x),1/fabs(gr->Max.y-gr->Min.y),1/fabs(gr->Max.z-gr->Min.z));
	mglPoint nx(ax->GetNx(),ax->GetNy());

	mreal dt = 0.5/(ax->GetNx() > ax->GetNy() ? ax->GetNx() : ax->GetNy());
	mreal e,f,g,ff[4],gg[4],h,s=2,acc=dt/20;
	if(u<0 || v<0)	{	dt = -dt;	u = -u;	v = -v;	s *= -1;}
	long k=0;
	bool end = false;
	if(nboth) do{
		mglPoint dif;
		pp[k].x = x->Spline1(dif,u,0,0);	f = ax->Spline1(u,v,0)/dif.x;
		pp[k].y = y->Spline1(dif,v,0,0);	g = ay->Spline1(u,v,0)/dif.x;
		pp[k].z = zVal;
		if(mgl_isbad(f+g))	break;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		h = hypot(f,g);	pp[k].c = gr->GetC(ss,s*h);
		if(end || h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		e = u+ff[0]/2;	h = v+gg[0]/2;
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		e = u+ff[1]/2;	h = v+gg[1]/2;
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		e = u+ff[2];	h = v+gg[2];
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		v += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1;
	} while(!end);
	else do{
		mglPoint dif;
		mreal xu,xv,yu,yv,det,xx,yy;
		pp[k].x = x->Spline1(dif,u,v,0);	xu=dif.x;	xv=dif.y;
		pp[k].y = y->Spline1(dif,u,v,0);	yu=dif.x;	yv=dif.y;
		xx = ax->Spline1(u,v,0);	yy = ay->Spline1(u,v,0);
		det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		pp[k].z = zVal;
		if(mgl_isbad(f+g))	break;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		h = hypot(f,g);	pp[k].c = gr->GetC(ss,s*h);
		if(end || h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		e = u+ff[0]/2;	h = v+gg[0]/2;
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		e = u+ff[1]/2;	h = v+gg[1]/2;
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		e = u+ff[2];	h = v+gg[2];
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		v += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1;
	} while(!end);
	if(k>1)
	{
		long a=long(0.3*gr->GetArrowSize()/fabs(dt));
		const long kq = gr->AllocPnts(k);
//#pragma omp parallel for // NOTE: parallel is used in above function
		for(long i=0;i<k;i++)	gr->AddPntQ(kq+i,pp[i],pp[i].c);
		gr->curve_plot(k,kq);
		if(vv && dt<0)	for(long i=a;i<k;i+=a)
			gr->vect_plot(kq+i,kq+i-1,a/3.);
		if(vv && dt>0)	for(long i=a;i<k;i+=a)
			gr->vect_plot(kq+i-1,kq+i,a/3.);
	}
	delete []pp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,ax,ay,"Flow"))	return;

	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?5:long(r+0.5);
	static int cgid=1;	gr->StartGroup("Flow",cgid++);

	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	bool vv = mglchr(sch,'v');
	// allocate memory
	mreal zVal = gr->Min.z;
	bool cnt=!mglchr(sch,'#');

	std::vector<mreal> u, v;
	long nx=ax->GetNx(), ny=ax->GetNy();
	if(mglchr(sch,'.'))
	{
		mglData a(nx,ny);
		r = mgl_isnan(r)?0.5:r;
		const mreal di = 1/mreal(nx-1), dj = 1/mreal(ny-1), ds=r*di, dt=r*dj;
#pragma omp parallel for
		for(long i=0;i<nx*ny;i++)
			a.a[i] = ax->vthr(i)*ax->vthr(i)+ay->vthr(i)*ay->vthr(i);
#pragma omp parallel for collapse(2)
		for(long j=1;j<ny-1;j++)	for(long i=1;i<nx-1;i++)
		{
			long i0 = i+nx*j;	mreal v0 = a.a[i0];
			if(v0<=a.a[i0-1-nx] && v0<=a.a[i0-nx] && v0<=a.a[i0+1-nx] && v0<=a.a[i0-1] && v0<=a.a[i0+1] && v0<=a.a[i0-1+nx] && v0<=a.a[i0+nx] && v0<=a.a[i0+1+nx])
#pragma omp critical(flow)
			{
				mreal s = i*di, t = j*dj;
				u.push_back(s+ds);	v.push_back(t+dt);
				u.push_back(-s-ds);	v.push_back(-t-dt);
				u.push_back(s-ds);	v.push_back(t-dt);
				u.push_back(-s+ds);	v.push_back(-t+dt);
//mgl_mark(gr,x->v(i),y->v(j),0,"b.");
			}
/*			{
				if((ax->vthr(i0-1+nx)-ay->vthr(i0-1+nx))*(ax->vthr(i0+1-nx)-ay->vthr(i0+1-nx))<=0 &&
				(ax->vthr(i0-1-nx)+ay->vthr(i0-1-nx))*(ax->vthr(i0+1+nx)+ay->vthr(i0+1+nx))<=0)
//				&& (ax->vthr(i0-1)*ay->vthr(i0-nx)<0 || ax->vthr(i0+1)*ay->vthr(i0+ny)<0))
#pragma omp critical(flow)
				{
					mreal s = i*di, t = j*dj;
					u.push_back(s+ds);	v.push_back(t+dt);
					u.push_back(-s-ds);	v.push_back(-t-dt);
					u.push_back(s-ds);	v.push_back(t-dt);
					u.push_back(-s+ds);	v.push_back(-t+dt);
mgl_mark(gr,x->v(i),y->v(j),0,"b.");
				}
				else if(ax->vthr(i0-1)*ax->vthr(i0+1)<=0 && ay->vthr(i0-nx)*ay->vthr(i0+nx)<=0)
//				&& (ax->vthr(i0-1)*ay->vthr(i0-nx)<0 || ax->vthr(i0+1)*ay->vthr(i0+ny)<0))
#pragma omp critical(flow)
				{
					mreal s = i*di, t = j*dj;
					u.push_back(s+ds);	v.push_back(t);
					u.push_back(-s-ds);	v.push_back(-t);
					u.push_back(s-ds);	v.push_back(t);
					u.push_back(-s+ds);	v.push_back(-t);
mgl_mark(gr,x->v(i),y->v(j),0,"r.");
				}
			}*/
		}
	}
	else if(mglchr(sch,'*'))	for(long i=0;i<num;i++)	for(long j=0;j<num;j++)
	{
		mreal t = (i+1.)/(num+1.), s = (j+1.)/(num+1.);
		u.push_back(s);		v.push_back(t);
		u.push_back(-s);	v.push_back(-t);
	}
	else	for(long i=0;i<num;i++)
	{
		mreal t = (i+1.)/(num+1.);
		u.push_back(0);		v.push_back(t);
		u.push_back(0);		v.push_back(-t);
		u.push_back(1);		v.push_back(t);
		u.push_back(-1);	v.push_back(-t);
		u.push_back(t);		v.push_back(0);
		u.push_back(-t);	v.push_back(0);
		u.push_back(t);		v.push_back(1);
		u.push_back(-t);	v.push_back(-1);
		if(cnt)
		{
			u.push_back(t);		v.push_back(0.5);
			u.push_back(-t);	v.push_back(-0.5);
			u.push_back(0.5);	v.push_back(t);
			u.push_back(-0.5);	v.push_back(-t);
		}
	}
	for(long k=0;k<ax->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		if(ax->GetNz()>1)	zVal = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(k)/(ax->GetNz()-1);
		HMDT bx=mgl_data_subdata(ax,-1,-1,k), by=mgl_data_subdata(ay,-1,-1,k);
#pragma omp parallel for
		for(long i=0;i<long(u.size());i++)	if(!gr->NeedStop())
			flow(gr, zVal, u[i], v[i], x, y, bx, by,ss,vv);
		mgl_delete_data(bx);	mgl_delete_data(by);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_2d(HMGL gr, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_flow_xy(gr,&x,&y,ax,ay,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow_xy(_GR_, _DA_(x), _DA_(y), _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;	}
void MGL_EXPORT mgl_flow_2d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow_2d(_GR_, _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_xy(HMGL gr, double x0, double y0, double z0, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	if(mgl_isnan(z0))	z0 = gr->Min.z;
	mreal u,v;
	long n=ax->GetNx(), m=ax->GetNy();
	bool nboth = x->GetNx()*x->GetNy()!=n*m || y->GetNx()*y->GetNy()!=n*m;
	if(mgl_check_dim2(gr,x,y,ax,ay,"FlowP"))	return;
	bool forward=true, backward=true;
	if(mglchr(sch,'<'))	{	forward=false;	backward=true;	}
	if(mglchr(sch,'>'))	{	forward=true;	backward=false;	}

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("FlowP",cgid++);

	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	bool vv = mglchr(sch,'v');
	// find coordinates u, v
	mreal dm=INFINITY;
	long i0=0,j0=0;
	if(nboth)
	{
		mreal dx=INFINITY, dy=INFINITY;
		for(long i=0;i<n;i++)	for(long j=0;j<m;j++)	// first find closest
		{	mreal d = fabs(x->v(i)-x0);	if(d<dx)	{	i0=i;	dx=d;	}	}
		for(long i=0;i<n;i++)	for(long j=0;j<m;j++)	// first find closest
		{	mreal d = fabs(y->v(j)-y0);	if(d<dy)	{	j0=j;	dy=d;	}	}
		dm = hypot(dx,dy);
	}
	else	for(long i=0;i<n;i++)	for(long j=0;j<m;j++)	// first find closest
	{
		mreal d = hypot(x->v(i,j)-x0,y->v(i,j)-y0);
		if(d<dm)	{	i0=i;	j0=j;	dm=d;	}
	}
	if(dm==0)	{	u = i0/mreal(n);	v = j0/mreal(m);	}	// we find it
	else
	{
		mreal dxu,dxv,dyu,dyv, dx, dy;
		if(nboth)
		{
			dx = x->v(i0)-x0;	dy = y->v(j0)-y0;
			dxu= x->dvx(i0);	dyv= y->dvx(j0);
			u = (i0+dx/dxu)/n;	v = (j0+dy/dyv)/m;
		}
		else
		{
			dx = x->v(i0,j0)-x0;	dy = y->v(i0,j0)-y0;
			dxu= x->dvx(i0,j0);		dyu= y->dvx(i0,j0);
			dxv= x->dvy(i0,j0);		dyv= y->dvy(i0,j0);
			mreal d = dxv*dyu-dxu*dyv;
			u = (i0+(dxv*dy-dx*dyv)/d)/n;
			v = (j0-(dxu*dy-dx*dyu)/d)/m;
		}
	}
	if(forward)		flow(gr, z0, u, v, x, y, ax, ay,ss,vv);
	if(backward)	flow(gr, z0,-u,-v, x, y, ax, ay,ss,vv);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_2d(HMGL gr, double x0, double y0, double z0, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_flowp_xy(gr,x0,y0,z0,&x,&y,ax,ay,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_xy_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flowp_xy(_GR_, *x0,*y0,*z0, _DA_(x), _DA_(y), _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;
}
void MGL_EXPORT mgl_flowp_2d_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flowp_2d(_GR_, *x0,*y0,*z0, _DA_(ax), _DA_(ay), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Flow 3d series
//
//-----------------------------------------------------------------------------
void flow(mglBase *gr, double u, double v, double w, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az,long ss,bool vv, bool xo, bool zo)
{
	static long n=100*(ax->GetNx()+ax->GetNy()+ax->GetNz());
	long nn = ax->GetNN();
	bool nboth = x->GetNN()!=nn || y->GetNN()!=nn || z->GetNN()!=nn;
	mglPoint *pp = new mglPoint[n], dp;
	mglPoint dx(1/fabs(gr->Max.x-gr->Min.x),1/fabs(gr->Max.y-gr->Min.y),1/fabs(gr->Max.z-gr->Min.z));
	mglPoint nx(ax->GetNx(),ax->GetNy(),ax->GetNz());

	nn = (ax->GetNx() > ax->GetNy() ? ax->GetNz() : ax->GetNy());
	nn = (nn > ax->GetNz() ? nn : ax->GetNz());
	mreal dt = 0.2/nn, e,f,g,ee[4],ff[4],gg[4],h,s=2,u1,v1,w1,acc=dt/20;
	if(u<0 || v<0 || w<0)
	{	dt = -dt;	u = -u;	v = -v;	w = -w;	s *= -1;}
	long k=0;
	bool end = false;
	if(nboth) do{
		mglPoint dif;
		pp[k].x = x->Spline1(dif,u,0,0);	e = ax->Spline1(u,v,w)/dif.x;
		pp[k].y = y->Spline1(dif,v,0,0);	f = ay->Spline1(u,v,w)/dif.x;
		pp[k].z = z->Spline1(dif,w,0,0);	g = az->Spline1(u,v,w)/dif.x;
		if(mgl_isbad(e+f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = sqrt(e*e+f*f+g*g);	pp[k].c = gr->GetC(ss,s*h);
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ee[0]=e*dt/h;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		u1 = u+ee[0]/2;	v1 = v+ff[0]/2;	w1 = w+gg[0]/2;
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[1]=e*dt/h;	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		u1 = u+ee[1]/2;	v1 = v+ff[1]/2;	w1 = w+gg[1]/2;
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[2]=e*dt/h;	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		u1 = u+ee[2];	v1 = v+ff[2];	w1 = w+gg[2];
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[3]=e*dt/h;	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ee[0]/6+ee[1]/3+ee[2]/3+ee[3]/6;
		v += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		w += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1 || w<0 || w>1;
	} while(!end);
	else do{
		mglPoint dif;
		mreal xu,xv,xw,yu,yv,yw,zv,zu,zw,det,xx,yy,zz;
		pp[k].x = x->Spline1(dif,u,v,w);	xu=dif.x;	xv=dif.y;	xw=dif.z;
		pp[k].y = y->Spline1(dif,u,v,w);	yu=dif.x;	yv=dif.y;	yw=dif.z;
		pp[k].z = z->Spline1(dif,u,v,w);	zu=dif.x;	zv=dif.y;	zw=dif.z;
		xx = ax->Spline1(u,v,w);	yy = ay->Spline1(u,v,w);	zz = az->Spline1(u,v,w);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		if(mgl_isbad(e+f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = sqrt(e*e+f*f+g*g);	pp[k].c = gr->GetC(ss,s*h);
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ee[0]=e*dt/h;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		u1 = u+ee[0]/2;	v1 = v+ff[0]/2;	w1 = w+gg[0]/2;
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[1]=e*dt/h;	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		u1 = u+ee[1]/2;	v1 = v+ff[1]/2;	w1 = w+gg[1]/2;
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[2]=e*dt/h;	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		u1 = u+ee[2];	v1 = v+ff[2];	w1 = w+gg[2];
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[3]=e*dt/h;	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ee[0]/6+ee[1]/3+ee[2]/3+ee[3]/6;
		v += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		w += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1 || w<0 || w>1;
	} while(!end);
	if(k>1)	// TODO exclude AddPnt() and use curve_plot()!!!
	{
		long j,a=long(0.3*gr->GetArrowSize()/fabs(dt));
		mreal rr = mgl_anorm(gr->Max-gr->Min)*gr->BarWidth/25, ll;
		mglPoint q1,q2,l;
		long n1=-1,n2=-1,n3=-1,n4=-1;

		gr->Reserve(4*k);	j = gr->AddPnt(pp[0],pp[0].c);
		l = pp[1] - pp[0];	l /= mgl_anorm(l);
		q1.Set(l.y,-l.x,0);	ll = mgl_anorm(q1);
		if(ll)	q1 /= ll;	else	q1.Set(0,1,0);
		q2 = q1^l;
		if(xo)	{	n1 = gr->AddPnt(pp[0],-1,q2);	n2 = gr->AddPnt(pp[0]+rr*q1,-1,q2);	}
		if(zo)	{	n3 = gr->AddPnt(pp[0],-1,q1);	n4 = gr->AddPnt(pp[0]+rr*q2,-1,q1);	}
		for(long i=1;i<k;i++)
		{
			long jj=j;	j = gr->AddPnt(pp[i],pp[i].c);
			if(vv && i%a==0)
			{
				if(dt<0)	gr->vect_plot(j,jj,a/3);
				else		gr->vect_plot(jj,j,a/3);
			}
			else	gr->line_plot(jj,j);
			l = pp[i]-pp[i-1];		l /= mgl_anorm(l);
			q1 -= l*(l*q1);	q1/= mgl_anorm(q1);	q2 = q1^l;
			long m1 = n1, m2 = n2, m3 = n3, m4 = n4;
			if(xo)
			{	n1 = gr->AddPnt(pp[i],pp[i].c,q2);	n2 = gr->AddPnt(pp[i]+rr*q1,pp[i].c,q2);	gr->quad_plot(n1,n2,m1,m2);	}
			if(zo)
			{	n3 = gr->AddPnt(pp[i],pp[i].c,q1);	n4 = gr->AddPnt(pp[i]+rr*q2,pp[i].c,q1);	gr->quad_plot(n3,n4,m3,m4);	}
		}
		/*	TODO	long a=long(0.3*gr->GetArrowSize()/fabs(dt));
		gr->Reserve(k);
		long *nn = new long[k];
		for(long i=0;i<k;i++)	nn[i] = gr->AddPnt(pp[i],pp[i].c);
		gr->curve_plot(k,nn);
		if(vv && dt<0)	for(long i=a;i<k;i+=a)
			gr->vect_plot(nn[i],nn[i-1],a/3.);
		if(vv && dt>0)	for(long i=a;i<k;i+=a)
			gr->vect_plot(nn[i-1],nn[i],a/3.);
		delete []nn;*/
	}
	delete []pp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, double sVal, const char *opt)
{
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"Flow3"))	return;
	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?5:long(r+0.5);
	static int cgid=1;	gr->StartGroup("Flow3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	bool vv = mglchr(sch,'v'), tt = mglchr(sch,'t');
	std::vector<mglPoint> u;
	const double f = 1./(num+1);

	if(dir=='x')
	{
		long n = ax->GetNx()-1;
		sVal = (sVal<0 || sVal>n) ? 0.5 : sVal/n;
		for(long j=0;j<num;j++)	for(long i=0;i<num;i++)
		{
			u.push_back(mglPoint(sVal,f*(i+1),f*(j+1)));
			u.push_back(mglPoint(-sVal,-f*(i+1),-f*(j+1)));
		}
	}
	else if(dir=='y')
	{
		long n = ax->GetNy()-1;
		sVal = (sVal<0 || sVal>n) ? 0.5 : sVal/n;
		for(long j=0;j<num;j++)	for(long i=0;i<num;i++)
		{
			u.push_back(mglPoint(f*(i+1),sVal,f*(j+1)));
			u.push_back(mglPoint(-f*(i+1),-sVal,-f*(j+1)));
		}
	}
	else if(dir=='z')
	{
		long n = ax->GetNy()-1;
		sVal = (sVal<0 || sVal>n) ? 0.5 : sVal/n;
		for(long j=0;j<num;j++)	for(long i=0;i<num;i++)
		{
			u.push_back(mglPoint(f*(i+1),f*(j+1),sVal));
			u.push_back(mglPoint(-f*(i+1),-f*(j+1),-sVal));
		}
	}

#pragma omp parallel for
	for(long i=0;i<long(u.size());i++)	if(!gr->NeedStop())
		flow(gr, u[i].x, u[i].y, u[i].z, x, y, z, ax, ay, az, ss,vv,tt,tt);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow3(HMGL gr, HCDT ax, HCDT ay, HCDT az, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()),z(ax->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_flow3_xyz(gr,&x,&y,&z,ax,ay,az,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, double *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, *sVal, o);	delete []o;	delete []s;	}
void MGL_EXPORT mgl_flow3_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, double *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow3(_GR_, _DA_(ax), _DA_(ay), _DA_(az), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"Flow3d"))	return;

	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	static int cgid=1;	gr->StartGroup("Flow3d",cgid++);
	bool cnt=!mglchr(sch,'#');
	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	bool vv = mglchr(sch,'v'), xo = mglchr(sch,'x'), zo = mglchr(sch,'z');

	std::vector<mglPoint> u;
	for(long i=0;i<num;i++)	for(long j=0;j<num;j++)
	{
		mreal t = (i+1.)/(num+1.), s = (j+1.)/(num+1.);
		u.push_back(mglPoint(t,s,0));	u.push_back(mglPoint(-t,-s,0));
		u.push_back(mglPoint(t,s,1));	u.push_back(mglPoint(-t,-s,-1));

		u.push_back(mglPoint(t,0,s));	u.push_back(mglPoint(-t,0,-s));
		u.push_back(mglPoint(t,1,s));	u.push_back(mglPoint(-t,-1,-s));

		u.push_back(mglPoint(0,s,t));	u.push_back(mglPoint(0,-s,-t));
		u.push_back(mglPoint(1,s,t));	u.push_back(mglPoint(-1,-s,-t));
		if(cnt)
		{
			u.push_back(mglPoint(t,s,0.5));	u.push_back(mglPoint(-t,-s,-0.5));
			u.push_back(mglPoint(t,0.5,s));	u.push_back(mglPoint(-t,-0.5,-s));
			u.push_back(mglPoint(0.5,s,t));	u.push_back(mglPoint(-0.5,-s,-t));
		}
	}
#pragma omp parallel for
	for(long i=0;i<long(u.size());i++)	if(!gr->NeedStop())
		flow(gr, u[i].x, u[i].y, u[i].z, x, y, z, ax, ay, az,ss,vv,xo,zo);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_3d(HMGL gr, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()), z(ax->GetNz());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_flow_xyz(gr,&x,&y,&z,ax,ay,az,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flow_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, o);	delete []o;	delete []s;	}
void MGL_EXPORT mgl_flow_3d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flow_3d(_GR_, _DA_(ax), _DA_(ay), _DA_(az), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_xyz(HMGL gr, double x0, double y0, double z0, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	mglPoint p(x0,y0,z0);
	mreal u,v,w;
	long n=ax->GetNx(),m=ax->GetNy(),l=ax->GetNz();
	bool nboth = !(x->GetNN()==n*m*l && y->GetNN()==n*m*l && z->GetNN()==n*m*l);
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"FlowP3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("FlowP3",cgid++);
	gr->SetPenPal("-");
	long ss = gr->AddTexture(sch);
	bool vv = mglchr(sch,'v'), xo = mglchr(sch,'x'), zo = mglchr(sch,'z');
	bool forward=true, backward=true;
	if(mglchr(sch,'<'))	{	forward=false;	backward=true;	}
	if(mglchr(sch,'>'))	{	forward=true;	backward=false;	}

	// find coordinates u, v, w
	mreal dm=INFINITY;
	long i0=0,j0=0,k0=0;
	if(nboth)	// first find closest
	{
		mreal dx=INFINITY, dy=INFINITY, dz=INFINITY;
		for(long i=0;i<n;i++)
		{	mreal d = fabs(x->v(i)-p.x);	if(d<dx)	{	i0=i;	dx=d;	}	}
		dm = INFINITY;
		for(long j=0;j<m;j++)
		{	mreal d = fabs(y->v(j)-p.y);	if(d<dy)	{	j0=j;	dy=d;	}	}
		dm = INFINITY;
		for(long k=0;k<l;k++)
		{	mreal d = fabs(z->v(k)-p.z);	if(d<dz)	{	k0=k;	dz=d;	}	}
		dm = sqrt(dx*dx+dy*dy+dz*dz);
	}
	else	for(long i=0;i<n;i++)	for(long j=0;j<m;j++)	for(long k=0;k<l;k++)	// first find closest
	{
		mreal dx = x->v(i,j,k)-p.x,	dy = y->v(i,j,k)-p.y,	dz = x->v(i,j,k)-p.z;
		mreal d = sqrt(dx*dx+dy*dy+dz*dz);
		if(d<dm)	{	i0=i;	j0=j;	k0=k;	dm=d;	}
	}
	if(dm==0)	// we find it
	{	u=i0/mreal(n);	v=j0/mreal(m);	w=k0/mreal(l);	}
	else
	{
		mreal dxu,dxv,dxw,dyu,dyv,dyw,dzu,dzv,dzw, dx,dy,dz;
		if(nboth)
		{
			dx = x->v(i0)-p.x;	dy = y->v(j0)-p.y;	dz = z->v(k0)-p.z;
			dxu= x->dvx(i0);	dyv= y->dvx(j0);	dzw= z->dvx(k0);
			u = (i0+dx/dxu)/n;	v = (j0+dy/dyv)/m;	w = (k0+dz/dzw)/m;
		}
		else
		{
			dx = x->v(i0,j0,k0)-p.x;	dy = y->v(i0,j0,k0)-p.y;	dz = z->v(i0,j0,k0)-p.z;
			dxu= x->dvx(i0,j0,k0);		dyu= y->dvx(i0,j0,k0);		dzu= z->dvx(i0,j0,k0);
			dxv= x->dvy(i0,j0,k0);		dyv= y->dvy(i0,j0,k0);		dzv= z->dvy(i0,j0,k0);
			dxw= x->dvz(i0,j0,k0);		dyw= y->dvz(i0,j0,k0);		dzw= z->dvz(i0,j0,k0);
			mreal d = dxu*(dyw*dzv-dyv*dzw)+dxv*(dyu*dzw-dyw*dzu)+dxw*(dyv*dzu-dyu*dzv);
			u = (i0+(dx*(dyw*dzv-dyv*dzw)+dxv*(dy*dzw-dyw*dz)+dxw*(dyv*dz-dy*dzv))/d)/n;
			v = (j0-(dx*(dyw*dzu-dyu*dzw)+dxu*(dy*dzw-dyw*dz)+dxw*(dyu*dz-dy*dzu))/d)/m;
			w = (i0+(dx*(dyv*dzu-dyu*dzv)+dxu*(dy*dzv-dyv*dz)+dxv*(dyu*dz-dy*dzu))/d)/l;
		}
	}
	if(forward)		flow(gr, u, v, w, x, y, z, ax, ay, az,ss,vv,xo,zo);
	if(backward)	flow(gr,-u,-v,-w, x, y, z, ax, ay, az,ss,vv,xo,zo);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_3d(HMGL gr, double x0, double y0, double z0, HCDT ax, HCDT ay, HCDT az, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()), z(ax->GetNz());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_flowp_xyz(gr, x0,y0,z0, &x,&y,&z,ax,ay,az,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_flowp_xyz_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flowp_xyz(_GR_, *x0,*y0,*z0, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, o);
	delete []o;	delete []s;	}
void MGL_EXPORT mgl_flowp_3d_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_flowp_3d(_GR_, *x0,*y0,*z0, _DA_(ax), _DA_(ay), _DA_(az), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Grad series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT phi, const char *sch, const char *opt)
{
	mglData ax(phi), ay,az,xx,yy,zz;
	ay.Set(ax);	az.Set(ax);	xx.Set(ax);	yy.Set(ax);	zz.Set(ax);
	long n=xx.nx, m=xx.ny, l=xx.nz, nn = n*m*l;
	if(x->GetNN()==nn && y->GetNN()==nn && z->GetNN()==nn)
	{	xx.Set(x);	yy.Set(y);	zz.Set(z);	}	// nothing to do
	else if(x->GetNx()==n && y->GetNx()==m && z->GetNx()==l)
#pragma omp parallel for collapse(3)
		for(long i=0;i<n;i++)	for(long j=0;j<m;j++)	for(long k=0;k<l;k++)
		{
			long i0 = i+n*(j+m*k);
			xx.a[i0] = x->v(i);	yy.a[i0] = y->v(j);	zz.a[i0] = z->v(k);
		}
	else	{	gr->SetWarn(mglWarnDim,"Grad");	return;	}
	ax.Diff(xx,yy,zz);	ay.Diff(yy,xx,zz);	az.Diff(zz,xx,yy);
	mgl_flow_xyz(gr,&xx,&yy,&zz,&ax,&ay,&az,sch,opt);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad_xy(HMGL gr, HCDT x, HCDT y, HCDT phi, const char *sch, const char *opt)
{
	mglData ax(phi), ay,xx,yy;
	ay.Set(ax);	xx.Set(ax);	yy.Set(ax);
	long n = phi->GetNx(), m=phi->GetNy(), nn=n*m;
	if(x->GetNx()*x->GetNy()==nn && y->GetNx()*y->GetNy()==nn)	{	xx.Set(x);	yy.Set(y);	}
	else if(x->GetNx()==n && y->GetNx()==m)
#pragma omp parallel for collapse(2)
		for(long i=0;i<n;i++)	for(long j=0;j<m;j++)
		{	long i0 = i+n*j;	xx.a[i0] = x->v(i);	yy.a[i0] = y->v(j);	}
	else	{	gr->SetWarn(mglWarnDim,"Grad");	return;	}
	ax.Diff(xx,yy);	ay.Diff(yy,xx);
	mgl_flow_xy(gr,&xx,&yy,&ax,&ay,sch,opt);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad(HMGL gr, HCDT phi, const char *sch, const char *opt)
{
	mglDataV x(phi->GetNx()), y(phi->GetNy()), z(phi->GetNz());
	gr->SaveState(opt);
	x.Fill(gr->Min.x,gr->Max.x);	y.Fill(gr->Min.y,gr->Max.y);	z.Fill(gr->Min.z,gr->Max.z);
	if(phi->GetNz()==1)	mgl_grad_xy(gr,&x,&y,phi,sch,0);
	else				mgl_grad_xyz(gr,&x,&y,&z,phi,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ph, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_grad_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ph), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ph, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_grad_xy(_GR_, _DA_(x), _DA_(y), _DA_(ph), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grad_(uintptr_t *gr, uintptr_t *ph, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_grad(_GR_, _DA_(ph), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Pipe 2d series
//
//-----------------------------------------------------------------------------
void static flowr(mglBase *gr, double zVal, double u, double v, HCDT x, HCDT y, HCDT ax, HCDT ay, double r0,long sc)
{
	long n=100*(ax->GetNx()+ax->GetNy());
	bool nboth = x->GetNx()*x->GetNy()!=ax->GetNx()*ax->GetNy() || y->GetNx()*y->GetNy()!=ax->GetNx()*ax->GetNy();

	mglPoint *pp = new mglPoint[n], dp;
	mreal *cc = new mreal[n];
	mglPoint dx(1/fabs(gr->Max.x-gr->Min.x),1/fabs(gr->Max.y-gr->Min.y),1/fabs(gr->Max.z-gr->Min.z));
	mglPoint nx(ax->GetNx(),ax->GetNy());

	double dt = 0.5/(ax->GetNx() > ax->GetNy() ? ax->GetNx() : ax->GetNy()),e,f,g,ff[4],gg[4],h,s=2,acc=dt/20;
	double ss = 16./mgl_ipow(gr->Max.c - gr->Min.c,2);
	if(u<0 || v<0)	{	dt = -dt;	u = -u;	v = -v;	s *= -1;}
	long k=0;
	bool end = false;
	if(nboth) do{
		mglPoint dif;
		pp[k].x = x->Spline1(dif,u,0,0);	f = ax->Spline1(u,v,0)/dif.x;
		pp[k].y = y->Spline1(dif,v,0,0);	g = ay->Spline1(u,v,0)/dif.x;
		pp[k].z = zVal;
		if(mgl_isbad(f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = hypot(f,g);	cc[k] = gr->GetC(sc,s*h);
		pp[k].c = r0>0 ? r0*sqrt(1e-2+ss*h*h)/2 : -r0/sqrt(1e-2+ss*h*h)/5;
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		e = u+ff[0]/2;	h = v+gg[0]/2;
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		e = u+ff[1]/2;	h = v+gg[1]/2;
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		e = u+ff[2];	h = v+gg[2];
		x->Spline1(dif,e,0,0);	f = ax->Spline1(e,h,0)/dif.x;
		y->Spline1(dif,h,0,0);	g = ay->Spline1(e,h,0)/dif.x;
		h = 1+hypot(f,g);	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		v += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1;
	} while(!end);
	else do{
		mglPoint dif;
		mreal xu,xv,yu,yv,det,xx,yy;
			pp[k].x = x->Spline1(dif,u,v,0);	xu=dif.x;	xv=dif.y;
			pp[k].y = y->Spline1(dif,u,v,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(u,v,0);	yy = ay->Spline1(u,v,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		pp[k].z = zVal;
		if(mgl_isbad(f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = hypot(f,g);	cc[k] = gr->GetC(sc,s*h);
		pp[k].c = r0>0 ? r0*sqrt(1e-2+ss*h*h)/2 : -r0/sqrt(1e-2+ss*h*h)/5;
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		e = u+ff[0]/2;	h = v+gg[0]/2;
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		e = u+ff[1]/2;	h = v+gg[1]/2;
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		e = u+ff[2];	h = v+gg[2];
			x->Spline1(dif,e,h,0);	xu=dif.x;	xv=dif.y;
			y->Spline1(dif,e,h,0);	yu=dif.x;	yv=dif.y;
			xx = ax->Spline1(e,h,0);	yy = ay->Spline1(e,h,0);
			det = xv*yu-xu*yv;	f = (yy*xv-xx*yv)/det;	g = (xx*yu-yy*xu)/det;
		h = 1+hypot(f,g);	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		v += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1;
	} while(!end);
	if(k>1)
	{
		const int num=!(gr->GetQuality()&3)?13:25;
		mglPoint l=pp[1]-pp[0],t=!l,q=t^l;
		t.Normalize();	q.Normalize();
		double rr=pp[0].c,dr=l.c;

		const long kq = gr->AllocPnts(num*k);
		for(long j=0;j<num;j++)
		{
			int fi=j*360/(num-1);
			float co = mgl_cos[fi%360], si = mgl_cos[(270+fi)%360];
			mglPoint p = pp[0] + t*(rr*co) + q*(rr*si);
			mglPoint d = (t*si - q*co)^(l + t*(dr*co) + q*(dr*si));
			gr->AddPntQ(kq+j,p,cc[0],d);
		}
		for(long i=1;i<k;i++)
		{
			mglPoint l = pp[i]-pp[i-1];
			mglPoint t = !l;	t.Normalize();
			mglPoint q = t^l;	q.Normalize();
			double rr=pp[i].c;	dr=l.c;
			for(long j=0;j<num;j++)
			{
				int fi=j*360/(num-1);
				float co = mgl_cos[fi%360], si = mgl_cos[(270+fi)%360];
				mglPoint p = pp[i] + t*(rr*co) + q*(rr*si);
				mglPoint d = (t*si - q*co)^(l + t*(dr*co) + q*(dr*si));
				gr->AddPntQ(kq+i*num+j,p,cc[i],d);
			}
		}
		for(long i=1;i<k;i++)	for(long j=1;j<num;j++)
		{	long iq=kq+j+i*num;	gr->quad_plot(iq-1,iq,iq-num-1,iq-num);	}
	}
	delete []pp;	delete []cc;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, double r0, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,ax,ay,"Pipe"))	return;

	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?5:long(r+0.5);
	static int cgid=1;	gr->StartGroup("Pipe",cgid++);

	long ss = gr->AddTexture(sch);
	// allocate memory
	mreal zVal = gr->Min.z;
	bool cnt=!mglchr(sch,'#');
	if(mglchr(sch,'i'))	r0 = -fabs(r0);

	std::vector<mreal> u, v;
	if(mglchr(sch,'*'))	for(long i=0;i<num;i++)	for(long j=0;j<num;j++)
	{
		mreal t = (i+1.)/(num+1.), s = (j+1.)/(num+1.);
		u.push_back(s);		v.push_back(t);
		u.push_back(-s);		v.push_back(-t);
	}
	else	for(long i=0;i<num;i++)
	{
		mreal t = (i+1.)/(num+1.);
		u.push_back(0);		v.push_back(t);
		u.push_back(0);		v.push_back(-t);
		u.push_back(1);		v.push_back(t);
		u.push_back(-1);	v.push_back(-t);
		u.push_back(t);		v.push_back(0);
		u.push_back(-t);	v.push_back(0);
		u.push_back(t);		v.push_back(1);
		u.push_back(-t);	v.push_back(-1);
		if(cnt)
		{
			u.push_back(t);		v.push_back(0.5);
			u.push_back(-t);	v.push_back(-0.5);
			u.push_back(0.5);	v.push_back(t);
			u.push_back(-0.5);	v.push_back(-t);
		}
	}
	for(long k=0;k<ax->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		if(ax->GetNz()>1)	zVal = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(k)/(ax->GetNz()-1);
		HMDT bx=mgl_data_subdata(ax,-1,-1,k), by=mgl_data_subdata(ay,-1,-1,k);
#pragma omp parallel for
		for(long i=0;i<long(u.size());i++)	if(!gr->NeedStop())
			flowr(gr, zVal, u[i], v[i], x, y, bx, by,r0,ss);
		mgl_delete_data(bx);	mgl_delete_data(by);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_2d(HMGL gr, HCDT ax, HCDT ay, const char *sch, double r0, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_pipe_xy(gr,&x,&y,ax,ay,sch,r0,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, mreal *r0, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_pipe_xy(_GR_, _DA_(x), _DA_(y), _DA_(ax), _DA_(ay), s, *r0, o);	delete []o;	delete []s;	}
void MGL_EXPORT mgl_pipe_2d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, const char *sch, mreal *r0, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_pipe_2d(_GR_, _DA_(ax), _DA_(ay), s, *r0, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Pipe 3d series
//
//-----------------------------------------------------------------------------
void flowr(mglBase *gr, double u, double v, double w, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, double r0,long sc)
{
	static long n=100*(ax->GetNx()+ax->GetNy()+ax->GetNz());
	long nn = ax->GetNN();
	bool nboth = x->GetNN()!=nn || y->GetNN()!=nn || z->GetNN()!=nn;
	mglPoint *pp = new mglPoint[n], dp;
	mreal *cc = new mreal[n];
	mglPoint dx(1/fabs(gr->Max.x-gr->Min.x),1/fabs(gr->Max.y-gr->Min.y),1/fabs(gr->Max.z-gr->Min.z));
	mglPoint nx(ax->GetNx(),ax->GetNy(),ax->GetNz());

	nn = (ax->GetNx() > ax->GetNy() ? ax->GetNx() : ax->GetNy());
	nn = (nn > ax->GetNz() ? nn : ax->GetNz());
	double dt = 0.2/nn, e,f,g,ee[4],ff[4],gg[4],h,s=2,u1,v1,w1,acc=dt/20;
	double ss = 16./mgl_ipow(gr->Max.c - gr->Min.c,2);

	if(u<0 || v<0 || w<0)
	{	dt = -dt;	u = -u;	v = -v;	w = -w;	s *= -1;}
	long k=0;
	bool end = false;
	if(nboth) do{
		mglPoint dif;
		pp[k].x = x->Spline1(dif,u,0,0);	e = ax->Spline1(u,v,w)/dif.x;
		pp[k].y = y->Spline1(dif,v,0,0);	f = ay->Spline1(u,v,w)/dif.x;
		pp[k].z = z->Spline1(dif,w,0,0);	g = az->Spline1(u,v,w)/dif.x;
		if(mgl_isbad(e+f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = sqrt(e*e+f*f+g*g);	cc[k] = gr->GetC(sc,s*h);
		pp[k].c = r0>0 ? r0*sqrt(1e-2+ss*h*h)/2 : -r0/sqrt(1e-2+ss*h*h)/5;
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ee[0]=e*dt/h;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		u1 = u+ee[0]/2;	v1 = v+ff[0]/2;	w1 = w+gg[0]/2;
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[1]=e*dt/h;	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		u1 = u+ee[1]/2;	v1 = v+ff[1]/2;	w1 = w+gg[1]/2;
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[2]=e*dt/h;	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		u1 = u+ee[2];	v1 = v+ff[2];	w1 = w+gg[2];
		x->Spline1(dif,u1,0,0);	e = ax->Spline1(u1,v1,w1)/dif.x;
		y->Spline1(dif,v1,0,0);	f = ay->Spline1(u1,v1,w1)/dif.x;
		z->Spline1(dif,w1,0,0);	g = az->Spline1(u1,v1,w1)/dif.x;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[3]=e*dt/h;	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ee[0]/6+ee[1]/3+ee[2]/3+ee[3]/6;
		v += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		w += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1 || w<0 || w>1;
	} while(!end);
	else do{
		mglPoint dif;
		mreal xu,xv,xw,yu,yv,yw,zv,zu,zw,det,xx,yy,zz;
		pp[k].x = x->Spline1(dif,u,v,w);	xu=dif.x;	xv=dif.y;	xw=dif.z;
		pp[k].y = y->Spline1(dif,u,v,w);	yu=dif.x;	yv=dif.y;	yw=dif.z;
		pp[k].z = z->Spline1(dif,u,v,w);	zu=dif.x;	zv=dif.y;	zw=dif.z;
		xx = ax->Spline1(u,v,w);	yy = ay->Spline1(u,v,w);	zz = az->Spline1(u,v,w);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		if(mgl_isbad(e+f+g))	end = true;
		else	for(long m=0;m<k-1;m+=10)	// determines encircle
			if(mgl_anorm((pp[k]-pp[m])/dx)<acc)	end = true;
		if(end)	break;
		h = sqrt(e*e+f*f+g*g);	cc[k] = gr->GetC(sc,s*h);
		pp[k].c = r0>0 ? r0*sqrt(1e-2+ss*h*h)/2 : -r0/sqrt(1e-2+ss*h*h)/5;
		if(h<1e-5)	break;	// stationary point
		if(k==0 || mgl_anorm((pp[k]-pp[k-1])/nx)>=1)	k++;
		// find next point by midpoint method
		h+=1;	ee[0]=e*dt/h;	ff[0]=f*dt/h;	gg[0]=g*dt/h;
		u1 = u+ee[0]/2;	v1 = v+ff[0]/2;	w1 = w+gg[0]/2;
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[1]=e*dt/h;	ff[1]=f*dt/h;	gg[1]=g*dt/h;
		u1 = u+ee[1]/2;	v1 = v+ff[1]/2;	w1 = w+gg[1]/2;
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[2]=e*dt/h;	ff[2]=f*dt/h;	gg[2]=g*dt/h;
		u1 = u+ee[2];	v1 = v+ff[2];	w1 = w+gg[2];
		x->Spline1(dif,u1,v1,w1);	xu=dif.x;	xv=dif.y;	xw=dif.z;	xx = ax->Spline1(u1,v1,w1);
		y->Spline1(dif,u1,v1,w1);	yu=dif.x;	yv=dif.y;	yw=dif.z;	yy = ay->Spline1(u1,v1,w1);
		z->Spline1(dif,u1,v1,w1);	zu=dif.x;	zv=dif.y;	zw=dif.z;	zz = az->Spline1(u1,v1,w1);
		det = -xu*yv*zw+xv*yu*zw+xu*yw*zv-xw*yu*zv-xv*yw*zu+xw*yv*zu;
		e = (-xv*yw*zz+xw*yv*zz+xv*yy*zw-xx*yv*zw-xw*yy*zv+xx*yw*zv)/det;
		f = (xu*yw*zz-xw*yu*zz-xu*yy*zw+xx*yu*zw+xw*yy*zu-xx*yw*zu)/det;
		g = (-xu*yv*zz+xv*yu*zz+xu*yy*zv-xx*yu*zv-xv*yy*zu+xx*yv*zu)/det;
		h = 1+sqrt(e*e+f*f+g*g);
		ee[3]=e*dt/h;	ff[3]=f*dt/h;	gg[3]=g*dt/h;
		u += ee[0]/6+ee[1]/3+ee[2]/3+ee[3]/6;
		v += ff[0]/6+ff[1]/3+ff[2]/3+ff[3]/6;
		w += gg[0]/6+gg[1]/3+gg[2]/3+gg[3]/6;
		// condition of end
		end = end || k>=n || u<0 || v<0 || u>1 || v>1 || w<0 || w>1;
	} while(!end);
	if(k>1)
	{
		const int num=!(gr->GetQuality()&3)?13:25;
		mglPoint l=pp[1]-pp[0],t=!l,q=t^l,p,d;
		t.Normalize();	q.Normalize();
		double rr=pp[0].c,dr=l.c;
		long kq = gr->AllocPnts(num*k);
		for(long j=0;j<num;j++)
		{
			int fi=j*360/(num-1);
			float co = mgl_cos[fi%360], si = mgl_cos[(270+fi)%360];
			p = pp[0] + t*(rr*co) + q*(rr*si);
			d = (t*si - q*co)^(l + t*(dr*co) + q*(dr*si));
			gr->AddPntQ(kq+j,p,cc[0],d);
		}
		for(long i=1;i<k;i++)
		{
			l = pp[i]-pp[i-1];
			t = !l;	t.Normalize();
			q = t^l;	q.Normalize();
			double rr=pp[i].c;	dr=l.c;
			for(long j=0;j<num;j++)
			{
				int fi=j*360/(num-1);
				float co = mgl_cos[fi%360], si = mgl_cos[(270+fi)%360];
				p = pp[i] + t*(rr*co) + q*(rr*si);
				d = (t*si - q*co)^(l + t*(dr*co) + q*(dr*si));
				gr->AddPntQ(kq+i*num+j,p,cc[i],d);
			}
		}
		for(long i=1;i<k;i++)	for(long j=1;j<num;j++)
		{	long iq=kq+j+i*num;	gr->quad_plot(iq-1,iq,iq-num-1,iq-num);	}
	}
	delete []pp;	delete []cc;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *sch, double r0, const char *opt)
{
	if(mgl_check_vec3(gr,x,y,z,ax,ay,az,"Vect"))	return;

	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	static int cgid=1;	gr->StartGroup("Pipe3",cgid++);
	if(mglchr(sch,'i'))	r0 = -fabs(r0);

	long ss = gr->AddTexture(sch);
	bool cnt=!mglchr(sch,'#');

	std::vector<mreal> u, v, w;
	for(long i=0;i<num;i++)	for(long j=0;j<num;j++)
	{
		mreal t = (i+1.)/(num+1.), s = (j+1.)/(num+1.);
		u.push_back(t);		v.push_back(s);		w.push_back(0);
		u.push_back(-t);	v.push_back(-s);	w.push_back(0);
		u.push_back(t);		v.push_back(s);		w.push_back(1);
		u.push_back(-t);	v.push_back(-s);	w.push_back(-1);

		u.push_back(t);		v.push_back(0);		w.push_back(s);
		u.push_back(-t);	v.push_back(0);		w.push_back(-s);
		u.push_back(t);		v.push_back(1);		w.push_back(s);
		u.push_back(-t);	v.push_back(-1);	w.push_back(-s);

		u.push_back(0);		v.push_back(s);		w.push_back(t);
		u.push_back(0);		v.push_back(-s);	w.push_back(-t);
		u.push_back(1);		v.push_back(s);		w.push_back(t);
		u.push_back(-1);	v.push_back(-s);	w.push_back(-t);
		if(cnt)
		{
			u.push_back(t);		v.push_back(s);		w.push_back(0.5);
			u.push_back(-t);	v.push_back(-s);	w.push_back(-0.5);
			u.push_back(t);		v.push_back(0.5);	w.push_back(s);
			u.push_back(-t);	v.push_back(-0.5);	w.push_back(-s);
			u.push_back(0.5);	v.push_back(s);		w.push_back(t);
			u.push_back(-0.5);	v.push_back(-s);	w.push_back(-t);
		}
	}
#pragma omp parallel for
	for(long i=0;i<long(u.size());i++)	if(!gr->NeedStop())
		flowr(gr, u[i], v[i], w[i], x, y, z, ax, ay, az,r0,ss);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_3d(HMGL gr, HCDT ax, HCDT ay, HCDT az, const char *sch, double r0, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy()), z(ax->GetNz());	// NOTE mglDataV here is useless
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_pipe_xyz(gr,&x,&y,&z,ax,ay,az,sch,r0,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pipe_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, mreal *r0, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_pipe_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(ax), _DA_(ay), _DA_(az), s, *r0, o);
	delete []o;	delete []s;	}
void MGL_EXPORT mgl_pipe_3d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, uintptr_t *az, const char *sch, mreal *r0, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_pipe_3d(_GR_, _DA_(ax), _DA_(ay), _DA_(az), s, *r0, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
