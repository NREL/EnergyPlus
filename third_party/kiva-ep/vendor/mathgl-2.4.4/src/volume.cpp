/***************************************************************************
 * surf.cpp is part of Math Graphic Library
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
#include "mgl2/define.h"
#include "mgl2/volume.h"
#include "mgl2/data.h"
#include "mgl2/eval.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//	CloudQ series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cloud_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	if(!(gr->GetQuality()&3))	return;	// do nothing in fast_draw
	long n=a->GetNx(),m=a->GetNy(),l=a->GetNz();
	bool nboth = mgl_isnboth(x,y,z,a);
	if(mgl_check_dim3(gr,!nboth,x,y,z,a,0,"Cloud"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Cloud",cgid++);

	long tx=1,ty=1,tz=1;
	if(gr->MeshNum>1)
	{
		tx=(n-1)/(gr->MeshNum-1);	if(tx<1)	tx=1;
		ty=(m-1)/(gr->MeshNum-1);	if(ty<1)	ty=1;
		tz=(l-1)/(gr->MeshNum-1);	if(tz<1)	tz=1;
	}

	mreal	alpha = gr->AlphaDef;
	bool inv = mglchr(sch,'i');
	bool dot = mglchr(sch,'.');
	alpha /= pow((n/tx)*(m/ty)*(l/tz),1./3)/20;
	if(alpha>1)	alpha = 1;
	long ss = gr->AddTexture(sch);

	// x, y, z -- have the same size as a
	n /= tx;	m /= ty;	l /= tz;
	gr->Reserve(n*m*l);
	mglPoint q(NAN);
	const long kq = gr->AllocPnts(n*m*l),nm=n*m;
#pragma omp parallel for
	for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		mglPoint p = nboth ? mglPoint(x->v(i*tx),y->v(j*ty),z->v(k*tz)) : mglPoint(x->v(i*tx,j*ty,k*tz),y->v(i*tx,j*ty,k*tz),z->v(i*tx,j*ty,k*tz));
		mreal aa = gr->GetA(a->v(i*tx,j*ty,k*tz));
		mreal bb = inv ? (1-aa)*(1-aa)*alpha : aa*aa*alpha;
		gr->AddPntQ(kq+i+n*(j+m*k), p,gr->GetC(ss,aa,false),q,bb);
	}
	if(dot)	for(long i=0;i<n*m*l;i++)	gr->mark_plot(kq+i,'.');
	else	for(long k=0;k<l;k++)
	{
		if(gr->NeedStop())	break;
		for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
		{
			long i0 = kq+i+n*(j+m*k);
			if(i<n-1 && j<m-1)	gr->quad_plot(i0,i0+1,i0+n, i0+n+1);
			if(i<n-1 && k<l-1)	gr->quad_plot(i0,i0+1,i0+nm,i0+nm+1);
			if(k<l-1 && j<m-1)	gr->quad_plot(i0,i0+n,i0+nm,i0+n+nm);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cloud(HMGL gr, HCDT a, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_cloud_xyz(gr,&x,&y,&z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cloud_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cloud_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cloud_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cloud(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Surf3 series
//
//-----------------------------------------------------------------------------
mreal static mgl_get_norm(mreal x, mreal d1, mreal d2, mreal d3)
{
	mreal nx = d1*(1-x) + d2*x;
	if(mgl_isbad(nx))
	{
		nx = d1*(1+x) - d3*x;
		if(mgl_isbad(nx))
		{
			if(mgl_isfin(d1))	nx = d1;
			if(mgl_isfin(d2))	nx = d2;
			if(mgl_isfin(d3))	nx = d3;
		}
	}
	return nx;
}
mglPoint static mgl_normal_3d(HCDT a, mglPoint p, bool inv, long n,long m,long l)
{
	mreal x=p.x, y=p.y, z=p.z;
	mreal nx=0, ny=0, nz=0;
	long i=long(x), j=long(y), k=long(z);
	i = i<n-1 ? i:n-2;	j = j<m-1 ? j:m-2;	k = k<l-1 ? k:l-2;
	x-=i;	y-=j;	z-=k;

	nx = mgl_get_norm(x, a->dvx(i,j,k), a->dvx(i+1,j,k), i>0?a->dvx(i-1,j,k):NAN);
	ny = mgl_get_norm(y, a->dvy(i,j,k), a->dvy(i,j+1,k), j>0?a->dvy(i,j-1,k):NAN);
	nz = mgl_get_norm(z, a->dvz(i,j,k), a->dvz(i,j,k+1), k>0?a->dvz(i,j,k-1):NAN);
	return inv ? mglPoint(nx,ny,nz) : mglPoint(-nx,-ny,-nz);
}
//-----------------------------------------------------------------------------
mreal static mgl_normal_1d(HCDT a, mreal x, long n)
{
	long i=long(x);
	i = i<n-1 ? i:n-2;	x-=i;
	return mgl_get_norm(x, a->dvx(i), a->dvx(i+1), i>0?a->dvx(i-1):NAN);
}
//-----------------------------------------------------------------------------
mglPoint static mgl_find_norm(bool nboth, HCDT x, HCDT y, HCDT z, HCDT a, mglPoint u, bool inv, long n,long m,long l)
{
	mglPoint s = mgl_normal_3d(a,u,inv,n,m,l), t, q;
	if(nboth)
	{
		q.x = s.x/mgl_normal_1d(x,u.x,n);
		q.y = s.y/mgl_normal_1d(y,u.y,m);
		q.z = s.z/mgl_normal_1d(z,u.z,l);
	}
	else
	{
		t = mgl_normal_3d(x,u,true,n,m,l);	q.x = (s*t)/(t*t);
		t = mgl_normal_3d(y,u,true,n,m,l);	q.y = (s*t)/(t*t);
		t = mgl_normal_3d(z,u,true,n,m,l);	q.z = (s*t)/(t*t);
	}
	return q;
}
//-----------------------------------------------------------------------------
inline mreal static mgl_cos_pp(const mglPoint *kk,long i0,long i1,long i2)
{
	mglPoint dp1 = kk[i1]-kk[i0], dp2 = kk[i2]-kk[i0];
	mreal p1=dp1*dp1,p2=dp2*dp2,pc=dp1*dp2;
	return p1*p2>1e-10 ? pc/sqrt(p1*p2) : NAN;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_plot(HMGL gr, long n,long m,long *kx1,long *kx2,long *ky1,long *ky2, long *kz, std::vector<mglPoint> kk, int wire)
{
	long id[12],us[12],pd[12];
	mglPoint pp[12];

#pragma omp parallel for private(id,us,pd,pp) collapse(2)
	for(long j=0;j<m-1;j++)	for(long i=0;i<n-1;i++)
	{
		long i0 = i+n*j,ni = 0,ii,jj;
		// find ID of points of Surf3 intersection with cell i0
		for(int i=0;i<12;i++)	id[i]=-1;
//		memset(id,-1,12*sizeof(long));	long ni = 0;
		if(kx1[i0]>=0)		id[ni++] = kx1[i0];
		if(ky1[i0]>=0)		id[ni++] = ky1[i0];
		if(kx1[i0+n]>=0)	id[ni++] = kx1[i0+n];
		if(ky1[i0+1]>=0)	id[ni++] = ky1[i0+1];
		if(kz[i0]>=0)		id[ni++] = kz[i0];
		if(kz[i0+1]>=0)		id[ni++] = kz[i0+1];
		if(kz[i0+n+1]>=0)	id[ni++] = kz[i0+n+1];
		if(kz[i0+n]>=0)		id[ni++] = kz[i0+n];
		if(kx2[i0]>=0)		id[ni++] = kx2[i0];
		if(ky2[i0]>=0)		id[ni++] = ky2[i0];
		if(kx2[i0+n]>=0)	id[ni++] = kx2[i0+n];
		if(ky2[i0+1]>=0)	id[ni++] = ky2[i0+1];
		if(ni<3)	continue;

		for(jj=0;jj<ni;jj++)
		{	pp[jj]=kk[id[jj]];	pd[jj]=mgl_int(pp[jj].c);	}
		// remove points which is too close to first one
		for(jj=1;jj<ni;)
		{
			mreal d = mgl_norm(pp[jj] - pp[0]);
			if(d>1e-5)	jj++;
			else
			{	ni--;	for(ii=jj;ii<ni;ii++)	id[ii]=id[ii+1];	}
		}
		// continue if number of points <3 i.e. there is no triangle
		if(ni<3)	continue;
		memset(us,0,12*sizeof(long));
		// firstly let find most outstanding point
		mreal d0=2;
		for(jj=1,ii=2;ii<ni;ii++)
		{
			mreal d = mgl_cos_pp(pp,0,ii,1);
			if(d<d0)	{	d0=d;	jj=ii;	}
		}
		// copy first 2 points as base
		long p1 = pd[0], p2 = pd[jj], p3;
		// select the same orientation of all triangles of the surface
		us[0] = us[jj] = 1;
		// find all triangles
		for(long k=2;k<ni;k++)
		{
			// find closest point in sence cosine of angle
			for(i0=-1,ii=1,d0=-2;ii<ni;ii++)
			{
				if(us[ii])	continue;
				mreal d = mgl_cos_pp(pp,0,ii,jj);
				if(d>d0)	{	d0=d;	i0=ii;	}
			}
			if(i0<0)	break;	// no more triangles. NOTE: should be never here
			jj = i0;	us[jj]=1;	p3 = pd[jj];
			if(wire==1)
			{
				gr->line_plot(p1, p2);
				gr->line_plot(p1, p3);
				gr->line_plot(p2, p3);
			}
			else if(wire==2)
			{
				gr->mark_plot(p1, '.');
				gr->mark_plot(p2, '.');
				gr->mark_plot(p3, '.');
			}
			else	gr->trig_plot(p1, p2, p3);
			p2 = p3;
		}
	}
}
//-----------------------------------------------------------------------------
void static mgl_surf3ca_gen(HMGL gr, double val, HCDT x, HCDT y, HCDT z, HCDT a, HCDT c, HCDT b, const char *sch)
{
	long n=a->GetNx(),m=a->GetNy(),l=a->GetNz();
	bool nboth = mgl_isnboth(x,y,z,a);
	int wire = mglchr(sch,'#')?1:0;
	if(mglchr(sch,'.'))	wire = 2;
	bool inv = (mglchr(sch,'-'));
	long ss = gr->AddTexture(sch);

	long *kx1 = new long[n*m],	*kx2 = new long[n*m];
	long *ky1 = new long[n*m],	*ky2 = new long[n*m];
	long *kz  = new long[n*m];
	std::vector<mglPoint> kk;
	kk.reserve(n*m*l);

	for(long k=0;k<l;k++)
	{
		if(gr->NeedStop())	break;
		memcpy(kx1,kx2,n*m*sizeof(long));	memset(kx2,-1,n*m*sizeof(long));
		memcpy(ky1,ky2,n*m*sizeof(long));	memset(ky2,-1,n*m*sizeof(long));
		memset(kz ,-1,n*m*sizeof(long));
		gr->Reserve(n*m);	gr->Reserve(n*m);
		size_t kk1 = kk.size();
		for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
		{
			long i1 = i+n*j;
			mreal a0 = a->v(i,j,k);
			if(mgl_isnan(a0))	continue;
			if(i<n-1)
			{
				mreal d = mgl_d(val,a0,a->v(i+1,j,k));
				if(d>=0 && d<1)
				{	kx2[i1] = kk.size();	kk.push_back(mglPoint(i+d,j,k));	}
			}
			if(j<m-1)
			{
				mreal d = mgl_d(val,a0,a->v(i,j+1,k));
				if(d>=0 && d<1)
				{	ky2[i1] = kk.size();	kk.push_back(mglPoint(i,j+d,k));	}
			}
			if(k>0)
			{
				mreal d = mgl_d(val,a->v(i,j,k-1),a0);
				if(d>=0 && d<1)
				{	kz[i1] = kk.size();		kk.push_back(mglPoint(i,j,k+d-1));	}
			}
		}
		mreal cv=gr->GetC(ss,val);
		if(b && c)
		{
			const long kq = gr->AllocPnts(kk.size());
#pragma omp parallel for
			for(msize i=kk1;i<kk.size();i++)
			{
				mglPoint &u = kk[i];
				double cc = c->linear(u.x,u.y,u.z), bb = b->linear(u.x,u.y,u.z);
				gr->AddPntQ(kq+i,nboth ? mglPoint(x->linear(u.x,0,0),y->linear(u.y,0,0),z->linear(u.z,0,0)) :
					mglPoint(x->linear(u.x,u.y,u.z),y->linear(u.x,u.y,u.z),z->linear(u.x,u.y,u.z)),
					gr->GetC(ss,cc), mgl_find_norm(nboth, x,y,z,a, u, inv,n,m,l), gr->GetA(bb));
				u.c = kq+i;
			}
		}
		else if(c)
		{
			const long kq = gr->AllocPnts(kk.size());
#pragma omp parallel for
			for(msize i=kk1;i<kk.size();i++)
			{
				mglPoint &u = kk[i];
				double cc = c->linear(u.x,u.y,u.z);
				gr->AddPntQ(kq+i,nboth ? mglPoint(x->linear(u.x,0,0),y->linear(u.y,0,0),z->linear(u.z,0,0)) :
					mglPoint(x->linear(u.x,u.y,u.z),y->linear(u.x,u.y,u.z),z->linear(u.x,u.y,u.z)),
					gr->GetC(ss,cc), mgl_find_norm(nboth, x,y,z,a, u, inv,n,m,l));
				u.c = kq+i;
			}
		}
		else if(b)
		{
			const long kq = gr->AllocPnts(kk.size());
#pragma omp parallel for
			for(msize i=kk1;i<kk.size();i++)
			{
				mglPoint &u = kk[i];
				double bb = b->linear(u.x,u.y,u.z);
				gr->AddPntQ(kq+i,nboth ? mglPoint(x->linear(u.x,0,0),y->linear(u.y,0,0),z->linear(u.z,0,0)) :
					mglPoint(x->linear(u.x,u.y,u.z),y->linear(u.x,u.y,u.z),z->linear(u.x,u.y,u.z)),
					cv, mgl_find_norm(nboth, x,y,z,a, u, inv,n,m,l), gr->GetA(bb));
				u.c = kq+i;
			}
		}
		else
		{
			const long kq = gr->AllocPnts(kk.size());
#pragma omp parallel for
			for(msize i=kk1;i<kk.size();i++)
			{
				mglPoint &u = kk[i];
				gr->AddPntQ(kq+i,nboth ? mglPoint(x->linear(u.x,0,0),y->linear(u.y,0,0),z->linear(u.z,0,0)) :
					mglPoint(x->linear(u.x,u.y,u.z),y->linear(u.x,u.y,u.z),z->linear(u.x,u.y,u.z)),
					cv, mgl_find_norm(nboth, x,y,z,a, u, inv,n,m,l));
				u.c = kq+i;
			}
		}

		if(k>0)	mgl_surf3_plot(gr,n,m,kx1,kx2,ky1,ky2,kz,kk,wire);
	}
	delete []kx1;	delete []kx2;	delete []ky1;
	delete []ky2;	delete []kz;	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_xyz_val(HMGL gr, double val, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	if(mgl_check_dim3(gr,mgl_isboth(x,y,z,a),x,y,z,a,0,"Surf3"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Surf3",cgid++);
	mgl_surf3ca_gen(gr, val, x, y, z, a, 0, 0, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_val(HMGL gr, double val, HCDT a, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()), z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3_xyz_val(gr,val,&x,&y,&z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	for(long i=0;i<num;i++)
	{
		mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
		mgl_surf3_xyz_val(gr,v,x,y,z,a,sch,0);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3(HMGL gr, HCDT a, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()), z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3_xyz(gr,&x,&y,&z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_xyz_val_(uintptr_t *gr, mreal *Val, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3_xyz_val(_GR_, *Val, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_val_(uintptr_t *gr, mreal *Val, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3_val(_GR_, *Val, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Surf3A series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_xyz_val(HMGL gr, double val, HCDT x, HCDT y, HCDT z, HCDT a, HCDT b, const char *sch, const char *opt)
{
	if(mgl_check_dim3(gr,!mgl_isnboth(x,y,z,a),x,y,z,a,b,"Surf3A"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Surf3A",cgid++);
	mgl_surf3ca_gen(gr, val, x, y, z, a, 0, b, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_val(HMGL gr, double val, HCDT a, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3a_xyz_val(gr,val,&x,&y,&z,a,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, HCDT b, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	if(b->GetNx()==num && b->GetNy()==1 && b->GetNz()==1)
	{
		mreal a0=gr->AlphaDef;
		for(long i=0;i<num;i++)
		{
			mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
			gr->AlphaDef = gr->GetA(b->v(i));
			mgl_surf3_xyz_val(gr,v,x,y,z,a,sch,0);
		}
		gr->AlphaDef = a0;
	}
	else
		for(long i=0;i<num;i++)
		{
			mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
			mgl_surf3a_xyz_val(gr,v,x,y,z,a,b,sch,0);
		}
	gr->LoadState();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a(HMGL gr, HCDT a, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3a_xyz(gr,&x,&y,&z,a,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_xyz_val_(uintptr_t *gr, mreal *Val, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3a_xyz_val(_GR_, *Val, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_val_(uintptr_t *gr, mreal *Val, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	 mgl_surf3a_val(_GR_, *Val, _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3a_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3a_(uintptr_t *gr, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3a(_GR_, _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Surf3C series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_xyz_val(HMGL gr, double val, HCDT x, HCDT y, HCDT z, HCDT a, HCDT c, const char *sch, const char *opt)
{
	if(mgl_check_dim3(gr,!mgl_isnboth(x,y,z,a),x,y,z,a,c,"Surf3C"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Surf3C",cgid++);
	mgl_surf3ca_gen(gr, val, x, y, z, a, c, 0, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_val(HMGL gr, double val, HCDT a, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3c_xyz_val(gr,val,&x,&y,&z,a,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, HCDT b, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	for(long i=0;i<num;i++)
	{
		mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
		mgl_surf3c_xyz_val(gr,v,x,y,z,a,b,sch,0);
	}
	gr->LoadState();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c(HMGL gr, HCDT a, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3c_xyz(gr,&x,&y,&z,a,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_xyz_val_(uintptr_t *gr, mreal *Val, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3c_xyz_val(_GR_, *Val, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_val_(uintptr_t *gr, mreal *Val, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	 mgl_surf3c_val(_GR_, *Val, _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3c_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3c_(uintptr_t *gr, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3c(_GR_, _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Surf3C series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_xyz_val(HMGL gr, double val, HCDT x, HCDT y, HCDT z, HCDT a, HCDT c, HCDT b, const char *sch, const char *opt)
{
	if(mgl_check_dim3(gr,!mgl_isnboth(x,y,z,a),x,y,z,a,c,"Surf3C") || mgl_check_dim3(gr,!mgl_isnboth(x,y,z,a),x,y,z,a,b,"Surf3C"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Surf3C",cgid++);
	mgl_surf3ca_gen(gr, val, x, y, z, a, c, b, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_val(HMGL gr, double val, HCDT a, HCDT c, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3ca_xyz_val(gr,val,&x,&y,&z,a,c,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, HCDT c, HCDT b, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long num = mgl_isnan(r)?3:long(r+0.5);
	for(long i=0;i<num;i++)
	{
		mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
		mgl_surf3ca_xyz_val(gr,v,x,y,z,a,c,b,sch,0);
	}
	gr->LoadState();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca(HMGL gr, HCDT a, HCDT c, HCDT b, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_surf3ca_xyz(gr,&x,&y,&z,a,c,b,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_xyz_val_(uintptr_t *gr, mreal *Val, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *c, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3ca_xyz_val(_GR_, *Val, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(c), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_val_(uintptr_t *gr, mreal *Val, uintptr_t *a, uintptr_t *c, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	 mgl_surf3ca_val(_GR_, *Val, _DA_(a), _DA_(c), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, uintptr_t *c, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3ca_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), _DA_(c), _DA_(b), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf3ca_(uintptr_t *gr, uintptr_t *a, uintptr_t *c, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf3ca(_GR_, _DA_(a), _DA_(c), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Beam series
//
//-----------------------------------------------------------------------------
// flag & 0x1	--	accompanied coordinates
// flag & 0x2	--	project to r*z
// flag & 0x4	--	normalize field
void MGL_EXPORT mgl_beam_val(HMGL gr, double val, HCDT tr, HCDT g1, HCDT g2, HCDT a, double r, const char *stl, int flag)
{
	long n = a->GetNz(),m=a->GetNx(),l=a->GetNy();
	if(n<2 || m<2 || l<2)	{	gr->SetWarn(mglWarnLow,"Beam");	return;	}
	if(a->Minimal()<0)		{	gr->SetWarn(mglWarnNeg,"Beam");	return;	}
	if(tr->GetNx()<3 || tr->GetNy()<n || g1->GetNx()<3 || g1->GetNy()<n || g2->GetNx()<3 || g2->GetNy()<n)
	{	gr->SetWarn(mglWarnDim,"Beam");	return;	}
	mglData x(a),y(a),z(a),b(a);
	mreal asum0=1;	r = fabs(r);
	if(flag & 4)	for(long j=0;j<m*l;j++)	asum0 += a->vthr(j)*a->vthr(j);
	if(asum0==0)	{	gr->SetWarn(mglWarnZero,"Beam");	return;	}
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{
		if(gr->NeedStop())	continue;
		if(flag & 4)
		{
			mreal asum=0, amax=0;
			for(long j=0;j<m*l;j++)
			{
				mreal aa = a->vthr(j+m*l*i);
				asum += aa*aa;	amax = amax>aa ? amax : aa;
			}
			amax = amax?sqrt(asum/asum0)/amax:0;
			for(long j=0;j<m*l;j++)	b.a[j+m*l*i] = b.a[j+m*l*i]*amax;
		}
		const long ii=m*l*i;
		if(flag & 1)	for(long k=0;k<l;k++)	for(long j=0;j<m;j++)
		{
			long i0 = ii+j+m*k;
			x.a[i0] = 2*j/(m-1.)-1;
			y.a[i0] = 2*k/(l-1.)-1;
			z.a[i0] = gr->Max.z*i/(n-1.);
		}
		else	for(long k=0;k<l;k++)	for(long j=0;j<m;j++)
		{
			long i0 = ii+j+m*k;
			x.a[i0] = tr->v(0,i) + g1->v(0,i)*(2*j/(m-1.)-1)*r + g2->v(0,i)*(2*k/(l-1.)-1)*r;
			y.a[i0] = tr->v(1,i) + g1->v(1,i)*(2*j/(m-1.)-1)*r + g2->v(1,i)*(2*k/(l-1.)-1)*r;
			z.a[i0] = tr->v(2,i) + g1->v(2,i)*(2*j/(m-1.)-1)*r + g2->v(2,i)*(2*k/(l-1.)-1)*r;
		}
		if(flag & 2)	for(long j=0;j<m*l;j++)
		{	long i0 = j+ii;	x.a[i0] = hypot(x.a[i0],y.a[i0]);	}
	}
	mgl_surf3_xyz_val(gr,val,&x,&y,&z,&b,stl,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_beam(HMGL gr, HCDT tr, HCDT g1, HCDT g2, HCDT a, double r, const char *stl, int flag, int num)
{
	num = num<=0 ? 1 : num;
	for(long i=0;i<num;i++)
	{
		mreal v = gr->Max.c + (gr->Min.c-gr->Max.c)*(i+1.)/(num+1);
		mgl_beam_val(gr,v,tr,g1,g2,a,r,stl,flag);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_beam_val_(uintptr_t *gr, mreal *val, uintptr_t *tr, uintptr_t *g1, uintptr_t *g2, uintptr_t *a, mreal *r, const char *sch, int *norm,int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	mgl_beam_val(_GR_, *val,_DA_(tr),_DA_(g1),_DA_(g2),_DA_(a),*r,s,*norm);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_beam_(uintptr_t *gr, uintptr_t *tr, uintptr_t *g1, uintptr_t *g2, uintptr_t *a, mreal *r, const char *sch, int *norm, int *num,int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	mgl_beam(_GR_, _DA_(tr), _DA_(g1), _DA_(g2), _DA_(a), *r,s,*norm,*num);	delete []s;	}
//-----------------------------------------------------------------------------
