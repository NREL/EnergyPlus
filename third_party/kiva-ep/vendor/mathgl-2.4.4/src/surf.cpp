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
#include "mgl2/surf.h"
#include "mgl2/data.h"
#include "mgl2/eval.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
void static mgl_mesh_plot(mglBase *gr, long kq, long n, long m, int how)
{
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n*m, dx = n>d?n/d:1, dy = m>d?m/d:1;
	// NOTE: number of lines in each direction can be reduced too
	if(how&1)	for(long j=0;j<m;j+=dy)
	{
		long s=0;
		for(long i=0;i<n-1;i++)
		{	long iq=kq+i+n*j;	if(gr->ValidPnt(iq) && gr->ValidPnt(iq+1))	s++;	}
		d = gr->FaceNum>0 ? gr->FaceNum+1 : n;	s = s>d?s/d:1;
		gr->curve_plot(1+(n-1)/s,kq+n*j,s);
	}
	if(how&2)	for(long i=0;i<n;i+=dx)
	{
		long s=0;
		for(long j=0;j<m-1;j++)
		{	long iq=kq+i+n*j;	if(gr->ValidPnt(iq) && gr->ValidPnt(iq+n))	s++;	}
		d = gr->FaceNum>0 ? gr->FaceNum+1 : n;	s = s>d?s/d:1;
		gr->curve_plot(1+(m-1)/s,kq+i,n*s);
	}
}
//-----------------------------------------------------------------------------
void static mgl_surf_plot(mglBase *gr, long kq, long n, long m)
{
	long s=0;
	for(long j=0;j<m-1;j++)	for(long i=0;i<n-1;i++)
	{	long iq = kq+i+n*j;
		if(gr->ValidPnt(iq) && gr->ValidPnt(iq+1) && gr->ValidPnt(iq+n) && gr->ValidPnt(iq+n+1))
			s++;	}
	long dx=1,dy=1;
	if(gr->FaceNum && s>gr->FaceNum*gr->FaceNum)
	{
		int d = gr->FaceNum+1,ns=n*s/((n-1)*(m-1)),ms=m*s/((n-1)*(m-1));
		dx = ns>d?ns/d:1;		dy = ms>d?ms/d:1;
	}
	for(long j=0;j<m-dy;j+=dy)	for(long i=0;i<n-dx;i+=dx)
	{	long iq = kq+i+n*j;	gr->quad_plot(iq,iq+dx,iq+n*dy,iq+n*dy+dx);	}
}
//-----------------------------------------------------------------------------
//
//	Plot by formulas series
//
//-----------------------------------------------------------------------------
HMDT MGL_NO_EXPORT mglFormulaCalc(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fsurf(HMGL gr, const char *eqZ, const char *sch, const char *opt)
{	// NOTE Strong function variation analysis can be added here
	if(eqZ==0 || eqZ[0]==0)	return;		// nothing to plot
	mreal r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 100:long(r+0.5);
	mglData z(n,n);
	mglDataV x(n,n,1, gr->Min.x,gr->Max.x,'x');	x.Name(L"x");
	mglDataV y(n,n,1, gr->Min.y,gr->Max.y,'y');	y.Name(L"y");
	mglDataV t(n,n);	t.Name(L"#$mgl");
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&t);
	z.Move(mglFormulaCalc(eqZ,list));
	mgl_surf(gr, &z, sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fsurf_xyz(HMGL gr, const char *eqX, const char *eqY, const char *eqZ, const char *sch, const char *opt)
{	// NOTE Strong function variation analysis can be added here
	if(eqZ==0 || eqZ[0]==0)	return;		// nothing to plot
	mreal r = gr->SaveState(opt);
	long n = (mgl_isnan(r) || r<=0) ? 100:long(r+0.5);
	mglData z(n,n), x(n,n), y(n,n);
	mglDataV u(n,n,1, 0,1,'x');	u.Name(L"u");
	mglDataV v(n,n,1, 0,1,'y');	v.Name(L"v");
	mglDataV t(n,n);	t.Name(L"#$mgl");
	std::vector<mglDataA*> list;
	list.push_back(&u);	list.push_back(&v);	list.push_back(&t);
	x.Move(mglFormulaCalc(eqX,list));
	y.Move(mglFormulaCalc(eqY,list));
	z.Move(mglFormulaCalc(eqZ,list));
	mgl_surf_xy(gr,&x,&y,&z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fsurf_(uintptr_t *gr, const char *fy, const char *stl, const char *opt, int ly, int ls, int lo)
{	char *s=new char[ly+1];	memcpy(s,fy,ly);	s[ly]=0;
	char *p=new char[ls+1];	memcpy(p,stl,ls);	p[ls]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_fsurf(_GR_, s, p, o);	delete []o;	delete []s;	delete []p;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fsurf_xyz_(uintptr_t *gr, const char *fx, const char *fy, const char *fz, const char *stl, const char *opt, int lx, int ly, int lz, int ls, int lo)
{
	char *sx=new char[lx+1];	memcpy(sx,fx,lx);	sx[lx]=0;
	char *sy=new char[ly+1];	memcpy(sy,fy,ly);	sy[ly]=0;
	char *sz=new char[lz+1];	memcpy(sz,fz,lz);	sz[lz]=0;
	char *p=new char[ls+1];		memcpy(p,stl,ls);	p[ls]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_fsurf_xyz(_GR_, sx, sy, sz, p, o);	delete []o;
	delete []sx;	delete []sy;	delete []sz;	delete []p;
}
//-----------------------------------------------------------------------------
//
//	Mesh series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mesh_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Mesh"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Mesh",cgid++);
	gr->SetPenPal(sch,0,false);
	long ss = gr->AddTexture(sch);
	gr->Reserve(n*m*z->GetNz());

	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long kq = gr->AllocPnts(n*m);
#pragma omp parallel for collapse(2)
		for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
		{
			double zz = z->v(i,j,k);
			gr->AddPntQ(kq+i+n*j, mglPoint(GetX(x,i,j,k).x, GetY(y,i,j,k).x, zz),gr->GetC(ss,zz));
		}
		mgl_mesh_plot(gr,kq,n,m,3);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mesh(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_mesh_xy(gr,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mesh_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_mesh_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mesh_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_mesh(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Fall series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fall_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Fall"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Fall",cgid++);
	gr->SetPenPal(sch,0,false);
	long ss = gr->AddTexture(sch);
	gr->Reserve(n*m*z->GetNz());

	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long kq = gr->AllocPnts(n*m);
#pragma omp parallel for collapse(2)
		for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
		{
			double zz = z->v(i,j,k);
			gr->AddPntQ(kq+i+n*j, mglPoint(GetX(x,i,j,k).x, GetY(y,i,j,k).x, zz),gr->GetC(ss,zz));
		}
		mgl_mesh_plot(gr,kq,n,m, (mglchr(sch,'x')) ? 2:1);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fall(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_fall_xy(gr,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fall_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_fall_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fall_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_fall(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Grid series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Grid"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Grid",cgid++);
	double zVal = gr->Min.z;
	gr->SetPenPal(sch?sch:"k-");
	gr->Reserve(n*m*z->GetNz());

	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		if(z->GetNz()>1)	zVal = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(k)/(z->GetNz()-1);
		const long kq = gr->AllocPnts(n*m);
#pragma omp parallel for collapse(2)
		for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			gr->AddPntQ(kq+i+n*j,mglPoint(GetX(x,i,j,k).x, GetY(y,i,j,k).x, zVal),gr->CDef);
		mgl_mesh_plot(gr,kq,n,m,3);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid(HMGL gr, HCDT z,const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_grid_xy(gr,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_grid_xy(_GR_,_DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid_(uintptr_t *gr, uintptr_t *a,const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_grid(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Surf series
//
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_surf_gen(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch)
{
	long n=z->GetNx(),m=z->GetNy();
	long ss = gr->AddTexture(sch);
	bool wire = (mglchr(sch,'#'));
	gr->Reserve((n+1)*(m+1)*z->GetNz()*(wire?2:1));

	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long kq = gr->AllocPnts(n*m);
		if(a)
		{
#pragma omp parallel for collapse(2)
			for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			{
				mglPoint xx=GetX(x,i,j,k), yy=GetY(y,i,j,k);
				mglPoint q(xx.y, yy.y, z->dvx(i,j,k));
				mglPoint s(xx.z, yy.z, z->dvy(i,j,k));
				gr->AddPntQ(kq+i+n*j,mglPoint(xx.x, yy.x, z->v(i,j,k)),
					gr->GetC(ss,c->v(i,j,k)), q^s, gr->GetA(a->v(i,j,k)));
			}
		}
		else
		{
#pragma omp parallel for collapse(2)
			for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
			{
				mglPoint xx=GetX(x,i,j,k), yy=GetY(y,i,j,k);
				mglPoint q(xx.y, yy.y, z->dvx(i,j,k));
				mglPoint s(xx.z, yy.z, z->dvy(i,j,k));
				gr->AddPntQ(kq+i+n*j,mglPoint(xx.x, yy.x, z->v(i,j,k)), gr->GetC(ss,c->v(i,j,k)), q^s);
			}
		}
		if(sch && mglchr(sch,'.'))
			for(long i=0;i<n*m;i++)	gr->mark_plot(kq+i,'.');
		else	mgl_surf_plot(gr,kq,n,m);
		if(wire)
		{
			gr->SetPenPal("k-");
			const long nq = gr->AllocPnts(n*m);
#pragma omp parallel for
			for(long i=0;i<n*m;i++)	gr->CopyNtoC(nq+i,kq+i,gr->CDef);
			mgl_mesh_plot(gr,nq,n,m,3);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,0,"Surf"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Surf",cgid++);
	mgl_surf_gen(gr, x, y, z, z, 0, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_surf_xy(gr,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surf_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surf(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	SurfCA series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfca_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,c,"SurfCA"))	return;
	if(mgl_check_dim2(gr,x,y,z,a,"SurfCA"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("SurfCA",cgid++);
	mgl_surf_gen(gr, x, y, z, c, a, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfca(HMGL gr, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_surfca_xy(gr,&x,&y,z,c,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfca_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfca_xy(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(c), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfca_(uintptr_t *gr, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfca(_GR_, _DA_(z), _DA_(c), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	SurfC series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfc_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,c,"SurfC"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("SurfC",cgid++);
	mgl_surf_gen(gr, x, y, z, c, 0, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfc(HMGL gr, HCDT z, HCDT c, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_surfc_xy(gr,&x,&y,z,c,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfc_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfc_xy(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfc_(uintptr_t *gr, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfc(_GR_, _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	SurfA series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfa_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,c,"SurfA"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("SurfA",cgid++);
	mgl_surf_gen(gr, x, y, z, z, c, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfa(HMGL gr, HCDT z, HCDT c, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_surfa_xy(gr,&x,&y,z,c,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfa_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfa_xy(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_surfa_(uintptr_t *gr, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_surfa(_GR_, _DA_(z), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Belt series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_belt_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_beltc_xy(gr,x,y,z,z,sch,opt);	}
void MGL_EXPORT mgl_beltc_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,c,"Belt"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Belt",cgid++);
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n*m, dx = n>d?n/d:1, dy = m>d?m/d:1;
	long ss = gr->AddTexture(sch);
	gr->Reserve(2*n*m*z->GetNz());
	bool how = !mglchr(sch,'x');

	int dk = c->GetNz()>=z->GetNz() ? 1:0;
	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		if(how)	for(long i=0;i<n-dx;i+=dx)
		{
			const long kq = gr->AllocPnts(2*m);
#pragma omp parallel for
			for(long j=0;j<m;j++)
			{
				mglPoint xx=GetX(x,i,j,k), yy=GetY(y,i,j,k);
				mglPoint p1(xx.x, yy.x, z->v(i,j,k));
				mglPoint s(xx.z, yy.z, z->dvy(i,j,k));
				mglPoint q(xx.y, yy.y, 0);	s = q^s;
				double cc = gr->GetC(ss,c->v(i,j,dk*k));
				mglPoint p2(GetX(x,i+dx,j,k).x,GetY(y,i+dx,j,k).x,p1.z);
				gr->AddPntQ(kq+2*j,p1,cc,s);
				gr->AddPntQ(kq+2*j+1,p2,cc,s);
			}
			mgl_surf_plot(gr,kq,2,m);
		}
		else	for(long j=0;j<m-dy;j+=dy)
		{
			const long kq = gr->AllocPnts(2*n);
#pragma omp parallel for
			for(long i=0;i<n;i++)	// ñîçäàåì ìàññèâ òî÷åê
			{
				mglPoint xx=GetX(x,i,j,k), yy=GetY(y,i,j,k);
				mglPoint p1(xx.x, yy.x, z->v(i,j,k));
				mglPoint q(xx.y, yy.y, z->dvx(i,j,k));
				mglPoint s(xx.z, yy.z, 0);	s = q^s;
				double cc = gr->GetC(ss,c->v(i,j,dk*k));
				mglPoint p2(GetX(x,i,j+dy,k).x,GetY(y,i,j+dy,k).x,p1.z);
				gr->AddPntQ(kq+2*i,p1,cc,s);
				gr->AddPntQ(kq+2*i+1,p2,cc,s);
			}
			mgl_surf_plot(gr,kq,2,n);
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_belt(HMGL gr, HCDT z, const char *sch, const char *opt)
{	mgl_beltc(gr,z,z,sch,opt);	}
void MGL_EXPORT mgl_beltc(HMGL gr, HCDT z, HCDT c, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()), y(z->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_beltc_xy(gr,&x,&y,z,c,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_belt_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_belt_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_belt_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_belt(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_beltc_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_beltc_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), _DA_(c), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_beltc_(uintptr_t *gr, uintptr_t *a, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_beltc(_GR_, _DA_(a), _DA_(c), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Dens series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens_xy(HMGL gr, HCDT x, HCDT y, HCDT c, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,c,0,"Dens"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Dens",cgid++);
	mreal	zVal = gr->Min.z;

	mglDataV z(c->GetNx(),c->GetNy(),c->GetNz());
	if(z.GetNz()>1)	z.Fill(gr->Min.z,gr->Max.z,'z');
	else	z.Fill(zVal);
	mgl_surf_gen(gr, x, y, &z, c, 0, sch);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens(HMGL gr, HCDT c, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(c->GetNx()), y(c->GetNy());
	x.Fill(gr->Min.x, gr->Max.x);
	y.Fill(gr->Min.y, gr->Max.y);
	mgl_dens_xy(gr,&x,&y,c,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_dens_xy(_GR_,_DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_dens(_GR_,_DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	STFA series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stfa_xy(HMGL gr, HCDT x, HCDT y, HCDT re, HCDT im, int dn, const char *sch, const char *opt)
{	mglData tmp(mglSTFA(*re,*im,dn,'x'));	mgl_dens_xy(gr,x,y,&tmp,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stfa(HMGL gr, HCDT re, HCDT im, int dn, const char *sch, const char *opt)
{	mglData tmp(mglSTFA(*re,*im,dn,'x'));	mgl_dens(gr,&tmp,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stfa_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *re, uintptr_t *im, int *dn, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_stfa_xy(_GR_,_DA_(x), _DA_(y), _DA_(re), _DA_(im), *dn, s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stfa_(uintptr_t *gr, uintptr_t *re, uintptr_t *im, int *dn, const char *sch, const char *opt, int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_stfa(_GR_,_DA_(re), _DA_(im), *dn, s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Boxs series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxs_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Boxs",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Boxs",cgid++);
	long ly = y->GetNy()>=m ? y->GetNy() : y->GetNx(), lx = x->GetNx();
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n*m, dx = n>d?n/d:1, dy = m>d?m/d:1;

	long ss = gr->AddTexture(sch);
	bool wire = mglchr(sch,'#');
	bool full = mglchr(sch,'@');
	gr->Reserve(8*n*m*z->GetNz());

	mglPoint t(wire||full?NAN:0,0,1);
	double z0=gr->GetOrgZ('x');
	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long ni=1+(n-1)/dx, mi=1+(m-1)/dy;
		const long kq = gr->AllocPnts(8*ni*mi);
#pragma omp parallel for collapse(2)
		for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
		{
			long i = di*dx, j = dj*dy;
			double zz = z->v(i,j,k), c  = gr->GetC(ss,zz);
			mglPoint xx=GetX(x,i,j,k), yy = GetY(y,i,j,k);
			double x1 = i<lx-dx ? GetX(x,i+dx,j,k).x:NAN;
			double y1 = i<lx-dx ? GetY(y,i+dx,j,k).x:NAN;
			double x2 = j<ly-dy ? GetX(x,i,j+dy,k).x:NAN;
			double y2 = j<ly-dy ? GetY(y,i,j+dy,k).x:NAN;
			double x3 = i<lx-dx && j<ly-dy ? GetX(x,i+dx,j+dy,k).x:NAN;
			double y3 = i<lx-dx && j<ly-dy ? GetY(y,i+dx,j+dy,k).x:NAN;
			double z1 = i<n-dx?z->v(i+dx,j,k):NAN;
			double z2 = j<m-dy?z->v(i,j+dy,k):NAN;
			mglPoint q(xx.y,yy.y,0);
			mglPoint s(xx.z,yy.z,0);
			long iq = kq+8*(di+ni*dj);
			gr->AddPntQ(iq,mglPoint(xx.x,yy.x,zz),c,t);
			gr->AddPntQ(iq+1,mglPoint(x1,y1,zz),c,t);
			gr->AddPntQ(iq+2,mglPoint(x2,y2,zz),c,t);
			gr->AddPntQ(iq+3,mglPoint(x3,y3,zz),c,t);

			if(full)
			{
				gr->AddPntQ(iq+4,mglPoint(xx.x,yy.x,z0),c,t);
				gr->AddPntQ(iq+5,mglPoint(x1,y1,z0),c,t);
				gr->AddPntQ(iq+6,mglPoint(x2,y2,z0),c,t);
				gr->AddPntQ(iq+7,mglPoint(x3,y3,z0),c,t);
			}
			else
			{
				gr->AddPntQ(iq+4,mglPoint(x1,y1,z1),c,wire?t:q);
				gr->AddPntQ(iq+5,mglPoint(x3,y3,z1),c,wire?t:q);
				gr->AddPntQ(iq+6,mglPoint(x2,y2,z2),c,wire?t:s);
				gr->AddPntQ(iq+7,mglPoint(x3,y3,z2),c,wire?t:s);
			}
		}
		if(wire)	for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
		{
			long iq = kq+8*(di+ni*dj);
			gr->line_plot(iq,iq+1);		gr->line_plot(iq,iq+2);
			gr->line_plot(iq+3,iq+1);	gr->line_plot(iq+3,iq+2);

			if(full)
			{
				gr->line_plot(iq+4,iq+5);	gr->line_plot(iq+4,iq+6);
				gr->line_plot(iq+7,iq+5);	gr->line_plot(iq+7,iq+6);
				gr->line_plot(iq,iq+4);		gr->line_plot(iq+2,iq+6);
				gr->line_plot(iq+1,iq+5);	gr->line_plot(iq+3,iq+7);
			}
			else
			{
				gr->line_plot(iq+1,iq+4);	gr->line_plot(iq+5,iq+4);
				gr->line_plot(iq+5,iq+3);	gr->line_plot(iq+2,iq+6);
				gr->line_plot(iq+3,iq+7);	gr->line_plot(iq+6,iq+7);
			}
		}
		else	for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
		{
			long iq = kq+8*(di+ni*dj);
			gr->quad_plot(iq,iq+1,iq+2,iq+3);
			if(full)
			{
				gr->quad_plot(iq,iq+1,iq+4,iq+5);	gr->quad_plot(iq,iq+2,iq+4,iq+6);
				gr->quad_plot(iq+3,iq+1,iq+7,iq+5);	gr->quad_plot(iq+3,iq+2,iq+7,iq+6);
				gr->quad_plot(iq+4,iq+5,iq+6,iq+7);
			}
			else
			{
				gr->quad_plot(iq+1,iq+3,iq+4,iq+5);	gr->quad_plot(iq+2,iq+3,iq+6,iq+7);
			}
		}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxs(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()+1), y(z->GetNy()+1);
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_boxs_xy(gr,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxs_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_boxs_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_boxs_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_boxs(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Tile series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tile_xyc(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,c,"Tile",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Tile",cgid++);
	long ly = x->GetNy()>=z->GetNy() ? y->GetNy() : y->GetNx(), lx = x->GetNx();
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n*m, dx = n>d?n/d:1, dy = m>d?m/d:1;

	long ss = gr->AddTexture(sch);
	gr->Reserve(4*n*m*z->GetNz());
	bool alongX = mglchr(sch,'x');
	bool alongY = mglchr(mglchr(sch,':'),'y');
	const long ni=1+(n-1)/dx, mi=1+(m-1)/dy;

	mglPoint s(0,0,1);
	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long kq = gr->AllocPnts(4*ni*mi);
		if(alongX)
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double zz = z->v(i,j,k), cc = gr->GetC(ss,c->v(i,j,k));
				double xx = GetX(x,i,j,k).x, yy = GetY(y,i,j,k).x;
				gr->AddPntQ(iq,mglPoint(xx,yy,zz),cc,s);
				zz = i<lx-dx ? z->v(i+dx,j,k):NAN;
				yy = i<lx-dx ? GetY(y,i+dx,j,k).x:NAN;
				gr->AddPntQ(iq+1,mglPoint(xx,yy,zz),cc,s);
				zz = j<ly-dy ? z->v(i,j+dy,k):NAN;
				yy = j<ly-dy ? GetY(y,i,j+dy,k).x:NAN;
				gr->AddPntQ(iq+2,mglPoint(xx,yy,zz),cc,s);
				zz = i<lx-dx && j<ly-dy ? z->v(i+dx,j+dy,k):NAN;
				yy = i<lx-dx && j<ly-dy ? GetY(y,i+dx,j+dy,k).x:NAN;
				gr->AddPntQ(iq+3,mglPoint(xx,yy,zz),cc,s);
			}
		else if(alongY)
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double zz = z->v(i,j,k), cc = gr->GetC(ss,c->v(i,j,k));
				double xx = GetX(x,i,j,k).x, yy = GetY(y,i,j,k).x;
				gr->AddPntQ(iq,mglPoint(xx,yy,zz),cc,s);
				xx = i<lx-dx ? GetX(x,i+dx,j,k).x:NAN;
				zz = i<lx-dx ? z->v(i+dx,j,k):NAN;
				gr->AddPntQ(iq+1,mglPoint(xx,yy,zz),cc,s);
				xx = j<ly-dy ? GetX(x,i,j+dy,k).x:NAN;
				zz = j<ly-dy ? z->v(i,j+dy,k):NAN;
				gr->AddPntQ(iq+2,mglPoint(xx,yy,zz),cc,s);
				xx = i<lx-dx && j<ly-dy ? GetX(x,i+dx,j+dy,k).x:NAN;
				zz = i<lx-dx && j<ly-dy ? z->v(i+dx,j+dy,k):NAN;
				gr->AddPntQ(iq+3,mglPoint(xx,yy,zz),cc,s);
			}
		else
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double zz = z->v(i,j,k), cc = gr->GetC(ss,c->v(i,j,k));
				double xx = GetX(x,i,j,k).x, yy = GetY(y,i,j,k).x;
				gr->AddPntQ(iq,mglPoint(xx,yy,zz),cc,s);
				xx = i<lx-dx ? GetX(x,i+dx,j,k).x:NAN;
				yy = i<lx-dx ? GetY(y,i+dx,j,k).x:NAN;
				gr->AddPntQ(iq+1,mglPoint(xx,yy,zz),cc,s);
				xx = j<ly-dy ? GetX(x,i,j+dy,k).x:NAN;
				yy = j<ly-dy ? GetY(y,i,j+dy,k).x:NAN;
				gr->AddPntQ(iq+2,mglPoint(xx,yy,zz),cc,s);
				xx = i<lx-dx && j<ly-dy ? GetX(x,i+dx,j+dy,k).x:NAN;
				yy = i<lx-dx && j<ly-dy ? GetY(y,i+dx,j+dy,k).x:NAN;
				gr->AddPntQ(iq+3,mglPoint(xx,yy,zz),cc,s);
			}
		for(long i=0;i<ni*mi;i++)
		{	long iq=kq+4*i;	gr->quad_plot(iq,iq+1,iq+2,iq+3);	}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tile_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{	mgl_tile_xyc(gr,x,y,z,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tile(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()+1), y(z->GetNy()+1);
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_tile_xyc(gr,&x,&y,z,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tile_xyc_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tile_xyc(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(c), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tile_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tile_xy(_GR_, _DA_(x), _DA_(y), _DA_(z), s, o);	delete []o;	delete []s;	}
	//-----------------------------------------------------------------------------
	void MGL_EXPORT mgl_tile_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tile(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	TileS series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tiles_xyc(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT s, HCDT c, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,s,"TileS",true))	return;
	if(mgl_check_dim2(gr,x,y,z,c,"TileS",true))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("TileS",cgid++);
	long ly = x->GetNy()>=z->GetNy() ? y->GetNy() : y->GetNx(), lx = x->GetNx();
	int d = gr->MeshNum>0 ? gr->MeshNum+1 : n*m, dx = n>d?n/d:1, dy = m>d?m/d:1;

	long sc = gr->AddTexture(sch);
	gr->Reserve(4*n*m*z->GetNz());
	bool alongX = mglchr(sch,'x');
	bool alongY = mglchr(mglchr(sch,':'),'y');
	const long ni=1+(n-1)/dx, mi=1+(m-1)/dy;

	mglPoint t(0,0,1);
	for(long k=0;k<z->GetNz();k++)
	{
		if(gr->NeedStop())	break;
		const long kq = gr->AllocPnts(4*ni*mi);
		if(alongX)
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double xx = GetX(x,i,j,k).x, cc = gr->GetC(sc,c->v(i,j,k));
				double ss = (1-gr->GetA(s->v(i,j,k)))/2, sm = 1-ss;
				double x1 = z->v(i,j,k), y1 = GetY(y,i,j,k).x;
				double x2=NAN,x3=NAN,x4=NAN,y2=NAN,y3=NAN,y4=NAN;
				if(i<lx-dx)	{	x2 = z->v(i+dx,j,k)-x1;	y2 = GetY(y,i+dx,j,k).x-y1;	}
				if(j<ly-dy)	{	x4 = z->v(i,j+dy,k)-x1;	y4 = GetY(y,i,j+dy,k).x-y1;	}
				if(i<lx-dx && j<ly-dy)
				{	x3 = z->v(i+dx,j+dy,k)-x2-x4-x1;	y3 = GetY(y,i+dx,j+dy,k).x-y2-y4-y1;	}

				gr->AddPntQ(iq,mglPoint(xx, y1+y2*ss+y4*ss+y3*ss*ss, x1+x2*ss+x4*ss+x3*ss*ss),cc,t);
				gr->AddPntQ(iq+1,mglPoint(xx, y1+y2*sm+y4*ss+y3*ss*sm, x1+x2*sm+x4*ss+x3*ss*sm),cc,t);
				gr->AddPntQ(iq+2,mglPoint(xx, y1+y2*ss+y4*sm+y3*ss*sm, x1+x2*ss+x4*sm+x3*ss*sm),cc,t);
				gr->AddPntQ(iq+3,mglPoint(xx, y1+y2*sm+y4*sm+y3*sm*sm, x1+x2*sm+x4*sm+x3*sm*sm),cc,t);
			}
		else if(alongY)
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double yy = GetY(y,i,j,k).x, cc = gr->GetC(sc,c->v(i,j,k));
				double ss = (1-gr->GetA(s->v(i,j,k)))/2, sm = 1-ss;
				double x1 = GetX(x,i,j,k).x, y1 = z->v(i,j,k);
				double x2=NAN,x3=NAN,x4=NAN,y2=NAN,y3=NAN,y4=NAN;
				if(i<lx-dx)	{	x2 = GetX(x,i+dx,j,k).x-x1;	y2 = z->v(i+dx,j,k)-y1;	}
				if(j<ly-dy)	{	x4 = GetX(x,i,j+dy,k).x-x1;	y4 = z->v(i,j+dy,k)-y1;	}
				if(i<lx-dx && j<ly-dy)
				{	x3 = GetX(x,i+dx,j+dy,k).x-x2-x4-x1;	y3 = z->v(i+dx,j+dy,k)-y2-y4-y1;	}
				gr->AddPntQ(iq,mglPoint(x1+x2*ss+x4*ss+x3*ss*ss, yy, y1+y2*ss+y4*ss+y3*ss*ss),cc,t);
				gr->AddPntQ(iq+1,mglPoint(x1+x2*sm+x4*ss+x3*ss*sm, yy, y1+y2*sm+y4*ss+y3*ss*sm),cc,t);
				gr->AddPntQ(iq+2,mglPoint(x1+x2*ss+x4*sm+x3*ss*sm, yy, y1+y2*ss+y4*sm+y3*ss*sm),cc,t);
				gr->AddPntQ(iq+3,mglPoint(x1+x2*sm+x4*sm+x3*sm*sm, yy, y1+y2*sm+y4*sm+y3*sm*sm),cc,t);
			}
		else
#pragma omp parallel for collapse(2)
			for(long dj=0;dj<mi;dj++)	for(long di=0;di<ni;di++)
			{
				long i = di*dx, j = dj*dy, iq = kq+4*(di+ni*dj);
				double zz = z->v(i,j,k), cc = gr->GetC(sc,c->v(i,j,k));
				double ss = (1-gr->GetA(s->v(i,j,k)))/2, sm = 1-ss;
				double x1 = GetX(x,i,j,k).x, y1 = GetY(y,i,j,k).x;
				double x2=NAN,x3=NAN,x4=NAN,y2=NAN,y3=NAN,y4=NAN;
				if(i<lx-dx)	{	x2 = GetX(x,i+dx,j,k).x-x1;	y2 = GetY(y,i+dx,j,k).x-y1;	}
				if(j<ly-dy)	{	x4 = GetX(x,i,j+dy,k).x-x1;	y4 = GetY(y,i,j+dy,k).x-y1;	}
				if(i<lx-dx && j<ly-dy)
				{	x3 = GetX(x,i+dx,j+dy,k).x-x2-x4-x1;	y3 = GetY(y,i+dx,j+dy,k).x-y2-y4-y1;	}
				gr->AddPntQ(iq,mglPoint(x1+x2*ss+x4*ss+x3*ss*ss, y1+y2*ss+y4*ss+y3*ss*ss, zz),cc,t);
				gr->AddPntQ(iq+1,mglPoint(x1+x2*sm+x4*ss+x3*ss*sm, y1+y2*sm+y4*ss+y3*ss*sm, zz),cc,t);
				gr->AddPntQ(iq+2,mglPoint(x1+x2*ss+x4*sm+x3*ss*sm, y1+y2*ss+y4*sm+y3*ss*sm, zz),cc,t);
				gr->AddPntQ(iq+3,mglPoint(x1+x2*sm+x4*sm+x3*sm*sm, y1+y2*sm+y4*sm+y3*sm*sm, zz),cc,t);
			}
		for(long i=0;i<ni*mi;i++)
		{	long iq=kq+4*i;	gr->quad_plot(iq,iq+1,iq+2,iq+3);	}
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tiles_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT s, const char *sch, const char *opt)
{	mgl_tiles_xyc(gr,x,y,z,s,z,sch,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tiles(HMGL gr, HCDT z, HCDT s, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(z->GetNx()+1), y(z->GetNy()+1);
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_tiles_xyc(gr,&x,&y,z,s,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tiles_xyc_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, uintptr_t *c, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tiles_xyc(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(r), _DA_(c), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_tiles_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tiles_xy(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(r), s, o);	delete []o;	delete []s;	}
	//-----------------------------------------------------------------------------
	void MGL_EXPORT mgl_tiles_(uintptr_t *gr, uintptr_t *a, uintptr_t *r, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_tiles(_GR_, _DA_(a), _DA_(r), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Map series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_map_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	long n=ax->GetNx(),m=ax->GetNy();
	if(mgl_check_dim2(gr,x,y,ax,ay,"Map"))	return;

	bool nboth = !(x->GetNx()==n && y->GetNx()==n && x->GetNy()==m && y->GetNy()==m);
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Map",cgid++);

	long ss = gr->AddTexture(mgl_have_color(sch)?sch:"rgb",2);
	long s = nboth ?1:n;
	mglPoint t(NAN);
	gr->Reserve(n*m);

	const long kq = gr->AllocPnts(n*m);
#pragma omp parallel for collapse(2)
	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		long s1 = i>0 ? 1:0, s2 = i<n-1 ? 1:0;
		mreal xdx = (ax->v(i+s2,j)-ax->v(i-s1,j))/(GetX(x,i+s2,j).x-GetX(x,i-s1,j).x);
		mreal ydx = (ay->v(i+s2,j)-ay->v(i-s1,j))/(GetX(x,i+s2,j).x-GetX(x,i-s1,j).x);
		s1 = j>0 ? s:0;		s2 = j<m-1 ? s:0;
		mreal xdy = (ax->v(i,j+s2)-ax->v(i,j-s1))/(GetY(y,i,j+s2).x-GetY(y,i,j-s1).x);
		mreal ydy = (ay->v(i,j+s2)-ay->v(i,j-s1))/(GetY(y,i,j+s2).x-GetY(y,i,j-s1).x);
		xdx = xdx*ydy - xdy*ydx;	// Jacobian
		mreal xx,yy;

		if(nboth)
		{
			xx = (x->v(i) - gr->Min.x)/(gr->Max.x - gr->Min.x);
			yy = (y->v(j) - gr->Min.y)/(gr->Max.y - gr->Min.y);
		}
		else
		{
			xx = (x->v(i,j) - gr->Min.x)/(gr->Max.x - gr->Min.x);
			yy = (y->v(i,j) - gr->Min.y)/(gr->Max.y - gr->Min.y);
		}
		if(xx<0)	xx=0;
		if(xx>=1)	xx=1/MGL_FEPSILON;
		if(yy<0)	yy=0;
		if(yy>=1)	yy=1/MGL_FEPSILON;
		gr->AddPntQ(kq+i+n*j,mglPoint(ax->v(i,j), ay->v(i,j), xdx),gr->GetC(ss,xx,false),t,yy);
	}
	if(sch && mglchr(sch,'.'))
		for(long i=0;i<n*m;i++)	gr->mark_plot(kq+i,'.');
	else	mgl_surf_plot(gr,kq,n,m);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_map(HMGL gr, HCDT ax, HCDT ay, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(ax->GetNx()), y(ax->GetNy());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	mgl_map_xy(gr,&x,&y,ax,ay,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_map_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_map_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_map_(uintptr_t *gr, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];		memcpy(o,opt,lo);	o[lo]=0;
	mgl_map(_GR_, _DA_(a), _DA_(b), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
