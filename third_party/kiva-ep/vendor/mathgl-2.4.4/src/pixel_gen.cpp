/***************************************************************************
 * pixel.cpp is part of Math Graphic Library
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
#include "mgl2/canvas.h"
#include "mgl2/thread.h"
#if MGL_HAVE_OMP
#include <omp.h>
#endif

//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHREAD
static void *mgl_canvas_thr(void *par)
{	mglThreadG *t=(mglThreadG *)par;	(t->gr->*(t->f))(t->id, t->n, t->p);	return NULL;	}
#endif
void mglStartThread(void (mglCanvas::*func)(long i, long n, const void *p), mglCanvas *gr, long n, const void *p=NULL)
{
	if(!func || !gr)	return;
#if MGL_HAVE_PTHREAD
	if(mglNumThr<1)	mgl_set_num_thr(0);
	if(mglNumThr>1)
	{
		pthread_t *tmp=new pthread_t[mglNumThr];
		mglThreadG *par=new mglThreadG[mglNumThr];
		for(long i=0;i<mglNumThr;i++)	// put parameters into the structure
		{	par[i].gr=gr;	par[i].f=func;	par[i].n=n;	par[i].p=p;	par[i].id=i;	}
		for(long i=0;i<mglNumThr;i++)	pthread_create(tmp+i, 0, mgl_canvas_thr, par+i);
		for(long i=0;i<mglNumThr;i++)	pthread_join(tmp[i], 0);
		delete []tmp;	delete []par;
	}
	else
#endif
	{	mglNumThr = 1;	(gr->*func)(0,n,p);	}
}
//-----------------------------------------------------------------------------
void mglCanvas::SetSize(int w,int h,bool clf)
{
	if(w<=0 || h<=0)	{	SetWarn(mglWarnSize,"SetSize");	return;	}
	if(Width==w && Height==h)
	{
		InPlot(0,1,0,1,false);
		if(clf || (Quality&4))	Clf();
		return;
	}

	const double dx = double(w)/Width;
	const double dy = double(h)/Height;
	const double dz = sqrt(double(w*h))/Depth;
	Width = w;	Height = h;	Depth = long(sqrt(double(w*h)));
	const long s = long(w)*long(h);
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexClf);
#elif MGL_HAVE_OMP
	omp_set_lock((omp_lock_t*)lockClf);
#endif
	if(G)	{	delete []G;	delete []C;	delete []Z;	delete []G4;delete []GB;delete []OI;	G=0;	}
	G = new unsigned char[s*3];
	G4= new unsigned char[s*4];
	GB= new unsigned char[s*4];
	C = new unsigned char[s*12];
	Z = new float[s*3];	// only 3 planes
	OI= new int[s];
#pragma omp parallel for
	for(long i=0;i<s;i++)
	{	unsigned char *b=GB+4*i;
		b[0]=BDef[0];	b[1]=BDef[1];	b[2]=BDef[2];	b[3]=BDef[3];	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexClf);
#elif MGL_HAVE_OMP
	omp_unset_lock((omp_lock_t*)lockClf);
#endif

	InPlot(0,1,0,1,false);
	if(clf || (Quality&4))	Clf();
	else	// No clearing. So, need to scale
	{
#if MGL_HAVE_PTHREAD
		pthread_mutex_lock(&mutexPnt);
		pthread_mutex_lock(&mutexClf);
#elif MGL_HAVE_OMP
		omp_set_lock((omp_lock_t*)lockClf);
#endif
		const long m = long(Prm.size());
		double dd = dx>dy?dy:dx;
#pragma omp parallel for	// Scale text
		for(long i=0;i<m;i++)	if(Prm[i].type==4)
		{
			mglPnt &q = Pnt[Prm[i].n1];
			Prm[i].p *=dd;
			q.u *= dd;	q.v *= dd;
		}
		const long n = long(Pnt.size());
#pragma omp parallel for	// Scale coordinates
		for(long i=0;i<n;i++)
		{
			mglPnt &q = Pnt[i];
			q.x*=dx;	q.y*=dy;	q.z*=dz;
			q.xx*=dx;	q.yy*=dy;	q.zz*=dz;
			if(mgl_isnum(q.w))
			{	q.u*=dx;	q.v*=dy;	q.w*=dz;	}
		}
		for(size_t i=0;i<Sub.size();i++)
		{	mglBlock &q = Sub[i];	q.n1*=dx;	q.n2*=dx;	q.n3*=dy;	q.n4*=dy;	}
		for(size_t k=0;k<DrwDat.size();k++)	// scale frames too
		{
			mglStack<mglPnt>  &pnt = DrwDat[k].Pnt;
			const long n = long(pnt.size());
#pragma omp parallel for
			for(long i=0;i<n;i++)
			{
				mglPnt &q = pnt[i];
				q.x*=dx;	q.y*=dy;	q.z*=dz;
				q.xx*=dx;	q.yy*=dy;	q.zz*=dz;
				if(mgl_isnum(q.w))
				{	q.u*=dx;	q.v*=dy;	q.w*=dz;	}
			}
			std::vector<mglBlock>  &sub = DrwDat[k].Sub;
			for(size_t i=0;i<sub.size();i++)
			{	mglBlock &q = sub[i];	q.n1*=dx;	q.n2*=dx;	q.n3*=dy;	q.n4*=dy;	}
		}
#if MGL_HAVE_PTHREAD
		pthread_mutex_unlock(&mutexClf);
		pthread_mutex_unlock(&mutexPnt);
#elif MGL_HAVE_OMP
		omp_unset_lock((omp_lock_t*)lockClf);
#endif
		ClfZB();	Finish();
	}
}
//-----------------------------------------------------------------------------
void mglDrawReg::set(mglCanvas *gr, int nx, int ny, int m)
{
	int mx = m%nx, my = m/nx;	ObjId = gr->ObjId;
	PDef = gr->mask;	angle = gr->MaskAn;
	x1 = gr->GetWidth()*mx/nx;		y1 = gr->GetHeight()-gr->GetHeight()*(my+1)/ny;
	x2 = gr->GetWidth()*(mx+1)/nx-1;	y2 = gr->GetHeight()-gr->GetHeight()*my/ny-1;
}
//-----------------------------------------------------------------------------
void mglCanvas::PutDrawReg(mglDrawReg *d, const mglCanvas *gr)
{
	if(gr)
	{
		const int dd = d->x2 - d->x1;
		for(long j=d->y1;j<d->y2;j++)
		{
			long i = d->x1+Width*(Height-1-j);
			memcpy(OI+i,gr->OI+i,dd*sizeof(int));
			memcpy(Z+3*i,gr->Z+3*i,3*dd*sizeof(float));
			memcpy(C+12*i,gr->C+12*i,12*dd);
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::PostScale(const mglMatrix *M, mglPoint &p) const
{
	float f = 1./(2*M->pf),x=p.x,y=p.y,z=p.z;
	const float *b=M->b;
	p.x = M->x + f*(x*b[0] + y*b[1] + z*b[2]);
	p.y = M->y + f*(x*b[3] + y*b[4] + z*b[5]);
	p.z = M->z + f*(x*b[6] + y*b[7] + z*b[8]);
}
//-----------------------------------------------------------------------------
bool mglCanvas::ScalePoint(const mglMatrix *M, mglPoint &p, mglPoint &n, bool use_nan) const
{
	bool res = get(MGL_DISABLE_SCALE) || mglBase::ScalePoint(M,p,n,use_nan);
	PostScale(M,p);

	float nx=n.x, ny=n.y, nz=n.z;
	const float *b=M->b;
	n.x = nx*b[0] + ny*b[1] + nz*b[2];	// simpler for rotation only
	n.y = nx*b[3] + ny*b[4] + nz*b[5];
	n.z = nx*b[6] + ny*b[7] + nz*b[8];
	n.Normalize();
	return res;
}
//-----------------------------------------------------------------------------
long mglCanvas::ProjScale(int nf, long id, bool text)
{
	const mglPnt &pi = Pnt[id];
	mglPoint pp(pi.x,pi.y,pi.z), nn(pi.u,pi.v,pi.w), p, n;
	if(mgl_isnan(pp.x))	return -1;
	const float w=B1.b[0]/2, h=B1.b[4]/2, d=B1.b[8]/2, xx=B1.x-w/2, yy=B1.y-h/2;
	if(pi.sub>=0)
	{
		mglPoint q(RestorePnt(pp)/(2*B.pf));
		mglPoint u(RestorePnt(nn,true));	u.Normalize();
		if(nf==0)
		{	p.x = xx + q.x*w;	p.y = yy + q.y*h;	p.z = B1.z + q.z*d;	n = u;	}
		else if(nf==1)
		{	p.x = xx + q.x*w;	p.y = yy+h + q.z*h;	p.z = B1.z - q.y*d;	n.Set(u.x,u.z,-u.y);	}
		else if(nf==2)
		{	p.x = xx+w + q.z*w;	p.y = yy + q.y*h;	p.z = B1.z - q.x*d;	n.Set(u.z,u.y,-u.x);	}
		else
		{
			const float *b=B.b;	n = nn;
			p.x = xx+w + q.x*b[0]/2 + q.y*b[1]/2 + q.z*b[2]/2;
			p.y = yy+h + q.x*b[3]/2 + q.y*b[4]/2 + q.z*b[5]/2;
			p.z = B.z + q.x*b[6]/2 + q.y*b[7]/2 + q.z*b[8]/2;
		}
	}
	else
	{
		p.x = pi.x/2 + w*(nf/2);
		p.y = pi.y/2 + h*(nf%2);
		p.z = pi.z;	n=nn;
	}
	return CopyProj(id,p,text?n:nn,pi.sub);
}
//-----------------------------------------------------------------------------
void mglCanvas::LightScale(const mglMatrix *M, mglLight &ls)
{
	ls.p=ls.d;	ls.q=ls.r;
	ScalePoint(M,ls.q,ls.p,false);
	ls.p /= ls.p.norm();
}
//-----------------------------------------------------------------------------
void mglCanvas::LightScale(const mglMatrix *M)
{
	for(long i=0;i<10;i++)	if(light[i].n)	LightScale(M,light[i]);
	for(size_t j=0;j<Sub.size();j++)
		for(long i=0;i<10;i++)	if(light[i].n)	LightScale(&(Sub[j].B),Sub[j].light[i]);
}
//-----------------------------------------------------------------------------
// NOTE: Perspective is not fully supported now !!! Also it use LAST InPlot parameters!!!
mglPoint mglCanvas::RestorePnt(mglPoint ps, bool norm) const
{
	const float s3 = 2*B.pf;
	mglPoint p;

	const float W=Width/2, H=Height/2, D=Depth/2;
	const float *b=B.b,*d=Bp.b;
	float cx = B.z*d[2]+B.y*d[1]+B.x*d[0]-Bp.x*W-d[0]*W+W-d[1]*H-d[2]*D;
	const float c0 = b[6]*d[2]+b[3]*d[1]+b[0]*d[0];
	const float c1 = b[7]*d[2]+b[4]*d[1]+b[1]*d[0];
	const float c2 = b[8]*d[2]+b[5]*d[1]+b[2]*d[0];
	float cy = B.z*d[5]+B.y*d[4]+B.x*d[3]-d[3]*W-Bp.y*H-d[4]*H+H-d[5]*D;
	const float c3 = b[6]*d[5]+b[3]*d[4]+b[0]*d[3];
	const float c4 = b[7]*d[5]+b[4]*d[4]+b[1]*d[3];
	const float c5 = b[8]*d[5]+b[5]*d[4]+b[2]*d[3];
	float cz = B.z*d[8]+B.y*d[7]+B.x*d[6]-d[6]*W-d[7]*H-Bp.z*D-d[8]*D+D;
	const float c6 = b[6]*d[8]+b[3]*d[7]+b[0]*d[6];
	const float c7 = b[7]*d[8]+b[4]*d[7]+b[1]*d[6];
	const float c8 = b[8]*d[8]+b[5]*d[7]+b[2]*d[6];
	if(norm)	cx=cy=cz=0;

	if(mgl_isnum(ps.z))	// try to take into account perspective if z-value is provided
	{
		float dd = get_persp(Bp.pf,ps.z,Depth);
		ps.x = Width/2 + (ps.x-Width/2)/dd;
		ps.y = Height/2+ (ps.y-Height/2)/dd;
	}
	const float xx = ps.x-cx, yy = ps.y-cy, zz = ps.z-cz;
	const float d1=c0*c4-c1*c3, d2=c1*c5-c2*c4, d3=c0*c5-c2*c3;

	if(mgl_isnum(zz))	// try to use z-values
	{
		// put inverse matrix here: [x,y,z]=B^(-1)[xx,yy,zz]
		float det = (-c0*c4*c8+c1*c3*c8+c0*c5*c7-c2*c3*c7-c1*c5*c6+c2*c4*c6)/s3;
		p.x = (c2*c4-c1*c5)*zz+(c1*c8-c2*c7)*yy+(c5*c7-c4*c8)*xx;	p.x /= det;
		p.y = (c0*c5-c2*c3)*zz+(c2*c6-c0*c8)*yy+(c3*c8-c5*c6)*xx;	p.y /= det;
		p.z = (c1*c3-c0*c4)*zz+(c0*c7-c1*c6)*yy+(c4*c6-c3*c7)*xx;	p.z /= det;
	}
	else if(fabs(d1) > fabs(d2) && fabs(d1) > fabs(d3))	// x-y plane
	{
		p.z = 0;
		p.x = s3*(c4*xx-c1*yy)/d1;
		p.y = s3*(c0*yy-c3*xx)/d1;
	}
	else if(fabs(d2) > fabs(d3))	// y-z
	{
		p.x = 0;
		p.y = s3*(c5*xx-c2*yy)/d2;
		p.z = s3*(c1*yy-c4*xx)/d2;
	}
	else	// x-z
	{
		p.y = 0;
		p.x = s3*(c5*xx-c2*yy)/d3;
		p.z = s3*(c0*yy-c3*xx)/d3;
	}
	return p;
}
//-----------------------------------------------------------------------------
mglPoint mglCanvas::CalcXYZ(int xs, int ys, bool real) const
{
	if(xs<0 || ys<0 || xs>=Width || ys>=Height)	return mglPoint(NAN,NAN,NAN);
	mglPoint p, ps(xs,Height-ys,NAN);
	float zz = Z[3*(xs+Width*(Height-1-ys))];
	if(zz>-1e20f)	{	ps.z = zz;	real=false;	}
	p = RestorePnt(ps);
	return real ? mglPoint(NAN,NAN,NAN) : mglPoint(Min.x + (Max.x-Min.x)*(p.x+1)/2,
				Min.y + (Max.y-Min.y)*(p.y+1)/2, Min.z + (Max.z-Min.z)*(p.z+1)/2);
}
//-----------------------------------------------------------------------------
void mglCanvas::CalcScr(mglPoint p, int *xs, int *ys) const
{
	mglPoint n;
	ScalePoint(GetB(),p,n);
	if(xs)	*xs=int(p.x);
	if(ys)	*ys=int(p.y);
}
//-----------------------------------------------------------------------------
mglPoint mglCanvas::CalcScr(mglPoint p) const
{	int x,y;	CalcScr(p,&x,&y);	return mglPoint(x,y);	}
//-----------------------------------------------------------------------------
/*void static mgl_prm_swap(mglPrim &s1,mglPrim &s2,mglPrim *buf)
{
	memcpy(buf, &s1, sizeof(mglPrim));
	memcpy(&s1, &s2, sizeof(mglPrim));
	memcpy(&s2, buf, sizeof(mglPrim));
}
void static sort_prm_c(const size_t l0, const size_t r0, mglStack<mglPrim> &s, mglPrim *buf)
{
	if(l0==r0)	return;
	if(l0+1==r0)
	{
		if(s[r0].n1<s[l0].n1)	mgl_prm_swap(s[r0],s[l0],buf);
		return;
	}
	bool del= (buf==0);
	if(del)	buf = (mglPrim*)malloc(sizeof(mglPrim));

	size_t l=l0, r=r0;
	const long v = s[(l+r)/2].n1;

	for(size_t i=l0;i<=r0;i++)	// first collect <0
		if(s[i].n1<v)
		{
			if(i>l)	mgl_prm_swap(s[i],s[l],buf);
			l++;
		}
	r=l;
	for(size_t i=l;i<=r0;i++)	// now collect =0
		if(s[i].n1==v)
		{
			if(i>r)	mgl_prm_swap(s[i],s[r],buf);
			r++;
		}

	if(l>l0+1)	sort_prm_c(l0,l-1,s,buf);
	if(r<r0)	sort_prm_c(r,r0,s,buf);
	if(del)	free(buf);
}*/
//-----------------------------------------------------------------------------
uint32_t mglCanvas::GetColor(const mglPrim &p) const
{
	mglRGBA res, c1,c2,c3,c4;
	c1.c=pnt_col[p.type==1?p.n2:p.n1];
	unsigned r=c1.r[0], g=c1.r[1], b=c1.r[2], a=c1.r[3];
	switch(p.type)
	{
	case 3:
		c2.c=pnt_col[p.n2];	c3.c=pnt_col[p.n3];	c4.c=pnt_col[p.n4];
		res.r[0]=(r+c2.r[0]+c3.r[0]+c4.r[0])/4;
		res.r[1]=(g+c2.r[1]+c3.r[1]+c4.r[1])/4;
		res.r[2]=(b+c2.r[2]+c3.r[2]+c4.r[2])/4;
		res.r[3]=(a+c2.r[3]+c3.r[3]+c4.r[3])/4;	break;
	case 2:
		c2.c=pnt_col[p.n2];	c3.c=pnt_col[p.n3];
		res.r[0]=(r+c2.r[0]+c3.r[0])/3;
		res.r[1]=(g+c2.r[1]+c3.r[1])/3;
		res.r[2]=(b+c2.r[2]+c3.r[2])/3;
		res.r[3]=(a+c2.r[3]+c3.r[3])/3;	break;
	case 6:
		res.r[0]=p.n2&0xff;	res.r[1]=(p.n2/256)&0xff;	res.r[2]=(p.n2/65536)&0xff;	res.r[3]=255;	break;
//		res.c=p.n2;	break;
	default:
		res.c = c1.c;	break;
	}
	// add fog into resulting color
	float zf = FogDist*(p.z/Depth-0.5-FogDz);
	if(zf<0)	// add fog
	{
		unsigned char d = (unsigned char)(255*(1.-exp(5*zf)));
		unsigned char cb[4] = {BDef[0], BDef[1], BDef[2], d};
		if(d<255)	combine(res.r,cb);
	}
	return res.c;
}
//-----------------------------------------------------------------------------
HMGL mgl_qsort_gr=0;
MGL_NO_EXPORT int mgl_type_prior[8]={1,2,4,5, 0,3,0, 7};
int mglBase::PrmCmp(size_t i, size_t j) const
{
	if(i>=Prm.size() || j>=Prm.size())
		return 0;
	const mglPrim &a = Prm[i];
	const mglPrim &b = Prm[j];
	if(a.z!=b.z) 	return int(100*(a.z - b.z));
	int t1 = mgl_type_prior[a.type], t2 = mgl_type_prior[b.type];
	if(t1!=t2)		return t2 - t1;
	if(a.w!=b.w) 	return int(100*(b.w - a.w));
	return a.n3 - b.n3;
}
int MGL_LOCAL_PURE mgl_prm_cmp(const void *i,const void *j)
{
	return mgl_qsort_gr->PrmCmp(*((const size_t *)i), *((const size_t *)j));
}
//-----------------------------------------------------------------------------
void mglCanvas::PreparePrim(int fast)
{
	if(fast!=2)
	{
		mglStartThread(&mglCanvas::pxl_transform,this,Pnt.size());
		if(fast==0)	mglStartThread(&mglCanvas::pxl_setz,this,Prm.size());
		else	mglStartThread(&mglCanvas::pxl_setz_adv,this,Prm.size());
#pragma omp critical
		{
			ClearPrmInd();	mgl_qsort_gr = this;
			size_t n = Prm.size();
			PrmInd = new size_t[n];
			for(size_t i=0;i<n;i++)	PrmInd[i]=i;
			qsort(PrmInd,n,sizeof(size_t),mgl_prm_cmp);
			clr(MGL_FINISHED);
		}
	}
	if(fast>0)
	{
#pragma omp critical
		{	if(pnt_col)	delete []pnt_col;	pnt_col = new uint32_t[Pnt.size()];	}
		mglStartThread(&mglCanvas::pxl_pntcol,this,Pnt.size());
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::Finish()
{
	static mglMatrix bp;
	if(Quality==MGL_DRAW_NONE)	return;
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexPrm);
	pthread_mutex_lock(&mutexPnt);
	pthread_mutex_lock(&mutexClf);
#elif MGL_HAVE_OMP
	omp_set_lock((omp_lock_t*)lockClf);
#endif
	size_t n=Width*Height;
	if(Quality!=MGL_DRAW_DOTS)
	{
		if((Quality&MGL_DRAW_LMEM) || ( Bp!=bp && !(Quality&MGL_DRAW_LMEM) && Prm.size()>0))
			clr(MGL_FINISHED);
		if(!get(MGL_FINISHED))
		{
			if(!(Quality&MGL_DRAW_LMEM) && Prm.size()>0)
			{
				PreparePrim(0);	bp=Bp;	clr(MGL_FINISHED);
				mglStartThread(&mglCanvas::pxl_primdr,this,Prm.size());
			}
			BDef[3] = (Flag&3)!=2 ? 0:255;
			if(Quality&MGL_DRAW_NORM)	mglStartThread(&mglCanvas::pxl_combine,this,n);
			else	mglStartThread(&mglCanvas::pxl_memcpy,this,n);
			BDef[3] = 255;
		}
	}
	else
	{
		mglStartThread(&mglCanvas::pxl_dotsdr,this,Pnt.size());
		mglStartThread(&mglCanvas::pxl_memcpy,this,n);
	}
	int x2 = BBoxX2<0?Width:BBoxX2, y2 = BBoxY2<0?Height:BBoxY2;
	if(BBoxX1>=0 && BBoxX1<x2 && BBoxY1>=0 && BBoxY1<y2)
	{
		unsigned char ff[8]={255,255,255,255, 0,0,0,255}, *g1 = G4+BBoxX1*4-4;
		int ww = 8*Width;
		if(BBoxX1>0)	for(long i=0;i<Height/2-1;i++)
		{	unsigned char *g=g1+ww*i;
			g[0]=ff[0];	g[1]=ff[1];	g[2]=ff[2];	g[3]=ff[3];
			g[4]=ff[4];	g[5]=ff[5];	g[6]=ff[6];	g[7]=ff[7];	}
		g1 = G4+x2*4;
		if(x2<Width)	for(long i=0;i<Height/2-1;i++)
		{	unsigned char *g=g1+ww*i;
			g[0]=ff[0];	g[1]=ff[1];	g[2]=ff[2];	g[3]=ff[3];
			g[4]=ff[4];	g[5]=ff[5];	g[6]=ff[6];	g[7]=ff[7];	}
		g1 = G4+(BBoxY1-1)*4*Width;
		if(BBoxY1>0)	for(long i=0;i<Width/2-1;i++)
		{	unsigned char *g=g1+8*i;
			g[0]=ff[0];	g[1]=ff[1];	g[2]=ff[2];	g[3]=ff[3];
			g[4]=ff[4];	g[5]=ff[5];	g[6]=ff[6];	g[7]=ff[7];	}
		g1 = G4+y2*4*Width;
		if(y2<Height)	for(long i=0;i<Width/2-1;i++)
		{	unsigned char *g=g1+8*i;
			g[0]=ff[0];	g[1]=ff[1];	g[2]=ff[2];	g[3]=ff[3];
			g[4]=ff[4];	g[5]=ff[5];	g[6]=ff[6];	g[7]=ff[7];	}
	}
	mglStartThread(&mglCanvas::pxl_backgr,this,n);
	if(Quality!=MGL_DRAW_DOTS)	set(MGL_FINISHED);

#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexClf);
	pthread_mutex_unlock(&mutexPnt);
	pthread_mutex_unlock(&mutexPrm);
#elif MGL_HAVE_OMP
	omp_unset_lock((omp_lock_t*)lockClf);
#endif
}
//-----------------------------------------------------------------------------
void mglCanvas::ClfZB(bool force)
{
	if(!force && (Quality&MGL_DRAW_LMEM))	return;
	long n=Width*Height;
	memset(C,0,12*n);	memset(OI,0,n*sizeof(int));
#pragma omp parallel for
	for(long i=0;i<3*n;i++)	Z[i] = -1e20f;
	clr(MGL_FINISHED);
}
//-----------------------------------------------------------------------------
void mglCanvas::Clf(mglColor Back)
{
	Fog(0);	PDef = 0xffff;	pPos = 0;
	ClearFrame();
	if((Flag&3)==2)	Back.Set(0,0,0,0);
	if(Back!=NC)	FillBackground(Back);
}
//-----------------------------------------------------------------------------
void mglCanvas::Clf(const char *col)
{
	Fog(0);	PDef = 0xffff;	pPos = 0;
	ClearFrame();
	mglTexture txt(col,0,0);
	FillBackground(txt.col[1]);
}
//-----------------------------------------------------------------------------
void mglCanvas::Rasterize()
{
	Finish();
	memcpy(GB,G4,4*Width*Height);
}
//-----------------------------------------------------------------------------
bool MGL_NO_EXPORT mgl_read_image(unsigned char **g, int &w, int &h, const char *fname);
void mglCanvas::LoadBackground(const char *fname, double alpha)
{
	mgl_read_image(&GB,Width,Height,fname);
	if(alpha<1 && alpha>0)
#pragma omp parallel for
		for(long i=0;i<Width*Height;i++)	GB[4*i+3] = (unsigned char)(GB[4*i+3]*alpha);
}
//-----------------------------------------------------------------------------
void mglCanvas::FillBackground(const mglColor &cc)
{
	BDef[0] = (unsigned char)(255*cc.r);	BDef[1] = (unsigned char)(255*cc.g);
	BDef[2] = (unsigned char)(255*cc.b);	BDef[3] = (unsigned char)(255*cc.a);
#pragma omp parallel for
	for(long i=0;i<Width*Height;i++)
	{	unsigned char *b=GB+4*i;
		b[0]=BDef[0];	b[1]=BDef[1];	b[2]=BDef[2];	b[3]=BDef[3];	}
}
//-----------------------------------------------------------------------------
void mglCanvas::Combine(const mglCanvas *gr)
{
	if(!gr || Width!=gr->Width || Height!=gr->Height)	return;	// wrong sizes
	mglStartThread(&mglCanvas::pxl_other,this,Width*Height,gr);
}
//-----------------------------------------------------------------------------
unsigned char **mglCanvas::GetRGBLines(long &w, long &h, unsigned char *&f, bool alpha)
{
	unsigned char **p;
	Finish();
	long c = alpha?4:3, d = c*Width;
	unsigned char *gg = (alpha?G4:G);
	int x2 = BBoxX2<0?Width:BBoxX2, y2 = BBoxY2<0?Height:BBoxY2;
	if(BBoxX1>=0 && BBoxX1<x2 && BBoxY1>=0 && BBoxY1<y2)
	{
		gg += c*BBoxX1 + d*BBoxY1;
		w = x2-BBoxX1;	h = y2-BBoxY1;
	}
	else	{	w = Width;	h = Height;	}
	p = (unsigned char **)malloc(h * sizeof(unsigned char *));
	for(long j=0;j<h;j++)	p[j] = gg + d*j;
	f = 0;	return p;
}
//-----------------------------------------------------------------------------
