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
void mglCanvas::pxl_combine(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{
		unsigned char *cc = C+12*i, c[4], *b=GB+4*i, *g=G4+4*i;
		c[0]=b[0];	c[1]=b[1];	c[2]=b[2];	c[3]=b[3];
		combine(c,cc+8);	combine(c,cc+4);	combine(c,cc);
		g[0]=c[0];	g[1]=c[1];	g[2]=c[2];	g[3]=c[3];
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_memcpy(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{	unsigned char *g=G4+4*i, *c=C+12*i;
		g[0]=c[0];	g[1]=c[1];	g[2]=c[2];	g[3]=c[3];	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_backgr(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{	unsigned char *b=GB+4*i, c[4]={b[0],b[1],b[2],b[3]}, *g=G+3*i;
		combine(c,G4+4*i);	g[0]=c[0];	g[1]=c[1];	g[2]=c[2];	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_transform(long id, long n, const void *)
{
	const float *b = Bp.b;
	const float dx = -Bp.x*Width/2, dy = -Bp.y*Height/2, dz = Depth/2.;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{
		mglPnt &p=Pnt[i];
		if(p.sub>=0)
		{
			float x = p.xx-Width/2., y = p.yy-Height/2., z = p.zz-Depth/2.;
			p.x = b[0]*x + b[1]*y + b[2]*z + dx;
			p.y = b[3]*x + b[4]*y + b[5]*z + dy;
			p.z = b[6]*x + b[7]*y + b[8]*z + dz;
			float d = get_persp(Bp.pf,p.z,Depth);
			p.x = Width/2. + d*p.x;	p.y = Height/2. + d*p.y;
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_setz_adv(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{
		mglPrim &q=Prm[i];	q.z = Pnt[q.n1].z;
		if(q.type==3)	q.z = (q.z + Pnt[q.n2].z + Pnt[q.n3].z + Pnt[q.n4].z)/4;
		else if(q.type==2)	q.z = (q.z + Pnt[q.n2].z + Pnt[q.n3].z)/3;
		else if(q.type==1)	q.z = (q.z + Pnt[q.n2].z)/2;
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_pntcol(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{	mglRGBA c;	col2int(Pnt[i],c.r,HighId-1);	pnt_col[i]=c.c;	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_setz(long id, long n, const void *)
{
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{	mglPrim &q=Prm[i];	q.z = Pnt[q.n1].z;	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_primdr(long id, long , const void *)
{
#define Q	4	// should be >= sqrt(2*num_thr) ???
	const int nx=Q,ny=Q;	// TODO find dependence on Q for 1, 2, 4, 8 threads. Try to select optimal
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<nx*ny;i+=mglNumThr)
	{
		mglDrawReg d;	d.set(this,nx,ny,i);
		if(Quality&MGL_DRAW_NORM)	for(size_t k=0;k<Prm.size();k++)
		{
			if(Stop)	break;
			const mglPrim &p=GetPrm(k);	d.copy(p);
			long n1=p.n1, n2=p.n2, n3=p.n3, n4=p.n4;
			switch(p.type)
			{
				case 3:	quad_draw(Pnt[n1],Pnt[n2],Pnt[n3],Pnt[n4],&d);	break;
				case 1:	line_draw(Pnt[n1],Pnt[n2],&d);	break;
				case 4:	glyph_draw(p,&d);	break;
				case 0:	mark_draw(Pnt[n1],n4,p.s,&d);	break;
				case 2:	trig_draw(Pnt[n1],Pnt[n2],Pnt[n3],true,&d);	break;
			}
		}
		else if(Quality&MGL_DRAW_FAST)	for(size_t k=0;k<Prm.size();k++)
		{
			if(Stop)	break;
			const mglPrim &p=GetPrm(k);	d.copy(p);
			long n1=p.n1, n2=p.n2, n3=p.n3, n4=p.n4;
			switch(p.type)
			{
				case 3:	trig_draw(Pnt[n1],Pnt[n2],Pnt[n4],true,&d);
						trig_draw(Pnt[n1],Pnt[n3],Pnt[n4],true,&d);	break;
				case 1:	line_draw(Pnt[n1],Pnt[n2],&d);	break;
				case 4:	glyph_draw(p,&d);	break;
				case 0:	mark_draw(Pnt[n1],n4,p.s,&d);	break;
				case 2:	trig_draw(Pnt[n1],Pnt[n2],Pnt[n3],true,&d);	break;
			}
		}
		else	for(size_t k=0;k<Prm.size();k++)
		{
			if(Stop)	break;
			const mglPrim &p=GetPrm(k);	d.copy(p);
			long n1=p.n1, n2=p.n2, n3=p.n3, n4=p.n4;
			switch(p.type)
			{
				case 3:	fast_draw(Pnt[n1],Pnt[n4],&d);	fast_draw(Pnt[n2],Pnt[n3],&d);	break;
				case 1:	fast_draw(Pnt[n1],Pnt[n2],&d);	break;
				case 4:	glyph_draw(p,&d);	break;
				case 0:	mark_draw(Pnt[n1],n4,p.s,&d);	break;
				case 2:	fast_draw(Pnt[n1],Pnt[n2],&d);	fast_draw(Pnt[n1],Pnt[n3],&d);
				fast_draw(Pnt[n2],Pnt[n3],&d);	break;
			}
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_dotsdr(long id, long n, const void *)
{
	const float *b = Bp.b;
	const float dx = -Bp.x*Width/2, dy = -Bp.y*Height/2, dz = Depth/2.;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i=id;i<n;i+=mglNumThr)
	{
		unsigned char r[4]={0,0,0,255};
		const mglPnt &p=Pnt[i];
		if(p.sub<0)	continue;
		float x = p.xx-Width/2., y = p.yy-Height/2., z = p.zz-Depth/2.,xx,yy,zz;
		xx = b[0]*x + b[1]*y + b[2]*z + dx;
		yy = b[3]*x + b[4]*y + b[5]*z + dy;
		zz = b[6]*x + b[7]*y + b[8]*z + dz;
		float d = get_persp(Bp.pf,zz,Depth);
		xx = Width/2. + d*xx;	yy = Height/2. + d*yy;

		r[0] = (unsigned char)(255*p.r);
		r[1] = (unsigned char)(255*p.g);
		r[2] = (unsigned char)(255*p.b);
		long i0=long(xx)+Width*(Height-1-long(yy));
		unsigned char *c = C+12*i0;
		if(i0>=0 && i0<Width*Height && zz>Z[3*i0])
		{	Z[3*i0]=z;	c[0]=r[0];	c[1]=r[1];	c[2]=r[2];	c[3]=r[3];	OI[i0]=-1;	}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pxl_other(long id, long n, const void *p)
{
	const mglCanvas *gr = (const mglCanvas *)p;
	if(Quality&MGL_DRAW_NORM)
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long k=id;k<n;k+=mglNumThr)
		{
			long i = k%Width, j = Height-1-(k/Width);
			pnt_plot(i,j,gr->Z[3*k+2],gr->C+12*k+8,gr->OI[k]);
			pnt_plot(i,j,gr->Z[3*k+1],gr->C+12*k+4,gr->OI[k]);
			pnt_plot(i,j,gr->Z[3*k],gr->C+12*k,gr->OI[k]);
		}
	else
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
		for(long k=id;k<n;k+=mglNumThr)
		{
			long i = k%Width, j = Height-1-(k/Width);
			pnt_plot(i,j,gr->Z[3*k],gr->C+12*k,gr->OI[k]);
		}
}
//-----------------------------------------------------------------------------
void mglCanvas::pnt_plot(long x,long y,mreal z,const unsigned char ci[4], int obj_id)
{
	if(ci[3])
	{
		long i0=x+Width*(Height-1-y);
		unsigned char *cc = C+12*i0, c[4];
		c[0]=ci[0];	c[1]=ci[1];	c[2]=ci[2];	c[3]=ci[3];
		float *zz = Z+3*i0, zf = FogDist*(z/Depth-0.5-FogDz);
		// try to remove double transparency near vertexes
		if(fabs(z-zz[0])<1 && OI[i0]==obj_id && abs(cc[0]-ci[0])+abs(cc[1]-ci[1])+abs(cc[2]-ci[2])<5)
		{
			if(cc[3]<ci[3])
			{	cc[0]=c[0];	cc[1]=c[1];	cc[2]=c[2];	cc[3]=c[3];	}
			return;
		}
		if(zf<0)	// add fog
		{
			int d = int(255.f-255.f*exp(5.f*zf));
			unsigned char cb[4] = {BDef[0], BDef[1], BDef[2], (unsigned char)d};
			if(d==255)	return;
			combine(c,cb);
		}
		if(Quality&MGL_DRAW_NORM)
		{
			if(z>=zz[1])	// shift point on slice down and paste new point
			{
				zz[2] = zz[1];	combine(cc+8,cc+4);
				if(z>=zz[0])
				{	zz[1] = zz[0];	zz[0] = z;	OI[i0]=obj_id;
					cc[4]=cc[0];	cc[0]=c[0];	cc[5]=cc[1];	cc[1]=c[1];
					cc[6]=cc[2];	cc[2]=c[2];	cc[7]=cc[3];	cc[3]=c[3];	}
				else
				{	zz[1] = z;	cc[4]=c[0];	cc[5]=c[1];	cc[6]=c[2];	cc[7]=c[3];	}
			}
			else
			{
				if(z>=zz[2])	// shift point on slice down and paste new point
				{	zz[2] = z;	combine(cc+8,c);	}
				else		// point below the background
				{	combine(c,cc+8);	cc[8]=c[0];	cc[9]=c[1];	cc[10]=c[2];cc[11]=c[3];	}
			}
		}
		if(Quality&MGL_DRAW_FAST)
		{
			if(z>=zz[0])	// point upper the background
			{	zz[0]=z;	combine(cc,c);	OI[i0]=obj_id;	}
			else
			{	combine(c,cc);	cc[6]=cc[2];	cc[2]=c[2];	cc[7]=cc[3];	cc[3]=c[3];	}
		}
		else
		{
			if(z>=zz[0])	// point upper the background
			{	zz[0]=z;	cc[0]=c[0];	cc[1]=c[1];	cc[2]=c[2];	cc[3]=c[3];	OI[i0]=obj_id;	}
		}
	}
}
//-----------------------------------------------------------------------------
inline float mexp(float x)	//	exp(-x) ~ 1/(1+x+x^2/2+x^3/4+x^5/40)
{	return 1.f/(1.f+x*(1.f+x*x/2.f));	}
//{	return 1/(1+x*(1+x/2*(1+x/2*(1+x*x/10))));	}
//{	return exp(-x);	}
void mglCanvas::col2int(const mglPnt &p,unsigned char *r, int obj_id) const
{
//	if(!r)	return r;	// NOTE r must be provided!
	if(p.a<=0)	{	r[0]=r[1]=r[2]=r[3]=0;	return;	}
	float b0=0,b1=0,b2=0, ar,ag,ab,dif;
	const size_t nl = p.sub>=0?p.sub:-1-p.sub;
	const bool glob = !get(MGL_LOCAL_LIGHT);
	ar = ag = ab = glob?AmbBr:Sub[nl].AmbBr;
	dif = glob?DifBr:Sub[nl].DifBr;
	const mglLight *gll = glob?light:Sub[nl].light;

	if(mgl_isnum(p.u+p.v+p.w))
	{
		for(long i=0;i<10;i++)
		{
			const mglLight &ll=gll[i];
			if(!ll.n)	continue;
			if(mgl_isnan(ll.q.x))		// source at infinity
			{
				const mglPoint &lp = ll.p;
				float nn = 2*(p.u*lp.x+p.v*lp.y+p.w*lp.z) / (p.u*p.u+p.v*p.v+p.w*p.w+1e-6f);
				float d0 = lp.x - p.u*nn;
				float d1 = lp.y - p.v*nn;
				float d2 = lp.z - p.w*nn;
				nn = 1 + d2/sqrt(d0*d0+d1*d1+d2*d2+1e-6f);

//				nn = exp(-ll.a*nn)*ll.b*2;
				nn = mexp(ll.a*nn)*ll.b*2.f;
				const mglColor &lc = ll.c;
				b0 += nn*lc.r;
				b1 += nn*lc.g;
				b2 += nn*lc.b;
			}
			else		// diffuse and specular light
			{
				const mglPoint &lp=ll.p, &lq=ll.q;
				float d0 = lq.x-p.x;	// direction to light source
				float d1 = lq.y-p.y;
				float d2 = lq.z-p.z;
				float nn = 1+(d0*lp.x+d1*lp.y+d2*lp.z)/sqrt(d0*d0+d1*d1+d2*d2+1e-6f);
//				float bb = exp(-3*ll.a*nn);	nn = bb*dif*2;
				float bb = mexp(3*ll.a*nn);	nn = bb*dif*2;
				const mglColor &lc = ll.c;
				ar += nn*lc.r;
				ag += nn*lc.g;
				ab += nn*lc.b;

				nn = 2*(p.u*d0+p.v*d1+p.w*d2) / (p.u*p.u+p.v*p.v+p.w*p.w+1e-6f);
				d0 -= p.u*nn;	d1 -= p.v*nn;	d2 -= p.w*nn;
				nn = 1 + d2/sqrt(d0*d0+d1*d1+d2*d2+1e-6f);

//				nn = exp(-ll.a*nn)*bb*ll.b*2;
				nn = mexp(ll.a*nn)*bb*ll.b*2.f;
				b0 += nn*lc.r;
				b1 += nn*lc.g;
				b2 += nn*lc.b;
			}
		}
		b0 += (ar>1 ? 1:ar)*p.r;	// diffuse light
		b1 += (ag>1 ? 1:ag)*p.g;
		b2 += (ab>1 ? 1:ab)*p.b;
		b0 = b0<1 ? b0 : 1;			// normalize components
		b1 = b1<1 ? b1 : 1;
		b2 = b2<1 ? b2 : 1;
	}
	else
	{	b0=p.r;	b1=p.g;	b2=p.b;	}
	// try to highlight faces
	if(obj_id==HighId)	{	b0*=0.7;	b1*=0.7;	b2*=0.7;	}
	r[0] = (unsigned char)(255*b0);
	r[1] = (unsigned char)(255*b1);
	r[2] = (unsigned char)(255*b2);
//	r[3] = get(MGL_ENABLE_ALPHA) ? (unsigned char)(255*p.a) : 255;
	r[3] = (unsigned char)((Quality&MGL_DRAW_NORM)?255*p.a:255);
//	return r;
}
//-----------------------------------------------------------------------------
/// color mixing: color c1 is under color c2 !!!
void mglCanvas::combine(unsigned char *c1, const unsigned char *c2) const
{
	if(c2[3])
	{
		const unsigned a1=c1[3], a2=c2[3];
		if((Flag&3)==0)
		{
			unsigned b1=255-a2;
			c1[0] = (c1[0]*b1 + c2[0]*a2)/256;
			c1[1] = (c1[1]*b1 + c2[1]*a2)/256;
			c1[2] = (c1[2]*b1 + c2[2]*a2)/256;
			c1[3] = (unsigned char)(a2+a1*b1/255);
		}
		else if((Flag&3)==1)
		{
			c1[0] = (unsigned char)((255-a1*(255-c1[0])/256)*(255-a2*(255-c2[0])/256)/256);
			c1[1] = (unsigned char)((255-a1*(255-c1[1])/256)*(255-a2*(255-c2[1])/256)/256);
			c1[2] = (unsigned char)((255-a1*(255-c1[2])/256)*(255-a2*(255-c2[2])/256)/256);
			c1[3] = 255;
		}
		else if((Flag&3)==2)
		{
			unsigned b1,b2,b3;
			b1 = (c1[0]*a1 + c2[0]*a2)/255;	c1[0] = b1<255 ? b1 : 255;
			b2 = (c1[1]*a1 + c2[1]*a2)/255;	c1[1] = b2<255 ? b2 : 255;
			b3 = (c1[2]*a1 + c2[2]*a2)/255;	c1[2] = b3<255 ? b3 : 255;
			c1[3] = 255;
		}
	}
}
//-----------------------------------------------------------------------------
// it looks as MSV=4 is optimal for speed-vs-quality
#define MSV 4
int inline visible(long i, long j, const unsigned char m[8], mreal pw, int a)	// Check if pixel visible
{
	float c = mgl_cos[(a+360)%360], s = mgl_cos[(a+450)%360];
	int ii,jj,ss=0;
#if MSV==4
	ii = int(0.5+((i-0.33)*c+(j-0.33)*s)/pw)&7;	jj = int(0.5+((j-0.33)*c-(i-0.33)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i-0.33)*c+(j+0.33)*s)/pw)&7;	jj = int(0.5+((j+0.33)*c-(i-0.33)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i+0.33)*c+(j-0.33)*s)/pw)&7;	jj = int(0.5+((j-0.33)*c-(i+0.33)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i+0.33)*c+(j+0.33)*s)/pw)&7;	jj = int(0.5+((j+0.33)*c-(i+0.33)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
#elif MSV==9
	ii = int(0.5+((i-0.25)*c+(j-0.25)*s)/pw)&7;	jj = int(0.5+((j-0.25)*c-(i-0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i-0.25)*c+j*s)/pw)&7;	jj = int(0.5+(j*c-(i-0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i-0.25)*c+(j+0.25)*s)/pw)&7;	jj = int(0.5+((j+0.25)*c-(i-0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;

	ii = int(0.5+(i*c+(j-0.25)*s)/pw)&7;	jj = int(0.5+((j-0.25)*c-i*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+(i*c+j*s)/pw)&7;	jj = int(0.5+(j*c-i*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+(i*c+(j+0.25)*s)/pw)&7;	jj = int(0.5+((j+0.25)*c-i*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;

	ii = int(0.5+((i+0.25)*c+(j-0.25)*s)/pw)&7;	jj = int(0.5+((j-0.25)*c-(i+0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i+0.25)*c+j*s)/pw)&7;	jj = int(0.5+(j*c-(i+0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
	ii = int(0.5+((i+0.25)*c+(j+0.25)*s)/pw)&7;	jj = int(0.5+((j+0.25)*c-(i+0.25)*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
#elif MSV==1
	ii = int(0.5+(i*c+j*s)/pw)&7;	jj = int(0.5+(j*c-i*s)/pw)&7;
	if(m[jj] & (1L<<ii))	ss++;
#endif
	return ss;
}
//-----------------------------------------------------------------------------
/* Bilinear interpolation r(u,v) = r0 + (r1-r0)*u + (r2-r0)*v + (r3+r0-r1-r2)*u*v
	is used (where r is one of {x,y,z,R,G,B,A}. Variables u,v are determined
	for each point (x,y) and selected one pair which 0<u<1 and 0<v<1.*/
void mglCanvas::quad_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *d)
{
	if(Quality&MGL_DRAW_LMEM)
	{
		if(!(Quality&3))
		{	fast_draw(p1,p4,d);	fast_draw(p2,p3,d);	return;	}
		if(!(Quality&MGL_DRAW_NORM))
		{	trig_draw(p1,p2,p4,true,d);	trig_draw(p1,p3,p4,true,d);	return;	}
	}
	unsigned char r[4];
	long y1,x1,y2,x2;
	mglPnt d1(p2-p1), d2(p3-p1), d3(p4+p1-p2-p3);

	if(d1.x==0 && d1.y==0)	{	trig_draw(p1,p3,p4,true,d);	return;	}
	if(d2.x==0 && d2.y==0)	{	trig_draw(p1,p2,p4,true,d);	return;	}

	x1 = long(mgl_min(mgl_min(p1.x,p2.x), mgl_min(p3.x,p4.x)));	// bounding box
	y1 = long(mgl_min(mgl_min(p1.y,p2.y), mgl_min(p3.y,p4.y)));
	x2 = long(mgl_max(mgl_max(p1.x,p2.x), mgl_max(p3.x,p4.x)));
	y2 = long(mgl_max(mgl_max(p1.y,p2.y), mgl_max(p3.y,p4.y)));
	x1=mgl_imax(x1,d->x1);	x2=mgl_imin(x2,d->x2);
	y1=mgl_imax(y1,d->y1);	y2=mgl_imin(y2,d->y2);
	if(x1>x2 || y1>y2)	return;

	const float dd = d1.x*d2.y-d1.y*d2.x;
	const float dsx =-4*(d2.y*d3.x - d2.x*d3.y)*d1.y;
	const float dsy = 4*(d2.y*d3.x - d2.x*d3.y)*d1.x;

	mglPoint n1(mglPoint(p2.x-p1.x,p2.y-p1.y,p2.z-p1.z)^mglPoint(p3.x-p1.x,p3.y-p1.y,p3.z-p1.z));
	mglPoint n2(mglPoint(p2.x-p4.x,p2.y-p4.y,p2.z-p4.z)^mglPoint(p3.x-p4.x,p3.y-p4.y,p3.z-p4.z));
	mglPoint nr((n1.x+n2.x)*0.5,(n1.y+n2.y)*0.5,(n1.z+n2.z)*0.5);

	const float x0 = p1.x, y0 = p1.y;
	const int oi = d->ObjId, ang=d->angle;
	const mreal pw = d->PenWidth;
	const uint64_t pd = d->PDef;
	
	mglPnt tmp(p1+d1+d2+d3), pp(p1);
	if(mgl_isnan(tmp.u) && mgl_isnum(tmp.v))
	{	pp.u = nr.x;	pp.v = nr.y;	pp.w = nr.z;
		d1.u=d1.v=d1.w=d2.u=d2.v=d2.w=d3.u=d3.v=d3.w=0;	}
	
	for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
	{
		int ms = (pd==MGL_SOLID_MASK) ? 4 : visible(i,j,d->m, pw,ang);
		if(ms!=0)
		{
			float xx = (i-x0), yy = (j-y0), s;
			s = dsx*xx + dsy*yy + (dd+d3.y*xx-d3.x*yy)*(dd+d3.y*xx-d3.x*yy);
			if(s>=0)
			{
				s = sqrt(s);
				float qu = d3.x*yy - d3.y*xx + dd + s;
				float qv = d3.y*xx - d3.x*yy + dd + s;
				float u = 2.f*(d2.y*xx - d2.x*yy)/qu;
				float v = 2.f*(d1.x*yy - d1.y*xx)/qv;
				if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	// first root bad
				{
					qu = d3.x*yy - d3.y*xx + dd - s;
					qv = d3.y*xx - d3.x*yy + dd - s;
//					u = v = -1.f;
					u = 2.f*(d2.y*xx - d2.x*yy)/qu;	v = 2.f*(d1.x*yy - d1.y*xx)/qv;
					if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	continue;	// second root bad
				}
				mglPnt p(pp+d1*u+d2*v+d3*(u*v));	col2int(p,r,oi);
				r[3] = ms*r[3]/MSV;
				if(r[3])	pnt_plot(i,j,p.z,r,oi);
			}
		}
	}
}
//-----------------------------------------------------------------------------
/* Linear interpolation r(u,v) = r0 + (r1-r0)*u + (r2-r0)*v is used, where r is
	one of {x,y,z,R,G,B,A}. Variables u,v are determined for each point (x,y).
	Point plotted is u>0 and v>0 and u+v<1.*/
void mglCanvas::trig_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool anorm, const mglDrawReg *d)
{
	if(!(Quality&3) && anorm)
	{	fast_draw(p1,p2,d);	fast_draw(p1,p3,d);	fast_draw(p2,p3,d);	return;	}
	unsigned char r[4];
	long y1,x1,y2,x2;
	mglPnt d1(p2-p1), d2(p3-p1);

	const float tmp = d2.x*d1.y - d1.x*d2.y;
	if(fabs(tmp)<1e-5)	return;		// points lies on the same line
	const float dyv =-d1.x/tmp,	dxv = d1.y/tmp;
	const float dyu = d2.x/tmp,	dxu =-d2.y/tmp;

	x1 = long(mgl_min(p1.x<p2.x?p1.x:p2.x, p3.x));	// bounding box
	y1 = long(mgl_min(p1.y<p2.y?p1.y:p2.y, p3.y));
	x2 = long(mgl_max(p1.x>p2.x?p1.x:p2.x, p3.x));
	y2 = long(mgl_max(p1.y>p2.y?p1.y:p2.y, p3.y));
	x1=x1>d->x1?x1:d->x1;	x2=x2<d->x2?x2:d->x2;
	y1=y1>d->y1?y1:d->y1;	y2=y2<d->y2?y2:d->y2;
	if(x1>x2 || y1>y2)	return;
	// default normale
	const mglPoint nr(mglPoint(p2.x-p1.x,p2.y-p1.y,p2.z-p1.z)^mglPoint(p3.x-p1.x,p3.y-p1.y,p3.z-p1.z));
	const float x0 = p1.x, y0 = p1.y;
	// provide additional height to be well visible on the surfaces
	const float dz = anorm? 0 : (Width>2 ? 1 : 1e-5*Width);
	const int oi = d->ObjId, ang=d->angle;
	const mreal pw = d->PenWidth;
	const uint64_t pd = d->PDef;

	mglPnt tp(p1+d1+d2), pp(p1);
	if(mgl_isnan(tp.u) && mgl_isnum(tp.v))
	{	pp.u = nr.x;	pp.v = nr.y;	pp.w = nr.z;
		d1.u=d1.v=d1.w=d2.u=d2.v=d2.w=0;	}

	if(Quality&MGL_DRAW_NORM)	for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
	{
		int ms = (pd==MGL_SOLID_MASK) ? 4 : visible(i,j,d->m, pw,ang);
		if(ms!=0)
		{
			float xx = (i-x0), yy = (j-y0);
			float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;
			if(u<0 || v<0 || u+v>1)	continue;
			mglPnt p(pp+d1*u+d2*v);	col2int(p,r,oi);
			r[3] = ms*r[3]/MSV;
			if(r[3])	pnt_plot(i,j,p.z+dz,r,oi);
		}
	}
	else
	{
		col2int(p1,r,oi);
		float zz = p1.z+dz, dz1=d1.z, dz2=d2.z;
		unsigned char ra = r[3];
		if(ra)	for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
		{
			int ms = (pd==MGL_SOLID_MASK) ? 4 : visible(i,j,d->m, pw,ang);
			if(ms!=0)
			{
				float xx = (i-x0), yy = (j-y0);
				float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;
				if(u<0 || v<0 || u+v>1)	continue;
				r[3] = ms*ra/MSV;
				pnt_plot(i,j,zz+dz1*u+dz2*v,r,oi);
			}
		}
	}
}
//-----------------------------------------------------------------------------
inline unsigned char mgl_sline(unsigned char c,float x)
{	x*=x/2;	return (unsigned char)(c/(1+x+x*x/5));	}
void mglCanvas::line_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *dr)
{
	if((Quality&3)==MGL_DRAW_WIRE)	{	fast_draw(p1,p2,dr);	return;	}	// previously was <2. This may slightly slow down for Quality=1
	unsigned char r[4];
	long y1,x1,y2,x2;

	const float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
	const int oi = dr->ObjId;
	const float pw=dr->PenWidth*(oi==HighId?2:1), dpw=pen_delta*(oi==HighId?2:3);
	const mglPnt d(p2-p1);
	bool hor = fabs(d.x)>fabs(d.y);

	x1 = long(p1.x<p2.x?p1.x:p2.x);	y1 = long(p1.y<p2.y?p1.y:p2.y);	// bounding box
	x2 = long(p1.x>p2.x?p1.x:p2.x);	y2 = long(p1.y>p2.y?p1.y:p2.y);
	x1 -= pw+10/dpw;	x2 += pw+10/dpw;
	y1 -= pw+10/dpw;	y2 += pw+10/dpw;
	x1=x1>dr->x1?x1:dr->x1;	x2=x2<dr->x2?x2:dr->x2;
	y1=y1>dr->y1?y1:dr->y1;	y2=y2<dr->y2?y2:dr->y2;
	const float dd = hypot(d.x, d.y);
	if(x1>x2 || y1>y2 || dd<1e-5)	return;

	const float dxv = d.y/dd, dyv =-d.x/dd;
	const float dxu = d.x/dd, dyu = d.y/dd;

	const uint64_t pd = dr->PDef;
	const mreal pp = dr->pPos, V = (pw-1)*(pw-1)/4, S = (1-pw)/2;
	if(hor)	for(long i=x1;i<=x2;i++)
	{
		y1 = int(p1.y+d.y*(i-p1.x)/d.x - pw - 10/dpw);
		y2 = int(p1.y+d.y*(i-p1.x)/d.x + pw + 10/dpw);
		y1=y1>dr->y1?y1:dr->y1;	y2=y2<dr->y2?y2:dr->y2;
		for(long j=y1;j<=y2;j++)
		{
			float xx = (i-p1.x), yy = (j-p1.y);
			float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;	v = v*v;
			if(u<0)			{	v += 16*u*u;	u=0;	}
			else if(u>dd)	{	v += 16*(u-dd)*(u-dd);	u=dd;	}
			if( pd & ((uint64_t)1<<(long(pp+u/pw)%16)) )
			{
				mglPnt p(p1+d*(u/dd));	col2int(p,r,oi);
				r[3] = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				if(r[3])	pnt_plot(i,j,p.z+dz,r,oi);
			}
		}
	}
	else	for(long j=y1;j<=y2;j++)
	{
		x1 = int(p1.x+d.x*(j-p1.y)/d.y - pw - 10/dpw);
		x2 = int(p1.x+d.x*(j-p1.y)/d.y + pw + 10/dpw);
		x1=x1>dr->x1?x1:dr->x1;	x2=x2<dr->x2?x2:dr->x2;
		for(long i=x1;i<=x2;i++)
		{
			float xx = (i-p1.x), yy = (j-p1.y);
			float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;	v = v*v;
			if(u<0)			{	v += 16*u*u;	u=0;	}
			else if(u>dd)	{	v += 16*(u-dd)*(u-dd);	u=dd;	}
			if( pd & ((uint64_t)1<<(long(pp+u/pw)%16)) )
			{
				mglPnt p(p1+d*(u/dd));	col2int(p,r,oi);
				r[3] = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				if(r[3])	pnt_plot(i,j,p.z+dz,r,oi);
			}
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pnt_fast(long x,long y,mreal z,const unsigned char ci[4], int obj_id)
{
	long i0=x+Width*(Height-1-y);
	unsigned char *cc = C+12*i0;
	if(ci[3]!=0 && z>Z[3*i0])	// point upper the background
	{	Z[3*i0]=z;		OI[i0]=obj_id;
		cc[0]=ci[0];	cc[1]=ci[1];	cc[2]=ci[2];	cc[3]=ci[3];	}
}
//-----------------------------------------------------------------------------
void mglCanvas::fast_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *dr)
{
	if(p1.x==p2.x && p1.y==p2.y) return;
	const mglPnt d(p2-p1);
	const int oi = dr->ObjId;
	unsigned char r[4];	col2int(p1,r,oi);
	long y1,x1,y2,x2;

	const bool hor = fabs(d.x)>fabs(d.y);

	x1 = long(p1.x<p2.x?p1.x:p2.x);	y1 = long(p1.y<p2.y?p1.y:p2.y);	// bounding box
	x2 = long(p1.x>p2.x?p1.x:p2.x);	y2 = long(p1.y>p2.y?p1.y:p2.y);
	x1=x1>dr->x1?x1:dr->x1;	x2=x2<dr->x2?x2:dr->x2;
	y1=y1>dr->y1?y1:dr->y1;	y2=y2<dr->y2?y2:dr->y2;
	if(x1>x2 || y1>y2)	return;
	const float dz = Width>2 ? 1 : 1e-5*Width;	// provide additional height to be well visible on the surfaces

	if(hor)	for(long i=x1;i<=x2;i++)
	{
		long c = long(p1.y+d.y*(i-p1.x)/d.x);
		if(c>=y1 && c<=y2)
			pnt_fast(i, c, p1.z+d.z*(i-p1.x)/d.x+dz, r,oi);
	}
	else	for(long i=y1;i<=y2;i++)
	{
		long c = long(p1.x+d.x*(i-p1.y)/d.y);
		if(c>=x1 && c<=x2)
			pnt_fast(c, i, p1.z+d.z*(i-p1.y)/d.y+dz, r,oi);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::pnt_draw(const mglPnt &p, const mglDrawReg *dr)
{
//	if(k<0 || !dr)	return;
	const int oi = dr->ObjId;
	const float pw=(oi==HighId?6:3)*dr->PenWidth,dpw=(oi==HighId?2:3)*pen_delta;
	unsigned char cs[4], cc;
	col2int(p,cs,oi);	cc = cs[3];
	if(cc==0)	return;
	const long s = long(pw+10/dpw+fabs(pw));
	const long i1=mgl_max(-s,dr->x1-p.x),i2=mgl_min(s,dr->x2-p.x);
	const long j1=mgl_max(-s,dr->y1-p.y),j2=mgl_min(s,dr->y2-p.y);
	const mreal V = (pw-1)*(pw-1)/4, S = (1-pw)/2;
	if(!(Quality&3))	for(long j=j1;j<=j2;j++)	for(long i=i1;i<=i2;i++)	// fast draw
	{
		float v = i*i+j*j;
		if(v>1+V)	continue;
		if(cs[3])	pnt_plot(p.x+i,p.y+j,p.z,cs,oi);
	}
	else	for(long j=j1;j<=j2;j++)	for(long i=i1;i<=i2;i++)
	{
		float v = i*i+j*j;
		cs[3] = v<V ? cc : mgl_sline(cc,dpw*(sqrt(v)+S));
		if(cs[3])	pnt_plot(p.x+i,p.y+j,p.z,cs,oi);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::mark_draw(const mglPnt &q, char type, mreal size, mglDrawReg *dr)
{
	const int oi = dr->ObjId;
	unsigned char cs[4];	col2int(q,cs,oi);
	const unsigned char ca = cs[3];// = size>0 ? 255 : 255*q.t;
	const mreal ss=(strchr("xsSoO",type)?1:1.1)*fabs(size), dpw=(oi==HighId?2:3)*pen_delta;
	mreal PW=1;

	if(type=='.' || ss==0)
	{
		PW = 3*(ss?ss:sqrt(font_factor/400));
		if(oi==HighId)	PW *= 2;
		const mreal pw = PW;
		mreal dd = pw+10/dpw;
		long x1 = long(q.x-dd), y1 = long(q.y-dd);	// bounding box
		long x2 = long(q.x+dd), y2 = long(q.y+dd);
		x1=x1>dr->x1?x1:dr->x1;	x2=x2<dr->x2?x2:dr->x2;
		y1=y1>dr->y1?y1:dr->y1;	y2=y2<dr->y2?y2:dr->y2;
		if(x1>x2 || y1>y2)	return;
		const float V=(pw-1)*(pw-1)/4,S=(1-pw)/2;

		for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
		{
			float dx=i-q.x, dy=j-q.y, v=dx*dx+dy*dy;
			int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
			cs[3] = ca*sum/255;
			if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
		}
	}
	else
	{
		dr->PDef = MGL_SOLID_MASK;	dr->angle = 0;
		PW = dr->PenWidth*sqrt(fabs(50*size));
		if(PW<1)	PW=1;
		if(oi==HighId)	PW *= 2;
		const mreal pw = PW;

		mreal dd = ss+pw+10/dpw;
		long x1 = long(q.x-dd), y1 = long(q.y-dd);	// bounding box
		long x2 = long(q.x+dd), y2 = long(q.y+dd);
		x1=x1>dr->x1?x1:dr->x1;	x2=x2<dr->x2?x2:dr->x2;
		y1=y1>dr->y1?y1:dr->y1;	y2=y2<dr->y2?y2:dr->y2;
		if(x1>x2 || y1>y2)	return;
		const float V=(pw-1)*(pw-1)/4,S=(1-pw)/2;

		switch(type)
		{
		case 'P':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = (dx-ss)*(dx-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dy)-ss;	v = (dx+ss)*(dx+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dy)-ss;	v = dx*dx+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy-ss)*(dy-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy+ss)*(dy+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = dy*dy+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case '+':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = dx*dx+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = dy*dy+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'X':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = (dx-ss)*(dx-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dy)-ss;	v = (dx+ss)*(dx+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy-ss)*(dy-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy+ss)*(dy+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));

				u = fabs(dx+dy)-2*ss;	v = dx-dy;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx-dy)-2*ss;	v = dx+dy;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));

				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'x':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dx+dy)-2*ss;	v = dx-dy;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx-dy)-2*ss;	v = dx+dy;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'S':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u = fabs(dy)-ss;	if(u<0)	u=0;
				v = fabs(dx)-ss;	if(v<0)	v=0;	v = u*u+v*v;
				int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 's':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = (dx-ss)*(dx-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dy)-ss;	v = (dx+ss)*(dx+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy-ss)*(dy-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx)-ss;	v = (dy+ss)*(dy+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'D':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u = fabs(dx-dy)-ss;	if(u<0)	u=0;
				v = fabs(dx+dy)-ss;	if(v<0)	v=0;	v = u*u+v*v;
				int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'd':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dx+dy)-ss;	v = (dx-dy-ss)*(dx-dy-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx+dy)-ss;	v = (dx-dy+ss)*(dx-dy+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx-dy)-ss;	v = (dx+dy-ss)*(dx+dy-ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(dx-dy)-ss;	v = (dx+dy+ss)*(dx+dy+ss)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'Y':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy+ss/2)-ss/2;	v = dx*dx+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.87*dx+0.5*dy-ss/2)-ss/2;	v = (0.5*dx-0.87*dy)*(0.5*dx-0.87*dy)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(-0.87*dx+0.5*dy-ss/2)-ss/2;	v = (0.5*dx+0.87*dy)*(0.5*dx+0.87*dy)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case '*':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = dx*dx+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.87*dx+0.5*dy)-ss;	v = (0.5*dx-0.87*dy)*(0.5*dx-0.87*dy)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(-0.87*dx+0.5*dy)-ss;	v = (0.5*dx+0.87*dy)*(0.5*dx+0.87*dy)+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'T':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u=dy/1.5+ss/3;	v=(dx+ss-u)/2;
				if(u>0 && v>0 && u+v<ss)	cs[3]=ca;
				else
				{
					int sum=0;
					u = fabs(dx)-ss;	v = dy+ss/2;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dx+0.83*dy)-0.9*ss;	v = 0.83*dx-0.55*dy+0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dx-0.83*dy)-0.9*ss;	v = 0.83*dx+0.55*dy-0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				}
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case '^':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dx)-ss;	v = dy+ss/2;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dx+0.83*dy)-0.9*ss;	v = 0.83*dx-0.55*dy+0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dx-0.83*dy)-0.9*ss;	v = 0.83*dx+0.55*dy-0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'V':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u=-dy/1.5+ss/3;	v=(dx+ss-u)/2;
				if(u>0 && v>0 && u+v<ss)	cs[3]=ca;
				else
				{
					int sum=0;
					u = fabs(dx)-ss;	v = dy-ss/2;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dx+0.83*dy)-0.9*ss;	v = 0.83*dx-0.55*dy-0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dx-0.83*dy)-0.9*ss;	v = 0.83*dx+0.55*dy+0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				}
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'v':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dx)-ss;	v = dy-ss/2;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dx+0.83*dy)-0.9*ss;	v = 0.83*dx-0.55*dy-0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dx-0.83*dy)-0.9*ss;	v = 0.83*dx+0.55*dy+0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'L':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u=-dx/1.5+ss/3;	v=(dy+ss-u)/2;
				if(u>0 && v>0 && u+v<ss)	cs[3]=ca;
				else
				{
					int sum=0;
					u = fabs(dy)-ss;	v = dx-ss/2;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dy+0.83*dx)-0.9*ss;	v = 0.83*dy-0.55*dx-0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dy-0.83*dx)-0.9*ss;	v = 0.83*dy+0.55*dx+0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				}
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case '<':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = dx-ss/2;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dy+0.83*dx)-0.9*ss;	v = 0.83*dy-0.55*dx-0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dy-0.83*dx)-0.9*ss;	v = 0.83*dy+0.55*dx+0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'R':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				u=dx/1.5+ss/3;	v=(dy+ss-u)/2;
				if(u>0 && v>0 && u+v<ss)	cs[3]=ca;
				else
				{
					int sum=0;
					u = fabs(dy)-ss;	v = dx+ss/2;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dy+0.83*dx)-0.9*ss;	v = 0.83*dy-0.55*dx+0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					u = fabs(0.55*dy-0.83*dx)-0.9*ss;	v = 0.83*dy+0.55*dx-0.55*ss;	v = v*v+(u<0?0:u*u);
					sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
					sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				}
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case '>':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v,u;
				int sum=0;
				u = fabs(dy)-ss;	v = dx+ss/2;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dy+0.83*dx)-0.9*ss;	v = 0.83*dy-0.55*dx+0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				u = fabs(0.55*dy-0.83*dx)-0.9*ss;	v = 0.83*dy+0.55*dx-0.55*ss;	v = v*v+(u<0?0:u*u);
				sum += v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'O':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v;
				v = hypot(dx,dy)-ss;	v=v<0?0:v*v;
				int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'o':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v;
				v = hypot(dx,dy)-ss;	v=v*v;
				int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		case 'C':
			for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
			{
				float dx=i-q.x, dy=j-q.y, v;
				v = hypot(dx,dy)-ss;	v=v*v;
				int sum = v<V ? 255 : mgl_sline(255,dpw*(sqrt(v)+S));
				v = dx*dx+dy*dy;
				sum += v<(2*pw-1)*(2*pw-1)/4 ? 255 : mgl_sline(255,dpw*(sqrt(v)+(1-2*pw)/2));
				sum = sum>255?255:sum;	cs[3] = ca*sum/255;
				if(cs[3])	pnt_plot(i,j,q.z+1,cs,oi);
			}
			break;
		}
	}
}
//-----------------------------------------------------------------------------
// scale direction for new view/zoom
float mglCanvas::GetGlyphPhi(const mglPnt &q, float phi)
{
	float x,y,z,ll;
	if(q.sub<0)
	{	x = q.u;	y = q.v;	z = q.w;	}
	else
	{
		x = Bp.b[0]*q.u + Bp.b[1]*q.v + Bp.b[2]*q.w;
		y = Bp.b[3]*q.u + Bp.b[4]*q.v + Bp.b[5]*q.w;
		z = Bp.b[6]*q.u + Bp.b[7]*q.v + Bp.b[8]*q.w;

		float dv= get_persp(Bp.pf,q.z,Depth);
		float c = get_pfact(Bp.pf,Depth);
		x += (q.x-Width/2)*z*c*dv;
		y += (q.y-Height/2)*z*c*dv;
	}
	ll = x*x+y*y;
	if(ll < 1e-10)	return NAN;
	if(ll==ll && phi<1e4)
	{
		phi = -atan2(y,x)*180/M_PI;
//		if(fabs(phi)>90) 	phi+=180;	// NOTE this is 2nd part of rotation changes (see also text_plot())
	}
	else phi=0;
	return phi;
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_draw(const mglPrim &P, mglDrawReg *d)
{
	float phi = GetGlyphPhi(Pnt[P.n2],P.w);
	if(mgl_isnan(phi))	return;
	if(d)	{	d->PDef = MGL_SOLID_MASK;	d->angle = 0;	d->PenWidth=(P.n3&4)?1.2:0.8;	}

	mglPnt p=Pnt[P.n1];	p.a=1;
	mreal fact = get_persp(Bp.pf,p.z,Depth);
	mreal pf=p.sub<0?1:sqrt((Bp.b[0]*Bp.b[0]+Bp.b[1]*Bp.b[1]+Bp.b[3]*Bp.b[3]+Bp.b[4]*Bp.b[4])/2)*fact;
	mreal size=P.s, f = P.p*pf*P.s;
	p.u *= pf*size;	p.v *= pf*size;

	const mglGlyph &g = Glf[P.n4];
	if(P.n3&8)
	{
		if(!(P.n3&4))	glyph_line(phi,p,f,true, d);
		glyph_line(phi,p,f,false, d);
	}
	else
	{
		if(!(P.n3&4))	glyph_fill(phi,p,f,g, d);
		glyph_wire(phi,p,f,g, d);
	}
}
//-----------------------------------------------------------------------------
void static mgl_addpnts(mreal x1,mreal y1,mreal x2,mreal y2, std::vector<mreal> *b)
{
// 	if(x1>x2)	{	mreal t=x1;	x1=x2;	x2=t;	t=y1;	y1=y2;	y2=t;	}
	if(y1<y2)	for(int i=long(y1);i<=long(y2)+1;i++)
	{
 		mreal d = (i-y1)/(y2-y1);
 		if(d>=0 && d<=1)	b[i].push_back(x1+d*(x2-x1));
	}
	else	for(int i=long(y2);i<=long(y1)+1;i++)
	{
// 		mreal xx1 = x1+(x2-x1)*(i-y1)/(y2-y1);
// 		mreal xx2 = x1+(x2-x1)*(i+1-y1)/(y2-y1);
// 		if(xx1>xx2)	{	mreal t=xx1;	xx1=xx2;	xx2=t;	}
// 		if(xx1<x1)	xx1=x1;
// 		if(xx2>x2)	xx2=x2;
// 		if(i>y1 && i<y2)
// 		{
// 			b[i].push_back(xx1);
// 			if(xx2>=xx1+1)	{	b[i].push_back(xx2);	b[i].push_back(xx2);	}
// 		}
 		mreal d = (i-y1)/(y2-y1);
 		if(d>=0 && d<=1)	b[i].push_back(x1+d*(x2-x1));
	}
}
void mglCanvas::glyph_fill(mreal phi, const mglPnt &pp, mreal f, const mglGlyph &g, const mglDrawReg *d)
{
	if(g.trig && g.nt>0)	// slow but look very nice :(
	{
		const mreal co=cos(phi*M_PI/180), si=sin(phi*M_PI/180);
		mglPnt q0=pp, q1=pp, q2=pp;
		q0.u=q0.v=q1.u=q1.v=q2.u=q2.v=NAN;
		for(long ik=0;ik<g.nt;ik++)
		{
			long ii = 6*ik; mreal x, y;
			x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
			q0.x = pp.x+(x*co+y*si)/2;	q0.y = pp.y+(y*co-x*si)/2;	ii+=2;
			x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
			q1.x = pp.x+(x*co+y*si)/2;	q1.y = pp.y+(y*co-x*si)/2;	ii+=2;
			x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
			q2.x = pp.x+(x*co+y*si)/2;	q2.y = pp.y+(y*co-x*si)/2;
			trig_draw(q0,q1,q2,false,d);
		}
		return;
	}
	if(!g.line || g.nl<=0)	return;
	const mreal co=cos(phi*M_PI/180), si=sin(phi*M_PI/180);

	mreal x1 = 1e10, x2=-1e10, y1=1e10, y2=-1e10;
	for(long i=0;i<g.nl;i++)	// find sizes of glyph
	{
		long ii=2*i;
		if(g.line[ii]==0x3fff && g.line[ii+1]==0x3fff)	continue;
		mreal x = pp.u + g.line[ii]*f, y = pp.v + g.line[ii+1]*f;
		mreal xx = pp.x+(x*co+y*si)/2, yy = pp.y+(y*co-x*si)/2;
		if(xx<x1)	x1=xx;
		if(xx>x2)	x2=xx;
		if(yy<y1)	y1=yy;
		if(yy>y2)	y2=yy;
	}
	x1-=2;	x2+=2;	y1-=2;	y2+=2;
	long w = long(x2-x1+1), h = long(y2-y1+1), il=0;
	long x0=long(x1), y0=long(y1),i1=1,i2=w-2,j1=1,j2=h-2;
	if(d)	// apply mglDrawReg
	{
		if(x0+i1<d->x1)	i1 = d->x1-x0;
		if(x0+i2>d->x2)	i2 = d->x2-x0;
		if(y0+j1<d->y1)	j1 = d->y1-y0;
		if(y0+j2>d->y2)	j2 = d->y2-y0;
	}
	else
	{
		if(x0+i1<0)		i1 = -x0;
		if(x0+i2>Width)	i2 = Width-x0;
		if(y0+j1<0)		j1 = -y0;
		if(y0+j2>Height)j2 = Height-y0;
	}
	if(i1>=i2 || j1>=j2)	return;

	std::vector<mreal> *b = new std::vector<mreal>[h];
	const float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
	const int oi = d?d->ObjId:-1;
	unsigned char r[4];	col2int(pp,r,oi);
	if(r[3])	for(long i=0;i<g.nl;i++)	// add bounding points
	{
		long ii=2*i;
		mreal x = pp.u + g.line[ii]*f, y = pp.v + g.line[ii+1]*f;
		mreal xx1 = pp.x+(x*co+y*si)/2-x1, yy1 = pp.y+(y*co-x*si)/2-y1, xx2, yy2;
		if(g.line[ii]==0x3fff && g.line[ii+1]==0x3fff)	// line breakthrough
		{	il = i+1;	continue;	}
		else if(i==g.nl-1 || (g.line[ii+2]==0x3fff && g.line[ii+3]==0x3fff))	// enclose the circle
		{
			ii=2*il;	x = pp.u + g.line[ii]*f;	y = pp.v + g.line[ii+1]*f;
			xx2 = pp.x+(x*co+y*si)/2-x1;	yy2 = pp.y+(y*co-x*si)/2-y1;
		}
		else	// ordinary line
		{
			ii+=2;	x = pp.u + g.line[ii]*f;	y = pp.v + g.line[ii+1]*f;
			xx2 = pp.x+(x*co+y*si)/2-x1;	yy2 = pp.y+(y*co-x*si)/2-y1;
		}
		mgl_addpnts(xx1,yy1,xx2,yy2,b);
		// draw boundary lines in any case ???
		if(fabs(xx2-xx1)>fabs(yy2-yy1))	// horizontal line
		{
			mreal d = (yy2-yy1)/(xx2-xx1), a = yy1-d*xx1+0.5;
			if(xx1>xx2)	{	mreal t=xx1;	xx1=xx2;	xx2=t;	}
			for(long k=xx1;k<=xx2;k++)
			{
				long ii = long(k), jj = long(a+d*k);
				if(ii>=i1 && ii<=i2 && jj>=j1 && jj<=j2)	pnt_plot(x0+ii,y0+jj,pp.z+dz,r,oi);
			}
		}
		else	// vertical line
		{
			mreal d = (xx2-xx1)/(yy2-yy1), a = xx1-d*yy1+0.5;
			if(yy1>yy2)	{	mreal t=yy1;	yy1=yy2;	yy2=t;	}
			for(long k=yy1;k<=yy2;k++)
			{
				long jj = long(k), ii = long(a+d*k);
				if(ii>=i1 && ii<=i2 && jj>=j1 && jj<=j2)	pnt_plot(x0+ii,y0+jj,pp.z+dz,r,oi);
			}
		}
	}
	// TODO add smoothing -- if 3 neighbors >0 => set 1; if 3 neighbors=0 => set 0 ???
	for(long j=j1;j<=j2;j++)	// draw glyph
	{
		if(b[j].size()<2)	continue;
		std::sort(b[j].begin(),b[j].end());
		for(size_t k=0;k<b[j].size();k+=2)
		{
			long ii1 = long(b[j][k]+0.5), ii2=long(b[j][k+1]+0.5);
//			if(ii1==ii2 && b[j].size()%2==1)	{	k++;	ii2=long(b[j][k+1]+0.5);	}
			if(ii1<i1)	ii1=i1;
			if(ii2>i2)	ii2=i2;
			for(long i=ii1;i<=ii2;i++)	pnt_plot(x0+i,y0+j,pp.z+dz,r,oi);
		}
	}
	delete []b;
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_wire(mreal phi, const mglPnt &pp, mreal f, const mglGlyph &g, const mglDrawReg *d)
{
	if(!g.line || g.nl<=0)	return;
	long il=0;
	const mreal co=cos(phi*M_PI/180), si=sin(phi*M_PI/180);
	mglPnt q0=pp, q1=pp;	q0.u=q0.v=q1.u=q1.v=NAN;
	mglPoint p1,p2;
	for(long ik=0;ik<g.nl;ik++)
	{
		long ii = 2*ik;
		if(g.line[ii]==0x3fff && g.line[ii+1]==0x3fff)	// line breakthrough
		{	il = ik+1;	continue;	}
		else if(ik==g.nl-1 || (g.line[ii+2]==0x3fff && g.line[ii+3]==0x3fff))
		{	// enclose the circle
			mreal x,y;
			x = pp.u+g.line[ii]*f;	y = pp.v+g.line[ii+1]*f;
			q0.x = pp.x+(x*co+y*si)/2;	q0.y = pp.y+(y*co-x*si)/2;	ii=2*il;
			x = pp.u+g.line[ii]*f;	y = pp.v+g.line[ii+1]*f;
			q1.x = pp.x+(x*co+y*si)/2;	q1.y = pp.y+(y*co-x*si)/2;
			line_draw(q0,q1,d);
		}
		else
		{	// normal line
			mreal x,y;
			x = pp.u+g.line[ii]*f;	y = pp.v+g.line[ii+1]*f;
			q0.x = pp.x+(x*co+y*si)/2;	q0.y = pp.y+(y*co-x*si)/2;	ii+=2;
			x = pp.u+g.line[ii]*f;	y = pp.v+g.line[ii+1]*f;
			q1.x = pp.x+(x*co+y*si)/2;	q1.y = pp.y+(y*co-x*si)/2;
			line_draw(q0,q1,d);
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_line(mreal phi, const mglPnt &pp, mreal f, bool solid, const mglDrawReg *d)
{
	const mreal co=cos(phi*M_PI/180), si=sin(phi*M_PI/180);
	mglPnt q0=pp,q1=pp,q2=pp,q3=pp;
	q0.u=q0.v=q1.u=q1.v=q2.u=q2.v=q3.u=q3.v=NAN;

	mreal dy = 0.004,x,y;
	x=pp.u;		y=pp.v-dy;	q0.x=pp.x+(x*co+y*si)/2;	q0.y=pp.y+(y*co-x*si)/2;
	x=pp.u+f;	y=pp.v-dy;	q1.x=pp.x+(x*co+y*si)/2;	q1.y=pp.y+(y*co-x*si)/2;
	x=pp.u;		y=pp.v+dy;	q2.x=pp.x+(x*co+y*si)/2;	q2.y=pp.y+(y*co-x*si)/2;
	x=pp.u+f;	y=pp.v+dy;	q3.x=pp.x+(x*co+y*si)/2;	q3.y=pp.y+(y*co-x*si)/2;

	if(solid)	quad_draw(q0,q1,q3,q2,d);
	else
	{
		line_draw(q0,q1,d);	line_draw(q2,q1,d);
		line_draw(q0,q3,d);	line_draw(q2,q3,d);
	}
}
//-----------------------------------------------------------------------------
long mglCanvas::setPp(mglPnt &q, const mglPoint &p)
{
	q.xx=q.x=p.x;	q.yy=q.y=p.y;	q.zz=q.z=p.z;
	long k;
#pragma omp critical(pnt)
	{k=Pnt.size();	MGL_PUSH(Pnt,q,mutexPnt);}
	return k;
}
//-----------------------------------------------------------------------------
void mglCanvas::arrow_draw(long n1, long n2, char st, float ll)
{
	const mglPnt &p1=Pnt[n1], &p2=Pnt[n2];
	mglPnt q=p1; 	//q.u=q.v=q.w=0;

	mglPoint kl(p1.x-p2.x,p1.y-p2.y,p1.z-p2.z), kt, p0(p1.x,p1.y,p1.z), p;
	mreal d = hypot(kl.x,kl.y);
	if(d==0)	return;
	kl /= d;	kt = !kl;
	kl *= ll;	kt *= ll;

	Reserve(8);
	long k1,k2,k3,k4;

	switch(st)	// S,D -- cube, T -- sq.pyramid, I -- square, O -- sphere???, A,K,V -- cone???
	{
		case 'I':
			k1=setPp(q,p0+kt);	k2=setPp(q,p0-kt);	line_plot(k1,k2);	break;
		case 'D':
			k1=setPp(q,p0+kl);	k2=setPp(q,p0-kl);	k3=setPp(q,p0+kt);	k4=setPp(q,p0-kt);
			trig_plot(k1,k2,k3);	trig_plot(k1,k2,k4);	break;
		case 'S':
			k1=setPp(q,p0+kl+kt);	k2=setPp(q,p0+kl-kt);
			k3=setPp(q,p0-kl-kt);	k4=setPp(q,p0-kl+kt);
			quad_plot(k1,k2,k4,k3);	break;
		case 'X':
			k1=setPp(q,p0+kl+kt);	k2=setPp(q,p0+kl-kt);
			k3=setPp(q,p0-kl-kt);	k4=setPp(q,p0-kl+kt);
			line_plot(k1,k3);	line_plot(k2,k4);	break;
		case 'T':
			k1=setPp(q,p0-kl+kt);	k2=setPp(q,p0-kl-kt);	k3=setPp(q,p0+kl);
			trig_plot(k1,k2,k3);	break;
		case 'K':
			k1=setPp(q,p0+kt);	k2=setPp(q,p0-kt);	line_plot(k1,k2);
		case 'A':
			k1=setPp(q,p0-2.*kl+kt);	k2=setPp(q,p0-2.*kl-kt);	k3=setPp(q,p0-1.5*kl);
			trig_plot(n1,k3,k1);	trig_plot(n1,k3,k2);	break;
		case 'V':
			k1=setPp(q,p0+2.*kl+kt);	k2=setPp(q,p0+2.*kl-kt);	k3=setPp(q,p0+1.5*kl);
			trig_plot(n1,k3,k1);	trig_plot(n1,k3,k2);	break;
		case 'O':	// let draw icosahedron
		{
			const int n = 12;	k1=setPp(q,p0+kl);
			for(int i=1;i<=n;i++)
			{
				mreal u = 2*i*M_PI/n;
				k2 = k1;	k1 = setPp(q,p0+kl*cos(u)+kt*sin(u));
				trig_plot(n1,k1,k2);
			}
			break;
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::arrow_plot_3d(long n1, long n2, char st, float ll)
{
	const mglPnt &p1=Pnt[n1], &p2=Pnt[n2];
	mglPnt q=p1; 	//q.u=q.v=q.w=0;

	mglPoint kl(p1.x-p2.x,p1.y-p2.y,p1.z-p2.z), kt, kz, p0(p1.x,p1.y,p1.z), p;
	if(kl.norm()==0)	return;
	kl.Normalize();	kt = !kl;	kz = kl^kt;
	kl *= ll;	kt *= ll;	kz *= ll;

	Reserve(8);
	long k1,k2,k3,k4,k5, k6,k7,k8;

	switch(st)	// S,D -- cube, T -- sq.pyramid, I -- square, O -- sphere???, A,K,V -- cone???
	{
		case 'I':
			k1=setPp(q,p0+kt);	k2=setPp(q,p0+kz);
			k3=setPp(q,p0-kt);	k4=setPp(q,p0-kz);
			quad_plot(k1,k2,k4,k3);	break;
		case 'D':
			k1=setPp(q,p0+kl);	k2=setPp(q,p0-kl);	k5=k3=setPp(q,p0+kt);
			k4=setPp(q,p0+kz);	trig_plot(k1,k3,k4);	trig_plot(k2,k3,k4);	k3=k4;
			k4=setPp(q,p0-kt);	trig_plot(k1,k3,k4);	trig_plot(k2,k3,k4);	k3=k4;
			k4=setPp(q,p0-kz);	trig_plot(k1,k3,k4);	trig_plot(k2,k3,k4);	k3=k4;
			trig_plot(k1,k3,k5);	trig_plot(k2,k3,k5);	break;
		case 'S':
			k1=setPp(q,p0+kl+kt);	k2=setPp(q,p0+kl+kz);	k3=setPp(q,p0+kl-kt);	k4=setPp(q,p0+kl-kz);
			k5=setPp(q,p0-kl+kt);	k6=setPp(q,p0-kl+kz);	k7=setPp(q,p0-kl-kt);	k8=setPp(q,p0-kl-kz);
			quad_plot(k1,k2,k4,k3);	quad_plot(k1,k2,k5,k6);	quad_plot(k3,k2,k7,k6);
			quad_plot(k1,k4,k5,k8);	quad_plot(k3,k4,k7,k8);	quad_plot(k5,k6,k8,k7);	break;
		case 'X':
			k1=setPp(q,p0+kl+kt);	k2=setPp(q,p0+kl+kz);	k3=setPp(q,p0+kl-kt);	k4=setPp(q,p0+kl-kz);
			k5=setPp(q,p0-kl+kt);	k6=setPp(q,p0-kl+kz);	k7=setPp(q,p0-kl-kt);	k8=setPp(q,p0-kl-kz);
			line_plot(k1,k7);	line_plot(k2,k8);	line_plot(k3,k5);	line_plot(k4,k6);	break;
		case 'T':
			k1=setPp(q,p0-kl+kt);	k2=setPp(q,p0-kl+kz);	k3=setPp(q,p0-kl-kt);
			k4=setPp(q,p0-kl-kz);	k5=setPp(q,p0+kl);
			trig_plot(k1,k2,k5);	trig_plot(k2,k3,k5);
			trig_plot(k3,k4,k5);	trig_plot(k1,k4,k5);	break;
		case 'K':
			k1=setPp(q,p0+kt);	k2=setPp(q,p0+kz);
			k3=setPp(q,p0-kt);	k4=setPp(q,p0-kz);	quad_plot(k1,k2,k4,k3);
		case 'A':
			k1=setPp(q,p0-2.*kl+kt);	k2=setPp(q,p0-2.*kl+kz);	k3=setPp(q,p0-2.*kl-kt);
			k4=setPp(q,p0-2.*kl-kz);	k5=setPp(q,p0-1.5*kl);
			trig_plot(n1,k5,k1);	trig_plot(n1,k5,k2);
			trig_plot(n1,k5,k3);	trig_plot(n1,k5,k4);	break;
		case 'V':
			k1=setPp(q,p0+2.*kl+kt);	k2=setPp(q,p0+2.*kl+kz);	k3=setPp(q,p0+2.*kl-kt);
			k4=setPp(q,p0+2.*kl-kz);	k5=setPp(q,p0+1.5*kl);
			trig_plot(n1,k5,k1);	trig_plot(n1,k5,k2);
			trig_plot(n1,k5,k3);	trig_plot(n1,k5,k4);	break;
		case 'O':	// let draw icosahedron
		{
			const int n = 12, m = n/2;	Reserve(n*m);
			long *nn=new long[2*n], n1=setPp(q,p0+kl), n2=setPp(q,p0-kl);
			mreal u,v,rr;
			for(long i=0;i<m;i++)	for(long j=0;j<n;j++)
			{
				if(i>0 && i<m-1)
				{
					u = i*M_PI/(m-1.);	v = 2*M_PI*j/(n-1.)-1;	rr = sin(u);
					nn[j+n]=nn[j];	nn[j]=setPp(q,p0+kl*cos(u)+kt*rr*cos(v)+kz*rr*sin(v));
				}
				else if(i==0)	nn[j] = n1;
				else if(i==m-1)	{	nn[j+n]=nn[j];	nn[j]=n2;	}
				if(i*j>0)	quad_plot(nn[j-1], nn[j], nn[j+n-1], nn[j+n]);
			}
			delete []nn;	break;
		}
	}
}
//-----------------------------------------------------------------------------
bool mglCanvas::quad_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4) const
{
	long y1,x1,y2,x2;
	mglPnt d1(p2-p1), d2(p3-p1), d3(p4+p1-p2-p3);

	if(d1.x==0 && d1.y==0)	return trig_vis(p1,p3,p4);
	if(d2.x==0 && d2.y==0)	return trig_vis(p1,p2,p4);

	x1 = long(mgl_min(mgl_min(p1.x,p2.x), mgl_min(p3.x,p4.x)));	// bounding box
	y1 = long(mgl_min(mgl_min(p1.y,p2.y), mgl_min(p3.y,p4.y)));
	x2 = long(mgl_max(mgl_max(p1.x,p2.x), mgl_max(p3.x,p4.x)));
	y2 = long(mgl_max(mgl_max(p1.y,p2.y), mgl_max(p3.y,p4.y)));
	x1=mgl_imax(x1,0);	x2=mgl_imin(x2,Width);
	y1=mgl_imax(y1,0);	y2=mgl_imin(y2,Height);
//	if(x1>x2 || y1>y2)	return;

	const float dd = d1.x*d2.y-d1.y*d2.x;
	const float dsx =-4*(d2.y*d3.x - d2.x*d3.y)*d1.y;
	const float dsy = 4*(d2.y*d3.x - d2.x*d3.y)*d1.x;

	const float x0 = p1.x, y0 = p1.y;
	bool vis = false;
	for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
	{
		float xx = (i-x0), yy = (j-y0), s;
		s = dsx*xx + dsy*yy + (dd+d3.y*xx-d3.x*yy)*(dd+d3.y*xx-d3.x*yy);
		if(s>=0)
		{
			s = sqrt(s);
			float qu = d3.x*yy - d3.y*xx + dd + s;
			float qv = d3.y*xx - d3.x*yy + dd + s;
			float u = 2.f*(d2.y*xx - d2.x*yy)/qu;
			float v = 2.f*(d1.x*yy - d1.y*xx)/qv;
			if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	// first root bad
			{
				qu = d3.x*yy - d3.y*xx + dd - s;
				qv = d3.y*xx - d3.x*yy + dd - s;
//					u = v = -1.f;
				u = 2.f*(d2.y*xx - d2.x*yy)/qu;	v = 2.f*(d1.x*yy - d1.y*xx)/qv;
				if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	continue;	// second root bad
			}
			float zz = p1.z+d1.z*u+d2.z*v+d3.z*(u*v);
			if(zz>=Z[3*(i+Width*(Height-1-j))]-2)	vis=true;
		}
	}
	return vis;
}
//-----------------------------------------------------------------------------
bool mglCanvas::trig_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3) const
{
	long y1,x1,y2,x2;
	const mglPnt d1(p2-p1), d2(p3-p1);

	const float tmp = d2.x*d1.y - d1.x*d2.y;
	if(fabs(tmp)<1e-5)	return false;		// points lies on the same line
	const float dyv =-d1.x/tmp,	dxv = d1.y/tmp;
	const float dyu = d2.x/tmp,	dxu =-d2.y/tmp;

	x1 = long(mgl_min(p1.x<p2.x?p1.x:p2.x, p3.x));	// bounding box
	y1 = long(mgl_min(p1.y<p2.y?p1.y:p2.y, p3.y));
	x2 = long(mgl_max(p1.x>p2.x?p1.x:p2.x, p3.x));
	y2 = long(mgl_max(p1.y>p2.y?p1.y:p2.y, p3.y));
	x1=x1>0?x1:0;	x2=x2<Width?x2:Width;
	y1=y1>0?y1:0;	y2=y2<Height?y2:Height;
//	if(x1>x2 || y1>y2)	return;
	// default normale
	const float x0 = p1.x, y0 = p1.y;
	bool vis=false;
	// provide additional height to be well visible on the surfaces
	for(long j=y1;j<=y2;j++)	for(long i=x1;i<=x2;i++)
	{
		float xx = (i-x0), yy = (j-y0);
		float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;
		if(u<0 || v<0 || u+v>1)	continue;
		float zz = p1.z+d1.z*u+d2.z*v;
		if(zz>=Z[3*(i+Width*(Height-1-j))]-2)	vis=true;
	}
	return vis;
}
//-----------------------------------------------------------------------------
