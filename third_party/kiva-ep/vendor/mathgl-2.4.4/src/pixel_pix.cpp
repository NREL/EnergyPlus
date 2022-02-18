/***************************************************************************
 * pixel_bit.cpp is part of Math Graphic Library
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
void mglCanvas::pxl_primpx(long id, long n, const void *)	// NOTE this variant is too slow ... may be later in CUDA???
{
	mglDrawReg d;	d.set(this,1,1,id);
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for firstprivate(d)
#endif
	for(long ii=0;ii<n;ii+=mglNumThr)
	{
		long i=ii%Width, j=ii/Width;
		for(size_t k=0;k<Prm.size();k++)
		{
			if(Stop)	break;
			const mglPrim &p=GetPrm(k);
			d.PDef = p.n3;	d.pPos = p.s;
			d.ObjId = p.id;	d.PenWidth=p.w;
			d.angle = p.angl;
			if(p.type==2 || p.type==3) d.PDef = p.m;
			switch(p.type)
			{
			case 0:	mark_pix(i,j,Pnt[p.n1],p.n4,p.s,&d);	break;
			case 1:	line_pix(i,j,Pnt[p.n1],Pnt[p.n2],&d);	break;
			case 2:	trig_pix(i,j,Pnt[p.n1],Pnt[p.n2],Pnt[p.n3],true,&d);	break;
			case 3:	quad_pix(i,j,Pnt[p.n1],Pnt[p.n2],Pnt[p.n3],Pnt[p.n4],&d);	break;
			case 4:	glyph_pix(i,j,p,&d);	break;
			}
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::quad_pix(long i, long j, const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *d)
{
	if(!visible(i,j,d->m, d->PenWidth,d->angle))	return;
	mglPnt d1(p2-p1), d2(p3-p1), d3(p4+p1-p2-p3);
	float dd = d1.x*d2.y-d1.y*d2.x;
	float dsx =-4*(d2.y*d3.x - d2.x*d3.y)*d1.y;
	float dsy = 4*(d2.y*d3.x - d2.x*d3.y)*d1.x;
	float xx = (i-p1.x), yy = (j-p1.y), s;
	s = dsx*xx + dsy*yy + (dd+d3.y*xx-d3.x*yy)*(dd+d3.y*xx-d3.x*yy);
	if(s<0)	return;	// no solution
	s = sqrt(s);
	float qu = d3.x*yy - d3.y*xx + dd + s, u=-1;
	float qv = d3.y*xx - d3.x*yy + dd + s, v=-1;
	if(qu && qv)
	{
		u = 2.f*(d2.y*xx - d2.x*yy)/qu;
		v = 2.f*(d1.x*yy - d1.y*xx)/qv;
	}
	if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	// first root bad
	{
		qu = d3.x*yy - d3.y*xx + dd - s;
		qv = d3.y*xx - d3.x*yy + dd - s;
		u = v = -1.f;
		if(qu && qv)
		{
			u = 2.f*(d2.y*xx - d2.x*yy)/qu;
			v = 2.f*(d1.x*yy - d1.y*xx)/qv;
		}
		if(u*(1.f-u)<0.f || v*(1.f-v)<0.f)	return;	// second root bad
	}
	mglPnt p(p1+d1*u+d2*v+d3*(u*v));
	if(mgl_isnan(p.u) && mgl_isnum(p.v))
	{
		mglPoint n1(mglPoint(p2.x-p1.x,p2.y-p1.y,p2.z-p1.z)^mglPoint(p3.x-p1.x,p3.y-p1.y,p3.z-p1.z));
		mglPoint n2(mglPoint(p2.x-p4.x,p2.y-p4.y,p2.z-p4.z)^mglPoint(p3.x-p4.x,p3.y-p4.y,p3.z-p4.z));
		p.u = (n1.x+n2.x)*0.5;
		p.v = (n1.y+n2.y)*0.5;
		p.w = (n1.z+n2.z)*0.5;
	}
	unsigned char r[4];	col2int(p,r,d->ObjId);
	if(r[3])	pnt_plot(i,j,p.z,r,d->ObjId);
}
//-----------------------------------------------------------------------------
void mglCanvas::trig_pix(long i, long j, const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool anorm, const mglDrawReg *d)
{
	if(!visible(i,j,d->m, d->PenWidth,d->angle))	return;
	mglPnt d1(p2-p1), d2(p3-p1);
	float dd = d2.x*d1.y - d1.x*d2.y;
	if(fabs(dd)<1e-5)	return;		// points lies on the same line
	float dyv =-d1.x/dd, dxv = d1.y/dd, dyu = d2.x/dd, dxu =-d2.y/dd;
	float xx = (i-p1.x), yy = (j-p1.y);
	float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;
	if(u<0 || v<0 || u+v>1)	return;
	mglPnt p(p1+d1*u+d2*v);
	if(mgl_isnan(p.u) && mgl_isnum(p.v) && anorm)
	{	mglPoint nr(mglPoint(p2.x-p1.x,p2.y-p1.y,p2.z-p1.z)^mglPoint(p3.x-p1.x,p3.y-p1.y,p3.z-p1.z));
		p.u = nr.x;	p.v = nr.y;	p.w = nr.z;	}
	unsigned char r[4];
	col2int(p,r,d->ObjId);
	if(r[3])	pnt_plot(i,j,p.z,r,d->ObjId);
}
//-----------------------------------------------------------------------------
void mglCanvas::line_pix(long i, long j, const mglPnt &p1, const mglPnt &p2, const mglDrawReg *dr)
{
	float xx = (i-p1.x), yy = (j-p1.y);
	mglPnt d(p2-p1);
	float dd = hypot(d.x, d.y);
	float dxv = d.y/dd, dyv =-d.x/dd, dxu = d.x/dd, dyu = d.y/dd;
	float u = dxu*xx+dyu*yy, v = dxv*xx+dyv*yy;	v = v*v;
	if(u<0)			v += u*u;
	else if(u>dd)	v += (u-dd)*(u-dd);
	float pw=dr->PenWidth, dpw=3*pen_delta;
	if(dr->ObjId==HighId)	{	pw *= 2;	dpw=2*pen_delta;	}
	if(v>pw*pw || !(dr->PDef & ( (uint64_t)1<<long(fmod(dr->pPos+u/pw, 16)) ) ))	return;
	mglPnt p(p1+d*(u/dd));
	unsigned char r[4];
	col2int(p,r,dr->ObjId);
	r[3] = v<(pw-1)*(pw-1)/4 ? 255 : mgl_sline(255,dpw*(sqrt(v)+(1-pw)/2));
	float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
	if(r[3])	pnt_plot(i,j,p.z+dz,r,dr->ObjId);
}
//-----------------------------------------------------------------------------
void mglCanvas::pnt_pix(long i, long j, const mglPnt &p, const mglDrawReg *dr)
{
	float pw=3*dr->PenWidth,dpw=3*pen_delta;
	if(dr->ObjId==HighId)	{	pw *= 2;	dpw=2*pen_delta;	}
	unsigned char cs[4];
	col2int(p,cs,dr->ObjId);
	float xx = (i-p.x), yy = (j-p.y), v = xx*xx+yy*yy;
	if(cs[3]==0 || v>(10/dpw+pw)*(10/dpw+pw))	return;
	if(v<(pw-1)*(pw-1)/4)	cs[3] = mgl_sline(cs[3],dpw*(sqrt(v)+(1-pw)/2));
	if(cs[3])	pnt_plot(i,j,p.z,cs,dr->ObjId);
}
//-----------------------------------------------------------------------------
void mglCanvas::mark_pix(long i, long j, const mglPnt &q, char type, mreal size, mglDrawReg *d)
{
	unsigned char cs[4];	col2int(q,cs,d->ObjId);	cs[3] = size>0 ? 255 : 255*q.t;
	mglPnt p0=q,p1=q,p2=q,p3=q;
	mreal ss=fabs(size);

	if(type=='.' || ss==0)
	{
		if(d)	d->PenWidth = ss?ss:sqrt(font_factor/400);
		pnt_pix(i,j,q,d);
	}
	else
	{
		if(d)
		{
			d->PDef = MGL_SOLID_MASK;	d->angle = 0;
			d->PenWidth*=fabs(50*size);
			if(d->PenWidth<1)	d->PenWidth=1;
		}
		if(!strchr("xsSoO",type))	ss *= 1.1;
		switch(type)
		{
		case 'P':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p3,d);	line_pix(i,j,p3,p0,d);
		case '+':
			p0.x = q.x-ss;	p0.y = q.y;	p1.x = q.x+ss;	p1.y = q.y;	line_pix(i,j,p0,p1,d);
			p2.x = q.x;	p2.y = q.y-ss;	p3.x = q.x;	p3.y = q.y+ss;	line_pix(i,j,p2,p3,d);
			break;
		case 'X':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p3,d);	line_pix(i,j,p3,p0,d);
		case 'x':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y+ss;	line_pix(i,j,p0,p1,d);
			p2.x = q.x+ss;	p2.y = q.y-ss;	p3.x = q.x-ss;	p3.y = q.y+ss;	line_pix(i,j,p2,p3,d);
			break;
		case 'S':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x-ss;	p1.y = q.y+ss;
			p2.x= q.x+ss;	p2.y= q.y+ss;	p3.x = q.x+ss;	p3.y = q.y-ss;
			quad_pix(i,j,p0,p1,p3,p2,d);
		case 's':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p3,d);	line_pix(i,j,p3,p0,d);
			break;
		case 'D':
			p0.x = q.x;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y;
			p2.x= q.x;	p2.y= q.y+ss;	p3.x = q.x-ss;	p3.y = q.y;
			quad_pix(i,j,p0,p1,p3,p2,d);
		case 'd':
			p0.x = q.x;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y;
			p2.x = q.x;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p3,d);	line_pix(i,j,p3,p0,d);
			break;
		case 'Y':
			p1.x = q.x;	p1.y = q.y-ss;	line_pix(i,j,q,p1,d);
			p2.x = q.x-0.8*ss;	p2.y = q.y+0.6*ss;	line_pix(i,j,q,p2,d);
			p3.x = q.x+0.8*ss;	p3.y = q.y+0.6*ss;	line_pix(i,j,q,p3,d);
			break;
		case '*':
			p0.x = q.x-ss;		p0.y = q.y;
			p1.x = q.x+ss;		p1.y = q.y;	line_pix(i,j,p0,p1,d);
			p0.x = q.x-0.6*ss;	p0.y = q.y-0.8*ss;
			p1.x = q.x+0.6*ss;	p1.y = q.y+0.8*ss;	line_pix(i,j,p0,p1,d);
			p0.x = q.x-0.6*ss;	p0.y = q.y+0.8*ss;
			p1.x = q.x+0.6*ss;	p1.y = q.y-0.8*ss;	line_pix(i,j,p0,p1,d);
			break;
		case 'T':
			p0.x = q.x-ss;	p0.y = q.y-ss/2;
			p1.x = q.x+ss;	p1.y = q.y-ss/2;
			p2.x= q.x;		p2.y= q.y+ss;
			trig_pix(i,j,p0,p1,p2,false,d);
		case '^':
			p0.x = q.x-ss;	p0.y = q.y-ss/2;
			p1.x = q.x+ss;	p1.y = q.y-ss/2;
			p2.x= q.x;		p2.y= q.y+ss;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p0,d);	break;
		case 'V':
			p0.x = q.x-ss;	p0.y = q.y+ss/2;
			p1.x = q.x+ss;	p1.y = q.y+ss/2;
			p2.x= q.x;		p2.y= q.y-ss;
			trig_pix(i,j,p0,p1,p2,false,d);
		case 'v':
			p0.x = q.x-ss;	p0.y = q.y+ss/2;
			p1.x = q.x+ss;	p1.y = q.y+ss/2;
			p2.x= q.x;		p2.y= q.y-ss;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p0,d);	break;
		case 'L':
			p0.x = q.x+ss/2;	p0.y = q.y+ss;
			p1.x = q.x+ss/2;	p1.y = q.y-ss;
			p2.x= q.x-ss;		p2.y= q.y;
			trig_pix(i,j,p0,p1,p2,false,d);
		case '<':
			p0.x = q.x+ss/2;	p0.y = q.y+ss;
			p1.x = q.x+ss/2;	p1.y = q.y-ss;
			p2.x= q.x-ss;		p2.y= q.y;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p0,d);	break;
		case 'R':
			p0.x = q.x-ss/2;	p0.y = q.y+ss;
			p1.x = q.x-ss/2;	p1.y = q.y-ss;
			p2.x= q.x+ss;		p2.y= q.y;
			trig_pix(i,j,p0,p1,p2,false,d);
		case '>':
			p0.x = q.x-ss/2;	p0.y = q.y+ss;
			p1.x = q.x-ss/2;	p1.y = q.y-ss;
			p2.x= q.x+ss;		p2.y= q.y;
			line_pix(i,j,p0,p1,d);	line_pix(i,j,p1,p2,d);
			line_pix(i,j,p2,p0,d);	break;
		case 'O':
			{
				float xx = (i-q.x), yy = (j-q.y);
				float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
				if(xx*xx+yy*yy<ss*ss && cs[3])	pnt_plot(i,j,q.z+dz,cs,d->ObjId);
			}
		case 'o':
			{
				float pw=d->PenWidth;
				float xx = (i-q.x), yy = (j-q.y), v = hypot(xx,yy);
				v = (v-ss)*(v-ss);
//				if(v>pw*pw)	return;
				if(v>(pw-1)*(pw-1)/4)	cs[3] = mgl_sline(cs[3],2*(sqrt(v)+(1-pw)/2));
				float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
				if(cs[3])	pnt_plot(i,j,q.z+dz,cs,d->ObjId);
			}
			break;
		case 'C':
			pnt_pix(i,j,q,d);
			{
				float pw=d->PenWidth;
				float xx = (i-q.x), yy = (j-q.y), v = hypot(xx,yy);
				v = (v-ss)*(v-ss);
//				if(v>pw*pw)	return;
				if(v>(pw-1)*(pw-1)/4)	cs[3] = mgl_sline(cs[3],2*(sqrt(v)+(1-pw)/2));
				float dz = Width>2 ? 1 : 1e-5*Width;		// provide additional height to be well visible on the surfaces
				if(cs[3])	pnt_plot(i,j,q.z+dz,cs,d->ObjId);
			}
			break;
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_pix(long i, long j, const mglPrim &P, mglDrawReg *d)
{
	float phi = GetGlyphPhi(Pnt[P.n2],P.w);
	if(mgl_isnan(phi))	return;

	if(d)	{	d->PDef = MGL_SOLID_MASK;	d->angle = 0;	d->PenWidth=1;	}
	mglPnt p=Pnt[P.n1];
	mreal pf=sqrt((Bp.b[0]*Bp.b[0]+Bp.b[1]*Bp.b[1]+Bp.b[3]*Bp.b[3]+Bp.b[4]*Bp.b[4])/2), f = P.p*pf;

	mglMatrix M;
	M.b[0] = M.b[4] = M.b[8] = P.s;
	M.RotateN(phi,0,0,1);
	M.x=p.x;	M.y=p.y;	M.z=p.z;	M.pf = 1;
	p.u *= pf;	p.v *= pf;

	const mglGlyph &g = Glf[P.n4];
	if(P.n3&8)
	{
		if(!(P.n3&4))	glyph_lpix(i,j,&M,p,f,true, d);
		glyph_lpix(i,j,&M,p,f,false, d);
	}
	else
	{
		if(!(P.n3&4))	glyph_fpix(i,j,&M,p,f,g, d);
		glyph_wpix(i,j,&M,p,f,g, d);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_fpix(long i, long j, const mglMatrix *M, const mglPnt &pp, mreal f, const mglGlyph &g, const mglDrawReg *d)
{
	if(!g.trig || g.nt<=0)	return;
	mglPnt q0=pp, q1=pp, q2=pp;
	q0.u=q0.v=q1.u=q1.v=q2.u=q2.v=NAN;
	for(long ik=0;ik<g.nt;ik++)
	{
		long ii = 6*ik;	mglPoint p;
		p.Set(f*g.trig[ii]+pp.u,f*g.trig[ii+1]+pp.v,0);
		PostScale(M,p);	q0.x = p.x;	q0.y = p.y;	q0.z = p.z;
		ii+=2;	p.Set(f*g.trig[ii]+pp.u,f*g.trig[ii+1]+pp.v,0);
		PostScale(M,p);	q1.x = p.x;	q1.y = p.y;	q1.z = p.z;
		ii+=2;	p.Set(f*g.trig[ii]+pp.u,f*g.trig[ii+1]+pp.v,0);
		PostScale(M,p);	q2.x = p.x;	q2.y = p.y;	q2.z = p.z;
		trig_pix(i,j,q0,q1,q2,false,d);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_wpix(long i, long j, const mglMatrix *M, const mglPnt &pp, mreal f, const mglGlyph &g, const mglDrawReg *d)
{
	if(!g.line || g.nl<=0)	return;
	long il=0;
	mglPnt q0=pp, q1=pp;	q0.u=q0.v=q1.u=q1.v=NAN;
	mglPoint p1,p2;
	for(long ik=0;ik<g.nl;ik++)
	{
		long ii = 2*ik;
		if(g.line[ii]==0x3fff && g.line[ii+1]==0x3fff)	// line breakthrough
		{	il = ik+1;	continue;	}
		else if(ik==g.nl-1 || (g.line[ii+2]==0x3fff && g.line[ii+3]==0x3fff))
		{	// enclose the circle. May be in future this block should be commented
			p1.Set(f*g.line[ii]+pp.u,f*g.line[ii+1]+pp.v,0);	ii=2*il;
			p2.Set(f*g.line[ii]+pp.u,f*g.line[ii+1]+pp.v,0);
		}
		else
		{	// normal line
			p1.Set(f*g.line[ii]+pp.u,f*g.line[ii+1]+pp.v,0);	ii+=2;
			p2.Set(f*g.line[ii]+pp.u,f*g.line[ii+1]+pp.v,0);
		}
		PostScale(M,p1);	PostScale(M,p2);
		q0.x = p1.x;	q0.y = p1.y;	q0.z = p1.z;
		q1.x = p2.x;	q1.y = p2.y;	q1.z = p2.z;
		line_pix(i,j,q0,q1,d);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::glyph_lpix(long i, long j, const mglMatrix *M, const mglPnt &pp, mreal f, bool solid, const mglDrawReg *d)
{
	mglPnt q0=pp,q1=pp,q2=pp,q3=pp;
	q0.u=q0.v=q1.u=q1.v=q2.u=q2.v=q3.u=q3.v=NAN;
	mglPoint p1,p2,p3,p4;

	mreal dy = 0.004;
	p1.Set(pp.u,pp.v-dy,0);	PostScale(M,p1);
	p2.Set(pp.u,pp.v+dy,0);	PostScale(M,p2);
	p3.Set(fabs(f)+pp.u,pp.v+dy,0);	PostScale(M,p3);
	p4.Set(fabs(f)+pp.u,pp.v-dy,0);	PostScale(M,p4);

	q0.x = p1.x;	q0.y = p1.y;	q0.z = p1.z;
	q1.x = p2.x;	q1.y = p2.y;	q1.z = p2.z;
	q2.x = p3.x;	q2.y = p3.y;	q2.z = p3.z;
	q3.x = p4.x;	q3.y = p4.y;	q3.z = p4.z;

	if(solid)	quad_pix(i,j,q0,q1,q3,q2,d);
	else
	{
		line_pix(i,j,q0,q1,d);	line_pix(i,j,q2,q1,d);
		line_pix(i,j,q0,q3,d);	line_pix(i,j,q2,q3,d);
	}
}
//-----------------------------------------------------------------------------
