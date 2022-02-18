/***************************************************************************
 * cont.cpp is part of Math Graphic Library
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
// NOTE: Borland before 2007 (i.e. < 0x0600) use <algorithm.h>, after 0x0630 it use <algorithm>.
// I don't find information about 2009, 2010 versions (i.e. 0x0610 and 0x0620).
// May be condition below can be rewritten as (__CODEGEARC__ >=  0x0600)
#if !defined(__BORLANDC__) || (__CODEGEARC__ >=  0x0630)
#include <algorithm>
#else
#include <algorithm.h>
#endif
#include <list>

#include "mgl2/surf.h"
#include "mgl2/cont.h"
#include "mgl2/data.h"
#include "mgl2/eval.h"
#include "mgl2/font.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//	Text printing along a curve
//
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_string_curve(mglBase *gr,long f,long ,const long *ff,const long *nn,const wchar_t *text, const char *font, mreal size)
{
	if(f<0 || nn[f]<0)	return;	// do nothing since there is no curve
	if(!font)	font="";
	int pos = strchr(font,'T') ? 1:-1, align;
	bool cc=mglGetStyle(font,0,&align);		align = align&3;
	mreal h=gr->TextHeight(font,size)/2, g = 1.1*h;
	wchar_t L[2]=L"a";

	if(align==1)	// TODO divide curve by 2
	{}

	std::vector<mglPoint> qa, qb;	// curves above and below original
	if(ff[f]<0)	for(long i=nn[f];i>=0 && i!=f;i=nn[i])	// find first real point
		if(ff[i]>=0)	{	f=i;	break;	}
	if(ff[f]<0)	return;
	mreal c=cc?gr->AddTexture(font) : gr->GetClrC(ff[f]);
	mglPoint p=gr->GetPntP(ff[f]), q=p, s;
	for(long i=nn[f];i>=0 && i!=f;i=nn[i])	// find second real point
		if(ff[i]>=0)	{	s=gr->GetPntP(ff[i]);	break;	}
	mglPoint l=!(s-q), t=l;
	qa.push_back(q+l*g);	qb.push_back(q-l*h);
	for(long i=nn[f];i>=0 && i!=f;i=nn[i])	// construct curves
	{
		p=q;	q=s;	l=t;
		if(nn[i]>=0 && ff[nn[i]]>=0)	{	s=gr->GetPntP(ff[nn[i]]);	t=!(s-q);	}
		mreal tet = t.x*l.y-t.y*l.x;
		mreal tt = 1+fabs(t.x*l.x+t.y*l.y);
		if(tet>0)
		{	qa.push_back(q+l*g);	qa.push_back(q+t*g);	qb.push_back(q-(l+t)*(h/tt));	}
		else if(tet<0)
		{	qb.push_back(q-l*h);	qb.push_back(q-t*h);	qa.push_back(q+(l+t)*(g/tt));	}
		else
		{	qa.push_back(q+l*g);	qb.push_back(q-l*h);	}
	}
	if(pos>0)	qa=qb;
	// adjust text direction
	bool rev = align==2;
	const char *ffont=mglchr(font,':');
	char *fnt = new char[strlen(font)+5];
	if(ffont) strcpy(fnt,ffont);	else *fnt=0;
/*	if(qa[0].x>qa[1].x)
	{
		if(align==0){	strcat(fnt,":R");	align=2;	}
		else if(align==1)	rev = true;
		else		{	strcat(fnt,":L");	align=0;	}
	}*/
	if(mglchr(font,'T'))	strcat(fnt,":T");
	if(rev)	reverse(qa.begin(),qa.end());
	long len = mgl_wcslen(text);
	mreal *wdt=new mreal[len+1];
	for(long j=0;j<len;j++)	{	L[0]=text[j];	wdt[j]=1.2*gr->TextWidth(L,font,size);	}
	wdt[len]=0;

	// place glyphs points
	mglPoint *pt=new mglPoint[len+1];
	pt[0] = qa[0];
	long i=0, k, m = qa.size();
	mreal t1,t2, tt=0;
	for(long j=0;j<len;j++)
	{
		mreal w = align==1 ? wdt[j] : (wdt[j]+wdt[j+1])/2;
		p = pt[j];	w *= 1.03;
		for(k=i+1;k<m;k++)	if((p-qa[k]).norm()>w)	break;
		if(k>i+1 && k<m)	tt=-1;
		i = k<m ? k-1 : m-2;		// check if end of curve
		q = qa[i];	s = qa[i+1];	// points of line segment
		mreal a = (q-s)*(q-s), b = (q-p)*(q-s), d = (q-p)*(q-p)-w*w;
		w = sqrt(b*b-a*d);		// NOTE: b*b>a*d should be here!
		if(b*b>1e3*a*d)	{	t1 = d/(b+w);	t2 = d/(b-w);	}	// keep precision
		else			{	t1 = (b-w)/a;	t2 = (b+w)/a;	}
		if(t1<0 || t1<tt)	t1=t2;	// t1<t2 should be here!
		tt=t1;	pt[j+1] = q+(s-q)*tt;
	}
	if(rev)	pos=-pos;
	mreal dc = (cc && len>1)?1/MGL_FEPSILON/(len-1):0;
	for(long j=0;j<len;j++)	// draw text
	{
		L[0] = text[align!=2?j:len-1-j];	s = pt[j+1]-pt[j];	l = !s;
		gr->text_plot(gr->AddPnt(pt[j]+(pos*h)*l,c+dc*i,align!=2?s:-s,-1,-1),L,fnt,size,0.05,c+dc*j);
	}
	delete []wdt;	delete []pt;	delete []fnt;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_textw_xyz(HMGL gr, HCDT x, HCDT y, HCDT z,const wchar_t *text, const char *font, const char *opt)
{
	long n=y->GetNx();
	if(mgl_check_dim1(gr,x,y,z,0,"Text"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("TextC",cgid++);

	long kq = gr->AllocPnts(n);
	long *nn = new long[n], *ff = new long[n];
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{	ff[i] = kq+i;	nn[i] = i+1;
		gr->AddPntQ(kq+i,mglPoint(x->v(i),y->v(i),z->v(i)),-1);	}
	nn[n-1]=-1;
	mgl_string_curve(gr,0,n,ff,nn,text,font,-1);
	delete []ff;	delete []nn;	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_textw_xy(HMGL gr, HCDT x, HCDT y, const wchar_t *text, const char *font, const char *opt)
{
	gr->SaveState(opt);
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_textw_xyz(gr,x,y,&z,text,font,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_textw_y(HMGL gr, HCDT y, const wchar_t *text, const char *font, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(y->GetNx()), z(y->GetNx());
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_textw_xyz(gr,&x,y,&z,text,font,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_xyz(HMGL gr, HCDT x, HCDT y, HCDT z,const char *text, const char *font, const char *opt)
{
	MGL_TO_WCS(text,mgl_textw_xyz(gr,x,y,z, wcs, font, opt));
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_xy(HMGL gr, HCDT x, HCDT y, const char *text, const char *font, const char *opt)
{
	mglDataV z(y->GetNx());	z.Fill(gr->AdjustZMin());
	mgl_text_xyz(gr,x,y,&z,text,font,opt);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_y(HMGL gr, HCDT y, const char *text, const char *font, const char *opt)
{
	mglDataV x(y->GetNx()), z(y->GetNx());
	x.Fill(gr->Min.x,gr->Max.x);	z.Fill(gr->AdjustZMin());
	mgl_text_xyz(gr,&x,y,&z,text,font,opt);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z,const char *text,const char *font, const char *opt,int l,int n,int lo)
{	char *s=new char[l+1];	memcpy(s,text,l);	s[l]=0;
char *f=new char[n+1];	memcpy(f,font,n);	f[n]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_text_xyz(_GR_, _DA_(x),_DA_(y), _DA_(z), s, f, o);
delete []o;	delete []s;	delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, const char *text, const char *font, const char *opt, int l,int n,int lo)
{	char *s=new char[l+1];	memcpy(s,text,l);	s[l]=0;
char *f=new char[n+1];	memcpy(f,font,n);	f[n]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_text_xy(_GR_, _DA_(x),_DA_(y),s,f,o);
delete []o;	delete []s;	delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_text_y_(uintptr_t *gr, uintptr_t *y, const char *text, const char *font, const char *opt, int l,int n,int lo)
{	char *s=new char[l+1];	memcpy(s,text,l);	s[l]=0;
char *f=new char[n+1];	memcpy(f,font,n);	f[n]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_text_y(_GR_, _DA_(y),s,f,o);	delete []o;	delete []s;	delete []f;	}
//-----------------------------------------------------------------------------
//
//	Cont series
//
//-----------------------------------------------------------------------------
#include "cont.hpp"
//-----------------------------------------------------------------------------
std::vector<mglSegment> MGL_EXPORT mgl_get_lines(mreal val, HCDT a, HCDT x, HCDT y, HCDT z, long ak)
{
	long n=a->GetNx(), m=a->GetNy();
	std::vector<mglSegment> lines;
	// first add all possible lines
	for(long j=0;j<m-1;j++)	for(long i=0;i<n-1;i++)
	{
		mreal v1=a->v(i,j,ak),v2=a->v(i+1,j,ak),v3=a->v(i,j+1,ak),v4=a->v(i+1,j+1,ak);
		mreal dl=mgl_d(val,v1,v3),dr=mgl_d(val,v2,v4),dp=mgl_d(val,v1,v2),dn=mgl_d(val,v3,v4);
		bool added=false;
		if(v1>val || v4>val)
		{
			mglSegment line;
			if(line.set(0,dl,dn,1,i,j,ak,x,y,z))	{	lines.push_back(line);	added=true;	}
			if(line.set(1,dr,dp,0,i,j,ak,x,y,z))	{	lines.push_back(line);	added=true;	}
		}
		else
		{
			mglSegment line;
			if(line.set(0,dl,dp,0,i,j,ak,x,y,z))	{	lines.push_back(line);	added=true;	}
			if(line.set(1,dr,dn,1,i,j,ak,x,y,z))	{	lines.push_back(line);	added=true;	}
		}
		if(!added)	// try to add any other variants
		{
			mglSegment line;
			if(line.set(0,dl,1,dr,i,j,ak,x,y,z))		lines.push_back(line);
			else if(line.set(dp,0,dn,1,i,j,ak,x,y,z))	lines.push_back(line);
			else if(line.set(0,dl,dn,1,i,j,ak,x,y,z))	lines.push_back(line);
			else if(line.set(1,dr,dp,0,i,j,ak,x,y,z))	lines.push_back(line);
			else if(line.set(0,dl,dp,0,i,j,ak,x,y,z))	lines.push_back(line);
			else if(line.set(1,dr,dn,1,i,j,ak,x,y,z))	lines.push_back(line);
		}
	}
	return lines;
}
//-----------------------------------------------------------------------------
std::vector<mglSegment> MGL_EXPORT mgl_get_curvs(const mglPoint &Min, const mglPoint &Max, std::vector<mglSegment> lines)
{
	long n = lines.size(), m = n;
	const long nsl=(n>0 && n*n>100)?sqrt(double(n)):10;
	mreal dxsl = nsl/((Max.x-Min.x)*MGL_FEPSILON), x0 = Min.x;
	mreal dysl = nsl/((Max.y-Min.y)*MGL_FEPSILON), y0 = Min.y;
	std::vector<long> *xsl, *ysl;
	xsl = new std::vector<long>[nsl+1];
	ysl = new std::vector<long>[nsl+1];
	for(long i=0;i<n;i++)	// group lines by position of its x-coor
	{
		long i1 = (lines[i].p1.x-x0)*dxsl, i2 = (lines[i].p2.x-x0)*dxsl;
		if(i1<0)	i1=0;
		if(i1>nsl)	i1=nsl;
		if(i2<0)	i2=0;
		if(i2>nsl)	i2=nsl;
		if(i1==i2 && i1*(i1-nsl)<=0)	xsl[i1].push_back(i);
		else
		{
			if(i1*(i1-nsl)<=0)	xsl[i1].push_back(i);
			if(i2*(i2-nsl)<=0)	xsl[i2].push_back(i);
		}
		i1 = (lines[i].p1.y-y0)*dysl;
		i2 = (lines[i].p2.y-y0)*dysl;
		if(i1<0)	i1=0;
		if(i1>nsl)	i1=nsl;
		if(i2<0)	i2=0;
		if(i2>nsl)	i2=nsl;
		if(i1==i2 && i1*(i1-nsl)<=0)	ysl[i1].push_back(i);
		else
		{
			if(i1*(i1-nsl)<=0)	ysl[i1].push_back(i);
			if(i2*(i2-nsl)<=0)	ysl[i2].push_back(i);
		}
	}
	size_t xm=0,ym=0;
	for(long i=0;i<=nsl;i++)
	{
		if(xm<xsl[i].size())	xm=xsl[i].size();
		if(ym<ysl[i].size())	ym=ysl[i].size();
	}
	std::vector<mglSegment> curvs;
	char *used = new char[n];	memset(used,0,n);
	// create curves from lines
	while(m>0)	// NOTE! This algorithm can be *very* slow!!!
	{
		mglSegment curv;
		bool added = false;
		for(long i=0;i<n;i++)	if(!used[i])	// find any first line segment
		{
			curv.before(lines[i].p1);
			curv.after(lines[i].p2);
			used[i]=1;	m--;
			added=true;	break;
		}
		while(added && m>0)
		{
			added = false;
			long i1, i2;
			if(xm<=ym)
			{	i1 = (curv.p1.x-x0)*dxsl;	i2 = (curv.p2.x-x0)*dxsl;	}
			else
			{	i1 = (curv.p1.y-y0)*dysl;	i2 = (curv.p2.y-y0)*dysl;	}
			if(i1<0)	i1=0;
			if(i1>nsl)	i1=nsl;
			if(i2<0)	i2=0;
			if(i2>nsl)	i2=nsl;
			const std::vector<long> &isl1=(xm<=ym)?xsl[i1]:ysl[i1];
			for(size_t i=0;i<isl1.size();i++)	// first find continuation of first point
			{
				long ii = isl1[i];
				const mglSegment &l=lines[ii];
				if(used[ii])	continue;
				if(l.p1==curv.p1)		{	curv.before(l.p2);	used[ii]=1;	m--;	added=true;	break;	}
				else if(l.p2==curv.p1)	{	curv.before(l.p1);	used[ii]=1;	m--;	added=true;	break;	}
			}
			const std::vector<long> &isl2=(xm<=ym)?xsl[i2]:ysl[i2];
			if(m>0)	for(size_t i=0;i<isl2.size();i++)	// now the same for second point
			{
				long ii = isl2[i];
				const mglSegment &l=lines[ii];
				if(used[ii])	continue;
				if(l.p1==curv.p2)		{	curv.after(l.p2);	used[ii]=1;	m--;	added=true;	break;	}
				else if(l.p2==curv.p2)	{	curv.after(l.p1);	used[ii]=1;	m--;	added=true;	break;	}
			}
		}
		curvs.push_back(curv);
	}
	delete []used;	delete []xsl;	delete []ysl;
	return curvs;
}
std::vector<mglSegment> MGL_EXPORT mgl_get_curvs(HMGL gr, std::vector<mglSegment> lines)
{	return mgl_get_curvs(gr->Min, gr->Max, lines);	}
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_draw_curvs(HMGL gr, mreal val, mreal c, int text, const std::vector<mglSegment> &curvs)
{
	long pc=0;
	for(size_t i=0;i<curvs.size();i++)	pc += curvs[i].pp.size();
	gr->Reserve(pc);
	// fill arguments for other functions
	long *ff = new long[pc], *nn = new long[pc], m=0;
	for(size_t i=0;i<curvs.size();i++)
	{
		const std::list<mglPoint> &pp=curvs[i].pp;
		for(std::list<mglPoint>::const_iterator it=pp.begin(); it != pp.end(); ++it)
		{
			ff[m] = gr->AddPnt(*it, c);
			nn[m] = m+1;	m++;
		}
		nn[m-1]=-1;
	}
	if(text && pc>1)
	{
		wchar_t wcs[64];
		mglprintf(wcs,64,L"%4.3g",val);
		mreal del = 2*gr->TextWidth(wcs,"",-0.5);
		// find width and height of drawing area
		mreal ar=gr->GetRatio(), w=gr->GetWidth(), h = gr->GetHeight();
		ar = (ar>1?1/ar:1)*gr->FontFactor();
		if(del<ar/2)	del = ar/2;

		long m=long(2*w/del)+3, n=long(2*h/del)+3;
		long *oo=new long[n*m];
		mreal *rr=new mreal[n*m];
		for(long i=0;i<n*m;i++)	{	oo[i]=-1;	rr[i]=del*del;	}
		int ii1 = (1664525*pc+1013904223)&0xffff, ii2 = (1664525*ii1+1013904223)&0xffff;
		mreal x0 = (del*ii1)/0xffff, y0 = (del*ii2)/0xffff;
		for(long k=0;k<pc;k++)	// print label several times if possible
		{
			if(nn[k]<0)	continue;
			const mglPoint t = gr->GetPntP(ff[k]);
			mreal tx = t.x+x0, ty = t.y+y0;		// quasi-random shift
			long i = long(tx/del);	tx -= i*del;
			long j = long(ty/del);	ty -= j*del;
			if(i>=0 && i<m && j>=0 && j<n)
			{
				tx = tx*tx+ty*ty;	i += m*j;
				if(rr[i]>tx)	{	rr[i]=tx;	oo[i]=k;	}
			}
		}
		for(long i=0;i<n*m;i++)	if(oo[i]>=0)
			mgl_string_curve(gr,oo[i],pc,ff,nn,wcs,text==1?"t:C":"T:C",-0.5);
		delete []oo;	delete []rr;
	}
	for(long i=0;i<pc;i++)	if(nn[i]>=0)	gr->line_plot(ff[i], ff[nn[i]]);
	delete []nn;	delete []ff;
}
//-----------------------------------------------------------------------------
HMDT mgl_data_conts(mreal val, HCDT dat)
{
	mglPoint Min(0,0,0), Max(1,1,1);
	mglDataV x(dat->GetNx(),dat->GetNy(),dat->GetNz(),0,1,'x');
	mglDataV y(dat->GetNx(),dat->GetNy(),dat->GetNz(),0,1,'y');
	mglDataV z(dat->GetNx(),dat->GetNy(),dat->GetNz(),0,1,'z');
	std::vector<mglSegment> curvs = mgl_get_curvs(Min,Max,mgl_get_lines(val,dat,&x,&y,&z,0));
	long pc=curvs.size(), m=0;
	if(pc==0)	return NULL;
	for(size_t i=0;i<curvs.size();i++)	pc += curvs[i].pp.size();
	// fill arguments for other functions
	HMDT res = new mglData(3,pc);
	for(size_t i=0;i<curvs.size();i++)
	{
		const std::list<mglPoint> &pp=curvs[i].pp;
		for(std::list<mglPoint>::const_iterator it=pp.begin(); it != pp.end(); ++it)
		{
			long i0 = 3*m;
			res->a[i0] = (*it).x;
			res->a[1+i0] = (*it).y;
			res->a[2+i0] = (*it).z;
			m++;
		}
		long i0 = 3*m;	m++;
		res->a[i0] = res->a[1+i0] = res->a[2+i0] = NAN;
	}
	return res;
}
//-----------------------------------------------------------------------------
// NOTE! All data MUST have the same size! Only first slice is used!
void MGL_EXPORT mgl_cont_gen(HMGL gr, mreal val, HCDT a, HCDT x, HCDT y, HCDT z, mreal c, int text,long ak)
{
	long n=a->GetNx(), m=a->GetNy();
	if(n<2 || m<2 || x->GetNx()*x->GetNy()!=n*m || y->GetNx()*y->GetNy()!=n*m || z->GetNx()*z->GetNy()!=n*m)
	{	gr->SetWarn(mglWarnDim,"ContGen");	return;	}

	mgl_draw_curvs(gr,val,c,text,mgl_get_curvs(gr,mgl_get_lines(val,a,x,y,z,ak)));
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_gen(HMGL gr, double val, HCDT a, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,a,"ContGen"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContGen",cgid++);

	int text=0;
	if(mglchr(sch,'t'))	text=1;
	if(mglchr(sch,'T'))	text=2;
	gr->SetPenPal(sch);
	mgl_cont_gen(gr,val,a,x,y,z,gr->CDef,text,0);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_xy_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Cont"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Cont",cgid++);

	int text=0;
	if(mglchr(sch,'t'))	text=1;
	if(mglchr(sch,'T'))	text=2;
	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	long s=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx();i++)	for(long j=0;j<z->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		mreal v0 = v->v(i), z0 = fixed ? gr->Min.z : v0;
		if(z->GetNz()>1)
			z0 = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(j)/(z->GetNz()-1);
		mglDataV zz(n, m);	zz.Fill(z0,z0);
		mgl_cont_gen(gr,v0,z,x,y,&zz,gr->GetC(s,v0),text,j);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_val(HMGL gr, HCDT v, HCDT z, const char *sch, const char *opt)
{
	long n = z->GetNx(), m = z->GetNy();
	if(m<2 || n<2)	{	gr->SetWarn(mglWarnLow,"Cont");	return;	}
	gr->SaveState(opt);
	mglDataV x(n, m), y(n, m);
	x.Fill(gr->Min.x,gr->Max.x,'x');
	y.Fill(gr->Min.y,gr->Max.y,'y');
	mgl_cont_xy_val(gr,v,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
#define norm(x,y)	((x)*(x)+(y)*(y))
std::vector<mreal> MGL_NO_EXPORT mgl_find_saddle_val(HCDT z)
{
	long nx=z->GetNx(), ny=z->GetNy();
	std::vector<mreal> v;
	for(long i=1;i<nx-1;i++)
	{
		mreal dd=z->vthr(i), x1=z->vthr(i+1), x2=z->vthr(i-1), dp=z->vthr(i+nx);
		if(dd<=x1 && dd<=x2 && dd>=dp)	v.push_back(z->vthr(i));
		if(dd>=x1 && dd>=x2 && dd<=dp)	v.push_back(z->vthr(i));
		long i0 = i+nx*(ny-1);
		dd=z->vthr(i0);	x1=z->vthr(i0+1);	x2=z->vthr(i0-1);	dp=z->vthr(i0-nx);
		if(dd<=x1 && dd<=x2 && dd>=dp)	v.push_back(z->vthr(i0));
		if(dd>=x1 && dd>=x2 && dd<=dp)	v.push_back(z->vthr(i0));
	}
	for(long j=1;j<ny-1;j++)
	{
		long i0 = nx*j;
		mreal dd=z->vthr(i0), dp=z->vthr(i0+1), y1=z->vthr(i0+nx), y2=z->vthr(i0-nx);
		if(dd<=dp && dd>=y1 && dd>=y2)	v.push_back(z->vthr(i0));
		if(dd>=dp && dd<=y1 && dd<=y2)	v.push_back(z->vthr(i0));
		i0 = nx*j+nx-1;
		dd=z->vthr(i0);	dp=z->vthr(i0-1);	y1=z->vthr(i0+nx);	y2=z->vthr(i0-nx);
		if(dd<=dp && dd>=y1 && dd>=y2)	v.push_back(z->vthr(i0));
		if(dd>=dp && dd<=y1 && dd<=y2)	v.push_back(z->vthr(i0));
	}
	for(long j=1;j<ny-1;j++)	for(long i=1;i<nx-1;i++)
	{
		long i0 = i+nx*j;	bool ok=false;
		mreal dd=z->vthr(i0),x1=z->vthr(i0+1),x2=z->vthr(i0-1),y1=z->vthr(i0+nx),y2=z->vthr(i0-nx);
		if(dd<=x1 && dd<=x2 && dd>=y1 && dd>=y2)	ok=true;
		if(dd>=x1 && dd>=x2 && dd<=y1 && dd<=y2)	ok=true;
		x1=z->vthr(i0+1+nx);	x2=z->vthr(i0-1-nx);
		y1=z->vthr(i0-1+nx);	y2=z->vthr(i0+1-nx);
		if(dd<=x1 && dd<=x2 && dd>=y1 && dd>=y2)	ok=true;
		if(dd>=x1 && dd>=x2 && dd<=y1 && dd<=y2)	ok=true;
		if(ok)	v.push_back(z->vthr(i0));
	}
	return v;
}
void MGL_EXPORT mgl_cont_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	if(mglchr(sch,'.'))
	{
		mglDataS v;	v.dat = mgl_find_saddle_val(z);
		if(v.dat.size()>0)
		{
			std::sort( v.dat.begin(), v.dat.end() );
			v.dat.erase( std::unique( v.dat.begin(), v.dat.end() ), v.dat.end() );
			mgl_cont_xy_val(gr,&v,x,y,z,sch,0);
		}
		else	gr->SetWarn(mglWarnCnt,"Cont");
	}
	else
	{
		long Num = mgl_isnan(r)?7:long(r+0.5);
		if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
		mglData v(Num);
		for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
		mgl_cont_xy_val(gr,&v,x,y,z,sch,0);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	if(mglchr(sch,'.'))
	{
		mglDataS v;	v.dat = mgl_find_saddle_val(z);
		if(v.dat.size()>0)
		{
			std::sort( v.dat.begin(), v.dat.end() );
			v.dat.erase( std::unique( v.dat.begin(), v.dat.end() ), v.dat.end() );
			mgl_cont_val(gr,&v,z,sch,0);
		}
		else	gr->SetWarn(mglWarnCnt,"Cont");
	}
	else
	{
		long Num = mgl_isnan(r)?7:long(r+0.5);
		if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
		mglData v(Num);
		for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
		mgl_cont_val(gr,&v,z,sch,0);
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_xy_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_cont_xy_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(a), s, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_cont_val(_GR_, _DA_(v), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_cont_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_cont(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	ContF series
//
//-----------------------------------------------------------------------------
long static mgl_add_pnt(HMGL gr, mreal d, HCDT x, HCDT y, HCDT z, long i1, long j1, long i2, long j2, mreal c, bool edge)
{
	long res=-1;
	if(edge || (d>0 && d<1))
	{
		mglPoint p(x->v(i1,j1)*(1-d)+x->v(i2,j2)*d,
					y->v(i1,j1)*(1-d)+y->v(i2,j2)*d,
					z->v(i1,j1)*(1-d)+z->v(i2,j2)*d);
		mglPoint u(x->dvx(i1,j1)*(1-d)+x->dvx(i2,j2)*d,
					y->dvx(i1,j1)*(1-d)+y->dvx(i2,j2)*d,
					z->dvx(i1,j1)*(1-d)+z->dvx(i2,j2)*d);
		mglPoint v(x->dvy(i1,j1)*(1-d)+x->dvy(i2,j2)*d,
					y->dvy(i1,j1)*(1-d)+y->dvy(i2,j2)*d,
					z->dvy(i1,j1)*(1-d)+z->dvy(i2,j2)*d);
		res = gr->AddPnt(p,c,u^v);
	}
	return res;
}
//-----------------------------------------------------------------------------
void static mgl_add_range(HMGL gr, HCDT a, HCDT x, HCDT y, HCDT z, long i1, long j1, long di, long dj, mreal c, long &u1, long &u2, long ak, mreal v1, mreal v2)
{
	long i2=i1+di, j2=j1+dj;
	mreal f1 = a->v(i1,j1,ak),	f2 = a->v(i2,j2,ak), d1, d2;
	d1 = mgl_d(v1,f1,f2);
	u1 = mgl_add_pnt(gr,d1,x,y,z,i1,j1,i2,j2,c,false);
	d2 = mgl_d(v2,f1,f2);
	u2 = mgl_add_pnt(gr,d2,x,y,z,i1,j1,i2,j2,c,false);
	if(d1>d2)	{	j2=u1;	u1=u2;	u2=j2;	}
	if(u1<0)	{	u1=u2;	u2=-1;	}
}
//-----------------------------------------------------------------------------
void static mgl_add_edges(HMGL gr, HCDT a, HCDT x, HCDT y, HCDT z, long i1, long j1, long di, long dj, mreal c, long &u1, long &u2, long ak, mreal v1, mreal v2)
{
	long i2=i1+di, j2=j1+dj;
	u1 = u2 = -1;
	mreal f1 = a->v(i1,j1,ak),	f2 = a->v(i2,j2,ak);
	if(f1<=v2 && f1>=v1)	u1 = mgl_add_pnt(gr,0,x,y,z,i1,j1,i2,j2,c,true);
	if(f2<=v2 && f2>=v1)	u2 = mgl_add_pnt(gr,1,x,y,z,i1,j1,i2,j2,c,true);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_gen(HMGL gr, mreal v1, mreal v2, HCDT a, HCDT x, HCDT y, HCDT z, mreal c, long ak)
{
	long n=a->GetNx(), m=a->GetNy();
	if(n<2 || m<2 || x->GetNx()*x->GetNy()!=n*m || y->GetNx()*y->GetNy()!=n*m || z->GetNx()*z->GetNy()!=n*m)
	{	gr->SetWarn(mglWarnDim,"ContFGen");	return;	}

	gr->Reserve(8*n*m);
	long *kk = new long[4*n], l1,l2, r1,r2, t1,t2, u1,u2, b1,b2, d1,d2, p[8],num;
	memset(kk,-1,2*n*sizeof(long));
	for(long i=0;i<n-1;i++)	// add intersection points for first line
	{
		mgl_add_range(gr,a,x,y,z, i,0,1,0, c,u1,u2, ak,v1,v2);
		kk[4*i]=u1;		kk[4*i+1]=u2;
		mgl_add_edges(gr,a,x,y,z, i,0,1,0, c,d1,d2, ak,v1,v2);
		kk[4*i+2]=d1;		kk[4*i+3]=d2;
	}
	for(long j=1;j<m;j++)	// add intersection points
	{
		mgl_add_range(gr,a,x,y,z, 0,j-1,0,1, c,r1,r2, ak,v1,v2);
		for(long i=0;i<n-1;i++)
		{
			l1 = r1;		l2 = r2;	num=0;
			t1 = kk[4*i];	t2 = kk[4*i+1];
			b1 = kk[4*i+2];	b2 = kk[4*i+3];
			// right edge
			mgl_add_range(gr,a,x,y,z, i+1,j-1,0,1, c,r1,r2, ak,v1,v2);
			// top edge
			mgl_add_range(gr,a,x,y,z, i,j,1,0, c,u1,u2, ak,v1,v2);
			kk[4*i]=u1;		kk[4*i+1]=u2;
			mgl_add_edges(gr,a,x,y,z, i,j,1,0, c,d1,d2, ak,v1,v2);
			kk[4*i+2]=d1;	kk[4*i+3]=d2;
			// collect points
			if(b1>=0)	p[num++] = b1;
			if(t1>=0)	p[num++] = t1;
			if(t2>=0)	p[num++] = t2;
			if(b2>=0)	p[num++] = b2;
			if(r1>=0)	p[num++] = r1;
			if(r2>=0)	p[num++] = r2;
			if(d2>=0)	p[num++] = d2;
			if(u2>=0)	p[num++] = u2;
			if(u1>=0)	p[num++] = u1;
			if(d1>=0)	p[num++] = d1;
			if(l2>=0)	p[num++] = l2;
			if(l1>=0)	p[num++] = l1;

			//	d1	u1	u2	d2
			//	l2			r2
			//	l1			r1
			//	b1	t1	t2	b2

			// draw it
			bool b1d2 = a->v(i+1,j,ak)>v2 && a->v(i,j-1,ak)>v2;
			bool b2d1 = a->v(i,j,ak)>v2 && a->v(i+1,j-1,ak)>v2;
//			mreal vv = a->linearD(i+0.5,j-0.5,ak,0,0,0);
//			vv = (vv-v1)*(vv-v2);
			if(num<3)	continue;
			if(num==4)	gr->quad_plot(p[0],p[1],p[3],p[2]);
			else if(num==3)	gr->trig_plot(p[0],p[1],p[2]);
			else if(num==5)
			{
				gr->quad_plot(p[0],p[1],p[3],p[2]);
				gr->trig_plot(p[0],p[3],p[4]);
			}
			else if(num==6)
			{
				if(b1>=0 && b2>=0)
				{
					gr->quad_plot(b1,b2,l1,r1);
					gr->quad_plot(l1,r1,u1,u2);
				}
				else if(d1>=0 && d2>=0)
				{
					gr->quad_plot(d1,d2,l1,r1);
					gr->quad_plot(l1,r1,t1,t2);
				}
				else if(b1>=0 && d2>=0)
				{
					if(b2d1)
					{	gr->trig_plot(b1,t1,l1);	gr->trig_plot(r1,u1,d2);	}
					else
					{	gr->quad_plot(b1,t1,l1,r1);	gr->quad_plot(l1,r1,u1,d2);	}
				}
				else if(d1>=0 && b2>=0)
				{
					if(b1d2)
					{	gr->trig_plot(t1,b2,r1);	gr->trig_plot(l1,d1,u1);	}
					else
					{	gr->quad_plot(t1,b2,l1,r1);	gr->quad_plot(l1,r1,d1,u1);	}
				}
				else if(b1>=0 && d1>=0)
				{
					gr->quad_plot(b1,d1,t1,u1);
					gr->quad_plot(t1,u1,r1,r2);
				}
				else if(d2>=0 && b2>=0)
				{
					gr->quad_plot(d2,b2,u1,t1);
					gr->quad_plot(t1,u1,l1,l2);
				}
			}
			else if(num==7)
			{
				if(b1>=0)
				{
					gr->trig_plot(b1,l1,t1);	gr->quad_plot(r1,r2,u1,u2);
					if(!b2d1)	gr->quad_plot(l1,t1,u1,r1);
				}
				else if(b2>=0)
				{
					gr->trig_plot(b2,r1,t1);	gr->quad_plot(l1,l2,u2,u1);
					if(!b1d2)	gr->quad_plot(r1,t1,u2,l1);
				}
				else if(d2>=0)
				{
					gr->trig_plot(d2,r1,u1);	gr->quad_plot(l1,l2,t1,t2);
					if(!b2d1)	gr->quad_plot(r1,u1,t2,l2);
				}
				else if(d1>=0)
				{
					gr->trig_plot(d1,l1,u1);	gr->quad_plot(r1,r2,t2,t1);
					if(!b1d2)	gr->quad_plot(l1,u1,t1,r2);
				}
			}
			else if(num==8)
			{
				if(b2d1)
				{	if(l2<0)	{	l2=l1;	l1=b1;	}
					if(r2<0)	r2=d2;
					if(t2<0)	{	t2=t1;	t1=b1;	}
					if(u2<0)	u2=d2;
					gr->quad_plot(r1,r2,u1,u2);	gr->quad_plot(l1,l2,t1,t2);	}
				else
				{	if(l2<0)	l2=d1;
					if(r2<0)	{	r2=r1;	r1=b2;	}
					if(t2<0)	t2=b2;
					if(u2<0)	{	u2=u1;	u1=d1;	}
					gr->quad_plot(r1,r2,t2,t1);	gr->quad_plot(l1,l2,u2,u1);	}
			}
		}
	}
	delete []kk;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_gen(HMGL gr, double v1, mreal v2, HCDT a, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	if(mgl_check_dim2(gr,x,y,z,a,"ContFGen"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContFGen",cgid++);

	gr->SetPenPal(sch);
	mgl_contf_gen(gr,v1,v2,a,x,y,z,gr->CDef,0);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_xy_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"ContF"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContF",cgid++);
	long s=gr->AddTexture(sch);

	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx()-1;i++)	for(long j=0;j<z->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		mreal v0 = v->v(i), z0 = fixed ? gr->Min.z : v0;
		if(z->GetNz()>1)
			z0 = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(j)/(z->GetNz()-1);
		mglDataV zz(n, m);	zz.Fill(z0,z0);
		mgl_contf_gen(gr,v0,v->v(i+1),z,x,y,&zz,gr->GetC(s,v0),j);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_val(HMGL gr, HCDT v, HCDT z, const char *sch, const char *opt)
{
	long n = z->GetNx(), m = z->GetNy();
	if(n<2 || m<2)	{	gr->SetWarn(mglWarnLow,"Cont");	return;	}
	gr->SaveState(opt);
	mglDataV x(n, m), y(n, m);
	x.Fill(gr->Min.x,gr->Max.x,'x');
	y.Fill(gr->Min.y,gr->Max.y,'y');
	mgl_contf_xy_val(gr,v,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
	mglDataV v(Num+2);	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contf_xy_val(gr,&v,x,y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
	mglDataV v(Num+2);	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contf_val(gr,&v,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_xy_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contf_xy_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contf_val(_GR_, _DA_(v), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contf_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contf(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	ContP series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contp_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,a,"Cont"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Cont",cgid++);

	int text=0;
	if(mglchr(sch,'t'))	text=1;
	if(mglchr(sch,'T'))	text=2;
	bool fill = mglchr(sch,'f');
	long s=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx();i++)	for(long j=0;j<a->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		if(fill)
			mgl_contf_gen(gr,v->v(i),v->v(i+1),a,x,y,z,gr->GetC(s,v->v(i)),j);
		else
			mgl_cont_gen(gr,v->v(i),a,x,y,z,gr->GetC(s,v->v(i)),text,j);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contp(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_contp_val(gr,&v,x,y,z,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contp_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contp_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contp_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contp(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	ContD series
//
//-----------------------------------------------------------------------------
int static mgl_get_ncol(const char *sch, char *res)
{
	long j=0;
	if(sch)	for(long i=0;sch[i]&&sch[i]!=':';i++)	if(strchr(MGL_COLORS,sch[i]))
	{	if(res)	res[j]=sch[i];	j++;	}
	return j?j:strlen(MGL_DEF_PAL);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_xy_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long j=0,n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"ContD"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContD",cgid++);

	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	if(sch)	for(long i=0;sch[i];i++)	if(strchr(MGL_COLORS,sch[i]))	j++;
	if(j==0)	sch = MGL_DEF_PAL;
	long s = gr->AddTexture(sch,1);
	int nc = gr->GetNumPal(s*256);
	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
	mreal dc = nc>1 ? 1/(MGL_FEPSILON*(nc-1)) : 0;
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx()-1;i++)	for(long j=0;j<z->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		mreal v0 = v->v(i), z0 = fixed ? gr->Min.z : v0;
		if(z->GetNz()>1)
			z0 = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(j)/(z->GetNz()-1);
		mglDataV zz(n, m);	zz.Fill(z0,z0);
		mgl_contf_gen(gr,v0,v->v(i+1),z,x,y,&zz,s+(i%nc)*dc,j);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_val(HMGL gr, HCDT v, HCDT z, const char *sch, const char *opt)
{
	long n = z->GetNx(), m = z->GetNy();
	if(n<2 || m<2)	{	gr->SetWarn(mglWarnLow,"ContD");	return;	}
	gr->SaveState(opt);
	mglDataV x(n, m), y(n, m);
	x.Fill(gr->Min.x,gr->Max.x,'x');
	y.Fill(gr->Min.y,gr->Max.y,'y');
	mgl_contd_xy_val(gr,v,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV v(mgl_get_ncol(sch,0)+1);
	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contd_xy_val(gr,&v,x,y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	gr->SaveState(opt);
	mglDataV v(mgl_get_ncol(sch,0)+1);
	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contd_val(gr,&v,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_xy_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contd_xy_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contd_val(_GR_, _DA_(v), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contd_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contd_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contd(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	ContV series
//
//-----------------------------------------------------------------------------
// NOTE! All data MUST have the same size! Only first slice is used!
void MGL_EXPORT mgl_contv_gen(HMGL gr, mreal val, mreal dval, HCDT a, HCDT x, HCDT y, HCDT z, mreal c, long ak)
{
	long n=a->GetNx(), m=a->GetNy();
	if(n<2 || m<2 || x->GetNx()*x->GetNy()!=n*m || y->GetNx()*y->GetNy()!=n*m || z->GetNx()*z->GetNy()!=n*m)
	{	gr->SetWarn(mglWarnDim,"ContGen");	return;	}

	const std::vector<mglSegment> curvs = mgl_get_curvs(gr,mgl_get_lines(val,a,x,y,z,ak));
	for(size_t i=0;i<curvs.size();i++)
	{
		const std::list<mglPoint> &pp=curvs[i].pp;
		long f2=-1,g2=-1;
		for(std::list<mglPoint>::const_iterator it=pp.begin(); it != pp.end(); ++it)
		{
			mglPoint p=*it,q(p.y,-p.x);
			long f1 = f2;	f2 = gr->AddPnt(p,c,q);	p.z+=dval;
			long g1 = g2;	g2 = gr->AddPnt(p,c,q);
			gr->quad_plot(f1,g1,f2,g2);
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_xy_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"ContV"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContV",cgid++);
	bool fixed=(mglchr(sch,'_')) || (gr->Min.z==gr->Max.z);
	long s=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx();i++)	for(long j=0;j<z->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		mreal v0 = v->v(i), z0 = fixed ? gr->Min.z : v0;
		if(z->GetNz()>1)	z0 = gr->Min.z+(gr->Max.z-gr->Min.z)*mreal(j)/(z->GetNz()-1);
		mglDataV zz(n, m);	zz.Fill(z0,z0);
		mreal dv = (gr->Max.c-gr->Min.c)/8;
		if(i>0)	dv = v->v(i-1)-v->v(i);
		else if(i<v->GetNx()-1)	dv = v->v(i)-v->v(i+1);
		if(fixed)	dv=-dv;
		mgl_contv_gen(gr,v0,dv,z,x,y,&zz,gr->GetC(s,v0),j);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_val(HMGL gr, HCDT v, HCDT z, const char *sch, const char *opt)
{
	long n = z->GetNx(), m = z->GetNy();
	if(n<2 || m<2)	{	gr->SetWarn(mglWarnLow,"Cont");	return;	}
	gr->SaveState(opt);
	mglDataV x(n, m), y(n, m);
	x.Fill(gr->Min.x,gr->Max.x,'x');
	y.Fill(gr->Min.y,gr->Max.y,'y');
	mgl_contv_xy_val(gr,v,&x,&y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_xy(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_contv_xy_val(gr,&v,x,y,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv(HMGL gr, HCDT z, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_contv_val(gr,&v,z,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_xy_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contv_xy_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(a), s, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contv_val(_GR_, _DA_(v), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contv_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contv_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_contv(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Cont3 series
//
//-----------------------------------------------------------------------------
struct _mgl_slice	{	mglData x,y,z,a;	};
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_get_slice(_mgl_slice &s, HCDT x, HCDT y, HCDT z, HCDT a, char dir, mreal d, bool both)
{
	long n=a->GetNx(),m=a->GetNy(),l=a->GetNz(), nx=1,ny=1,p;

	if(dir=='x')	{	nx = m;	ny = l;	if(d<0)	d = n/2.;	}
	if(dir=='y')	{	nx = n;	ny = l;	if(d<0)	d = m/2.;	}
	if(dir=='z')	{	nx = n;	ny = m;	if(d<0)	d = l/2.;	}
	s.x.Create(nx,ny);	s.y.Create(nx,ny);
	s.z.Create(nx,ny);	s.a.Create(nx,ny);
	p = long(d);	d -= p;
	if(dir=='x' && p>=n-1)	{	d+=p-n+2;	p=n-2;	}
	if(dir=='y' && p>=m-1)	{	d+=p-m+2.;	p=m-2;	}
	if(dir=='z' && p>=l-1)	{	d+=p-l+2;	p=l-2;	}
	mreal v;

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
				s.a.a[i0] = a->v(p,i,j)*(1-d) + a->v(p+1,i,j)*d;
			}
		if(dir=='y')
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;
				s.x.a[i0] = x->v(i,p,j)*(1-d) + x->v(i,p+1,j)*d;
				s.y.a[i0] = y->v(i,p,j)*(1-d) + y->v(i,p+1,j)*d;
				s.z.a[i0] = z->v(i,p,j)*(1-d) + z->v(i,p+1,j)*d;
				s.a.a[i0] = a->v(i,p,j)*(1-d) + a->v(i,p+1,j)*d;
			}
		if(dir=='z')
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;
				s.x.a[i0] = x->v(i,j,p)*(1-d) + x->v(i,j,p+1)*d;
				s.y.a[i0] = y->v(i,j,p)*(1-d) + y->v(i,j,p+1)*d;
				s.z.a[i0] = z->v(i,j,p)*(1-d) + z->v(i,j,p+1)*d;
				s.a.a[i0] = a->v(i,j,p)*(1-d) + a->v(i,j,p+1)*d;
			}
	}
	else	// x, y, z -- vectors
	{
		if(dir=='x')
		{
			v = x->v(p)*(1-d)+x->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.x.a[i0] = v;
				s.y.a[i0] = y->v(i);	s.z.a[i0] = z->v(j);
				s.a.a[i0] = a->v(p,i,j)*(1-d) + a->v(p+1,i,j)*d;
			}
		}
		if(dir=='y')
		{
			v = y->v(p)*(1-d)+y->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.y.a[i0] = v;
				s.x.a[i0] = x->v(i);	s.z.a[i0] = z->v(j);
				s.a.a[i0] = a->v(i,p,j)*(1-d) + a->v(i,p+1,j)*d;
			}
		}
		if(dir=='z')
		{
			v = z->v(p)*(1-d)+z->v(p+1)*d;
#pragma omp parallel for collapse(2)
			for(long j=0;j<ny;j++)	for(long i=0;i<nx;i++)
			{
				long i0 = i+nx*j;	s.z.a[i0] = v;
				s.x.a[i0] = x->v(i);	s.y.a[i0] = y->v(j);
				s.a.a[i0] = a->v(i,j,p)*(1-d) + a->v(i,j,p+1)*d;
			}
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_xyz_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	bool both = mgl_isboth(x,y,z,a);
	if(mgl_check_dim3(gr,both,x,y,z,a,0,"Cont3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Cont3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	int text=0;
	if(mglchr(sch,'t'))	text=1;
	if(mglchr(sch,'T'))	text=2;
	long ss=gr->AddTexture(sch);
	gr->SetPenPal(sch);

	_mgl_slice s;
	mgl_get_slice(s,x,y,z,a,dir,sVal,both);
#pragma omp parallel for
	for(long i=0;i<v->GetNx();i++)
	{
		mreal v0 = v->v(i);
		mgl_cont_gen(gr,v0,&s.a,&s.x,&s.y,&s.z,gr->GetC(ss,v0),text,0);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_val(HMGL gr, HCDT v, HCDT a, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_cont3_xyz_val(gr,v,&x,&y,&z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont3");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_cont3_xyz_val(gr,&v,x,y,z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3(HMGL gr, HCDT a, const char *sch, double sVal, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Cont3");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_cont3_val(gr,&v,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_xyz_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cont3_xyz_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cont3_val(_GR_, _DA_(v), _DA_(a), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cont3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_cont3_(uintptr_t *gr, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_cont3(_GR_, _DA_(a), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Dens3 series
//
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_surf_gen(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch);
void MGL_EXPORT mgl_dens3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	bool both = mgl_isboth(x,y,z,a);
	if(mgl_check_dim3(gr,both,x,y,z,a,0,"Dens3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Dens3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	_mgl_slice s;
	mgl_get_slice(s,x,y,z,a,dir,sVal,both);
	mgl_surf_gen(gr, &s.x,&s.y,&s.z,&s.a, 0, sch);
//	mgl_surfc_xy(gr,&s.x,&s.y,&s.z,&s.a,sch,0);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens3(HMGL gr, HCDT a, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_dens3_xyz(gr,&x,&y,&z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_dens3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_dens3_(uintptr_t *gr, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_dens3(_GR_, _DA_(a), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Grid3 series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	bool both = mgl_isboth(x,y,z,a);
	if(mgl_check_dim3(gr,both,x,y,z,a,0,"Grid3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Grid3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	_mgl_slice s;
	mgl_get_slice(s,x,y,z,a,dir,sVal,both);
	mgl_mesh_xy(gr,&s.x,&s.y,&s.z,sch,0);
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid3(HMGL gr, HCDT a, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()), z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_grid3_xyz(gr,&x,&y,&z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_grid3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_grid3_(uintptr_t *gr, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_grid3(_GR_, _DA_(a), s, *sVal, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	ContF3 series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_xyz_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	bool both = mgl_isboth(x,y,z,a);
	if(mgl_check_dim3(gr,both,x,y,z,a,0,"ContF3"))	return;

	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("ContF3",cgid++);
	char dir='y';
	if(mglchr(sch,'x'))	dir='x';
	if(mglchr(sch,'z'))	dir='z';

	long ss=gr->AddTexture(sch);
	_mgl_slice s;
	mgl_get_slice(s,x,y,z,a,dir,sVal,both);
#pragma omp parallel for
	for(long i=0;i<v->GetNx()-1;i++)
	{
		mreal v0 = v->v(i);
		mgl_contf_gen(gr,v0,v->v(i+1),&s.a,&s.x,&s.y,&s.z,gr->GetC(ss,v0),0);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_val(HMGL gr, HCDT v, HCDT a, const char *sch, double sVal, const char *opt)
{
	gr->SaveState(opt);
	mglDataV x(a->GetNx()), y(a->GetNy()),z(a->GetNz());
	x.Fill(gr->Min.x,gr->Max.x);
	y.Fill(gr->Min.y,gr->Max.y);
	z.Fill(gr->Min.z,gr->Max.z);
	mgl_contf3_xyz_val(gr,v,&x,&y,&z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, double sVal, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"ContF3");	return;	}
	mglDataV v(Num+2);	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contf3_xyz_val(gr,&v,x,y,z,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3(HMGL gr, HCDT a, const char *sch, double sVal, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?7:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"ContF3");	return;	}
	mglDataV v(Num+2);	v.Fill(gr->Min.c, gr->Max.c);
	mgl_contf3_val(gr,&v,a,sch,sVal,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_xyz_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_contf3_xyz_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_contf3_val(_GR_, _DA_(v), _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_xyz_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_contf3_xyz(_GR_, _DA_(x), _DA_(y), _DA_(z), _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_contf3_(uintptr_t *gr, uintptr_t *a, const char *sch, mreal *sVal, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_contf3(_GR_, _DA_(a), s, *sVal, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//	Axial series
//
//-----------------------------------------------------------------------------
long MGL_LOCAL_PURE mgl_find_prev(long i, long pc, long *nn)
{
	for(long k=0;k<pc;k++)	if(nn[k]==i)	return k;
	return -1;
}
void static mgl_axial_plot(mglBase *gr,long pc, mglPoint *ff, long *nn,char dir,mreal cc,int wire)
{
	mglPoint a(0,0,1),b,c,q1,q2;
	if(dir=='x')	a.Set(1,0,0);
	if(dir=='y')	a.Set(0,1,0);
	b = !a;	c = a^b;

	gr->Reserve(pc*82);
	for(long i=0;i<pc;i++)
	{
		if(nn[i]<0)	continue;
		long k = mgl_find_prev(i,pc,nn);
		q1 = k<0 ? ff[nn[i]]-ff[i]  : (ff[nn[i]]-ff[k])*0.5;
		q2 = nn[nn[i]]<0 ? ff[nn[i]]-ff[i]  : (ff[nn[nn[i]]]-ff[i])*0.5;

		long kq = gr->AllocPnts(41*2);
#pragma omp parallel for
		for(long j=0;j<41;j++)
		{
			float co = mgl_cos[(j*18)%360], si = mgl_cos[(270+j*18)%360];
//			fi = j*M_PI/20;		si = sin(fi);	co = cos(fi);
			mglPoint p1 = a*ff[i].y + b*(si*ff[i].x) +  c*(co*ff[i].x);
			mglPoint p2 = a*ff[nn[i]].y + b*(si*ff[nn[i]].x) +  c*(co*ff[nn[i]].x);
			if(wire)
			{	gr->AddPntQ(kq+2*j,p1,cc);	gr->AddPntQ(kq+2*j+1,p2,cc);	}
			else
			{
				gr->AddPntQ(kq+2*j, p1,cc,(a*q1.y + b*(si*q1.x) +  c*(co*q1.x))^(b*co-c*si));
				gr->AddPntQ(kq+2*j+1,p2,cc,(a*q2.y + b*(si*q2.x) +  c*(co*q2.x))^(b*co-c*si));
			}
		}
		if(wire==1)
		{
			gr->line_plot(kq,kq+1);
			for(long j=1;j<41;j++)
			{
				gr->line_plot(kq+2*j,kq+2*j+1);		gr->line_plot(kq+2*j,kq+2*j-2);
				gr->line_plot(kq+2*j-1,kq+2*j+1);	gr->line_plot(kq+2*j-1,kq+2*j-2);
			}
		}
		else if(wire)	for(long j=0;j<41;j++)
		{	gr->mark_plot(kq+2*j,'.');	gr->mark_plot(kq+2*j+1,'.');	}
		else	for(long j=1;j<41;j++)
		{	long i0 = kq+2*j;	gr->quad_plot(i0-2,i0-1,i0,i0+1);	}
	}
}
//-----------------------------------------------------------------------------
// NOTE! All data MUST have the same size! Only first slice is used!
void MGL_EXPORT mgl_axial_gen(HMGL gr, mreal val, HCDT a, HCDT x, HCDT y, mreal c, char dir,long ak,int wire)
{
	long n=a->GetNx(), m=a->GetNy();
	if(n<2 || m<2 || x->GetNx()*x->GetNy()!=n*m || y->GetNx()*y->GetNy()!=n*m)
	{	gr->SetWarn(mglWarnDim,"ContGen");	return;	}

	mglPoint *kk = new mglPoint[2*n*m],*pp = new mglPoint[2*n*m],p;
	long pc=0;
	// Usually number of points is much smaller. So, there is no reservation.
	//	gr->Reserve(2*n*m);

	// add intersection point of isoline and X or Y axis
	const mglData *mx = dynamic_cast<const mglData *>(x);
	const mglData *my = dynamic_cast<const mglData *>(y);
	const mglData *ma = dynamic_cast<const mglData *>(a);
	if(mx&&my&&ma)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		long i0 = i+n*j;
		mreal d = (i<n-1)?mgl_d(val,ma->a[i0+n*m*ak],ma->a[i0+1+n*m*ak]):-1;
		if(d>=0 && d<1)
		{
			pp[pc].Set(mx->a[i0]*(1-d)+mx->a[i0+1]*d, my->a[i0]*(1-d)+my->a[i0+1]*d);
			kk[pc].Set(i+d,j);	pc++;
		}
		d = (j<m-1)?mgl_d(val,ma->a[i0+n*m*ak],ma->a[i0+n*m*ak+n]):-1;
		if(d>=0 && d<1)
		{
			pp[pc].Set(mx->a[i0]*(1-d)+mx->a[i0+n]*d, my->a[i0]*(1-d)+my->a[i0+n]*d);
			kk[pc].Set(i,j+d);	pc++;
		}
	}
	else	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		mreal va=a->v(i,j,ak),vx=x->v(i,j),vy=y->v(i,j);
		mreal d = (i<n-1)?mgl_d(val,va,a->v(i+1,j,ak)):-1;
		if(d>=0 && d<1)
		{
			pp[pc].Set(vx*(1-d)+x->v(i+1,j)*d, vy*(1-d)+y->v(i+1,j)*d);
			kk[pc].Set(i+d,j);	pc++;
		}
		d = (j<m-1)?mgl_d(val,va,a->v(i,j+1,ak)):-1;
		if(d>=0 && d<1)
		{
			pp[pc].Set(vx*(1-d)+x->v(i,j+1)*d, vy*(1-d)+y->v(i,j+1)*d);
			kk[pc].Set(i,j+d);	pc++;
		}
	}
	// deallocate arrays and finish if no point
	if(pc==0)	{	delete []kk;	delete []pp;	return;	}
	// allocate arrays for curve
	long *nn = new long[pc], *ff = new long[pc];
	for(long i=0;i<pc;i++)	nn[i] = ff[i] = -1;
	// connect points to line
	long j=-1;	// current point
	do{
		if(j>=0)
		{
			mreal kx = kk[j].x, ky = kk[j].y;	long i = -1;
			long i11 = long(kx+1e-5), i12 = long(kx-1e-5);
			long j11 = long(ky+1e-5), j12 = long(ky-1e-5);
			for(long k=0;k<pc;k++)	// find closest point in grid
			{
				if(k==j || k==ff[j] || ff[k]!=-1)	continue;	// point is marked
				long i21 = long(kk[k].x+1e-5), i22 = long(kk[k].x-1e-5);
				long j21 = long(kk[k].y+1e-5), j22 = long(kk[k].y-1e-5);
				// check if in the same cell
				bool cond = (i11==i21 || i11==i22 || i12==i21 || i12==i22) &&
				(j11==j21 || j11==j22 || j12==j21 || j12==j22);
				if(cond){	i=k;	break;	}
			}
			if(i<0)	j = -1;	// no free close points
			else			// mark the point
			{	nn[j] = i;	ff[i] = j;	j = nn[i]<0 ? i : -1;	}
		}
		if(j<0)
		{
			for(long k=0;k<pc;k++)	if(nn[k]==-1)	// first check edges
			{
				if(kk[k].x==0 || fabs(kk[k].x-n+1)<1e-5 || kk[k].y==0 || fabs(kk[k].y-m+1)<1e-5)
				{	nn[k]=-2;	j = k;	break;	}
			}
			if(j<0)	for(long k=0;k<pc;k++)	if(nn[k]==-1)	// or any points inside
			{	j = k;	nn[k]=-2;	break;	}
		}
	}while(j>=0);
	mgl_axial_plot(gr,pc,pp,nn,dir,c,wire);
	delete []kk;	delete []nn;	delete []ff;	delete []pp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_xy_val(HMGL gr, HCDT v, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt)
{
	long n=z->GetNx(),m=z->GetNy();
	if(mgl_check_dim2(gr,x,y,z,0,"Axial"))	return;
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Axial",cgid++);
	long s=gr->AddTexture(sch);
	char dir='y';
	if(mglchr(sch,'x'))	dir = 'x';
	if(mglchr(sch,'z'))	dir = 'z';

	mglData xx, yy;
	if(x->GetNx()*x->GetNy()!=m*n || y->GetNx()*y->GetNy()!=m*n)	// make
	{
		xx.Create(n, m);		yy.Create(n, m);
		for(long i=0;i<n;i++)	xx.a[i]=x->v(i);
		for(long j=1;j<m;j++)	memcpy(xx.a+n*j,xx.a,n*sizeof(mreal));
		for(long j=0;j<m;j++)
		{	mreal t=y->v(j);	for(long i=0;i<n;i++)	yy.a[i+n*j]=t;	}
		x = &xx;	y = &yy;
	}
	// x, y -- have the same size z
	int wire = mglchr(sch,'#')?1:0;
	if(mglchr(sch,'.'))	wire = 2;
#pragma omp parallel for collapse(2)
	for(long i=0;i<v->GetNx();i++)	for(long j=0;j<z->GetNz();j++)
	{
		if(gr->NeedStop())	continue;
		mreal v0 = v->v(i);
		mgl_axial_gen(gr,v0,z,x,y,gr->GetC(s,v0),dir,j,wire);
	}
	gr->EndGroup();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_val(HMGL gr, HCDT v, HCDT a, const char *sch, const char *opt)
{
	long n=a->GetNx(), m=a->GetNy();
	if(n<2 || m<2)	{	gr->SetWarn(mglWarnLow,"Axial");	return;	}
	gr->SaveState(opt);
	mglDataV x(n, m), y(n, m);
	if(gr->Max.x*gr->Min.x>=0)	x.Fill(gr->Min.x,gr->Max.x,'x');
	else	x.Fill(0,gr->Max.x,'x');
	y.Fill(gr->Min.y,gr->Max.y,'y');
	mgl_axial_xy_val(gr,v,&x,&y,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_xy(HMGL gr, HCDT x, HCDT y, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?3:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Axial");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_axial_xy_val(gr,&v,x,y,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial(HMGL gr, HCDT a, const char *sch, const char *opt)
{
	mreal r = gr->SaveState(opt);
	long Num = mgl_isnan(r)?3:long(r+0.5);
	if(Num<1)	{	gr->SetWarn(mglWarnCnt,"Axial");	return;	}
	mglData v(Num);
	for(long i=0;i<Num;i++)	v.a[i] = gr->Min.c + (gr->Max.c-gr->Min.c)*mreal(i+1)/(Num+1);
	mgl_axial_val(gr,&v,a,sch,0);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_xy_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_axial_xy_val(_GR_, _DA_(v), _DA_(x), _DA_(y), _DA_(a), s, o);
delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_val_(uintptr_t *gr, uintptr_t *v, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_axial_val(_GR_, _DA_(v), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_axial_xy(_GR_, _DA_(x), _DA_(y), _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_axial_(uintptr_t *gr, uintptr_t *a, const char *sch, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_axial(_GR_, _DA_(a), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
//
//		Torus series
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_torus(HMGL gr, HCDT r, HCDT z, const char *sch, const char *opt)
{
	long i,j,n=r->GetNx();
	if(n*r->GetNy()!=z->GetNx()*z->GetNy())	{	gr->SetWarn(mglWarnDim,"Torus");	return;	}
	if(n<2)		{	gr->SetWarn(mglWarnLow,"Torus");	return;	}
	gr->SaveState(opt);
	static int cgid=1;	gr->StartGroup("Torus",cgid++);

	mglPoint *pp = new mglPoint[n];
	long *nn = new long[n];
	long ss=gr->AddTexture(sch);
	char dir='y';
	if(mglchr(sch,'x'))	dir = 'x';
	if(mglchr(sch,'z'))	dir = 'z';

	mreal c = gr->GetC(ss,gr->Min.c);
	const mglData *mr = dynamic_cast<const mglData *>(r);
	const mglData *mz = dynamic_cast<const mglData *>(z);
	int wire = mglchr(sch,'#')?1:0;
	if(mglchr(sch,'.'))	wire = 2;
	for(j=0;j<r->GetNy();j++)
	{
		if(mr&&mz)	for(i=0;i<n;i++)
		{
			nn[i] = i<n-1 ? i+1 : -1;
			pp[i].Set(mr->a[i+n*j], mz->a[i+n*j]);
		}
		else	for(i=0;i<n;i++)
		{
			nn[i] = i<n-1 ? i+1 : -1;
			pp[i].Set(r->v(i,j), z->v(i,j));
		}
		mgl_axial_plot(gr,n,pp,nn,dir,c,wire);
	}
	gr->EndGroup();
	delete []nn;	delete []pp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_torus_(uintptr_t *gr, uintptr_t *r, uintptr_t *z, const char *pen, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,pen,l);	s[l]=0;
char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
mgl_torus(_GR_, _DA_(r), _DA_(z), s, o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
