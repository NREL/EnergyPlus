/***************************************************************************
 * export_2d.cpp is part of Math Graphic Library
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
#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"
#include "mgl2/font.h"
#include <time.h>
#include <algorithm>
#include <sys/stat.h>
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
#define _Gr_	((mglCanvas *)(gr))
void mgl_printf(void *fp, bool gz, const char *str, ...);
//-----------------------------------------------------------------------------
static const char *mgl_get_dash(unsigned short d, mreal w,char dlm)
{
	static std::string s;
	if(d==0xffff)	return "";
	int f=0, p=d&1, n=p?0:1;
	s = p ? "" : "0";
	for(int i=0;i<16;i++)
	{
		int j = i;//15-i;
		if(((d>>j)&1) == p)	f++;
		else
		{
			s += mgl_str_num(f*w)+dlm;
			p = (d>>j)&1;	f = 1;	n++;
		}
	}
	s += mgl_str_num(f*w) + ((n%2) ? "" : " 0");
	return s.c_str();
}
//-----------------------------------------------------------------------------
bool MGL_LOCAL_PURE mgl_is_same(HMGL gr, long i, mreal wp,uint32_t cp, int st)
{
	const mglPrim &pr=_Gr_->GetPrm(i);
	if(abs(pr.type)!=1)	return false;
	if(pr.w>=1 && wp!=pr.w)	return false;
	if(pr.w<1 && wp!=1)	return false;
	if(st!=pr.n3)	return false;
	return (cp==_Gr_->GetPrmCol(i));
}
//-----------------------------------------------------------------------------
std::vector<long> static put_line(HMGL gr, long i, mreal wp, uint32_t cp,int st)
{
	std::vector<long> ids;
	long n1=gr->GetPrm(i).n1, n2=gr->GetPrm(i).n2;
	if(n1>n2)	{	n1=gr->GetPrm(i).n2;	n2=gr->GetPrm(i).n1;	}
	if(n1<0 || n2<0)	return ids;
	const mglPnt &pp1 = gr->GetPnt(n1), &pp2 = gr->GetPnt(n2);
	mreal x0=pp1.x, y0=pp1.y;
	bool ok=true;
	long j;	// first point
	while(ok)	// try to find starting point
	{
		for(ok=false,j=i+1;j<gr->GetPrmNum();j++)
		{
			mglPrim &q = gr->GetPrm(j);
			if(q.type>1)	break;
			if(mgl_is_same(gr, j, wp,cp,st) && q.type==1 && q.n1>=0 && q.n2>=0)	// previous point
			{
				const mglPnt &p1 = gr->GetPnt(q.n1);
				const mglPnt &p2 = gr->GetPnt(q.n2);
				if(p2.x==x0 && p2.y==y0)
				{
					ok = true;	ids.push_back(q.n1);
					x0 = p1.x;	y0 = p1.y;	q.type = -1;
				}
				else if(p1.x==x0 && p1.y==y0)
				{
					ok = true;	ids.push_back(q.n2);
					x0 = p2.x;	y0 = p2.y;	q.type = -1;
				}
			}
		}
	}
	std::reverse(ids.begin(),ids.end());
	ids.push_back(n1);	ids.push_back(n2);
	x0 = pp2.x;	y0 = pp2.y;	ok = true;
	while(ok)	// try to find starting point
	{
		for(ok=false,j=i+1;j<gr->GetPrmNum();j++)
		{
			mglPrim &q = gr->GetPrm(j);
			if(q.type>1)	break;
			if(mgl_is_same(gr, j,wp,cp,st) && q.type==1 && q.n1>=0 && q.n2>=0)	// next point
			{
				const mglPnt &p1 = gr->GetPnt(q.n1);
				const mglPnt &p2 = gr->GetPnt(q.n2);
				if(p2.x==x0 && p2.y==y0)
				{
					ok = true;	ids.push_back(q.n1);
					x0 = p1.x;	y0 = p1.y;	q.type = -1;
				}
				else if(p1.x==x0 && p1.y==y0)
				{
					ok = true;	ids.push_back(q.n2);
					x0 = p2.x;	y0 = p2.y;	q.type = -1;
				}
			}
		}
	}
	return ids;
}
//-----------------------------------------------------------------------------
//put_desc(fp,"%c%c%c_%04x {", "np %d %d mt %d %d ll %d %d ll cp fill\n",
//"np %d %d mt ", "%d %d ll ", "cp dr\n", "} def")
void static put_desc(HMGL gr, void *fp, bool gz, const char *pre, const char *ln1, const char *ln2, const char *ln3, const char *suf)
{
	long n=0;
	for(long i=0;i<gr->GetPrmNum();i++)	if(gr->GetPrm(i).type==4)	n++;
	if(n==0)	return;		// no glyphs
	wchar_t *g = new wchar_t[n];
	int *s = new int[n];	n=0;
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim q = gr->GetPrm(i);
		if(q.type!=4 || (q.n3&8))	continue;	// not a glyph
		bool is=false;
		for(long j=0;j<n;j++)	if(g[j]==q.n4 && s[j]==(q.n3&7))	is = true;
		if(is)	continue;		// glyph is described
		// have to describe
		g[n]=q.n4;	s[n]=q.n3&7;	n++;	// add to list of described
		// "%c%c%c_%04x {"
		mgl_printf(fp, gz, pre, q.n3&1?'b':'n', q.n3&2?'i':'n', q.n4);
		const mglGlyph &g = gr->GetGlf(q.n4);
		int nl=g.nl;
		const short *ln=g.line;
		bool np=true;
		if(ln && nl>0)	for(long ik=0;ik<nl;ik++)
		{
			long ii = 2*ik;
			if(ln[ii]==0x3fff && ln[ii+1]==0x3fff)	// line breakthrough
			{	mgl_printf(fp, gz, "%s",ln3);	np=true;	continue;	}
			else if(np)	mgl_printf(fp, gz, ln1,ln[ii],ln[ii+1]);
			else		mgl_printf(fp, gz, ln2,ln[ii],ln[ii+1]);
			np=false;
		}
		mgl_printf(fp, gz, "%s%s",ln3,suf);	// finish glyph description suf="} def"
	}
	delete []g;		delete []s;
}
//-----------------------------------------------------------------------------
bool MGL_NO_EXPORT mgl_eps_pattern(void *fp, bool gz, const mglPrim &q)
{
	static uint64_t pd=MGL_SOLID_MASK;	// mask if !=MGL_SOLID_MASK
	static mreal pw=0;		// width (like pen width) if !=0
	static int ang=0;		// angle (default is 0,45,90,315)
	int aa = 45*int(0.5+q.angl/45.);
	if(q.m==MGL_SOLID_MASK || q.w<=0)	return false;
	if(q.m==pd && q.w==pw && aa==ang)	return true;
	pd = q.m;	pw=q.w;	ang=aa;
	mreal d = ang%90 ? 4*M_SQRT2*pw : 4*pw;
	mgl_printf(fp, gz, "<<\n/PaintType 2 /PatternType 1 /TilingType 1\n");
	mgl_printf(fp, gz, "/BBox [-%g -%g %g %g] /XStep %g /YStep %g\n", d,d,d,d, 2*d,2*d);
	if(ang%90)
	{
		mgl_printf(fp, gz, "/PaintProc { gsave %d rotate\n",-ang);
		for(int i=-8;i<8;i++)	for(int j=-8;j<8;j++)
		{
			int ii = (8+i)&7, jj = (8+j)&7;	// TODO reduce number of used rectangles
			if(pd & (1L<<(ii+8*jj)))
				mgl_printf(fp, gz, "%g %g %g %g rf\n",i*pw,j*pw,pw,pw);
		}
		mgl_printf(fp, gz, "grestore}\n>> pat\n");
	}
	else
	{
		mgl_printf(fp, gz, "/PaintProc { gsave %d rotate\n",-ang);
		for(int i=-4;i<4;i++)	for(int j=-4;j<4;j++)
		{
			int ii = (8+i)&7, jj = (8+j)&7;
			if(pd & (1L<<(ii+8*jj)))
				mgl_printf(fp, gz, "%g %g %g %g rf\n",i*pw,j*pw,pw,pw);
		}
		mgl_printf(fp, gz, "grestore}\n>> pat\n");
	}
	return true;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_eps(HMGL gr, const char *fname,const char *descr)
{
	if(!fname || *fname==0)	return;
	if(gr->GetPrmNum()<1)	return;
	_Gr_->clr(MGL_FINISHED);	_Gr_->PreparePrim(1);
	time_t now;	time(&now);

	bool gz = fname[strlen(fname)-1]=='z';
	void *fp;
	if(!strcmp(fname,"-"))	fp = stdout;		// allow to write in stdout
	else		fp = gz ? (void*)gzopen(fname,"wt") : (void*)fopen(fname,"wt");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	int w = _Gr_->GetWidth(), h = _Gr_->GetHeight();

	int x1=gr->BBoxX1, x2=gr->BBoxX2<0?w:gr->BBoxX2, y1=gr->BBoxY1, y2=gr->BBoxY2<0?h:gr->BBoxY2;
	if(x1<0 || x1>=x2 || y1<0 || y1>=y2)	{	x1=y1=0;	x2=w;	y2=h;	}

	if(gz)
	{
		unsigned len = strlen(fname), pos=0;
		char *buf = new char[len+4];
		memcpy(buf,fname,len);
		if(buf[len-3]=='.')	pos = len-2;
		else if(buf[len-2]=='.')	pos = len-1;
		else	{	buf[len-1]='.';	pos = len;	}
		if(pos)	{	buf[pos]=buf[pos+1]='b';	buf[pos+2]=0;	}
		FILE *fb = fopen(buf,"w");
		fprintf(fb, "%%%%BoundingBox: %d %d %d %d\n", x1, h-y2, x2, h-y1);
		fclose(fb);	delete []buf;
	}

	const std::string loc = setlocale(LC_NUMERIC, "C");
	mgl_printf(fp, gz, "%%!PS-Adobe-3.0 EPSF-3.0\n%%%%BoundingBox: %d %d %d %d\n", x1, h-y2, x2, h-y1);
	mgl_printf(fp, gz, "%%%%Created by MathGL library\n%%%%Title: %s\n",descr ? descr : fname);
	mgl_printf(fp, gz, "%%%%CreationDate: %s\n",ctime(&now));
	mgl_printf(fp, gz, "%%%%LanguageLevel: 2\n50 dict begin");
	mgl_printf(fp, gz, "/lw {setlinewidth} def\n/rgb {setrgbcolor} def\n");
	mgl_printf(fp, gz, "/np {newpath} def\n/cp {closepath} def\n");
	mgl_printf(fp, gz, "/ll {lineto} def\n/mt {moveto} def\n");
	mgl_printf(fp, gz, "/rl {rlineto} def\n/rm {rmoveto} def\n/dr {stroke} def\n");
	mgl_printf(fp, gz, "/ss {%g} def\n",0.35*gr->mark_size());
	mgl_printf(fp, gz, "/s2 {%g} def\n",0.7*gr->mark_size());
	mgl_printf(fp, gz, "/sm {-%g} def\n",0.35*gr->mark_size());
	mgl_printf(fp, gz, "/m_c {ss 0.3 mul 0 360 arc} def\n");
	mgl_printf(fp, gz, "/d0 {[] 0 setdash} def\n/sd {setdash} def\n");
	mgl_printf(fp, gz, "/pat {[1 0 0 1 0 0] makepattern /Mask exch def [/Pattern /DeviceRGB] setcolorspace} def\n");
	mgl_printf(fp, gz, "/mask {Mask setpattern} def\n/rf {rectfill} def\n");

	bool m_p=false,m_x=false,m_d=false,m_v=false,m_t=false,
	m_s=false,m_a=false,m_o=false,m_T=false,
	m_V=false,m_S=false,m_D=false,m_Y=false,m_l=false,
	m_L=false,m_r=false,m_R=false,m_X=false,m_P=false;
	// add mark definition if present
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim q = gr->GetPrm(i);
		if(q.type>0)	continue;
		if(q.n4=='+')	m_p = true;
		if(q.n4=='x')	m_x = true;
		if(q.n4=='s')	m_s = true;
		if(q.n4=='d')	m_d = true;
		if(q.n4=='v')	m_v = true;
		if(q.n4=='^')	m_t = true;
		if(q.n4=='*')	m_a = true;
		if(q.n4=='o' || q.n4=='O' || q.n4=='C')	m_o = true;
		if(q.n4=='S')	m_S = true;
		if(q.n4=='D')	m_D = true;
		if(q.n4=='V')	m_V = true;
		if(q.n4=='T')	m_T = true;
		if(q.n4=='<')	m_l = true;
		if(q.n4=='L')	m_L = true;
		if(q.n4=='>')	m_r = true;
		if(q.n4=='R')	m_R = true;
		if(q.n4=='Y')	m_Y = true;
		if(q.n4=='P')	m_P = true;
		if(q.n4=='X')	m_X = true;
	}
	if(m_P)	{	m_p=true;	m_s=true;	}
	if(m_X)	{	m_x=true;	m_s=true;	}
	if(m_p)	mgl_printf(fp, gz, "/m_p {sm 0 rm s2 0 rl sm sm rm 0 s2 rl d0} def\n");
	if(m_x)	mgl_printf(fp, gz, "/m_x {sm sm rm s2 s2 rl 0 sm 2 mul rm sm 2 mul s2 rl d0} def\n");
	if(m_s)	mgl_printf(fp, gz, "/m_s {sm sm rm 0 s2 rl s2 0 rl 0 sm 2 mul rl cp d0} def\n");
	if(m_d)	mgl_printf(fp, gz, "/m_d {sm 0 rm ss ss rl ss sm rl sm sm rl cp d0} def\n");
	if(m_v)	mgl_printf(fp, gz, "/m_v {sm ss 2 div rm s2 0 rl sm sm 1.5 mul rl d0 cp} def\n");
	if(m_t)	mgl_printf(fp, gz, "/m_t {sm sm 2 div rm s2 0 rl sm ss 1.5 mul rl d0 cp} def\n");
	if(m_a)	mgl_printf(fp, gz, "/m_a {sm 0 rm s2 0 rl sm 1.6 mul sm 0.8 mul rm ss 1.2 mul ss 1.6 mul rl 0 sm 1.6 mul rm sm 1.2 mul ss 1.6 mul rl d0} def\n");
	if(m_o)	mgl_printf(fp, gz, "/m_o {ss 0 360 d0 arc} def\n");
	if(m_S)	mgl_printf(fp, gz, "/m_S {sm sm rm 0 s2 rl s2 0 rl 0 sm 2 mul rl cp} def\n");
	if(m_D)	mgl_printf(fp, gz, "/m_D {sm 0 rm ss ss rl ss sm rl sm sm rl cp} def\n");
	if(m_V)	mgl_printf(fp, gz, "/m_V {sm ss 2 div rm s2 0 rl sm sm 1.5 mul rl cp} def\n");
	if(m_T)	mgl_printf(fp, gz, "/m_T {sm sm 2 div rm s2 0 rl sm ss 1.5 mul rl cp} def\n");
	if(m_Y)	mgl_printf(fp, gz, "/m_Y {0 sm rm 0 ss rl sm ss rl s2 0 rm sm sm rl d0} def\n");
	if(m_r)	mgl_printf(fp, gz, "/m_r {sm 2 div sm rm 0 s2 rl ss 1.5 mul sm rl d0 cp} def\n");
	if(m_l)	mgl_printf(fp, gz, "/m_l {ss 2 div sm rm 0 s2 rl sm 1.5 mul sm rl d0 cp} def\n");
	if(m_R)	mgl_printf(fp, gz, "/m_R {sm 2 div sm rm 0 s2 rl ss 1.5 mul sm rl cp} def\n");
	if(m_L)	mgl_printf(fp, gz, "/m_L {ss 2 div sm rm 0 s2 rl sm 1.5 mul sm rl cp} def\n");
	if(m_P)	mgl_printf(fp, gz, "/m_P {m_p 0 sm rm m_s} def\n");
	if(m_X)	mgl_printf(fp, gz, "/m_X {m_x ss sm rm m_s} def\n");
	//	if(m_C)	mgl_printf(fp, gz, "/m_C {m_c m_o} def\n");
	mgl_printf(fp, gz, "0 setlinecap\n1 setlinejoin\n\n");	// manual setting round line cap

	// Write background image first
	const unsigned char *img = mgl_get_background(gr);
	bool same = true;
	unsigned char white[3]={255,255,255};
#pragma omp parallel for
	for(long i=0;i<w*h;i++)	if(memcmp(img,img+4*i,3))	same=false;
	if(!same)
	{
		mgl_printf(fp, gz, "gsave\t%d %d translate\n",x1,h-y2);
		mgl_printf(fp, gz, "%d %d 8 [1 0 0 1 0 0] {<", x2-x1,y2-y1);
		for(long j=y2-1;j>=y1;j--)	for(long i=x1;i<x2;i++)
		{
			if((i+w*(h-j-1))%40==0 && i+j>0)	mgl_printf(fp, gz, "\n");
			mgl_printf(fp, gz, "%02x%02x%02x",img[4*(i+w*j)],img[4*(i+w*j)+1],img[4*(i+w*j)+2]);
		}
		mgl_printf(fp, gz, "\n>} false 3 colorimage\ngrestore\n\n");
	}
	else if(memcmp(img,white,3))
		mgl_printf(fp, gz, "np 0 0 mt 0 %d ll %d %d ll %d 0 ll cp %g %g %g rgb fill\n", y2-y1, x2-x1, y2-y1, x2-x1, img[0]/255., img[1]/255., img[2]/255.);

	// write definition for all glyphs
	put_desc(gr,fp,gz,"/%c%c_%04x { np\n", "\t%d %d mt ", "%d %d ll ", "cp\n", "} def\n");
	// write primitives
	mreal wp=-1;
	float qs_old=gr->mark_size()/gr->FontFactor();
	mglRGBA cp;
	int st=0;
	char str[256]="", msk[256]="";
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q = gr->GetPrm(i);
		if(q.type<0)	continue;	// q.n1>=0 always
		cp.c = _Gr_->GetPrmCol(i);
		const mglPnt p1 = gr->GetPnt(q.n1);
		if(q.type>1)
		{
			snprintf(str,256,"%.2g %.2g %.2g rgb ", cp.r[0]/255.,cp.r[1]/255.,cp.r[2]/255.);	str[255]=0;
			snprintf(msk,256,"%.2g %.2g %.2g mask ", cp.r[0]/255.,cp.r[1]/255.,cp.r[2]/255.);	msk[255]=0;
		}

		if(q.type==0)	// mark
		{
			mreal x0 = p1.x,y0 = p1.y;
//			snprintf(str,256,"%.2g lw %.2g %.2g %.2g rgb ", 50*q.s*q.w>1?50*q.s*q.w:1, cp.r[0]/255.,cp.r[1]/255.,cp.r[2]/255.);
			snprintf(str,256,"%.2g lw %.2g %.2g %.2g rgb ", q.w>1?q.w:1, cp.r[0]/255.,cp.r[1]/255.,cp.r[2]/255.);
			str[255]=0;	wp=1;	// NOTE: this may renew line style if a mark inside!
			if(q.s!=qs_old)
			{
				mgl_printf(fp, gz, "/ss {%g} def\n",q.s);
				mgl_printf(fp, gz, "/s2 {%g} def\n",q.s*2);
				mgl_printf(fp, gz, "/sm {-%g} def\n",q.s);
				qs_old = q.s;
			}
			switch(q.n4)
			{
				case '+':	mgl_printf(fp, gz, "np %g %g mt m_p %sdr\n",x0,y0,str);	break;
				case 'x':	mgl_printf(fp, gz, "np %g %g mt m_x %sdr\n",x0,y0,str);	break;
				case 's':	mgl_printf(fp, gz, "np %g %g mt m_s %sdr\n",x0,y0,str);	break;
				case 'd':	mgl_printf(fp, gz, "np %g %g mt m_d %sdr\n",x0,y0,str);	break;
				case '*':	mgl_printf(fp, gz, "np %g %g mt m_a %sdr\n",x0,y0,str);	break;
				case 'v':	mgl_printf(fp, gz, "np %g %g mt m_v %sdr\n",x0,y0,str);	break;
				case '^':	mgl_printf(fp, gz, "np %g %g mt m_t %sdr\n",x0,y0,str);	break;
				case 'S':	mgl_printf(fp, gz, "np %g %g mt m_S %sfill\n",x0,y0,str);	break;
				case 'D':	mgl_printf(fp, gz, "np %g %g mt m_D %sfill\n",x0,y0,str);	break;
				case 'V':	mgl_printf(fp, gz, "np %g %g mt m_V %sfill\n",x0,y0,str);	break;
				case 'T':	mgl_printf(fp, gz, "np %g %g mt m_T %sfill\n",x0,y0,str);	break;
				case 'o':	mgl_printf(fp, gz, "%g %g m_o %sdr\n",x0,y0,str);break;
				case 'O':	mgl_printf(fp, gz, "%g %g m_o %sfill\n",x0,y0,str);break;
				case 'Y':	mgl_printf(fp, gz, "np %g %g mt m_Y %sdr\n",x0,y0,str);	break;
				case '<':	mgl_printf(fp, gz, "np %g %g mt m_l %sdr\n",x0,y0,str);	break;
				case '>':	mgl_printf(fp, gz, "np %g %g mt m_r %sdr\n",x0,y0,str);	break;
				case 'L':	mgl_printf(fp, gz, "np %g %g mt m_L %sfill\n",x0,y0,str);	break;
				case 'R':	mgl_printf(fp, gz, "np %g %g mt m_R %sfill\n",x0,y0,str);	break;
				case 'P':	mgl_printf(fp, gz, "np %g %g mt m_P %sdr\n",x0,y0,str);	break;
				case 'X':	mgl_printf(fp, gz, "np %g %g mt m_X %sdr\n",x0,y0,str);	break;
				case 'C':	mgl_printf(fp, gz, "%g %g m_o %g %g m_c %sdr\n",x0,y0,x0,y0,str);	break;
				case '.':	mgl_printf(fp, gz, "%g %g m_c %sfill\n",x0,y0,str);
			}
		}
		else if(q.type==3)	// quad
		{
			const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3), &p4=gr->GetPnt(q.n4);
			if(cp.r[3])	// TODO && gr->quad_vis(p1,p2,p3,p4))
			{
				bool mask = mgl_eps_pattern(fp,gz,q);
				mgl_printf(fp, gz, "np %g %g mt %g %g ll %g %g ll %g %g ll cp %sfill\n", p1.x, p1.y, p2.x, p2.y, p4.x, p4.y, p3.x, p3.y, mask?msk:str);
			}
		}
		else if(q.type==2)	// trig
		{
			const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3);
			if(cp.r[3])	// TODO && gr->trig_vis(p1,p2,p3))
			{
				bool mask = mgl_eps_pattern(fp,gz,q);
				mgl_printf(fp, gz, "np %g %g mt %g %g ll %g %g ll cp %sfill\n", p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, mask?msk:str);
			}
		}
		else if(q.type==1)	// line
		{
			snprintf(str,256,"%.2g lw %.2g %.2g %.2g rgb ", q.w>1 ? q.w:1., cp.r[0]/255.,cp.r[1]/255.,cp.r[2]/255.);
			str[255]=0;	wp = q.w>1  ? q.w:1;	st = q.n3;
			std::vector<long> ids = put_line(gr,i,wp,cp.c,st);
			for(size_t j=0;j<ids.size();j++)
			{
				const mglPnt &p = gr->GetPnt(ids[j]);
				float x0 = p.x, y0 = p.y;
				mgl_printf(fp, gz, j==0?"np %g %g mt ":"%g %g ll ",x0,y0);
			}
			const char *sd = mgl_get_dash(q.n3,q.w,' ');
			if(sd && sd[0])	mgl_printf(fp, gz, "%s [%s] %g sd dr\n",str,sd,q.w*q.s);
			else			mgl_printf(fp, gz, "%s d0 dr\n",str);
		}
		else if(q.type==4)	// glyph
		{
			float phi = gr->GetGlyphPhi(gr->GetPnt(q.n2),q.w);
			if(mgl_isnan(phi))	continue;
			mreal 	ss = q.s/2, xx = p1.u, yy = p1.v, zz = q.p;
			mgl_printf(fp, gz, "gsave\t%g %g translate %g %g scale %g rotate %s\n",
					   p1.x, p1.y, ss, ss, -phi, str);
			if(q.n3&8)	// this is "line"
			{
				mreal dy = 0.004,f=fabs(zz);
				mgl_printf(fp, gz, "np %g %g mt %g %g ll %g %g ll %g %g ll cp ",
						   xx,yy+dy, xx+f,yy+dy, xx+f,yy-dy, xx,yy-dy);
			}
			else
				mgl_printf(fp, gz, "%.3g %.3g translate %g %g scale %c%c_%04x ",
						   xx, yy, zz, zz, q.n3&1?'b':'n', q.n3&2?'i':'n', q.n4);
			if(q.n3&4)	mgl_printf(fp, gz, "dr");
			else	mgl_printf(fp, gz, "eofill");
			mgl_printf(fp, gz, " grestore\n");
		}
	}
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		mglPrim &q = gr->GetPrm(i);
		if(q.type==-1)	q.type = 1;
	}
	mgl_printf(fp, gz, "\nshowpage\n%%%%EOF\n");
	if(strcmp(fname,"-"))	{	if(gz)	gzclose((gzFile)fp);	else	fclose((FILE *)fp);	}
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_eps_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_eps(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_svg(HMGL gr, const char *fname,const char *descr)
{
	if(!fname || *fname==0)	return;
	if(gr->GetPrmNum()<1)	return;
	_Gr_->clr(MGL_FINISHED);	_Gr_->PreparePrim(1);
	time_t now;	time(&now);

	bool gz = fname[strlen(fname)-1]=='z';
	long hh = _Gr_->GetHeight(), ww = _Gr_->GetWidth();
	void *fp = stdout;		// allow to write in stdout
	bool head = true;
	if(strcmp(fname,"-"))	fp = gz ? (void*)gzopen(fname,"wt") : (void*)fopen(fname,"wt");
	else	head = false;
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}

	int x1=gr->BBoxX1, x2=gr->BBoxX2<0?ww:gr->BBoxX2, y1=gr->BBoxY1, y2=gr->BBoxY2<0?hh:gr->BBoxY2;
	if(x1<0 || x1>=x2 || y1<0 || y1>=y2)	{	x1=y1=0;	x2=ww;	y2=hh;	}
	ww = x2-x1;	hh = y2-y1;

	const std::string loc = setlocale(LC_NUMERIC, "C");
	if(head)
	{
		mgl_printf(fp, gz, "<?xml version=\"1.0\" standalone=\"no\"?>\n");
		mgl_printf(fp, gz, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20000303 Stylable//EN\" \"http://www.w3.org/TR/2000/03/WD-SVG-20000303/DTD/svg-20000303-stylable.dtd\">\n");
		mgl_printf(fp, gz, "<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n", ww, hh);
		mgl_printf(fp, gz, "<!--Created by MathGL library-->\n");
		mgl_printf(fp, gz, "<!--Title: %s-->\n<!--CreationDate: %s-->\n\n",descr?descr:fname,ctime(&now));
	}
	else
	{
		mgl_printf(fp, gz, "<!--Created by MathGL library-->\n");
		mgl_printf(fp, gz, "<svg width=\"%d\" height=\"%d\">\n", ww, hh);
	}

	// write definition for all glyphs
	put_desc(gr,fp,gz,"<defs><g id=\"%c%c_%04x\"><path d=\"", "\tM %d %d ",
			 "L %d %d ", "Z\n", "\"/></g></defs>\n");

	// Write background image first
	const unsigned char *img = mgl_get_background(gr);
	bool same = true;
	unsigned char white[3]={255,255,255};
#pragma omp parallel for
	for(long i=0;i<ww*hh;i++)	if(memcmp(img,img+4*i,3))	same=false;
	if(!same)
	{	// TODO write as <image width="100" height="100" xlink:href="data:image/png;base64,...">
/*		mgl_printf(fp, gz, "%d %d 8 [1 0 0 1 0 0] {<", ww,hh,1+ww*hh/40);
		for(long j=hh-1;j>=0;j--)	for(long i=0;i<ww;i++)
		{
			if((i+ww*(hh-j-1))%40==0 && i+j>0)	mgl_printf(fp, gz, "\n");
			mgl_printf(fp, gz, "%02x%02x%02x",img[4*(i+ww*j)],img[4*(i+ww*j)+1],img[4*(i+ww*j)+2]);
		}
		mgl_printf(fp, gz, "\n>} false 3 colorimage\n\n");*/
	}
	else if(memcmp(img,white,3))
	{
		mgl_printf(fp, gz, "<g fill=\"#%02x%02x%02x\" opacity=\"%g\">\n", img[0], img[1], img[2], img[3]/255.);
		mgl_printf(fp, gz, "<path d=\"M 0 0 L 0 %ld L %ld %ld L %ld 0 Z\"/> </g>\n", hh, ww, hh, ww);
	}


	// currentColor -> inherit ???
	mgl_printf(fp, gz, "<g fill=\"none\" stroke=\"none\" stroke-width=\"0.5\" stroke-linecap=\"butt\" stroke-linejoin=\"round\">\n");
	// write primitives
	mreal wp=-1;
	int st=0;
	mglRGBA cp;

//	hh += (y2+y1)/2;
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q = gr->GetPrm(i);
		if(q.type<0)	continue;	// q.n1>=0 always
		cp.c = _Gr_->GetPrmCol(i);
		const mglPnt p1=gr->GetPnt(q.n1);
		if(q.type==0)
		{
			mreal x=p1.x-x1,y=hh-p1.y,s=q.s;
			if(!strchr("xsSoO",q.n4))	s *= 1.1;
			wp = 1;
			if(strchr("SDVTLR",q.n4))
				mgl_printf(fp, gz, "<g fill=\"#%02x%02x%02x\">\n", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
			else
				mgl_printf(fp, gz, "<g stroke=\"#%02x%02x%02x\"  stroke-width=\"%g\">\n", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]), q.w>1?q.w:1);
//				mgl_printf(fp, gz, "<g stroke=\"#%02x%02x%02x\"  stroke-width=\"%g\">\n", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]), 50*q.s*q.w>1?50*q.s*q.w:1);
			switch(q.n4)
			{
			case 'P':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g M %g %g L %g %g M %g %g L %g %g L %g %g L %g %g L %g %g\"/>\n",
							x-s,y,x+s,y,x,y-s,x,y+s, x-s,y-s,x+s,y-s,x+s,y+s,x-s,y+s,x-s,y-s);	break;
			case '+':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g M %g %g L %g %g\"/>\n", x-s,y,x+s,y,x,y-s,x,y+s);	break;
			case 'X':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g M %g %g L %g %g M %g %g L %g %g L %g %g L %g %g L %g %g\"/>\n",
							x-s,y-s,x+s,y+s,x+s,y-s,x-s,y+s, x-s,y-s,x+s,y-s,x+s,y+s,x-s,y+s,x-s,y-s);	break;
			case 'x':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g M %g %g L %g %g\"/>\n", x-s,y-s,x+s,y+s,x+s,y-s,x-s,y+s);	break;
			case 's':
			case 'S':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g L %g %gZ\"/>\n", x-s,y-s,x+s,y-s,x+s,y+s,x-s,y+s);	break;
			case 'd':
			case 'D':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g L %g %gZ\"/>\n", x-s,y,x,y-s,x+s,y,x,y+s);	break;
			case 'v':
			case 'V':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %gZ\"/>\n", x-s,y-s/2,x+s,y-s/2,x,y+s);	break;
			case '^':
			case 'T':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %gZ\"/>\n", x-s,y+s/2,x+s,y+s/2,x,y-s);	break;
			case '<':
			case 'L':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %gZ\"/>\n", x+s/2,y+s,x+s/2,y-s,x-s,y);	break;
			case '>':
			case 'R':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %gZ\"/>\n", x-s/2,y+s,x-s/2,y-s,x+s,y);	break;
			case 'Y':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g M %g %g L %g %g\"/>\n", x,y-s, x,y, x+s,y+s, x,y, x-s,y+s);	break;
			case 'C':
				mgl_printf(fp, gz, "<circle style=\"fill:#%02x%02x%02x\" cx=\"%g\" cy=\"%g\" r=\"0.15\"/>\n<circle cx=\"%g\" cy=\"%g\" r=\"%g\"/>\n",
							int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),x,y,x,y,s);	break;
			case 'o':
				mgl_printf(fp, gz, "<circle cx=\"%g\" cy=\"%g\" r=\"%g\"/>\n", x,y,s);	break;
			case 'O':
				mgl_printf(fp, gz, "<circle style=\"fill:#%02x%02x%02x\" cx=\"%g\" cy=\"%g\" r=\"%g\"/>\n",
							int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),x,y,s);	break;
			case '*':
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g M %g %g L %g %g M %g %g L %g %g\"/>\n",
							x-s,y,x+s,y,x-0.6*s,y-0.8*s,x+0.6*s,y+0.8*s,x+0.6*s,y-0.8*s,x-0.6*s,y+0.8*s);	break;
			case '.':
				mgl_printf(fp, gz, "<circle style=\"fill:#%02x%02x%02x\" cx=\"%g\" cy=\"%g\" r=\"0.15\"/>\n",
							int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),x,y);	break;
			}
			mgl_printf(fp, gz, "</g>\n");
		}
		else if(q.type==1)
		{
			mgl_printf(fp,gz,"<g stroke=\"#%02x%02x%02x\"",int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
			if(q.n3)
			{
				mgl_printf(fp, gz, " stroke-dasharray=\"%s\"", mgl_get_dash(q.n3,q.w,','));
//				mgl_printf(fp, gz, " stroke-dashoffset=\"%g\"", q.s*q.w);
				mgl_printf(fp, gz, " stroke-dashoffset=\"%g\"", q.w);
			}
			if(q.w>1)	mgl_printf(fp, gz, " stroke-width=\"%g\"", q.w);
			wp = q.w>1  ? q.w:1;	st = q.n3;
			std::vector<long> ids = put_line(gr,i,wp,cp.c,st);
			for(size_t j=0;j<ids.size();j++)
			{
				const mglPnt &p = gr->GetPnt(ids[j]);
				mgl_printf(fp, gz, j==0?"><path d=\" M %g %g":" L %g %g",p.x-x1,hh-p.y);
			}
			mgl_printf(fp, gz, "\"/> </g>\n");
		}
		else if(q.type==2 && cp.r[3])
		{
			const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3);
			mgl_printf(fp, gz, "<g fill=\"#%02x%02x%02x\" opacity=\"%g\">\n", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),cp.r[3]/255.);
			mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g Z\"/> </g>\n", p1.x-x1, hh-p1.y, p2.x-x1, hh-p2.y, p3.x-x1, hh-p3.y);
		}
		else if(q.type==3 && cp.r[3])
		{
			const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3), &p4=gr->GetPnt(q.n4);
			mgl_printf(fp, gz, "<g fill=\"#%02x%02x%02x\" opacity=\"%g\">\n", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),cp.r[3]/255.);
			mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g L %g %g Z\"/> </g>\n", p1.x-x1, hh-p1.y, p2.x-x1, hh-p2.y, p4.x-x1, hh-p4.y, p3.x-x1, hh-p3.y);
		}
		else if(q.type==4)
		{
			float phi = gr->GetGlyphPhi(gr->GetPnt(q.n2),q.w);
			if(mgl_isnan(phi))	continue;
			mreal ss = q.s/2, xx = p1.u, yy = p1.v, zz = q.p;
			if(q.n3&8)	// this is "line"
			{
				mgl_printf(fp, gz, "<g transform=\"translate(%g,%g) scale(%.3g,%.3g) rotate(%g)\"", p1.x-x1, hh-p1.y, ss, -ss, -phi);
				if(q.n3&4)
					mgl_printf(fp, gz, " stroke=\"#%02x%02x%02x\">", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
				else
					mgl_printf(fp, gz, " fill=\"#%02x%02x%02x\">", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
				mreal dy = 0.004,f=fabs(zz);
				mgl_printf(fp, gz, "<path d=\"M %g %g L %g %g L %g %g L %g %g\"/></g>\n", xx,yy+dy, xx+f,yy+dy, xx+f,yy-dy, xx,yy-dy);
			}
			else
			{
				ss *= zz;
				mgl_printf(fp, gz, "<g transform=\"translate(%g,%g) scale(%.3g,%.3g) rotate(%g)\"", p1.x-x1, hh-p1.y, ss, -ss, -q.w);
				if(q.n3&4)
					mgl_printf(fp, gz, " stroke=\"#%02x%02x%02x\">", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
				else
					mgl_printf(fp, gz, " fill=\"#%02x%02x%02x\">", int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
				mgl_printf(fp, gz, "<use x=\"%g\" y=\"%g\" xlink:href=\"#%c%c_%04x\"/></g>\n", xx/zz, yy/zz, q.n3&1?'b':'n', q.n3&2?'i':'n', q.n4);
			}
		}
	}

	for(long i=0;i<gr->GetPrmNum();i++)
	{	mglPrim &q=gr->GetPrm(i);	if(q.type==-1)	q.type = 1;	}
	mgl_printf(fp, gz, "</g></svg>");
	if(strcmp(fname,"-"))	{	if(gz)	gzclose((gzFile)fp);	else	fclose((FILE *)fp);	}
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_svg_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_svg(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_tex(HMGL gr, const char *fname,const char *descr)
{
	if(gr->GetPrmNum()<1)	return;
	_Gr_->clr(MGL_FINISHED);	_Gr_->PreparePrim(1);

	FILE *fp = fopen(fname,"w");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");	fwide(fp,1);
	fwprintf(fp, L"%% Created by MathGL library\n%% Title: %s\n\n",descr?descr:fname);
	// provide marks
	fwprintf(fp, L"\\providecommand{\\mglp}[4]{\\draw[#3] (#1-#4, #2) -- (#1+#4,#2) (#1,#2-#4) -- (#1,#2+#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mglx}[4]{\\draw[#3] (#1-#4, #2-#4) -- (#1+#4,#2+#4) (#1+#4,#2-#4) -- (#1-#4,#2+#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mgls}[4]{\\draw[#3] (#1-#4, #2-#4) -- (#1+#4,#2-#4) -- (#1+#4,#2+#4) -- (#1-#4,#2+#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglS}[4]{\\fill[#3] (#1-#4, #2-#4) -- (#1+#4,#2-#4) -- (#1+#4,#2+#4) -- (#1-#4,#2+#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mgld}[4]{\\draw[#3] (#1, #2-#4) -- (#1+#4,#2) -- (#1,#2+#4) -- (#1-#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglD}[4]{\\fill[#3] (#1, #2-#4) -- (#1+#4,#2) -- (#1,#2+#4) -- (#1-#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglv}[4]{\\draw[#3] (#1-#4, #2+#4/2) -- (#1+#4,#2+#4/2) -- (#1,#2-#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglV}[4]{\\fill[#3] (#1-#4, #2+#4/2) -- (#1+#4,#2+#4/2) -- (#1,#2-#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglt}[4]{\\draw[#3] (#1-#4, #2-#4/2) -- (#1+#4,#2-#4/2) -- (#1,#2+#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglT}[4]{\\fill[#3] (#1-#4, #2-#4/2) -- (#1+#4,#2-#4/2) -- (#1,#2+#4) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mgll}[4]{\\draw[#3] (#1+#4/2, #2-#4) -- (#1+#4/2,#2+#4) -- (#1-#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglL}[4]{\\fill[#3] (#1+#4/2, #2-#4) -- (#1+#4/2,#2+#4) -- (#1-#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglr}[4]{\\draw[#3] (#1-#4/2, #2-#4) -- (#1-#4/2,#2+#4) -- (#1+#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglR}[4]{\\fill[#3] (#1-#4/2, #2-#4) -- (#1-#4/2,#2+#4) -- (#1+#4,#2) -- cycle;}\n");
	fwprintf(fp, L"\\providecommand{\\mglR}[4]{\\draw[#3] (#1, #2-#4) -- (#1,#2) -- (#1-#4,#2+#4) (#1,#2) -- (#1+#4,#2+#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mgla}[4]{\\draw[#3] (#1-#4, #2) -- (#1+#4,#2) (#1-0.6*#4,#2-0.8*#4) -- (#1+0.6*#4,#2+0.8*#4) (#1-0.6*#4,#2+0.8*#4) -- (#1+0.6*#4,#2-0.8*#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mglY}[4]{\\draw[#3] (#1, #2-#4) -- (#1,#2) (#1-#4,#2+#4) -- (#1,#2) (#1+#4,#2+#4) -- (#1,#2);}\n");
	fwprintf(fp, L"\\providecommand{\\mglo}[4]{\\draw[#3] (#1, #2) circle (#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mglO}[4]{\\fill[#3] (#1, #2) circle (#4);}\n");
	fwprintf(fp, L"\\providecommand{\\mglc}[3]{\\draw[#3] (#1, #2) circle (%g);}\n\n", 4e-4*gr->mark_size());
	fwprintf(fp, L"\\begin{tikzpicture}\n");

	// write primitives first
	mreal wp=-1;
	int st=0;
	mglRGBA cp;
	char cname[128];

	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q = gr->GetPrm(i);
		if(q.type<0)	continue;	// q.n1>=0 always
		cp.c = _Gr_->GetPrmCol(i);
		snprintf(cname,128,"color={rgb,255:red,%d;green,%d;blue,%d}",cp.r[0],cp.r[1],cp.r[2]);	cname[127]=0;

		const mglPnt p1=gr->GetPnt(q.n1);
		mreal x=p1.x/100,y=p1.y/100,s=q.s/100;
		if(q.type==0)
		{
			if(!strchr("xsSoO",q.n4))	s *= 1.1;
			wp = 1;
			switch(q.n4)	// NOTE: no thickness for marks in TeX
			{
				case 'P':
					fwprintf(fp, L"\\mglp{%.4g}{%.4g}{%s}{%.4g} \\mgls{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s,x,y,cname,s);	break;
				case 'X':
					fwprintf(fp, L"\\mglx{%.4g}{%.4g}{%s}{%.4g} \\mgls{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s,x,y,cname,s);	break;
				case 'C':
					fwprintf(fp, L"\\mglc{%.4g}{%.4g}{%s}{%.4g} \\mglo{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s,x,y,cname,s);	break;
				case '+':	fwprintf(fp, L"\\mglp{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'x':	fwprintf(fp, L"\\mglx{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 's':	fwprintf(fp, L"\\mgls{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'S':	fwprintf(fp, L"\\mglS{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'd':	fwprintf(fp, L"\\mgld{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'D':	fwprintf(fp, L"\\mglD{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case '^':	fwprintf(fp, L"\\mglt{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'T':	fwprintf(fp, L"\\mglT{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'v':	fwprintf(fp, L"\\mglv{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'V':	fwprintf(fp, L"\\mglV{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case '<':	fwprintf(fp, L"\\mgll{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'L':	fwprintf(fp, L"\\mglL{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case '>':	fwprintf(fp, L"\\mglr{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'R':	fwprintf(fp, L"\\mglR{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'Y':	fwprintf(fp, L"\\mglY{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'o':	fwprintf(fp, L"\\mglo{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case 'O':	fwprintf(fp, L"\\mglO{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				case '*':	fwprintf(fp, L"\\mgla{%.4g}{%.4g}{%s}{%.4g}\n", x,y,cname,s);	break;
				default:	fwprintf(fp, L"\\mglc{%.4g}{%.4g}{%s}\n", x,y,cname);	break;
			}
		}
		else if(q.type==2 && cp.r[3])
		{
			const mglPnt p2=gr->GetPnt(q.n2), p3=gr->GetPnt(q.n3);
			if(cp.r[3]<255)
				fwprintf(fp, L"\\fill[%s, fill opacity=%.4g] (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- cycle;\n", cname,cp.r[3]/255., x,y, p2.x/100,p2.y/100, p3.x/100,p3.y/100);
			else
				fwprintf(fp, L"\\fill[%s, fill] (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- cycle;\n", cname, x,y, p2.x/100,p2.y/100, p3.x/100,p3.y/100);
		}
		else if(q.type==3 && cp.r[3])
		{
			const mglPnt p2=gr->GetPnt(q.n2), p3=gr->GetPnt(q.n3), p4=gr->GetPnt(q.n4);
			if(cp.r[3]<255)
				fwprintf(fp, L"\\fill[%s, fill opacity=%.4g] (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- cycle;\n", cname,cp.r[3]/255., x,y, p2.x/100,p2.y/100, p4.x/100,p4.y/100, p3.x/100,p3.y/100);
			else
				fwprintf(fp, L"\\fill[%s, fill] (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- (%.4g,%.4g) -- cycle;\n", cname, x,y, p2.x/100,p2.y/100, p4.x/100,p4.y/100, p3.x/100,p3.y/100);

		}
		else if(q.type==1)	// lines
		{
			//const char *dash[]={"", "8 8","4 4","1 3","7 4 1 4","3 2 1 2"};
			const char *w[]={"semithick","thick","very thick","ultra thick"};
			int iw=int(q.w-0.5);	if(iw>3)	iw=3;
			if(iw<0)	fwprintf(fp,L"\\draw[%s] ",cname);
			else		fwprintf(fp,L"\\draw[%s,%s] ",cname,w[iw]);
			// TODO: add line dashing
			wp = q.w>1  ? q.w:1;	st = q.n3;
			std::vector<long> ids = put_line(gr,i,wp,cp.c,st);
			for(size_t j=0;j<ids.size();j++)
			{
				const mglPnt &p = gr->GetPnt(ids[j]);
				float x0 = p.x, y0 = p.y;
				fwprintf(fp, j==0?L"(%.4g,%.4g)":L" -- (%.4g,%.4g)",0.01*x0,y0*0.01);
			}
			fwprintf(fp, L";\n");
		}
		else if(q.type==6 && mgl_isnum(q.p))	// text
		{
			const mglText &t = gr->GetPtx(q.n3);
			mreal dy = q.w*cos(q.p*M_PI/180)/100, dx = q.w*sin(q.p*M_PI/180)/100;
			int f,a;	mglGetStyle(t.stl.c_str(), &f, &a);
			std::string ss=cname;
			if((a&3)==0)	ss.append(",anchor=base west");
			if((a&3)==1)	ss.append(",anchor=base");
			if((a&3)==2)	ss.append(",anchor=base east");
//			if(f&MGL_FONT_ITAL)	ss.append(",font=\\itshape");
//			if(f&MGL_FONT_BOLD)	ss.append(",font=\\bfshape");
			if(t.text.find('\\')!=std::string::npos || t.text.find('{')!=std::string::npos || t.text.find('_')!=std::string::npos || t.text.find('^')!=std::string::npos)
				fwprintf(fp,L"\\draw[%s] (%.4g,%.4g) node[rotate=%.2g]{$%ls$};\n", ss.c_str(),x-dx,y-dy, -q.p, t.text.c_str());
			else
				fwprintf(fp,L"\\draw[%s] (%.4g,%.4g) node[rotate=%.2g]{%ls};\n", ss.c_str(),x-dx,y-dy, -q.p, t.text.c_str());
		}
	}
	fwprintf(fp, L"\\end{tikzpicture}\n");
	for(long i=0;i<gr->GetPrmNum();i++)
	{	mglPrim &q=gr->GetPrm(i);	if(q.type==-1)	q.type = 1;	}
	fclose(fp);
	setlocale(LC_NUMERIC, loc.c_str());

	// provide main file for viewing figure
	fp=fopen("mglmain.tex","wt");
	if(fp)
	{
		fprintf(fp, "%% this file just show figure\n");
		fprintf(fp, "\\documentclass{article}\n\\usepackage{tikz}\n");
		fprintf(fp, "\\usepackage[T2A]{fontenc}\n\\usepackage[utf8]{inputenc}\n");
		fprintf(fp, "\\begin{document}\n\\input{%s}\n\\end{document}\n",fname);
		fclose(fp);
	}
}
void MGL_EXPORT mgl_write_tex_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_tex(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
