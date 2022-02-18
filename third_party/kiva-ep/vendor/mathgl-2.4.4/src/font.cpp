/***************************************************************************
 * font.cpp is part of Math Graphic Library
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
#include <locale.h>
#include <ctype.h>
#include <wctype.h>
#ifdef WIN32
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#endif

#if !defined(__BORLANDC__) || (__CODEGEARC__ >=  0x0630)
#include <algorithm>
#else
#include <algorithm.h>
#endif

#include "mgl2/base.h"
#include "mgl2/font.h"
#include "def_font.cc"
#include "tex_table.cc"
//-----------------------------------------------------------------------------
//mglFont mglDefFont("nofont");
mglFont mglDefFont;
#define MGL_USE_H12	{if(h1<y1) y1=h1;	if(h2>y2) y2=h2;	h1=1e5;	h2=-1e5;}
//-----------------------------------------------------------------------------
size_t MGL_EXPORT_PURE mgl_wcslen(const wchar_t *str)
{
	long i=0;
	if(str)	while(str[i])	i++;
	return i;
}
//-----------------------------------------------------------------------------
long MGL_EXPORT_PURE mgl_internal_code(unsigned s, const std::vector<mglGlyphDescr> &glyphs)
{
	long i1=0,i2=glyphs.size()-1;
	wchar_t j = wchar_t(s & MGL_FONT_MASK);
	// let suppose that id[i]<id[i+1]
	while(i1<i2)
	{
		long i = (i1+i2)/2;
		if(j<glyphs[i].id)		i2 = i;
		else if(j>glyphs[i].id)	i1=i+1;
		else return i;
	}
	return j==glyphs[i2].id ? i2 : -1;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mglGetStyle(const char *how, int *font, int *align)
{
	bool col=false;
	if(align)	*align = 1;	// centering text by default
	if(!how || *how==0)	return col;
	// NOTE: no brightness for text color
	for(;*how && *how!=':';how++)
	{
		if(strchr(MGL_COLORS,*how))		col = true;
		if(*how=='{' && how[1]=='x')	col = true;
	}
	if(align)
	{
		*align = 1;
		if(strchr(how,'R'))	*align = 2;
//		if(strchr(how,'C'))	*align = 1;
		if(strchr(how,'L'))	*align = 0;
		if(strchr(how,'D'))	*align+= 4;
	}
	if(font)
	{
		*font = 0;
		if(strchr(how,'b'))	*font = *font|MGL_FONT_BOLD;
		if(strchr(how,'i'))	*font = *font|MGL_FONT_ITAL;
		if(strchr(how,'w'))	*font = *font|MGL_FONT_WIRE;
		if(strchr(how,'o'))	*font = *font|MGL_FONT_OLINE;
		if(strchr(how,'u'))	*font = *font|MGL_FONT_ULINE;
	}
	return col;
}
//-----------------------------------------------------------------------------
float mglFont::Puts(const char *str,const char *how,float c1,float c2) const
{
	int font=0, align=1;	float w=0;
	mglGetStyle(how,&font,&align);
	MGL_TO_WCS(str,w = Puts(wcs,font,align,c1,c2));
	return w;
}
//-----------------------------------------------------------------------------
float mglFont::Width(const char *str,const char *how, float *y1, float *y2) const
{
	float w=0;
	MGL_TO_WCS(str,w = Width(wcs,how,y1,y2));
	return w;
}
//-----------------------------------------------------------------------------
float mglFont::Puts(const wchar_t *str,const char *how,float c1,float c2) const
{
	int font=0, align=1;
	mglGetStyle(how,&font,&align);
	return Puts(str, font, align,c1,c2);
}
//-----------------------------------------------------------------------------
float mglFont::Width(const wchar_t *str,const char *how, float *y1, float *y2) const
{
	int font=0, align=1;
	float v1,v2;
	if(!y1)	y1 = &v1;
	if(!y2)	y2 = &v2;
	mglGetStyle(how,&font,&align);
	return Width(str, font, align, *y1, *y2);
}
//-----------------------------------------------------------------------------
float mglFont::Puts(const wchar_t *str,int font,int align, float c1,float c2) const
{
	if(GetNumGlyph()==0 || !str || *str==0)	return 0;
	float ww=0,w=0,h = (align&4) ? 500./fact[0] : 0, y1=0,y2=0;
	size_t size = mgl_wcslen(str)+1,num=0;
	if(parse)
	{
		unsigned *wcs = new unsigned[size], *buf=wcs;
		memcpy(wcs,str,size*sizeof(wchar_t));
		Convert(str, wcs);
		for(size_t i=0;wcs[i];i++)
		{
			if(wcs[i]=='\n')	// parse '\n' symbol
			{
				wcs[i]=0;	w = Puts(buf,0,0,1.f,0x10|font,c1,c2, y1,y2);	// find width
				Puts(buf,-w*(align&3)/2.f,-h - 660*num/fact[0],1.f,font,c1,c2, y1,y2);	// draw it really
				buf=wcs+i+1;	num++;	if(w>ww)	ww=w;
			}
// 			if(wcs[i]=='\\' && wcs[i+1]=='n' && (wcs[i+2]>' ' || wcschr(L"{}[]()!@#$%^&*/-?.,_=+\\\"", wcs[i+2])))	// parse '\n' symbol
// 			{
// 				wcs[i]=0;	w = Puts(buf,0,0,1.f,0x10|font,c1,c2);	// find width
// 				Puts(buf,-w*(align&3)/2.f,-h - 720.*num/fact[0],1.f,font,c1,c2);	// draw it really
// 				buf=wcs+i+2;	num++;	if(w>ww)	ww=w;
// 			}
		}
		// draw string itself
		w = Puts(buf,0,0,1.f,0x10|font,c1,c2, y1,y2);	// find width
		Puts(buf,-w*(align&3)/2.f,-h - 660*num/fact[0],1.f,font,c1,c2, y1,y2);	// draw it really
		if(w>ww)	ww=w;
		delete []wcs;
	}
	else
	{
		int s = (font/MGL_FONT_BOLD)&3;
		h *= fact[0]/fact[s];
		for(size_t i=0;i<size;i++)		// find width
		{
			long j = str[i]!=' ' ? Internal(str[i]) : Internal('!');
			if(j==-1)	continue;
			w+= GetWidth(s,j)/fact[s];
		}
		ww = w;		w *= -(align&3)/2.f;
		if(gr)	for(size_t i=0;i<size;i++)		// draw it
		{
			long j=0;	//Internal('!');
			if(str[i]!=' ')
			{
				j = Internal(str[i]);
				if(j==-1)	continue;
				gr->Glyph(w, -h, 1, (s+(font&MGL_FONT_WIRE))?4:0, j, c1+i*(c2-c1)/(size-1));
			}
			w+= GetWidth(s,j)/fact[s];
		}
	}
	return ww;
}
//-----------------------------------------------------------------------------
float mglFont::Width(const wchar_t *str,int font, int align, float &y1, float &y2) const
{
	if(GetNumGlyph()==0 || !str || *str==0)	return 0;
	float ww=0,w=0, h1=1e5,h2=-1e5;
	float h = (align&4) ? 500./fact[0] : 0;
	size_t size = mgl_wcslen(str)+1, num=0;
	y1=1e5;	y2=-1e5;
	if(parse)
	{
		unsigned *wcs = new unsigned[size], *buf=wcs;
		memcpy(wcs,str,size*sizeof(wchar_t));
		Convert(str, wcs);
		for(size_t i=0;wcs[i];i++)	if(wcs[i]=='\n')	// parse '\n' symbol
		{
			wcs[i]=0;	w = Puts(buf,0,0,1.,0x10|font,'k','k', h1,h2);	// find width
			h1 -= h+660*num/fact[0];	h2 -= h+660*num/fact[0];
			MGL_USE_H12
			buf=wcs+i+1;	if(w>ww)	ww=w;	num++;
		}
		w = Puts(buf,0,0,1.,0x10|font,'k','k', h1,h2);
		h1 -= h+660*num/fact[0];	h2 -= h+660*num/fact[0];
		MGL_USE_H12
		if(w<ww)	w=ww;
		delete []wcs;
	}
	else
	{
		int s = (font/MGL_FONT_BOLD)&3;
		for(size_t i=0;i<size;i++)
		{
			long j = str[i]!=' ' ? Internal(str[i]) : Internal('!');
			if(j==-1)	continue;
			w+= GetWidth(s,j)/fact[s];
			h1 = glyphs[j].y1[s]/fact[s];	h2 = glyphs[j].y2[s]/fact[s];
			MGL_USE_H12
		}
	}
	return w;
}
//-----------------------------------------------------------------------------
float mglFont::Height(int font) const
{
	if(GetNumGlyph()==0)	return 0;
	int s = (font/MGL_FONT_BOLD)&3;
	return 660/fact[s];
}
//-----------------------------------------------------------------------------
float mglFont::Height(const char *how) const
{
	if(GetNumGlyph()==0)	return 0;
	int s=0;
	if(how)
	{
		if(strchr(how,'b'))	s = s|1;
		if(strchr(how,'i'))	s = s|2;
	}
	return 660/fact[s];
}
//-----------------------------------------------------------------------------
/// Table of acents and its UTF8 codes
MGL_NO_EXPORT mglTeXsymb mgl_act_symb[] = {
	{0x02c6, L"hat"}, {0x02dc, L"tilde"}, {0x02d9, L"dot"}, {0x00a8, L"ddot"}, {0x20db, L"dddot"}, {0x20dc, L"ddddot"}, {0x02ca, L"acute"}, {0x02c7, L"check"}, {0x02cb, L"grave"}, {0x20d7, L"vec"}, {0x02c9, L"bar"}, {0x02d8, L"breve"},
	/*end*/{0, L"\0"}};
//-----------------------------------------------------------------------------
int MGL_LOCAL_PURE mgl_tex_symb_cmp(const void *a, const void *b)
{
	const mglTeXsymb *aa = (const mglTeXsymb *)a;
	const mglTeXsymb *bb = (const mglTeXsymb *)b;
	return wcscmp(aa->tex, bb->tex);
}
//-----------------------------------------------------------------------------
// parse LaTeX commands (mostly symbols and acents, and some font-style commands)
unsigned mglFont::Parse(const wchar_t *s) const
{
	unsigned res = unsigned(-2);		// Default is no symbol
	if(!s || !s[0])	return res;
	mglTeXsymb tst, *rts;
	tst.tex = s;
	rts = (mglTeXsymb *) bsearch(&tst, mgl_tex_symb, mgl_tex_num, sizeof(mglTeXsymb), mgl_tex_symb_cmp);
	if(rts)	return rts->kod;

	for(long k=0;mgl_act_symb[k].kod;k++)	// acents
		if(!wcscmp(s,mgl_act_symb[k].tex))
			return mgl_act_symb[k].kod | MGL_FONT_ZEROW;
	// arbitrary UTF symbol
	if(s[0]=='u' && s[1]=='t' && s[2]=='f')
	{	long k = wcstoul(s+3,NULL,16);	return wchar_t(k);	}
	// font/style changes for next symbol
	if(!wcscmp(s,L"big"))			res = unsigned(-5);
	else if(!wcscmp(s,L"frac"))		res = unsigned(-6);
	else if(!wcscmp(s,L"stack"))	res = unsigned(-7);
	else if(!wcscmp(s,L"overset"))	res = unsigned(-8);
	else if(!wcscmp(s,L"underset"))	res = unsigned(-9);
	else if(!wcscmp(s,L"stackr"))	res = unsigned(-10);
	else if(!wcscmp(s,L"stackl"))	res = unsigned(-11);
	else if(!wcscmp(s,L"sub"))		res = unsigned(-9);	//unsigned(-12);
	else if(!wcscmp(s,L"sup"))		res = unsigned(-8);	//unsigned(-13);
	else if(!wcscmp(s,L"textsc"))	res = unsigned(-14);	// new
	else if(!wcscmp(s,L"dfrac"))	res = unsigned(-15);
	else if(!wcscmp(s,L"b"))		res = MGL_FONT_BOLD;
	else if(!wcscmp(s,L"i"))		res = MGL_FONT_ITAL;
	else if(!wcscmp(s,L"bi"))		res = MGL_FONT_BOLD|MGL_FONT_ITAL;
	else if(!wcscmp(s,L"r"))		res = unsigned(-1);
	else if(!wcscmp(s,L"a"))		res = MGL_FONT_OLINE;
	else if(!wcscmp(s,L"u"))		res = MGL_FONT_ULINE;
	else if(!wcscmp(s,L"n"))		res = '\n';
	else if(!wcscmp(s,L"overline"))	res = MGL_FONT_OLINE;
	else if(!wcscmp(s,L"underline"))res = MGL_FONT_ULINE;
	else if(!wcscmp(s,L"textbf"))	res = MGL_FONT_BOLD;
	else if(!wcscmp(s,L"textit"))	res = MGL_FONT_ITAL;
	else if(!wcscmp(s,L"textrm"))	res = unsigned(-1);
	else if(!wcscmp(s,L"T2A"))		res = unsigned(-1);
	else if(!wcscmp(s,L"w"))		res = MGL_FONT_WIRE;
	else if(!wcscmp(s,L"wire"))		res = MGL_FONT_WIRE;
	else if(!wcsncmp(s,L"color",5))	res = MGL_COLOR_MASK + (0xff & s[5]);
	return res;
}
//-----------------------------------------------------------------------------
void mglFont::Convert(const wchar_t *str, unsigned *res) const
{
	size_t j=0;
	wchar_t s[128]=L"";		// TeX command and current char
	for(size_t i=0;str[i];i++)
	{
		wchar_t ch = str[i];
		if(ch=='\\')	// It can be TeX command
		{
			if(wcschr(L"{}_^\\@# ",str[i+1]))	// No, it is usual symbol
				res[j++] = str[++i];
			else		// Yes, it is TeX command
			{
				size_t i0=i+1, k;
				for(k=0;isalnum(str[++i]) && k<127;k++)	s[k] = str[i];
				s[k] = 0;
				size_t r = Parse(s);
				if(r==unsigned(-2))			// command not found, so use next symbol itself
				{	res[j++] = str[i0];	i = i0;	}
				else if(r)
				{
					res[j++] = r;
					if(str[i]>' ')	i--;
					if(str[i]==0)	break;
				}
			}
		}
		else if(ch=='-' && str[i+1]=='-')	{	res[j++] = 0x2212;	i++;	}
		else if(ch=='\b'){}
		else if(ch<=' ' && ch!='\n')	res[j++] = ' ';	// no \t at this moment :(
		else if(ch=='_')	res[j++] = MGL_FONT_LOWER;
		else if(ch=='^')	res[j++] = MGL_FONT_UPPER;
		else if(ch=='@')	res[j++] = MGL_FONT_UPPER|MGL_FONT_LOWER;
		else if(ch=='{')	res[j++] = unsigned(-3);
		else if(ch=='}')	res[j++] = unsigned(-4);
		else if(ch=='#' && str[i+1]>' ')
			res[j++] = MGL_COLOR_MASK + (0xff & str[++i]);	// TODO inline colors -- stack of RGBA colors + index
		else	res[j++] = ch;				// It is just symbol
	}
	res[j] = 0;
}
//-----------------------------------------------------------------------------
float mglFont::get_ptr(long &i,unsigned *str, unsigned **b1, unsigned **b2,float &w1,float &w2, float f1, float f2, int st) const
{
	static unsigned s1[2]={0,0}, s2[2]={0,0};
	i++;
	if(str[i]==unsigned(-3))
	{
		i++;	*b1 = str+i;
		for(long k=1;k>0 && str[i];i++)
		{
			if(str[i]==unsigned(-4))	k--;
			if(str[i]==unsigned(-3))	k++;
		}
		str[i-1]=0;
	}
	else	{	s1[0] = str[i];	*b1 = s1;	i++;	}
	if(str[i]==unsigned(-3))
	{
		i++;	*b2 = str+i;
		for(long k=1;k>0 && str[i];i++)
		{
			if(str[i]==unsigned(-4))	k--;
			if(str[i]==unsigned(-3))	k++;
		}
		str[i-1]=0;
	}
	else	{	s2[0] = str[i];	*b2 = s2;	i++;	}
	i--;
	float y1=0,y2=0;
	w1 = Puts(*b1, 0, 0, f1, 0x10|st,'k','k', y1,y2);
	w2 = Puts(*b2, 0, 0, f2, 0x10|st,'k','k', y1,y2);
	return w1>w2 ? w1 : w2;
}
//-----------------------------------------------------------------------------
void mglFont::draw_ouline(int st, float x, float y, float f, float g, float ww, float ccol) const
{
	if(st&MGL_FONT_OLINE)
		gr->Glyph(x,y+499*f/g, ww*g, (st&MGL_FONT_WIRE)?12:8, 0, ccol);
	if(st&MGL_FONT_ULINE)
		gr->Glyph(x,y-200*f/g, ww*g, (st&MGL_FONT_WIRE)?12:8, 0, ccol);
}
//-----------------------------------------------------------------------------
#define MGL_CLEAR_STYLE {st = style;	yy = y;	ff = f;	ccol=c1+dc*i;	a = (st/MGL_FONT_BOLD)&3;}
float mglFont::Puts(const unsigned *text, float x,float y,float f,int style,float c1,float c2, float &y1,float &y2) const
{
	if(GetNumGlyph()==0)	return 0;
	float w=0;				// string width
	int st = style;			// current style
	unsigned *b1, *b2;		// pointer to substring
	unsigned *str;			// string itself
	float yy=y, ff=f, ww, w1, w2, h1=1e5,h2=-1e5;
	int a = (st/MGL_FONT_BOLD)&3;
	long i;
	for(i=0;text[i];i++);
	float dc=i>1?(c2-c1)/(i-1):0;
	str = new unsigned[i+1];
	memcpy(str,text,(i+1)*sizeof(unsigned));

	float ccol = 0;
	for(long i=0;str[i];i++)
	{
		ccol = ccol<0?ccol:c1+dc*i;
		unsigned s = str[i];		ww = 0;
		if(s==unsigned(-3))	// recursion call here for {}-block
		{
			i++;	b1 = str+i;
			for(long k=1;k>0 && str[i];i++)
			{
				if(str[i]==unsigned(-4))	k--;
				if(str[i]==unsigned(-3))	k++;
			}
			str[i-1]=0;	i--;
			ww = Puts(b1, x, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
		else if(s=='\n')	// newline
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff, st);
			Puts(b1, x+(ww-w1)/2, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy-660*ff/fact[a], ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-9))	// underset or sub
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff/4, st);
			Puts(b1, x+(ww-w1)/2, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy-175*ff/fact[a], ff/3, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-8))	// overset or sup
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff/4, st);
			Puts(b1, x+(ww-w1)/2, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy+400*ff/fact[a], ff/3, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h2,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
/*		else if(s==unsigned(-12))	// sub	// NOTE: unused because is the same as \underset now
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff/4, st);
			Puts(b1, x+(ww-w1)/2, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol);
			Puts(b2, x+(ww-w2)/2, yy-250*ff/fact[a], ff/4, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol);
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}*/
/*		else if(s==unsigned(-13))	// sup	// NOTE: unused because is the same as \overset now
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff/4, st);
			Puts(b1, x+(ww-w1)/2, yy, ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol);
			Puts(b2, x+(ww-w2)/2, yy+450*ff/fact[a], ff/4, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol);
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}*/
		else if(s==unsigned(-11))	// stackl
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff*0.45, ff*0.45, st);
			Puts(b1, x, yy+250*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x, yy-110*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-10))	// stacr
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff*0.45, ff*0.45, st);
			Puts(b1, x+(ww-w1), yy+250*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2), yy-110*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-7))	// stack
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff*0.45, ff*0.45, st);
			Puts(b1, x+(ww-w1)/2, yy+250*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy-110*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-6))	// frac
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff*0.45, ff*0.45, st);
			Puts(b1, x+(ww-w1)/2, yy+250*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy-60*ff/fact[a], ff*0.45, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
			{
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
				gr->Glyph(x,y+150*f/fact[a], ww*fact[a], (st&MGL_FONT_WIRE)?12:8, 0, ccol);
			}
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-15))	// dfrac
		{
			ww = get_ptr(i, str, &b1, &b2, w1, w2, ff, ff, st);
			Puts(b1, x+(ww-w1)/2, yy+315*ff/fact[a], ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			Puts(b2, x+(ww-w2)/2, yy-405*ff/fact[a], ff, (st&(~MGL_FONT_OLINE)&(~MGL_FONT_ULINE)), ccol,ccol, h1,h2);
			MGL_USE_H12
			if(gr && !(style&0x10))	// add under-/over- line now
			{
				draw_ouline(st,x,y,f,fact[a],ww,ccol);
				gr->Glyph(x,y+150*f/fact[a], ww*fact[a], (st&MGL_FONT_WIRE)?12:8, 0, ccol);
			}
			MGL_CLEAR_STYLE
		}
		else if(s==unsigned(-4))	MGL_CLEAR_STYLE	// should be never here but if I miss sth ...
		else if(s==unsigned(-14))	// script symbols
		{
			long k=1;
			if(str[i+1]==unsigned(-3))	for(long j=i+2;k>0 && str[j];j++)
			{
				if(str[j]==unsigned(-3))	k++;
				if(str[j]==unsigned(-4))	k--;
				if(iswlower(str[j]))
					str[j] = MGL_FONT_UPPER|MGL_FONT_LOWER|towupper(str[j]);
			}
		}
		else if(s==unsigned(-5))	// large symbol
			ff *= 1.5;
		else if(s==unsigned(-1))	// set normal font
			st = style & MGL_FONT_ROMAN;
		else if((s&MGL_COLOR_MASK)==MGL_COLOR_MASK)	// color specification
			ccol = -float(s & 0xff);	// TODO inline colors -- make textures
		else
		{
			unsigned ss = s&MGL_FONT_MASK;
			if(ss)	// draw symbol (glyph)
			{
				long j = Internal('!');
				float dx=0;
				if(ss>' ')
				{
					j = Internal(ss);
					if(j==-1)	continue;
					if(s & MGL_FONT_ZEROW)
					{
						long j=1;
						yy += 100*ff/fact[a];
						while(str[i+j]>=unsigned(-15))	j++;
						unsigned sn = str[i+j];
						if(sn<unsigned(-15) && (sn&MGL_FONT_MASK)>' ')	// specially center
						{
							long jj = Internal(sn&MGL_FONT_MASK);
							dx = jj<0?0:0.75*ff*(GetWidth(a,jj)-GetWidth(a,j))/fact[a];
							if(dx<0)	dx=0;
						}
					}
					h1 = yy+ff*glyphs[j].y1[a]/fact[a];	h2 = yy+ff*glyphs[j].y2[a]/fact[a];
					MGL_USE_H12
					if(gr && !(style&0x10))
					{
						if(st & MGL_FONT_WIRE)	gr->Glyph(x+dx,yy,ff,a+4,j,ccol);
						else					gr->Glyph(x+dx,yy,ff,a,j,ccol);
					}
				}
				ww = j>=0?ff*GetWidth(a,j)/fact[a]:0;
				if(gr && !(style&0x10))	// add under-/over- line now
					draw_ouline(st,x,y,f,fact[a],ww,ccol);
				if(s & MGL_FONT_ZEROW)	ww = 0;
				MGL_CLEAR_STYLE
			}
			// apply new styles
			if(s/MGL_FONT_BOLD)	st = st | (s & MGL_FONT_STYLE);
			a = (st/MGL_FONT_BOLD)&3;
			ss = (s/MGL_FONT_UPPER)%4;
			if(ss)
			{
				if(ss==1)		{	ff *=0.6;	yy += 250*ff/fact[a];	}	// =  500*0.4
				else if(ss==2)	{	ff *=0.6;	yy -=  130*ff/fact[a];	}	// = -500*0.16
				else if(ss==3)	{	ff *=0.6;	yy +=  60*ff/fact[a];	}	// =  500*0.12
			}
		}
		x += ww;	w += ww;
	}
	delete []str;
	return w;
}
//-----------------------------------------------------------------------------
// copy normal style as default for other styles
void mglFont::main_copy()
{
#pragma omp parallel for
	for(long i=0;i<long(glyphs.size());i++)
	{
		mglGlyphDescr &g = glyphs[i];
		g.numl[1] = g.numl[2] = g.numl[3] = g.numl[0];
		g.numt[1] = g.numt[2] = g.numt[3] = g.numt[0];
		g.ln[1] = g.ln[2] = g.ln[3] = g.ln[0];
		g.tr[1] = g.tr[2] = g.tr[3] = g.tr[0];
		g.width[1] = g.width[2] = g.width[3] = g.width[0];
	}
}
//-----------------------------------------------------------------------------
bool mglFont::read_def()
{
	// copy default factor for other font styles;
	fact[1] = fact[2] = fact[3] = fact[0] = mgl_fact*mgl_fgen;
	Buf = new short[mgl_cur];	// prealocate buffer
	memset(Buf,0,mgl_cur*sizeof(short));
	// now allocate memory for all fonts
	mem_alloc(mgl_numg);
	// and load symbols itself
#ifndef WIN32	// win32 don't initialized threads before main()
#pragma omp parallel for
#endif
	for(size_t i=0;i<mgl_numg;i++)
	{
		mglGlyphDescr &g = glyphs[i];
		g.id = mgl_gen_fnt[i][0];
		g.width[0] = g.width[1] = g.width[2] = g.width[3] = mgl_gen_fnt[i][1];
		g.numl[0] = g.numl[1] = g.numl[2] = g.numl[3] = mgl_gen_fnt[i][2];
		g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = mgl_gen_fnt[i][3];
		g.numt[0] = g.numt[1] = g.numt[2] = g.numt[3] = mgl_gen_fnt[i][4];
		g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = mgl_gen_fnt[i][5];
	}
	memcpy(Buf, mgl_buf_fnt, mgl_cur*sizeof(short));
	numb = mgl_cur;
	return true;
}
//-----------------------------------------------------------------------------
bool mglFont::read_data(const char *fname, int s, std::vector<short> &buf, std::vector<mglGlyphDescr> &extra)
{
	gzFile fp;
	char str[256];
	int n, tmpw, tmpnl, tmpnt, retVal;
	unsigned ss, tmpi, tmppl, tmppt;
	fp = gzopen(fname,"r");	if(!fp)	return false;	// false if no file
	// first string is comment (not used), second string have information
	if(!gzgets(fp,str,256) || strncmp(str,"# font",6) || !gzgets(fp,str,256))
	{	gzclose(fp);	return false;	}
	retVal = sscanf(str, "%d%f%u", &n, fact+s, &ss);
	//Check sscanf read all data  (3 items)
	if(retVal != 3)	{	gzclose(fp);	return false;	}

	for(int i=0;i<n;i++)
	{
		gzgets(fp,str,256);
		retVal = sscanf(str,"%u%d%d%u%d%u", &tmpi, &tmpw, &tmpnl, &tmppl, &tmpnt, &tmppt);
		if(retVal != 6)	{	gzclose(fp);	buf.clear();	return false;	}
		long j=Internal(unsigned(tmpi));
		if(j>=0)	// known symbol
		{
			mglGlyphDescr &g = glyphs[j];	g.width[s] = tmpw;
			g.ln[s] = -1-tmppl;		g.tr[s] = -1-tmppt;
			g.numl[s] = tmpnl;		g.numt[s] = tmpnt;
		}
		else
		{
			mglGlyphDescr g;	g.id = tmpi;
			g.width[0] = g.width[1] = g.width[2] = g.width[3] = tmpw;
			g.numl[0] = g.numl[1] = g.numl[2] = g.numl[3] = tmpnl;
			g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = -1-tmppl;
			g.numt[0] = g.numt[1] = g.numt[2] = g.numt[3] = tmpnt;
			g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = -1-tmppt;
#pragma omp critical
			extra.push_back(g);
		}
	}
	for(unsigned i=0;i<ss;i++)
	{
		for(int j=0;j<256;j++)	if((str[j] = gzgetc(fp))<=' ')	break;
		buf.push_back(atoi(str));
	}
	gzclose(fp);		// finish wire normal font
	return true;
}
//-----------------------------------------------------------------------------
bool mglFont::read_main(const char *fname, std::vector<short> &buf)
{
	gzFile fp;
	int tmpi, tmpw, tmpnl, tmpnt;
	unsigned s, tmppl, tmppt,numg;
	char str[256];

	fp = gzopen(fname,"r");	if(!fp)	return false;	// this font must be in any case
	// first string is comment (not used), second string have information
	if(!gzgets(fp,str,256) || strncmp(str,"# font",6) || !gzgets(fp,str,256))
	{	gzclose(fp);	return false;	}
	sscanf(str, "%u%f%u", &numg, fact, &s);
	fact[1] = fact[2] = fact[3] = fact[0];	// copy default factor for other font styles;
	// now allocate memory for all fonts
	mem_alloc(numg);
	// and load symbols itself
	for(size_t i=0;i<numg;i++)
	{
		gzgets(fp,str,256);
		sscanf(str,"%d%d%d%u%d%u", &tmpi, &tmpw, &tmpnl, &tmppl, &tmpnt, &tmppt);
		mglGlyphDescr &g = glyphs[i];	g.id = tmpi;
		g.width[0] = g.width[1] = g.width[2] = g.width[3] = tmpw;
		g.numl[0] = g.numl[1] = g.numl[2] = g.numl[3] = tmpnl;
		g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = tmppl;
		g.numt[0] = g.numt[1] = g.numt[2] = g.numt[3] = tmpnt;
		g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = tmppt;
	}
	for(unsigned i=0;i<s;i++)
	{
		for(int j=0;j<256;j++)	if((str[j] = gzgetc(fp))<=' ')	break;
		buf.push_back(atoi(str));
	}
	gzclose(fp);	// finish wire normal font
	return true;
}
//-----------------------------------------------------------------------------
void mglFont::FillY12()
{
#pragma omp parallel
	for(long i=0;i<long(glyphs.size());i++)
	{
		for(int s=0;s<4;s++)
		{
			int y1=0xffff, y2=-0xffff, nl=glyphs[i].numl[s];
			const short *ln = Buf+glyphs[i].ln[s];
			for(long k=0;k<nl;k++)
			{
				int y = ln[2*k+1];
				if(y==0x3fff)	continue;	// line breakthrough
				if(y<y1)	y1=y;
				if(y>y2)	y2=y;
			}
			glyphs[i].y1[s] = y1;
			glyphs[i].y2[s] = y2;
		}
	}
}
//-----------------------------------------------------------------------------
size_t mglFont::SaveBin(const char *fname)
{
	FILE *fp = fopen(fname,"wb");
	if(!fp)	return 0;
	size_t sum=0;
	fwrite(&numb,sizeof(size_t),1,fp);	sum += sizeof(size_t);
	fwrite(fact,sizeof(float),4,fp);	sum += sizeof(float)*4;
	fwrite(Buf,sizeof(short),numb,fp);	sum += sizeof(short)*numb;
	size_t len = glyphs.size();
	fwrite(&len,sizeof(size_t),1,fp);	sum += sizeof(long);
	fwrite(&(glyphs[0]),sizeof(mglGlyphDescr),len,fp);	sum += sizeof(mglGlyphDescr)*len;
	fclose(fp);	return sum;
}
//-----------------------------------------------------------------------------
bool mglFont::LoadBin(const char *base, const char *path)
{
	Clear();	// first clear old
	if(!path)	path = MGL_FONT_PATH;
	char str[256], sep='/';
	if(base && strstr(base,".vfmb"))
		snprintf(str,256,"%s%c%s",path,sep,base);
	else
		snprintf(str,256,"%s%c%s.vfmb",path,sep,base?base:"");
	str[255]=0;
	FILE *fp = fopen(str,"rb");		if(!fp)	return false;
	size_t s, len;
	bool res = true;
	s = fread(&numb,sizeof(size_t),1,fp);
	if(s<1)	res = false;
	s = fread(fact,sizeof(float),4,fp);
	if(s<4)	res = false;
	Buf = new short[numb];
	s = fread(Buf,sizeof(short),numb,fp);
	if(s<numb)	res = false;
	s = fread(&len,sizeof(size_t),1,fp);
	if(s<1)	res = false;
	if(res)
	{
		glyphs.clear();	glyphs.resize(len);
		s = fread(&(glyphs[0]),sizeof(mglGlyphDescr),len,fp);
		if(s<len)	res = false;
	}
//	if(!res)	Clear();
	fclose(fp);
	if(res)	FillY12();
	return res;
}
//-----------------------------------------------------------------------------
bool mglFont::Load(const char *base, const char *path)
{
#ifdef WIN32
	char *buf=0, sep='\\';
#else
	char *buf=0, sep='/';
#endif
	char str[256];
	std::string loc = setlocale(LC_NUMERIC,"C");
	if(!path)	path = MGL_FONT_PATH;
	if(base && *base)	// try to load binary files
	{
		buf = new char[strlen(base)+1];
		strcpy(buf,base);
		if(strchr(buf,sep))
		{
			int i;
			for(i=strlen(buf);i>=0 && buf[i]!=sep;i--);
			path = buf;		buf[i]=0;	base = buf+i+1;
		}
		if(LoadBin(base,path))
		{	delete []buf;	return true;	}
	}
	Clear();	// first clear old

	std::string sbase;
	if(base && strstr(base,".vfm"))	// bypass user-specified extension in base name
	{
		size_t len = strlen(base);
		sbase = std::string(base).substr(0,len-4);
		base = sbase.c_str();
	}
	snprintf(str,256,"%s%c%s.vfm",path,sep,base?base:"");	str[255]=0;
	std::vector<short> norm, bold, ital, both;
	if(!(base && *base) || !read_main(str,norm))
	{
		read_def();	setlocale(LC_NUMERIC,loc.c_str());
		if(buf)	delete []buf;
		FillY12();
		return true;
	}
	fact[1] = fact[2] = fact[3] = fact[0];

	std::vector<mglGlyphDescr> ex_b,ex_i,ex_bi;
#pragma omp parallel sections
	{
		//================== bold ===========================================
#pragma omp section
		{	char str[256];	snprintf(str,256,"%s%c%s_b.vfm",path,sep,base);	// this file may absent
			str[255]=0;	read_data(str, 1, bold, ex_b);	}

		//================== italic =========================================
#pragma omp section
		{	char str[256];	snprintf(str,256,"%s%c%s_i.vfm",path,sep,base);
			str[255]=0;	read_data(str, 2, ital, ex_i);	}

		//================== bold-italic ====================================
#pragma omp section
		{	char str[256];	snprintf(str,256,"%s%c%s_bi.vfm",path,sep,base);
			str[255]=0;	read_data(str, 3, both, ex_bi);	}
	}

	// now collect data
	numb = norm.size()+bold.size()+ital.size()+both.size();
	Buf = new short[numb];
	memcpy(Buf,&norm[0],norm.size()*sizeof(short));
	long cur = norm.size(), len = long(bold.size());
	if(bold.size()>0)
		memcpy(Buf+cur,&bold[0],bold.size()*sizeof(short));
#pragma omp parallel for
	for(long i=0;i<long(GetNumGlyph());i++)	if(glyphs[i].ln[1]<0)
	{	glyphs[i].ln[1] = cur-1-glyphs[i].ln[1];	glyphs[i].tr[1] = cur-1-glyphs[i].tr[1];	}
#pragma omp parallel for
	for(long i=0;i<long(ex_b.size());i++)	if(ex_b[i].ln[1]<0)
	{
		mglGlyphDescr &g = ex_b[i];
		g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = cur-1-g.ln[1];
		g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = cur-1-g.tr[1];
	}
	cur += len;		len = long(ital.size());
	if(ital.size()>0)
		memcpy(Buf+cur,&ital[0],ital.size()*sizeof(short));
#pragma omp parallel for
	for(long i=0;i<long(GetNumGlyph());i++)	if(glyphs[i].ln[2]<0)
	{	glyphs[i].ln[2] = cur-1-glyphs[i].ln[2];	glyphs[i].tr[2] = cur-1-glyphs[i].tr[2];	}
#pragma omp parallel for
	for(long i=0;i<long(ex_i.size());i++)	if(ex_i[i].ln[2]<0)
	{
		mglGlyphDescr &g = ex_i[i];
		g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = cur-1-g.ln[2];
		g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = cur-1-g.tr[2];
	}
	cur += len;
	if(both.size()>0)
		memcpy(Buf+cur,&both[0],both.size()*sizeof(short));
#pragma omp parallel for
	for(long i=0;i<long(GetNumGlyph());i++)	if(glyphs[i].ln[3]<0)
	{	glyphs[i].ln[3] = cur-1-glyphs[i].ln[3];	glyphs[i].tr[3] = cur-1-glyphs[i].tr[3];	}
#pragma omp parallel for
	for(long i=0;i<long(ex_bi.size());i++)	if(ex_bi[i].ln[3]<0)
	{
		mglGlyphDescr &g = ex_bi[i];
		g.ln[0] = g.ln[1] = g.ln[2] = g.ln[3] = cur-1-g.ln[3];
		g.tr[0] = g.tr[1] = g.tr[2] = g.tr[3] = cur-1-g.tr[3];
	}
	// now add missing symbols
	if(ex_b.size()==0)	ex_b = ex_i;
	else
	{
		for(size_t i=0;i<ex_i.size();i++)	// add from ex_i
		{
			long j = mgl_internal_code(ex_i[i].id, ex_b);
			if(j>=0)	// known symbol
			{
				mglGlyphDescr &g = ex_b[j], &f = ex_i[i];
				g.width[2] = f.width[2];
				g.ln[2] = f.ln[2];		g.tr[2] = f.tr[2];
				g.numl[2] = f.numl[2];	g.numt[2] = f.numt[2];
			}
			else	ex_b.push_back(ex_i[i]);
		}
		std::sort(ex_b.begin(),ex_b.end());
	}
	if(ex_b.size()==0)	ex_b = ex_bi;
	else
	{
		for(size_t i=0;i<ex_bi.size();i++)	// add from ex_bi
		{
			long j = mgl_internal_code(ex_bi[i].id, ex_b);
			if(j>=0)	// known symbol
			{
				mglGlyphDescr &g = ex_b[j], &f = ex_bi[i];
				g.width[2] = f.width[3];
				g.ln[2] = f.ln[3];		g.tr[2] = f.tr[3];
				g.numl[2] = f.numl[3];	g.numt[2] = f.numt[3];
			}
			else	ex_b.push_back(ex_bi[i]);
		}
		std::sort(ex_b.begin(),ex_b.end());
	}
	if(ex_b.size()>0)
	{
		glyphs.reserve(ex_b.size());	// preallocate memory
		glyphs.insert(glyphs.end(), ex_b.begin(), ex_b.end());
		std::sort(glyphs.begin(),glyphs.end());
	}

	// Finally normalize all factors
	fact[0] *= mgl_fgen;	fact[1] *= mgl_fgen;
	fact[2] *= mgl_fgen;	fact[3] *= mgl_fgen;
	FillY12();
	setlocale(LC_NUMERIC,loc.c_str());
	if(buf)	delete []buf;
	return true;
}
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHREAD
pthread_mutex_t mutexRnd;
#endif
//-----------------------------------------------------------------------------
float mgl_cos[360];
void static mgl_init()
{
	mgl_textdomain(NULL,"");
#if MGL_HAVE_PTHREAD
	pthread_mutex_init(&mutexRnd,0);
#endif
#ifndef WIN32	// win32 don't initialized threads before main()
#pragma omp parallel for
#endif
	for(long i=0;i<360;i++)	mgl_cos[i] = cos(i*M_PI/180.);
}
//-----------------------------------------------------------------------------
mglFont::mglFont(const char *name, const char *path)
{
	parse = true;	gr=0;	Buf=0;
//	if(this==&mglDefFont)	Load(name, path);	else	Copy(&mglDefFont);
	if(name && *name)	Load(name, path);
	else if(this!=&mglDefFont)	Copy(&mglDefFont);
	else
	{
		mgl_init();		// NOTE: this call init function for the library.
		Load(MGL_DEF_FONT_NAME,0);
	}
}
mglFont::~mglFont()	{	if(Buf)	delete []Buf;	}
void mglFont::Restore()	{	Copy(&mglDefFont);	}
//-----------------------------------------------------------------------------
void mglFont::Clear()
{
//#pragma omp critical(font)
	{	if(Buf)	delete []Buf;	Buf=0;	glyphs.clear();	}
}
//-----------------------------------------------------------------------------
void mglFont::Copy(mglFont *f)
{
	if(!f || f==this)	return;
#pragma omp critical(font)
	{	if(Buf)	delete []Buf;	Buf=0;	}
	// copy scale factors
	fact[0]=f->fact[0];	fact[1]=f->fact[1];	fact[2]=f->fact[2];	fact[3]=f->fact[3];
	// copy symbols descriptions
	numb = f->numb;	Buf = new short[numb];	memcpy(Buf, f->Buf, numb*sizeof(short));
	// copy symbol parameters
	glyphs.resize(f->glyphs.size());
	memcpy(&glyphs[0],&(f->glyphs)[0],glyphs.size()*sizeof(mglGlyphDescr));
}
//-----------------------------------------------------------------------------
long MGL_EXPORT mgl_check_tex_table()
{
	size_t i=0;	while(mgl_tex_symb[i].tex[0])	i++;
	long res = 0;
	if(mgl_tex_num!=i)
	{	printf("real=%zu, set=%zu\n",i,mgl_tex_num);	res = -1;	}
	for(i=0;mgl_tex_symb[i].tex[0];i++)
	{
		mglTeXsymb tst, *rts;	tst.tex = mgl_tex_symb[i].tex;
		rts = (mglTeXsymb *) bsearch(&tst, mgl_tex_symb, mgl_tex_num, sizeof(mglTeXsymb), mgl_tex_symb_cmp);
		if(!rts)
		{	printf(_("Bad '%ls' at %zu\n"),mgl_tex_symb[i].tex,i);	res = 1+i;	}
	}
	return res;
}
//---------------------------------------------------------------------------
bool static test_transl(const char *p)
{
	if(!p)	return false;
#if MGL_USE_GETTEXT
	std::string f = std::string(p) + "/ru/LC_MESSAGES/mathgl.mo";
	FILE *fp = fopen(f.c_str(),"r");
	if(fp)
	{
		bindtextdomain("mathgl", p);
		textdomain("mathgl");
		fclose(fp);	return true;
	}
#endif
	return false;
}
void MGL_EXPORT mgl_textdomain(const char *argv0, const char *loc)
{
	static const char *argv=NULL;
	if(!argv0)	argv0=argv;	else	argv=argv0;
	setlocale(LC_ALL, loc);	setlocale(LC_NUMERIC, "C");
#if MGL_USE_GETTEXT
	if(!test_transl(MGL_INSTALL_DIR"/share/locale/"))
		if(!test_transl("/usr/share/locale/"))
			if(!test_transl("/usr/local/share/locale/"))
			{
				char* cwd = getcwd(NULL,0);
				if(!test_transl(cwd))
				{
					free(cwd);
					const char *f = argv0?strrchr(argv0,'/'):NULL;
#ifdef WIN32
					if(!f)	f = argv0?strrchr(argv0,'\\'):NULL;
#endif
					if(f)
					{
						std::string p(argv0,f-argv0);
						if(!test_transl(p.c_str()))
							return;
					}
					else	return;
				}
				else	if(cwd)	free(cwd);
			}
#endif
}
void MGL_EXPORT mgl_textdomain_(const char *locale, int l)
{	char *s=new char[l+1];	memcpy(s,locale,l);	s[l]=0;
	mgl_textdomain(NULL,s);	delete []s;	}
//---------------------------------------------------------------------------
