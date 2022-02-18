/***************************************************************************
 * export_3d.cpp is part of Math Graphic Library
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
#include <time.h>
#include <stdarg.h>
#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
#define _Gr_	((mglCanvas *)(gr))
int MGL_NO_EXPORT mgl_tga_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_pnga_save(const char *fname, int w, int h, unsigned char **p);
void MGL_NO_EXPORT mgl_printf(void *fp, bool gz, const char *str, ...);
std::string MGL_NO_EXPORT mgl_sprintf(const char *str, ...);
//-----------------------------------------------------------------------------
void mglTexture::GetRGBA(unsigned char *f) const
{
	for(long i=255;i>=0;i--)
	{
		mglColor c1 = col[2*i], c2 = col[2*i+1];
		for(long j=0;j<256;j++)
		{
			long i0 = 4*(j+256*i);
			mglColor c = c1 + (c2-c1)*(j/255.);
			f[i0]   = int(255*c.r);
			f[i0+1] = int(255*c.g);
			f[i0+2] = int(255*c.b);
			f[i0+3] = int(255*c.a);
		}
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_obj_glyph_old(HMGL gr, const mglPrim &q, const mglPnt &p, FILE *fp)
{
	mreal f = q.p/2, dx=p.u/2, dy=p.v/2, x,y;
	mreal c=q.s*cos(q.w*M_PI/180), s=-q.s*sin(q.w*M_PI/180);
	if(mgl_isnan(q.s))	c=s=0;
	double b[4] = {c,-s, s,c};
	long i=q.n1+1;

	const mglGlyph &g = gr->GetGlf(q.n4);
	const mreal dd = 0.004;
	if(q.n3&8)
	{
		fprintf(fp,"v %g %g %g\n",p.x+b[0]*dx+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
		fprintf(fp,"v %g %g %g\n",p.x+b[0]*dx+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);
		fprintf(fp,"v %g %g %g\n",p.x+b[0]*(dx+f)+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
		fprintf(fp,"v %g %g %g\n",p.x+b[0]*(dx+f)+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);
		if(!(q.n3&4))	// glyph_line(p,f,true, d);
		{
			fprintf(fp,"f -1/%ld -3/%ld -2/%ld\n",i,i,i);
			fprintf(fp,"f -4/%ld -2/%ld -3/%ld\n",i,i,i);
		}
		else	// glyph_line(p,f,false, d);
		{
			fprintf(fp,"l -1/%ld -2/%ld\n",i,i);
			fprintf(fp,"l -3/%ld -4/%ld\n",i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n",i,i);
			fprintf(fp,"l -2/%ld -4/%ld\n",i,i);
		}
	}
	else
	{
		if(!(q.n3&4))	// glyph_fill(p,f,g, d);
		{
			for(long ik=0;ik<g.nt;ik++)
			{
				x = dx+f*g.trig[6*ik];		y = dy+f*g.trig[6*ik+1];
				fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
				x = dx+f*g.trig[6*ik+2];	y = dy+f*g.trig[6*ik+3];
				fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
				x = dx+f*g.trig[6*ik+4];	y = dy+f*g.trig[6*ik+5];
				fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
				fprintf(fp,"f -1/%ld -3/%ld -2/%ld\n",i,i,i);
			}
		}
		else	// glyph_wire(p,f,g, d);
		{
			long il=0;
			for(long ik=0;ik<g.nl;ik++)
			{
				x = g.line[2*ik];	y = g.line[2*ik+1];
				if(x==0x3fff && y==0x3fff)	// line breakthrough
				{	il = ik+1;	continue;	}
				else if(ik==g.nl-1 || (g.line[2*ik+2]==0x3fff && g.line[2*ik+3]==0x3fff))
				{	// enclose the circle. May be in future this block should be commented
					x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
					fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
					x = dx+f*g.line[2*il];		y = dy+f*g.line[2*il+1];
					fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
				}
				else
				{	// normal line
					x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
					fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
					x = dx+f*g.line[2*ik+2];	y = dy+f*g.line[2*ik+3];
					fprintf(fp,"v %g %g %g\n",p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
				}
				fprintf(fp,"l -1/%ld -2/%ld\n",i,i);
			}
		}
	}
}
//-----------------------------------------------------------------------------
/* M.Vidassov take/move it into src/obj.cpp */
void MGL_EXPORT mgl_obj_prim_old(HMGL gr, const mglPrim &q, const mglPnt &p, FILE *fp, mreal size)
{
	char type = q.n4;	mreal ss=size;
	long i=q.n1+1, n1=q.n1+1,n2=q.n2+1,n3=q.n3+1,n4=q.n4+1;
	switch(q.type)
	{
	case 0:
		if(!strchr("xsSoO",type))	ss *= 1.1;
		if(type=='.' || ss==0)	fprintf(fp,"p %ld\n", i);
		else	switch(type)
		{
		case 'P':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);
		case '+':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);	break;
		case 'X':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -4/%ld\n", i,i);	break;
		case 'x':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -4/%ld\n", i,i);	break;
		case 'S':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"f -4/%ld -3/%ld -2/%ld -1/%ld\n",i,i,i,i);	break;
		case 's':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);	break;
		case 'D':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"f -4/%ld -3/%ld -2/%ld -1/%ld\n",i,i,i,i);	break;
		case 'd':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);	break;
		case 'Y':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+0.8*ss,p.y+0.6*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.8*ss,p.y+0.6*ss,p.z);
			fprintf(fp,"l -3/%ld %ld/%ld\n", i,i,i);
			fprintf(fp,"l -2/%ld %ld/%ld\n", i,i,i);
			fprintf(fp,"l -1/%ld %ld/%ld\n", i,i,i);	break;
		case '*':
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"v %g %g %g\n",p.x+0.6*ss,p.y+0.8*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.6*ss,p.y-0.8*ss,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"v %g %g %g\n",p.x+0.6*ss,p.y-0.8*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.6*ss,p.y+0.8*ss,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);		break;
		case 'T':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '^':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'V':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case 'v':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'L':
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '<':
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'R':
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '>':
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'O':
			for(long j=0;j<=20;j++)
				fprintf(fp,"v %g %g %g\n",p.x+ss*mgl_cos[(j*36)%360],p.y+ss*mgl_cos[(270+j*36)%360],p.z);
			for(long j=0;j<20;j++)
				fprintf(fp,"f %ld/%ld %ld/%ld %ld/%ld\n", j-21,i, j-20,i, i,i);
			break;
		case 'C':	fprintf(fp,"p %ld\n", i);
		case 'o':
			for(long j=0;j<=20;j++)
				fprintf(fp,"v %g %g %g\n",p.x+ss*mgl_cos[(j*36)%360],p.y+ss*mgl_cos[(270+j*36)%360],p.z);
			for(long j=0;j<20;j++)
				fprintf(fp,"l %ld/%ld %ld/%ld\n", j-21,i, j-20,i);
			break;
		}
		break;
	case 1:	fprintf(fp,"l %ld/%ld %ld/%ld\n", n1,n1, n2,n2);	break;
	case 2:	fprintf(fp,"f %ld/%ld %ld/%ld %ld/%ld\n", n1,n1, n2,n2, n3,n3);	break;
	case 3:	fprintf(fp,"f %ld/%ld %ld/%ld %ld/%ld\n", n1,n1, n2,n2, n3,n3);
		fprintf(fp,"f %ld/%ld %ld/%ld %ld/%ld\n", n2,n2, n4,n4, n3,n3);	break;
	case 4:	mgl_obj_glyph_old(gr,q,p,fp);		break;
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_obj_old(HMGL gr, const char *fname,const char *descr, int use_png)
{
	if(gr->GetPrmNum()==0)	return;	// nothing to do
	long m1=0,m2=0;
	for(size_t i=0;i<gr->Grp.size();i++)	// prepare array of indirect indexing
	{	long m = gr->Grp[i].Id;	if(m<m1) m1=m;	if(m>m2) m2=m;	}
	long *ng = new long[m2-m1+1];
	for(size_t i=0;i<gr->Grp.size();i++)	ng[gr->Grp[i].Id-m1] = i;
	for(long i=0;i<gr->GetPrmNum();i++)	// collect data for groups
	// it is rather expensive (extra 4b per primitive) but need for export to 3D
	{
		long m = gr->GetPrm(i,false).id-m1;
		if(m>=0 && m<m2-m1+1)	gr->Grp[ng[m]].p.push_back(i);
	}
	delete []ng;

	size_t len=strlen(fname),ntxt=gr->GetTxtNum();
	FILE *fp=fopen(fname,"wt");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	// vertices definition
	const std::string loc = setlocale(LC_NUMERIC, "C");
	fprintf(fp,"# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	for(long i=0;i<gr->GetPntNum();i++)
	{
		const mglPnt &pp = gr->GetPnt(i);
		fprintf(fp,"v %g %g %g\n",pp.x,pp.y,pp.z);
		fprintf(fp,"vt %g %g\n",1-pp.ta,1-pp.c/ntxt);
//		if(mgl_isnan(pp.u))	fprintf(fp,"vn 0 0 0\n");
//		else fprintf(fp,"vn %g %g %g\n",pp.u,pp.v,pp.w);
	}
	// primitive definition in groups
	char *tname = new char[len+15];	strcpy(tname,fname);
	tname[len-4]=0;	fprintf(fp,"# Primitives Definitions\nmtllib %s.mtl\nusemtl %s\n",tname,tname);
	for(size_t i=0;i<gr->Grp.size();i++)
	{
		fprintf(fp,"g %s\n",gr->Grp[i].Lbl.c_str());
		std::vector<long> &p = gr->Grp[i].p;
		for(size_t j=0;j<p.size();j++)
		{
			const mglPrim &q=gr->GetPrm(p[j],false);
			mgl_obj_prim_old(gr, q, gr->GetPnt(q.n1), fp, mgl_isnan(q.s)?0:q.s);
		}
		gr->Grp[i].p.clear();	// we don't need indexes anymore
	}
	// try to save "ungrouped" primitives
	fclose(fp);
	// prepare MTL file
	tname[len-4]='.';	tname[len-3]='m';	tname[len-2]='t';	tname[len-1]='l';
	fp=fopen(tname,"wt");
	tname[len-4]=0;	fprintf(fp,"newmtl %s\n",tname);
	fprintf(fp,"Ka 1.000 1.000 1.000\n");
	fprintf(fp,"Kd 1.000 1.000 1.000\n");
	fprintf(fp,"Ks 0.000 0.000 0.000\n");
	fprintf(fp,"d 1.0\nTr 0.0\nillum 2\n");
	if(use_png)	strcat(tname,"_texture.png");
//	{	tname[len-4]='.';	tname[len-3]='p';	tname[len-2]='n';	tname[len-1]='g';	}
	else		strcat(tname,"_texture.tga");
//	{	tname[len-4]='.';	tname[len-3]='t';	tname[len-2]='g';	tname[len-1]='a';	}
	fprintf(fp,"map_Ka %s\nmap_Kd %s\nmap_Ks %s\n",tname,tname,tname);
	fclose(fp);
	// prepare texture file (TGA or PNG)
	long j=gr->GetTxtNum();
	unsigned char *buf = new unsigned char[4*256*256*j];
	unsigned char **pbuf= (unsigned char **)malloc(256*j*sizeof(unsigned char *));
	for(long i=0;i<256*j;i++)	pbuf[i] = buf+4*256*i;
	for(long i=0;i<j;i++)	gr->GetTxt(i).GetRGBA(buf+i*256*256*4);
	if(use_png)	mgl_pnga_save(tname,256,256*j,pbuf);
	else		mgl_tga_save(tname,256,256*j,pbuf);
	free(pbuf);	delete []buf;	delete []tname;
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_obj_old_(uintptr_t *gr, const char *fname,const char *descr, int *use_png,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_obj_old(_GR_,s,d,*use_png);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_stl(HMGL gr, const char *fname,const char *descr)
{
	if(gr->GetPrmNum()==0)	return;	// nothing to do
	FILE *fp = fopen(fname,"wt");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");
	fprintf(fp,"solid %s\n",(descr && *descr)?descr:"mathgl");
	mglPnt pp;
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q=gr->GetPrm(i,false);
		if(q.type==2)	//	triangles
		{
			pp = gr->GetPnt(q.n1);
			fprintf(fp,"facet normal %.2g %.2g %.2g\nouter loop\n",pp.u,pp.v,pp.w);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n2);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n3);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			fprintf(fp,"endloop\nendfacet\n");
		}
		if(q.type==3)	//	quadrangles
		{
			pp = gr->GetPnt(q.n1);
			fprintf(fp,"facet normal %.2g %.2g %.2g\nouter loop\n",pp.u,pp.v,pp.w);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n2);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n3);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			fprintf(fp,"endloop\nendfacet\n");
			pp = gr->GetPnt(q.n1);
			fprintf(fp,"facet normal %.2g %.2g %.2g\nouter loop\n",pp.u,pp.v,pp.w);
			pp = gr->GetPnt(q.n4);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n2);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			pp = gr->GetPnt(q.n3);
			fprintf(fp,"vertex %g %g %g\n",pp.x,pp.y,pp.z);
			fprintf(fp,"endloop\nendfacet\n");
		}
	}
	fprintf(fp,"endsolid %s",(descr && *descr)?descr:"mathgl");
	fclose(fp);
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_stl_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_stl(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_xyz(HMGL gr, const char *fname,const char *descr)
{
	if(gr->GetPrmNum()==0)	return;	// nothing to do

	FILE *fp=fopen(fname,"wt"), *ff;	// vertices definition
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");
	fprintf(fp,"# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	fprintf(fp,"# List of Vertices, with (x,y,z) coordinates.\n");
	for(long i=0;i<gr->GetPntNum();i++)
	{
		const mglPnt &pp = gr->GetPnt(i);
		fprintf(fp,"%g %g %g\n",pp.x,pp.y,pp.z);
	}
	fclose(fp);

	// primitive definition
	size_t len=strlen(fname);
	char *tname = new char[len+2];	strcpy(tname,fname);	tname[len+1]=tname[len]=0;
	tname[len]='l';	fp = fopen(tname,"wt");
	tname[len]='f';	ff = fopen(tname,"wt");
	fprintf(fp,"# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	fprintf(fp,"# Indices of vertices to connect for lines\n");
	fprintf(ff,"# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	fprintf(ff,"# Indices of vertices to connect for faces\n");
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q=gr->GetPrm(i,false);
		if(q.type==1)	fprintf(fp,"%ld %ld\n",q.n1+1,q.n2+1);
		if(q.type==2)	fprintf(ff,"%ld %ld %ld\n",q.n1+1,q.n2+1,q.n3+1);
		if(q.type==3)	fprintf(ff,"%ld %ld %ld\n%ld %ld %ld\n",q.n1+1,q.n2+1,q.n3+1,q.n4+1,q.n2+1,q.n3+1);
	}
	fclose(fp);	fclose(ff);	delete []tname;
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_xyz_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_xyz(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_off(HMGL gr, const char *fname,const char *descr, int colored)
{
	long nf=0;
	for(long i=0;i<gr->GetPrmNum();i++)	// find number of faces
	{
		const mglPrim &q=gr->GetPrm(i,false);
		if(q.type==2 || q.type==3)	nf++;
	}
	if(nf<=0)	return;	// nothing to do

	FILE *fp=fopen(fname,"wt");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");
	// vertices definition
	if(colored)
		fprintf(fp,"COFF\n# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	else
		fprintf(fp,"OFF\n# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	fprintf(fp,"# List of Vertices, with (x,y,z,r,g,b,a) coordinates.\n");
	fprintf(fp,"%ld %ld 0\n",gr->GetPntNum(), nf);
	for(long i=0;i<gr->GetPntNum();i++)
	{
		const mglPnt &pp = gr->GetPnt(i);
		if(colored)
			fprintf(fp,"%g %g %g %g %g %g %g\n", pp.x, pp.y, pp.z, pp.r, pp.g, pp.b, pp.a);
		else	fprintf(fp,"%g %g %g\n", pp.x, pp.y, pp.z);
	}
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim &q=gr->GetPrm(i,false);
		const mglPnt &p1=gr->GetPnt(q.n1);
		if(colored)
		{
			if(q.type==2)
				fprintf(fp,"3 %ld %ld %ld\n",q.n1,q.n2,q.n3);
			else if(q.type==3)
				fprintf(fp,"4 %ld %ld %ld %ld\n",q.n1,q.n2,q.n4,q.n3);
		}
		else
		{
			if(q.type==2)
			{
				const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3);
				if(p1.a>mgl_min_a || p2.a>mgl_min_a || p3.a>mgl_min_a)
					fprintf(fp,"3 %ld %ld %ld %.2g %.2g %.2g %.2g\n",q.n1,q.n2,q.n3,
							(p1.r+p2.r+p3.r)/3, (p1.g+p2.g+p3.g)/3, (p1.b+p2.b+p3.b)/3, (p1.a+p2.a+p3.a)/3);
			}
			else if(q.type==3)
			{
				const mglPnt &p2=gr->GetPnt(q.n2), &p3=gr->GetPnt(q.n3), &p4=gr->GetPnt(q.n4);
				if(p1.a>mgl_min_a || p2.a>mgl_min_a || p3.a>mgl_min_a || p4.a>mgl_min_a)
					fprintf(fp,"4 %ld %ld %ld %ld %.2g %.2g %.2g %.2g\n",q.n1,q.n2,q.n4,q.n3,
							(p1.r+p2.r+p3.r+p4.r)/4, (p1.g+p2.g+p3.g+p4.g)/4, (p1.b+p2.b+p3.b+p4.b)/4, (p1.a+p2.a+p3.a+p4.a)/4);
			}
		}
	}
	fclose(fp);
	setlocale(LC_NUMERIC, loc.c_str());
}
void MGL_EXPORT mgl_write_off_(uintptr_t *gr, const char *fname,const char *descr,int *colored,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_off(_GR_,s,d,*colored);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
bool mglCanvas::WriteJSON(const char *fname, bool force_zlib)
{
	bool fl = strcmp(fname,"-");
	bool gz = force_zlib || fname[strlen(fname)-1]=='z';
	void *fp = fl ? (gz ? (void*)gzopen(fname,"wt") : (void*)fopen(fname,"wt")) : stdout;
	if (!fp)	return true;
	std::string s=GetJSON();
	if(gz)	gzprintf((gzFile)fp, "%s", s.c_str());
	else	fprintf((FILE *)fp, "%s", s.c_str());
	if(fl)	{	if(gz)	gzclose((gzFile)fp);	else	fclose((FILE *)fp);	}
	return false;
}
//-----------------------------------------------------------------------------
MGL_EXPORT const char *mgl_get_json(HMGL gr)
{
	static std::string json;
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	if(g)	json = g->GetJSON();
	return json.c_str();
}
//-----------------------------------------------------------------------------
std::string mglCanvas::GetJSON()
{
	ClearUnused();	// clear unused points
	PreparePrim(3);
	std::string res, buf;
	long ll=0,l=(long)Pnt.size();
	long factor = Width>1?10:10000;
	const std::string loc = setlocale(LC_NUMERIC, "C");
	res = res + mgl_sprintf("{\n\"width\":%d,\t\"height\":%d,\t\"depth\":%d,\t\"plotid\":\"%s\",\t\"npnts\":%ld,\t\"pnts\":[\n",
			factor*Width, factor*Height, factor*Depth, PlotId.c_str(), l);
	for(long i=0;i<l;i++)
	{
		const mglPnt &q=Pnt[i];
		res += mgl_sprintf("[%ld,%ld,%ld,%d]%c\n", long(factor*q.xx), long(factor*(Height-q.yy)), long(factor*q.zz),q.sub, i+1<l?',':' ');
	}

	l = (long)Prm.size();	ll = 0;
	for(long i=0;i<l;i++)
	{	mglRGBA c;	c.c = GetPrmCol(i,false);	if(c.r[3])	ll++;	}

	res = res + mgl_sprintf("],\t\"nprim\":%ld,\t\"prim\":[\n",ll+1);

	std::vector<mglPoint> xy;	// vector for glyphs coordinates (to be separated from pnts)
	res.reserve(60*(ll+1));
#pragma omp parallel for private(buf)
	for(long i=0;i<l;i++)
	{
		const mglPrim &p=Prm[i];	mglRGBA cp;	cp.c = GetPrmCol(i,false);
		if(p.n1<0 || (p.type==1 && p.n2<0) || (p.type==2 && (p.n2<0 || p.n3<0)) || (p.type==3 && (p.n2<0 || p.n3<0 || p.n4<0)))
			continue;
		long n1=p.n1, n2=p.n2, n3=0, n4=(p.type==3||p.type==0)?p.n4:0;
		if(p.type==2 || p.type==3)	n3 = p.n3;
		if(p.type==4)
		{
			const mglPnt &q = Pnt[p.n1];
#pragma omp critical
			{n2 = xy.size();	xy.push_back(mglPoint(q.u,q.v,p.n2));}
			n3 = p.n3;	n4 = p.n4;
		}
		if(p.type==1 && n1>n2)	{	n1=p.n2;	n2=p.n1;	}
		long ps=p.s==p.s?long(100*factor*p.s):0, pw=p.w==p.w?long(100*p.w):0, pp=p.p==p.p?mgl_int(1e5*p.p):0;
		if(cp.r[3]==255 || p.type==0 || p.type==1 || p.type==4 || p.type==6)
			buf = mgl_sprintf("[%d,%ld,%ld,%ld,%ld,%d,%ld,%ld,%ld,0,\"#%02x%02x%02x\"],\n",
				p.type, n1, n2, n3, n4, p.id, ps,pw,pp, int(cp.r[0]),int(cp.r[1]),int(cp.r[2]));
		else if(cp.r[3])
			buf = mgl_sprintf("[%d,%ld,%ld,%ld,%ld,%d,%ld,%ld,%ld,0,\"rgba(%d,%d,%d,%.2g)\"],\n",
				p.type, n1, n2, n3, n4, p.id, ps,pw,pp, int(cp.r[0]),int(cp.r[1]),int(cp.r[2]),cp.r[3]/255.);
		else	buf = "";
#pragma omp critical
		res += buf;
	}
	res += "[-1,0,0,0,0,0,0,0,0,0,\"#000000\"]\n";	// need to add this empty block

	l = (long)xy.size();
	res = res + mgl_sprintf("],\t\"ncoor\":%lu,\t\"coor\":[\n",(unsigned long)l);
	for(long i=0;i<l;i++)
	{
		const mglPoint &p=xy[i];
		const mglPnt &q=Pnt[int(0.5+p.z)];
		long px=long(100*p.x), py=long(100*p.y);
		if(q.u==q.u && q.v==q.v && q.w==q.w)
			res = res + mgl_sprintf("[%ld,%ld,%ld,%ld,%ld]%c\n", px, py, long(100*q.u), long(100*q.v), long(100*q.w), i+1<l?',':' ');
		else
			res = res + mgl_sprintf("[%ld,%ld,1e11,1e11,1e11]%c\n", px, py, i+1<l?',':' ');
	}

	l = (long)Glf.size();
	res = res + mgl_sprintf("],\t\"nglfs\":%lu,\t\"glfs\":[\n",(unsigned long)l);
	for(long i=0;i<l;i++)
	{
		const mglGlyph &g=Glf[i];
		res = res + mgl_sprintf("[%ld,\n\t[", g.nl);
		for(long j=0;j<2*g.nl;j++)	res = res + mgl_sprintf("%d%c", g.line[j], j+1<2*g.nl?',':' ');
		res = res + mgl_sprintf("]\n]%c\n", i+1<l?',':' ');
	}
	res = res + mgl_sprintf("]\n}\n");
	setlocale(LC_NUMERIC, loc.c_str());
	return res;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_json(HMGL gr, const char *fname,const char *)
{	_Gr_->WriteJSON(fname);	}
void MGL_EXPORT mgl_write_json_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_json(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_json_z(HMGL gr, const char *fname,const char *)
{	_Gr_->WriteJSON(fname,true);	}
void MGL_EXPORT mgl_write_json_z_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_json_z(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
bool mglCanvas::ExportMGLD(const char *fname, const char *descr)
{
	if(Pnt.size()<1 || Prm.size()<1)	return true;
	FILE *fp=fopen(fname,"wt");
	if(!fp)	return true;
	const std::string loc = setlocale(LC_NUMERIC, "C");
	// NOTE: I'll save Ptx. So prim type=6 is useless,and no LaTeX
	fprintf(fp,"MGLD %lu %lu %lu %lu %d %d\n# %s\n", (unsigned long)Pnt.size(), (unsigned long)Prm.size(), (unsigned long)Txt.size(), (unsigned long)Glf.size(), Width, Height, (descr && *descr) ? descr : fname);
	fprintf(fp,"# Vertexes: x y z c t ta u v w r g b a\n");
	for(size_t i=0;i<Pnt.size();i++)
	{
		const mglPnt &q=Pnt[i];
		fprintf(fp,"%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\t%.4g\n", q.xx, q.yy, q.zz, q.c, q.ta, q.ta, q.u, q.v, q.w, q.r, q.g, q.b, q.a);
	}
	fprintf(fp,"# Primitives: type n1 n2 n3 n4 id s w p\n");
	for(size_t i=0;i<Prm.size();i++)
	{
		const mglPrim &p=Prm[i];
		long long unsigned mask = p.m;
		fprintf(fp,"%d\t%ld\t%ld\t%ld\t%ld\t%d\t%g\t%g\t%g\t%d\t%llu\n", p.type, p.n1, p.n2, p.n3, p.n4, p.id, p.s==p.s?p.s:0, p.w==p.w?p.w:0, p.p==p.p?p.p:0, p.angl, mask);
	}
	fprintf(fp,"# Textures: smooth alpha colors\n");
	for(size_t i=0;i<Txt.size();i++)
	{
		const mglTexture &t=Txt[i];
		fprintf(fp,"%d\t%.4g\t%s\n",t.Smooth,t.Alpha,t.Sch);
	}
	fprintf(fp,"# Glyphs: nt nl [trig] [line]\n");
	for(size_t i=0;i<Glf.size();i++)
	{
		const mglGlyph &g=Glf[i];
		fprintf(fp,"%ld\t%ld\n", g.nt, g.nl);
		if(g.trig)
		{
			for(long j=0;j<6*g.nt;j++)	fprintf(fp,"%d\t",g.trig[j]);
			fprintf(fp,"\n");
		}
		if(g.line)
		{
			for(long j=0;j<2*g.nl;j++)	fprintf(fp,"%d\t",g.line[j]);
			fprintf(fp,"\n");
		}
	}
	fclose(fp);
	setlocale(LC_NUMERIC, loc.c_str());
	return false;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_export_mgld(HMGL gr, const char *fname,const char *descr)
{	_Gr_->ExportMGLD(fname, descr);	}
void MGL_EXPORT mgl_export_mgld_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_export_mgld(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
bool mglCanvas::ImportMGLD(const char *fname, bool add)
{
	FILE *fp=fopen(fname,"rt");
	if(!fp)	return true;
	char *buf=new char[512];
	if(!fgets(buf,512,fp))	*buf=0;
	if(strncmp(buf,"MGLD",4))	{	delete []buf;	fclose(fp);	return true;	}
	unsigned long n=0,m=0,l=0,k=0, npnt=0, nglf=0;
	int w=0,h=0,d;
	sscanf(buf+5,"%lu%lu%lu%lu%d%d",&n,&m,&l,&k,&w,&h);
	if(w<=0 || h<=0)	{	w=Width;	h=Height;	}
	d = long(sqrt(double(w*h)));
	if(n==0 || m==0 || l==0)	{	delete []buf;	fclose(fp);	return true;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");
	if(!add)	{	Clf();	Txt.clear();	}
	else	{	ClfZB();	npnt=Pnt.size();	nglf=Glf.size();	}
	LightScale(&B);
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexGlf);
	pthread_mutex_lock(&mutexPnt);
	pthread_mutex_lock(&mutexPrm);
	pthread_mutex_lock(&mutexTxt);
#endif
#pragma omp critical
	{
		Pnt.reserve(n);	Prm.reserve(m);	Txt.reserve(l);	Glf.reserve(k);
		mglPnt p;	float tmp;
		for(unsigned long i=0;i<n;i++)
		{
			do {	if(!fgets(buf,512,fp))	*buf=0;	mgl_strtrim(buf);	} while(*buf=='#');
			sscanf(buf,"%g%g%g%g%g%g%g%g%g%g%g%g%g", &p.xx, &p.yy, &p.zz, &p.c, &tmp, &p.ta, &p.u, &p.v, &p.w, &p.r, &p.g, &p.b, &p.a);
			// rescale to current image size
			p.xx *= Width/double(w);	p.yy *= Height/double(h);	p.zz *= Depth/double(d);
			Pnt.push_back(p);
		}
		mglPrim q;
		for(unsigned long i=0;i<m;i++)
		{
			do {	if(!fgets(buf,512,fp))	*buf=0;	mgl_strtrim(buf);	} while(*buf=='#');
			long long unsigned mask=MGL_SOLID_MASK;
			sscanf(buf,"%hd%ld%ld%ld%ld%d%g%g%g%hd%llu", &q.type, &q.n1, &q.n2, &q.n3, &q.n4, &q.id, &q.s, &q.w, &q.p, &q.angl, &mask);
			q.n1 = q.n1>=0?q.n1+npnt:-1;	q.n2 = q.n2>=0?q.n2+npnt:-1;
			switch(q.type)
			{
			case 3:	q.n4 = q.n4>=0?q.n4+npnt:-1;
			case 2:	q.n3 = q.n3>=0?q.n3+npnt:-1;	q.m = mask;	break;
			case 4:	q.s *= (Width<Height?Width:Height)/double(w<h?w:h);
					q.n4 = q.n4>=0?q.n4+nglf:-1;	break;
			}
			Prm.push_back(q);
		}
		mglTexture t;
		for(unsigned long i=0;i<l;i++)
		{
			int sm=0;	float a;
			do {	if(!fgets(buf,512,fp))	*buf=0;	mgl_strtrim(buf);	} while(*buf=='#');
			size_t j,k=0;
			for(j=0;buf[j];j++)
			{
				if(buf[j]<=' ' && k)	{	sm++;	k=0;	}
				if(buf[j]>' ')	k=1;
				if(sm==2 && k)	break;
			}
			sscanf(buf,"%d%g", &sm, &a);
			t.Set(buf+j, sm, a);
			Txt.push_back(t);
		}
		mglGlyph g;
		for(unsigned long i=0;i<k;i++)
		{
			do {	if(!fgets(buf,512,fp))	*buf=0;	mgl_strtrim(buf);	} while(*buf=='#' || *buf==0);
			long nt=0,nl=0;
			sscanf(buf,"%ld%ld", &nt, &nl);	g.Create(nt,nl);
			for(long j=0;j<6*nt;j++)	fscanf(fp,"%hd",g.trig+j);
			for(long j=0;j<2*nl;j++)	fscanf(fp,"%hd",g.line+j);
			Glf.push_back(g);
		}
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexGlf);
	pthread_mutex_unlock(&mutexPnt);
	pthread_mutex_unlock(&mutexPrm);
	pthread_mutex_unlock(&mutexTxt);
#endif
	setlocale(LC_NUMERIC, loc.c_str());
	delete []buf;	fclose(fp);	return false;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_import_mgld(HMGL gr, const char *fname, int add)
{	_Gr_->ImportMGLD(fname, add);	}
void MGL_EXPORT mgl_import_mgld_(uintptr_t *gr, const char *fname, int *add, int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	mgl_import_mgld(_GR_,s,*add);	delete []s;	}
//-----------------------------------------------------------------------------
/*void MGL_EXPORT mgl_xgl_prim(const mglPrim &q, const mglPnt &p, FILE *fp, mreal size)
{
	char type = q.n4;	mreal ss=size*0.35;
	long i=q.n1;
	switch(q.type)
	{
	case 0:
		if(!strchr("xsSoO",type))	ss *= 1.1;
		if(type=='.' || ss==0)	fprintf(fp,"p %ld\n", i);
		else	switch(type)	// TODO: save mark by PATCH
		{
		case 'P':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);
		case '+':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);	break;
		case 'X':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -4/%ld\n", i,i);	break;
		case 'x':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -4/%ld\n", i,i);	break;
		case 'S':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"f -4/%ld -3/%ld -2/%ld -1/%ld\n",i,i,i,i);	break;
		case 's':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);	break;
		case 'D':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"f -4/%ld -3/%ld -2/%ld -1/%ld\n",i,i,i,i);	break;
		case 'd':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -4/%ld -3/%ld\n", i,i);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -4/%ld\n", i,i);	break;
		case 'Y':
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+0.8*ss,p.y+0.6*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.8*ss,p.y+0.6*ss,p.z);
			fprintf(fp,"l -3/%ld %ld/%ld\n", i,i,i);
			fprintf(fp,"l -2/%ld %ld/%ld\n", i,i,i);
			fprintf(fp,"l -1/%ld %ld/%ld\n", i,i,i);	break;
		case '*':
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"v %g %g %g\n",p.x+0.6*ss,p.y+0.8*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.6*ss,p.y-0.8*ss,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"v %g %g %g\n",p.x+0.6*ss,p.y-0.8*ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-0.6*ss,p.y+0.8*ss,p.z);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);		break;
		case 'T':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '^':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y-ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y+ss,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'V':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case 'v':
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y+ss/2,p.z);
			fprintf(fp,"v %g %g %g\n",p.x,p.y-ss,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'L':
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '<':
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss,p.y,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'R':
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"f -3/%ld -2/%ld -1/%ld\n", i,i,i);	break;
		case '>':
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y+ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x-ss/2,p.y-ss,p.z);
			fprintf(fp,"v %g %g %g\n",p.x+ss,p.y,p.z);
			fprintf(fp,"l -3/%ld -2/%ld\n", i,i);
			fprintf(fp,"l -2/%ld -1/%ld\n", i,i);
			fprintf(fp,"l -1/%ld -3/%ld\n", i,i);	break;
		case 'O':
			for(long j=0;j<=20;j++)
				fprintf(fp,"v %g %g %g\n",p.x+ss*mgl_cos[(j*36)%360],p.y+ss*mgl_cos[(270+j*36)%360],p.z);
			for(long j=0;j<20;j++)
				fprintf(fp,"f %ld/%ld %ld/%ld %ld/%ld\n", j-21,i, j-20,i, i,i);
			break;
		case 'C':	fprintf(fp,"p %ld\n", i);
		case 'o':
			for(long j=0;j<=20;j++)
				fprintf(fp,"v %g %g %g\n",p.x+ss*mgl_cos[(j*36)%360],p.y+ss*mgl_cos[(270+j*36)%360],p.z);
			for(long j=0;j<20;j++)
				fprintf(fp,"l %ld/%ld %ld/%ld\n", j-21,i, j-20,i);
			break;
		}
		break;
	case 1:	fprintf(fp,"l %ld/%ld %ld/%ld\n", q.n1,q.n1, q.n2,q.n2);	break;
	case 2:	fprintf(fp,"f %ld/%ld/%ld %ld/%ld/%ld %ld/%ld/%ld\n",
		q.n1,q.n1,q.n1, q.n2,q.n2,q.n2, q.n3,q.n3,q.n3);	break;
	case 3:	fprintf(fp,"f %ld/%ld/%ld %ld/%ld/%ld %ld/%ld/%ld %ld/%ld/%ld\n",
		q.n1,q.n1,q.n1, q.n2,q.n2,q.n2, q.n3,q.n3,q.n3, q.n4,q.n4,q.n4);	break;
	case 4:	break;	// TODO: add glyphs export later
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::WriteXGL(const char *fname,const char *descr)
{
	if(GetPrmNum()==0)	return;	// nothing to do
	FILE *fp=fopen(fname,"wt");
	if(!fp)	return true;
	fprintf(fp,"<WORLD>\n<NAME>%s</NAME>\n", (descr && *descr)?descr:fname);
	fprintf(fp,"<BACKGROUND><BACKCOLOR>%g, %g, %g</BACKCOLOR></BACKGROUND>\n", BDef[0]/255., BDef[1]/255., BDef[2]/255.);
	fprintf(fp,"<LIGHTING>\n<AMBIENT>%g, %g, %g</AMBIENT>\n",AmbBr, AmbBr, AmbBr);
	if(get(MGL_ENABLE_LIGHT))	for(size_t i=0;i<10;i++)
		if(light[i].n && mgl_isnan(light[i].r.x))
		{
			fprintf(fp, "<DIRECTIONALLIGHT>\n<DIRECTION>%g, %g, %g</DIRECTION>\n", light[i].d.x, light[i].d.y, light[i].d.z);
			fprintf(fp, "<SPECULAR>%g, %g, %g</SPECULAR>\n</DIRECTIONALLIGHT>\n", light[i].c.r, light[i].c.g, light[i].c.b);
		}
	fprintf(fp,"</LIGHTING>");

	// TODO: add textures

	long m1=0,m2=0,m;
	for(size_t i=0;i<Grp.size();i++)	// prepare array of indirect indexing
	{	m = Grp[i].Id;	if(m<m1) m1=m;	if(m>m2) m2=m;	}
	long *ng = new long[m2-m1+1];
	for(size_t i=0;i<Grp.size();i++)	ng[gr->Grp[i].Id-m1] = i;
	for(size_t i=0;i<GetPrmNum();i++)	// collect data for groups
	// it is rather expensive (extra 4b per primitive) but need for export to 3D
	{
		m = GetPrm(i,false).id-m1;
		if(m>=0 && m<m2-m1+1)	Grp[ng[m]].p.push_back(i);
	}
	delete []ng;

	std::vector<long> p;
	mglPrim q;
	char *pg=new char[GetPntNum()];
	for(size_t i=0;i<Grp.size();i++)	// first write objects
	{
		p = Grp[i].p;	memset(pg,0,GetPntNum());
		fprintf(fp,"<OBJECT>\n<NAME>%s</NAME>\n<MESH>\n",Grp[i].Lbl.c_str());
		for(size_t j=0;j<p.size();j++)		// collect Pnt for this object
		{
			const mglPrim q=GetPrm(p[j],false);	pg[q.n1]=1;
			if(q.type==3)	{	pg[q.n2]=1;	pg[q.n3]=1;	pg[q.n4]=1;	}
			else if(q.type==1)	pg[q.n2]=1;
			else if(q.type==2)	{	pg[q.n2]=1;	pg[q.n3]=1;	}
		}
		for(size_t j=0;j<GetPntNum();j++)	if(pg[j])	// write Pnt for this object
		{
			const mglPnt s=Pnt[j];
			fprintf(fp,"<P ID=\"%u\">%g, %g, %g</P>\n",j, s.x, s.y, s.z);
			fprintf(fp,"<N ID=\"%u\">%g, %g, %g</N>\n",j, s.x, s.y, s.z);
		}
		// TODO: add line styles
		for(size_t j=0;j<p.size();j++)	// now write primitives itself
		{
			const mglPrim q=GetPrm(p[j],false);
			mgl_xgl_prim(q, GetPnt(q.n1), fp, q.s*FontFactor());
		}
		fprintf(fp,"</MESH>\n</OBJECT>");	// finish with this object
		Grp[i].p.clear();	// we don't need indexes anymore
	}
	// TODO: try to save "ungrouped" primitives

	fprintf(fp,"</WORLD>");	fclose(fp);	delete []pg;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_xgl(HMGL gr, const char *fname,const char *descr)
{	_Gr_->WriteXGL(fname,descr);	}
void MGL_EXPORT mgl_write_xgl_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
	mgl_write_xgl(_GR_,s,d);	delete []s;		delete []d;	}*/
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_x3d_mdef(HMGL gr, void *fp, bool gz)
{
/*	bool m_p=false,m_x=false,m_d=false,m_v=false,m_t=false,
	m_s=false,m_a=false,m_o=false,m_T=false,
	m_V=false,m_S=false,m_D=false,m_Y=false,m_l=false,
	m_L=false,m_r=false,m_R=false,m_X=false,m_P=false;
	for(long i=0;i<gr->GetPrmNum();i++)
	{
		const mglPrim q = gr->GetPrm(i,false);
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
	if(m_p)	mgl_printf(fp, gz, "<ProtoDeclare name='m_p'><ProtoInterface/>\n<ProtoBody>"
		"<LineSet vertexCount='0,1,2,3'>\n<Coordinate point='-1 0 0, 1 0 0, 0 -1 0, 0 1 0'/>"
		"\n</LineSet></ProtoBody></ProtoDeclare>\n");*/
	/*if(m_x)	mgl_printf(fp, gz, "/m_x {sm sm rm s2 s2 rl 0 sm 2 mul rm sm 2 mul s2 rl d0} def\n");	// TODO
	 *	if(m_s)	mgl_printf(fp, gz, "/m_s {sm sm rm 0 s2 rl s2 0 rl 0 sm 2 mul rl cp d0} def\n");
	 *	if(m_d)	mgl_printf(fp, gz, "/m_d {sm 0 rm ss ss rl ss sm rl sm sm rl cp d0} def\n");
	 *	if(m_v)	mgl_printf(fp, gz, "/m_v {sm ss 2 div rm s2 0 rl sm sm 1.5 mul rl d0 cp} def\n");
	 *	if(m_t)	mgl_printf(fp, gz, "/m_t {sm sm 2 div rm s2 0 rl sm ss 1.5 mul rl d0 cp} def\n");
	 *	if(m_a)	mgl_printf(fp, gz, "/m_a {sm 0 rm s2 0 rl sm 1.6 mul sm 0.8 mul rm ss 1.2 mul ss 1.6 mul rl 0 sm 1.6 mul rm sm 1.2 mul ss 1.6 mul rl d0} def\n");
	 *	if(m_o)	mgl_printf(fp, gz, "/m_o {ss 0 360 d0 arc} def\n");
	 *	if(m_S)	mgl_printf(fp, gz, "/m_S {sm sm rm 0 s2 rl s2 0 rl 0 sm 2 mul rl cp} def\n");
	 *	if(m_D)	mgl_printf(fp, gz, "/m_D {sm 0 rm ss ss rl ss sm rl sm sm rl cp} def\n");
	 *	if(m_V)	mgl_printf(fp, gz, "/m_V {sm ss 2 div rm s2 0 rl sm sm 1.5 mul rl cp} def\n");
	 *	if(m_T)	mgl_printf(fp, gz, "/m_T {sm sm 2 div rm s2 0 rl sm ss 1.5 mul rl cp} def\n");
	 *	if(m_Y)	mgl_printf(fp, gz, "/m_Y {0 sm rm 0 ss rl sm ss rl s2 0 rm sm sm rl d0} def\n");
	 *	if(m_r)	mgl_printf(fp, gz, "/m_r {sm 2 div sm rm 0 s2 rl ss 1.5 mul sm rl d0 cp} def\n");
	 *	if(m_l)	mgl_printf(fp, gz, "/m_l {ss 2 div sm rm 0 s2 rl sm 1.5 mul sm rl d0 cp} def\n");
	 *	if(m_R)	mgl_printf(fp, gz, "/m_R {sm 2 div sm rm 0 s2 rl ss 1.5 mul sm rl cp} def\n");
	 *	if(m_L)	mgl_printf(fp, gz, "/m_L {ss 2 div sm rm 0 s2 rl sm 1.5 mul sm rl cp} def\n");
	 *	if(m_P)	mgl_printf(fp, gz, "/m_P {m_p 0 sm rm m_s} def\n");
	 *	if(m_X)	mgl_printf(fp, gz, "/m_X {m_x ss sm rm m_s} def\n");*/
	//	if(m_C)	mgl_printf(fp, gz, "/m_C {m_c m_o} def\n");
//	mgl_printf(fp, gz, "\n");
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_x3d_prim(const mglPrim &q, const mglPnt &p, const long *pnt, void *fp,bool gz, mreal size)
{
	// <ProtoInstance name='EmissiveMaterial'/>
/*		if(q.type==0)	// mark
		{
			mreal x0 = p1.x,y0 = p1.y;
			sprintf(str,"1 lw %.2g %.2g %.2g rgb ", cp.r,cp.g,cp.b);
			wp=1;
			if(q.s!=gr->mark_size()/gr->FontFactor())
			{
				mgl_printf(fp, gz, "/ss {%g} def\n",q.s*0.4*gr->FontFactor());
				mgl_printf(fp, gz, "/s2 {%g} def\n",q.s*0.8*gr->FontFactor());
				mgl_printf(fp, gz, "/sm {-%g} def\n",q.s*0.4*gr->FontFactor());
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
				default:	mgl_printf(fp, gz, "%g %g m_c %sfill\n",x0,y0,str);
			}
			if(q.s!=gr->mark_size()/gr->FontFactor())
			{
				mgl_printf(fp, gz, "/ss {%g} def\n",0.4*gr->mark_size());
				mgl_printf(fp, gz, "/s2 {%g} def\n",0.8*gr->mark_size());
				mgl_printf(fp, gz, "/sm {-%g} def\n",0.4*gr->mark_size());
			}
		}
		else if(q.type==3)	// quad
		{
			const mglPnt p2=gr->GetPnt(q.n2), p3=gr->GetPnt(q.n3), p4=gr->GetPnt(q.n4);
			mgl_printf(fp, gz, "np %g %g mt %g %g ll %g %g ll %g %g ll cp %sfill\n",
					   p1.x, p1.y, p2.x, p2.y, p4.x, p4.y, p3.x, p3.y, str);
		}
		else if(q.type==2)	// trig
		{
			const mglPnt p2=gr->GetPnt(q.n2), p3=gr->GetPnt(q.n3);
			mgl_printf(fp, gz, "np %g %g mt %g %g ll %g %g ll cp %sfill\n",
					   p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, str);
		}
		else if(q.type==1)	// line
		{
			sprintf(str,"%.2g lw %.2g %.2g %.2g rgb ", q.w>1 ? q.w:1., cp.r,cp.g,cp.b);
			wp = q.w>1  ? q.w:1;	st = q.n3;
			put_line(gr,fp,gz,i,wp,cp,st, "np %g %g mt ", "%g %g ll ", false, 1);
			const char *sd = mgl_get_dash(q.n3,q.w);
			if(sd && sd[0])	mgl_printf(fp, gz, "%s [%s] %g sd dr\n",str,sd,q.w*q.s);
			else			mgl_printf(fp, gz, "%s d0 dr\n",str);
		}
		else if(q.type==4)	// glyph
		{
			mreal 	ss = q.s/2, xx = p1.u, yy = p1.v, zz = p1.w;
			mgl_printf(fp, gz, "gsave\t%g %g translate %g %g scale %g rotate %s\n",
					   p1.x, p1.y, ss, ss, -q.w, str);
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
		}*/
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_x3d(HMGL gr, const char *fname,const char *descr)
{
	if(gr->GetPrmNum()<1)	return;
	time_t now;	time(&now);

	bool gz = fname[strlen(fname)-1]=='z';
	void *fp = gz ? (void*)gzopen(fname,"wt") : (void*)fopen(fname,"wt");
	if(!fp)		{	gr->SetWarn(mglWarnOpen,fname);	return;	}
	const std::string loc = setlocale(LC_NUMERIC, "C");
	mgl_printf(fp, gz, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	mgl_printf(fp, gz, "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.0//EN\" \"http://www.web3d.org/specifications/x3d-3.0.dtd\">\n");
	mgl_printf(fp, gz, "<X3D profile='Immersive'>\n<head>\n<meta name='filename' content='%s'/>\n",fname);
	mgl_printf(fp, gz, "<meta name='description' content='%s'/>\n",(descr && *descr)?descr:fname);
	mgl_printf(fp, gz, "<meta name='created' content='%s'/>\n",ctime(&now));
	mgl_printf(fp, gz, "<meta name='generator' content='MathGL, http://mathgl.sourceforge.net/'/>\n");
	mgl_printf(fp, gz, "</head>\n<Scene>\n");

	// 1. first we have to define proto for marks and glyphs
	mgl_x3d_mdef(gr, fp, gz);

	// here should be defined textures ... but since X3D support RGBA then omit it in this version

	// 2. now find group for primitives
	long m1=0,m2=0,m;
	for(size_t i=0;i<gr->Grp.size();i++)	// prepare array of indirect indexing
	{	m = gr->Grp[i].Id;	if(m<m1) m1=m;	if(m>m2) m2=m;	}
	long *ng = new long[m2-m1+1];
	for(size_t i=0;i<gr->Grp.size();i++)	ng[gr->Grp[i].Id-m1] = i;
	for(long i=0;i<gr->GetPrmNum();i++)	// collect data for groups
	// it is rather expensive (extra 4b per primitive) but need for export to 3D
	{
		m = gr->GetPrm(i,false).id-m1;
		if(m>=0 && m<m2-m1+1)	gr->Grp[ng[m]].p.push_back(i);
	}
	delete []ng;

	// primitive definition in groups
	long npnt = gr->GetPntNum();
	long *pnt=new long[npnt];
	mglPrim q;
	for(size_t i=0;i<gr->Grp.size();i++)
	{
		mgl_printf(fp,gz,"<Group><!--%s-->\n",gr->Grp[i].Lbl.c_str());
		std::vector<long> &p = gr->Grp[i].p;

		// define coordinates, colors and so on
		long line=-1, face=-1, other=-1;
		for(size_t j=0;j<p.size();j++)	// find points for this group
		{
			const mglPrim &q=gr->GetPrm(p[j],false);
			if(q.type==1)	line=q.n1;	// find kind of primitives in the group
			if(q.type==2 || q.type==3)	face =q.n1;
			if(q.type>3 || q.type==0)	other=q.n1;
		}

		// now save lines
		if(line>=0)
		{
			mglColor c=gr->GetPntC(line);
			bool same=true;	// check if there are the same colors for all line segments
			for(size_t j=0;j<p.size();j++)
			{
				const mglPrim &q=gr->GetPrm(p[j],false);
				if(q.type==1 && c!=gr->GetPntC(q.n1))	same=false;
			}
			memset(pnt,-1,npnt*sizeof(long));
			for(size_t j=0,k=0;j<p.size();j++)	// rearrange points for this group
			{
				const mglPrim &q=gr->GetPrm(p[j],false);
				if(q.type!=1)	continue;
				if(q.n1>=0 && pnt[q.n1]<0)	{	pnt[q.n1]=k;	k++;	}
				if(q.n2>=0 && pnt[q.n2]<0)	{	pnt[q.n2]=k;	k++;	}
			}
			mgl_printf(fp, gz, "<Shape><Coordinate DEF='Lpnts_%ld' point='",i);
			for(long j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.x,p.y,p.z);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");
			mgl_printf(fp, gz, "<Color DEF='Lclrs_%ld' color='",i);
			for(long j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.r,p.g,p.b);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");

			// TODO save IndexedLineSet here + manual color is same==true

			mgl_printf(fp, gz, "</Shape>");
		}

		// now save faces
		if(face>=0)
		{
			mglColor c=gr->GetPntC(face);
			bool same=true;	// check if there are the same colors for all line segments
			for(size_t j=0;j<p.size();j++)
			{
				const mglPrim &q=gr->GetPrm(p[j],false);
				if((q.type==2 || q.type==3) && c!=gr->GetPntC(q.n1))	same=false;
			}
			memset(pnt,-1,npnt*sizeof(long));
			for(size_t j=0,k=0;j<p.size();j++)	// rearrange points for this group
			{
				const mglPrim &q=gr->GetPrm(p[j],false);
				if(q.type!=2 && q.type!=3)	continue;
				if(q.n1>=0 && pnt[q.n1]<0)	{	pnt[q.n1]=k;	k++;	}
				if(q.n2>=0 && pnt[q.n2]<0)	{	pnt[q.n2]=k;	k++;	}
				if(q.n3>=0 && pnt[q.n3]<0)	{	pnt[q.n3]=k;	k++;	}
				if(q.type==3 && q.n4>=0 && pnt[q.n4]<0)	{	pnt[q.n4]=k;	k++;	}
			}
			mgl_printf(fp, gz, "<Shape><Coordinate DEF='Fpnts_%ld' point='",i);
			for(long j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.x,p.y,p.z);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");
			mgl_printf(fp, gz, "<Color DEF='Fclrs_%ld' color='",i);
			for(long j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.r,p.g,p.b);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");

			// TODO save IndexedLineSet here + manual color is same==true

			mgl_printf(fp, gz, "</Shape>");
		}

		// now save other primitives
		if(other>=0)
		{
/*			memset(pnt,-1,npnt*sizeof(long));
			for(j=0,k=0;j<p.size();j++)	// rearrange points for this group
			{
				const mglPrim &q=gr->GetPrm(p[j],false);
				if(q.type!=2 && q.type!=3)	continue;
				if(q.n1>=0 && pnt[q.n1]<0)	{	pnt[q.n1]=k;	k++;	}
				if(q.n2>=0 && pnt[q.n2]<0)	{	pnt[q.n2]=k;	k++;	}
				if(q.n3>=0 && pnt[q.n3]<0)	{	pnt[q.n3]=k;	k++;	}
				if(q.type==3 && q.n4>=0 && pnt[q.n4]<0)	{	pnt[q.n4]=k;	k++;	}
			}
			mgl_printf(fp, gz, "<Shape><Coordinate DEF='Fpnts_%ld' point='",i);
			for(j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.x,p.y,p.z);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");
			mgl_printf(fp, gz, "<Color DEF='Fclrs_%ld' color='",i);
			for(j=0;j<gr->GetPntNum();j++)	if(pnt[j]>=0)
			{	const mglPnt &p=gr->GetPnt(j);	mgl_printf(fp, gz, "%g %g %g, ", p.r,p.g,p.b);	}
			mgl_printf(fp, gz, "0.0 0.0 0.0'/>");

			// TODO save IndexedLineSet here + manual color is same==true

			mgl_printf(fp, gz, "</Shape>");*/
		}
		// no normals since mathgl ones are "signless" -- x3d should calculate it by itself

		for(size_t j=0;j<p.size();j++)
		{
			const mglPrim &q=gr->GetPrm(p[j],false);	// TODO: collect by type (quads,trig,line) and draw together???
			mgl_x3d_prim(q, gr->GetPnt(q.n1), pnt, fp,gz, q.s*gr->FontFactor());
		}
		mgl_printf(fp,gz,"</Group><!--%s-->\n",gr->Grp[i].Lbl.c_str());
		gr->Grp[i].p.clear();	// we don't need indexes anymore
	}
	mgl_printf(fp, gz, "</Scene>\n");
	if(gz)	gzclose((gzFile)fp);	else	fclose((FILE *)fp);
	setlocale(LC_NUMERIC, loc.c_str());
	delete []pnt;
}
void MGL_EXPORT mgl_write_x3d_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
char *d=new char[n+1];	memcpy(d,descr,n);	d[n]=0;
mgl_write_x3d(_GR_,s,d);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
