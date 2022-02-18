/***************************************************************************
 * obj.cpp is part of Math Graphic Library
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
#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
#define _Gr_	((mglCanvas *)(gr))
int MGL_NO_EXPORT mgl_tga_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_pnga_save(const char *fname, int w, int h, unsigned char **p);
void MGL_NO_EXPORT mgl_printf(void *fp, bool gz, const char *str, ...);
//-----------------------------------------------------------------------------
#include <climits>
#include <cassert>
#include <float.h>
#include <math.h>

#include <deque>
#include <map>
#include <iostream>

/* Size prefixes for printf/scanf for size_t and ptrdiff_t */
#ifdef _MSC_VER
#      define PRIuS "Iu"  /* printf size_t */
#      define PRIdS "Id"  /* printf ptrdiff_t */
#else
#      define PRIuS "zu"  /* printf size_t */
#      define PRIdS "zd"  /* printf ptrdiff_t */
#endif

//-----------------------------------------------------------------------------
void mglTexture::GetRGBAOBJ(unsigned char *f) const
{
	const size_t bw = 128; //border width
	for(size_t i=0;i<256;i++)
	{
		mglColor c1 = col[2*i], c2 = col[2*i+1], c;
		for(size_t j=0;j<512;j++)
		{
			size_t i0 = 4*(j+512*(255-i));
			if (j<bw)
				c = c1;
			else if (j>511-bw)
				c = c2;
			else
				c = c1 + (c2-c1)*((j-bw)/(255.));
			f[i0]   = int(255*c.r);
			f[i0+1] = int(255*c.g);
			f[i0+2] = int(255*c.b);
			f[i0+3] = int(255*c.a);
		}
	}
}

//-----------------------------------------------------------------------------

struct ObjUV
{
  ObjUV() :
  u(0.0), v(0.0) {}
  ObjUV(mreal U, mreal V) :
  u(U), v(V) {}
  mreal u;
  mreal v;

  void Set(mreal U, mreal V)
  { u = U; v = V; }

  bool operator==(const ObjUV &c) const
  {
    return u==c.u && v==c.v;
  }
  bool operator!=(const ObjUV &c) const
  {
    return !(u==c.u && v==c.v);
  }
  bool operator<(const ObjUV &c) const
  {
    if(u!=c.u)
      return (u<c.u);
    return (v<c.v);
  }
  friend std::ostream& operator << (std::ostream& out, const ObjUV& c)
  {
    out << "vt " << c.u << " " << c.v << "\n";
    return out;
  }
};

struct ObjUVs {
	std::map<ObjUV,size_t> texturecoords;
  FILE* const fp;
  ObjUVs(FILE *f) : fp(f) {}

  size_t addTextureCoords(mreal ta, mreal c, size_t ntxt)
	{
    const mreal gap = 1./512;
    const mreal u = 0.25+0.5*(ta*(1-2*gap)+gap);
    const mreal v = ((c-floor(c))*(1-2*gap) + gap + floor(c))/ntxt;

    const ObjUV point(u, v);
    std::map<ObjUV,size_t>::iterator pPoint = texturecoords.find(point);
    if(pPoint!=texturecoords.end())
      return pPoint->second;
    else
    {
      const size_t point_index = texturecoords.size()+1;
      texturecoords.insert(std::make_pair(point,point_index));
      fprintf(fp,"vt %.15g %.15g\n",u,v);

      return point_index;
    }
	}

};


class ObjXYZ
{
  public :
  mreal x;
  mreal y;
  mreal z;
  ObjXYZ() :
  x(0), y(0), z(0) {}
  ObjXYZ(mreal fx, mreal fy, mreal fz) :
  x(fx), y(fy), z(fz) {}

  void Set(mreal fx, mreal fy, mreal fz)
  { x = fx; y = fy; z = fz; }
  mreal Dot(const ObjXYZ & sPt) const
  { return(x*sPt.x)+(y*sPt.y)+(z*sPt.z); }
  mreal LengthSquared()
  { return(x*x+y*y+z*z); }

  friend ObjXYZ operator + (const ObjXYZ& a, const ObjXYZ& b)
  { return ObjXYZ(a.x+b.x,a.y+b.y,a.z+b.z); }
  friend ObjXYZ operator - (const ObjXYZ& a)
  { return ObjXYZ(-a.x,-a.y,-a.z); }
  friend ObjXYZ operator - (const ObjXYZ& a, const ObjXYZ& b)
  { return ObjXYZ(a.x-b.x,a.y-b.y,a.z-b.z); }
  friend ObjXYZ operator * (const ObjXYZ& a, const mreal d)
  { return ObjXYZ(a.x*d,a.y*d,a.z*d); }
  friend ObjXYZ operator * (const mreal d, const ObjXYZ& a)
  { return ObjXYZ(a.x*d,a.y*d,a.z*d); }
  friend ObjXYZ operator / (const ObjXYZ& a, const mreal d)
  { return ObjXYZ(a.x/d,a.y/d,a.z/d); }
  friend ObjXYZ operator * (const ObjXYZ& a, const ObjXYZ& b)
  { return ObjXYZ((a.y*b.z)-(a.z*b.y), (a.z*b.x)-(a.x*b.z), (a.x*b.y)-(a.y*b.x)); }

  mreal Length()
  {
    return sqrt(x*x+y*y+z*z);
  }

  bool Normalize()
  {
    mreal fLength=Length();
    if(fLength < FLT_EPSILON) return false;
    mreal factor=1.0/fLength;
    x *= factor;
    y *= factor;
    z *= factor;
    return true;
  }

  bool operator==(const ObjXYZ &v) const
  {
    return x==v.x && y==v.y && z==v.z;
  }
  bool operator!=(const ObjXYZ &v) const
  {
    return !(x==v.x && y==v.y && z==v.z);
  }
  bool operator<(const ObjXYZ &v) const
  {
    if(x!=v.x)
      return (x<v.x);
    if(y!=v.y)
      return (y<v.y);
    return (z<v.z);
  }
  friend std::ostream& operator << (std::ostream& out, const ObjXYZ& v)
  {
    out << "v " << v.x << " " << v.y << " " << v.z << "\n";
    return out;
  }
};

struct ObjXYZs {
	std::map<ObjXYZ,size_t> vertexcoords;
  FILE* const fp;
  ObjXYZs(FILE *f) : fp(f) {}

	size_t addVertexCoords(mreal x, mreal y, mreal z)
	{
		const ObjXYZ point(x,y,z);

		std::map<ObjXYZ,size_t>::iterator pPoint = vertexcoords.find(point);
		if(pPoint!=vertexcoords.end())
			return pPoint->second;
		else
		{
			const size_t point_index = vertexcoords.size()+1;
			vertexcoords.insert(std::make_pair(point,point_index));
      fprintf(fp,"v %.15g %.15g %.15g\n",x,y,z);

			return point_index;
		}
	}

};

struct ObjTriangle {
  size_t p1, p2, p3;
  size_t t1, t2, t3;
  ObjTriangle() :
    p1(0), p2(0), p3(0), t1(0), t2(0), t3(0) {}
  ObjTriangle(size_t P1, size_t T1, size_t P2, size_t T2, size_t P3, size_t T3) :
    p1(P1), p2(P2), p3(P3), t1(T1), t2(T2), t3(T3) {}
};

struct ObjLine {
  size_t p1, p2;
  ObjLine() :
  p1(0), p2(0) {}
  ObjLine(size_t P1, size_t P2) :
  p1(P1), p2(P2) {}
};

struct ObjGroup {
  std::deque<ObjTriangle> triangles;
  mglColor commoncolor;
  bool samecolor;
  std::map<size_t, std::deque<ObjLine> > lines;
  std::map<size_t, std::deque<size_t> > points;
  FILE* const fp;
  ObjXYZs &vertexcoords;
  ObjGroup(FILE *f,ObjXYZs& v) : commoncolor(NC), samecolor(true), fp(f), vertexcoords(v) {}

//  const HMGL gr;

//  ObjGroup(const HMGL g) : samecolor(true),	gr(g) {}

  void addSegment(size_t m, size_t p1, size_t p2)
  {
    lines[m].push_back(ObjLine(p1,p2));
  }
  void addLines(size_t m, size_t n, const size_t* p)
  {
    for (size_t i=0; i<n-1; i++) {
      lines[m].push_back(ObjLine(p[i],p[i+1]));
    }
  }
  void addLines(size_t m, size_t n, const mreal P[][3])
  {
    if(n==0)	return;
    size_t *p=new size_t[n];
    for (size_t i=0; i<n; i++) {
      p[i] = vertexcoords.addVertexCoords(P[i][0], P[i][1], P[i][2]);
    }
    for (size_t i=0; i<n-1; i++) {
      lines[m].push_back(ObjLine(p[i],p[i+1]));
    }
    delete []p;
  }

  void addPoint(size_t m, size_t p)
  {
    points[m].push_back(p);
  }
  void addTriangle(size_t p1, size_t t1, size_t p2, size_t t2, size_t p3, size_t t3)
  {
    triangles.push_back(ObjTriangle(p1,t1,p2,t2,p3,t3));
  }
  void addMonoTriangle(size_t t1, size_t p1, size_t p2, size_t p3)
  {
    triangles.push_back(ObjTriangle(p1,t1,p2,t1,p3,t1));
  }
  void addColourInfo(const mglPnt& p)
	{
		const mglColor color(p.r,p.g,p.b,p.a);

    if (samecolor) {
      if (commoncolor == NC) {
        commoncolor = color;
      } else {
        if (commoncolor != color)
          samecolor = false;
      }
    }
  }
  void writeLines() {
    for(std::map<size_t, std::deque<ObjLine> >::const_iterator pm = lines.begin(); pm != lines.end(); pm++)
    {
      fprintf(fp,"usemtl Material%" PRIuS "\n", pm->first);
      for(std::deque<ObjLine>::const_iterator pl = pm->second.begin(); pl != pm->second.end(); pl++)
        fprintf(fp,"l %" PRIuS " %" PRIuS "\n", pl->p1, pl->p2);
      }
  }
  void writePoints() {
    for(std::map<size_t, std::deque<size_t> >::const_iterator pm = points.begin(); pm != points.end(); pm++)
    {
      fprintf(fp,"usemtl Material%" PRIuS "\n", pm->first);
      for(std::deque<size_t>::const_iterator pp = pm->second.begin(); pp != pm->second.end(); pp++)
        fprintf(fp,"p %" PRIuS "\n", *pp);
    }
  }
  void writeTriangles() {
    for(std::deque<ObjTriangle>::const_iterator pt = triangles.begin(); pt != triangles.end(); pt++)
      fprintf(fp,"f %" PRIuS " %" PRIuS " %" PRIuS "\n", pt->p1, pt->p2, pt->p3);
  }
  void writeTexturedTriangles() {
    for(std::deque<ObjTriangle>::const_iterator pt = triangles.begin(); pt != triangles.end(); pt++)
      fprintf(fp,"f %" PRIuS "/%" PRIuS " %" PRIuS "/%" PRIuS " %" PRIuS "/%" PRIuS "\n", pt->p1,pt->t1, pt->p2,pt->t2, pt->p3,pt->t3);
  }
};

struct lt_mglColor
{
  bool operator()(const mglColor& c1, const mglColor& c2) const
  {
    if(c1.r!=c2.r)
      return (c1.r<c2.r);
    if(c1.g!=c2.g)
      return (c1.g<c2.g);
    if(c1.b!=c2.b)
      return (c1.b<c2.b);
    if(c1.a!=c2.a)
      return (c1.a<c2.a);
    return false;
  }
};

typedef std::map<mglColor,size_t,lt_mglColor> colormap;


struct ObjMaterials {
	colormap materialmap;
  FILE* const fp;
  ObjMaterials(FILE *f) : fp(f) {}

	size_t addColor(const mglColor& color)
	{
		colormap::iterator pc = materialmap.find(color);
		if(pc!=materialmap.end())
			return pc->second;
		else
		{
			const size_t color_index = materialmap.size();
			materialmap.insert(std::make_pair(color,color_index));
      fprintf(fp,"newmtl Material%" PRIuS "\n", color_index);
      fprintf(fp,"Ka 0.1 0.1 0.1\n");
      fprintf(fp,"Kd %g %g %g\n", color.r, color.g, color.b);
      fprintf(fp,"Ks 0.0 0.0 0.0\n");
      fprintf(fp,"d %g\nillum 2\nNs 15.0\n",color.a);

			return color_index;
		}
	}
	size_t addColor(mreal r, mreal g, mreal b, mreal a)
	{
		const mglColor color(r,g,b,a);
    return addColor(color);
  }
	size_t addColorInfo(const mglPnt& p)
	{
		return addColor(p.r,p.g,p.b,p.a);
  }
};

size_t MGL_LOCAL_CONST power_of_two(size_t input)
{
	size_t value = 1;
	while ( value < input )	value <<= 1;
	return value;
}

void MGL_EXPORT mgl_write_obj(HMGL gr, const char *fname,const char *descr, int use_png)
{
	mglCanvas *gg = dynamic_cast<mglCanvas *>(gr);
	if(!gg || gr->GetPrmNum()==0)	return;	// nothing to do

	{
		long mmin=0,mmax=0,m;
		for(size_t i=0;i<gr->Grp.size();i++)	// prepare array of indirect indexing
		{	m = gr->Grp[i].Id;	if(m<mmin) mmin=m;	if(m>mmax) mmax=m;	}
		long *ng = new long[mmax-mmin+1];
		for(size_t i=0;i<gr->Grp.size();i++)	ng[gr->Grp[i].Id-mmin] = i;
		for(size_t i=0;i<size_t(gr->GetPrmNum());i++)	// collect data for groups
		// it is rather expensive (extra 4b per primitive) but need for export to 3D
		{
			m = gr->GetPrm(i,false).id-mmin;
			if(m>=0 && m<mmax-mmin+1)	gr->Grp[ng[m]].p.push_back(i);
		}
		delete []ng;
	}

	const size_t len=strlen(fname);
	const size_t ntxt=gr->GetTxtNum();
	const size_t Ntxt=power_of_two(ntxt);
	const size_t pntnum = gr->GetPntNum();
	char *tname = new char[len+5];	strcpy(tname,fname);
	FILE *fp=fopen(fname,"wt");
	ObjXYZs vertexcoords(fp);
	std::vector<size_t> vcs(pntnum); // vertex coord ids
	ObjUVs texturecoords(fp);
	std::vector<size_t> tcs(pntnum); // texture coord ids

	// center point
	mglPnt p0;
	const mreal width  = gg->GetWidth();
	const mreal height = gg->GetHeight();
	const mreal depth  = sqrt(width*height);

	p0.x = width/2.;
	p0.y = height/2.;
	p0.z = (1.f-sqrt(width*height)/(2*depth))*depth;

	// vertices definition
	fprintf(fp,"# Created by MathGL library\n# Title: %s\n",(descr && *descr) ? descr : fname);
	for(size_t i=0;i<pntnum;i++)
	{
		const mglPnt &pp = gr->GetPnt(i);
		vcs[i] = vertexcoords.addVertexCoords(pp.x-p0.x, pp.y-p0.y, pp.z-p0.z);
		tcs[i] = texturecoords.addTextureCoords(pp.ta, pp.c, Ntxt);
	}

  // prepare MTL file
	tname[len-4]='.';	tname[len-3]='m';	tname[len-2]='t';	tname[len-1]='l';
	FILE *fpmat=fopen(tname,"wt");
	tname[len-4]='\0';
	fprintf(fpmat,"newmtl Material\n");
	fprintf(fpmat,"Ka 0.0 0.0 0.0\n");
	fprintf(fpmat,"Kd 1.0 1.0 1.0\n");
	fprintf(fpmat,"Ks 0.0 0.0 0.0\n");
	fprintf(fpmat,"d 1.0\nillum 2\n");
	if(use_png)
		fprintf(fpmat,"map_Kd %s_txt.png\n",tname);
	else
		fprintf(fpmat,"map_Kd %s_txt.tga\n",tname);
	if(use_png)
		strcat(tname,"_txt.png");
	else
		strcat(tname,"_txt.tga");
	// prepare texture file (TGA or PNG)
	const size_t txtwidth = 512;
	unsigned char *buf = new unsigned char[4*256*txtwidth*Ntxt];
	unsigned char **pbuf= (unsigned char **)malloc(256*Ntxt*sizeof(unsigned char *));
	for(size_t i=0;i<256*Ntxt;i++)	pbuf[i] = buf+4*txtwidth*i;
	for(size_t i=0;i<ntxt;i++)	gr->GetTxt(i).GetRGBAOBJ(buf+(Ntxt-1-i)*256*txtwidth*4);
	for(size_t i=ntxt;i<Ntxt;i++)
	{
		unsigned char *f=buf+(Ntxt-1-i)*256*txtwidth*4;
		const mglColor& c=BC;
		for(size_t k=0;k<256;k++)
		{
		for(size_t l=0;l<txtwidth;l++)
		{
			*f++ = int(255*c.r);
			*f++ = int(255*c.g);
			*f++ = int(255*c.b);
			*f++ = int(255*c.a);
		}
		}
	}
	if(use_png)
		mgl_pnga_save(tname,txtwidth,256*Ntxt,pbuf);
	else
		mgl_tga_save(tname,txtwidth,256*Ntxt,pbuf);
	free(pbuf);
	delete []buf;

	ObjMaterials materials(fpmat);

	// primitive definition in groups

	tname[len-4]='\0';
	fprintf(fp,"# Primitives Definitions\nmtllib %s.mtl\n",tname);
	for(size_t i=0;i<gr->Grp.size();i++)
	{
		std::vector<long> &p = gr->Grp[i].p;
		ObjGroup grp(fp, vertexcoords);
		for(size_t j=0;j<p.size();j++)
		{
			const mglPrim &q = gr->GetPrm(p[j],false);

			long n1=q.n1,n2=q.n2,n3=q.n3,n4=q.n4;
			switch(q.type)
			{
				case 0:
				if (gr->GetPnt(q.n1).a > mgl_min_a) {
				const mglPnt p = gr->GetPnt(q.n1) - p0;
				const mreal size = q.s;
				const char type = q.n4;
				mreal ss=size;
				const mglColor c(p.r, p.g, p.b, p.a);

				if(!strchr("xsSoO",type))	ss *= 1.1;
				if(type=='.' || ss==0)
				{
					const size_t m = materials.addColor(c);
					grp.addPoint(m, vcs[n1]);
				}
				else
				switch(type)
				{
					case 'P':
					{
						const size_t m = materials.addColor(c);
						const mreal P[5][3] =
						{
						{ p.x-ss,p.y-ss,p.z },
						{ p.x+ss,p.y-ss,p.z },
						{ p.x+ss,p.y+ss,p.z },
						{ p.x-ss,p.y+ss,p.z },
						{ p.x-ss,p.y-ss,p.z }
						};
						grp.addLines(m, 5, P);
					}
					case '+':
					{
						const size_t m = materials.addColor(c);
						const mreal P1[2][3] =
						{
						{ p.x-ss,p.y,p.z },
						{ p.x+ss,p.y,p.z }
						};
						grp.addLines(m, 2, P1);
						const mreal P2[2][3] =
						{
						{ p.x,p.y-ss,p.z },
						{ p.x,p.y+ss,p.z }
						};
						grp.addLines(m, 2, P2);
					}
						break;
					case 'X':
					{
						const size_t m = materials.addColor(c);
						const mreal P[5][3] =
						{
						{ p.x-ss,p.y-ss,p.z },
						{ p.x+ss,p.y-ss,p.z },
						{ p.x+ss,p.y+ss,p.z },
						{ p.x-ss,p.y+ss,p.z },
						{ p.x-ss,p.y-ss,p.z }
						};
						grp.addLines(m, 5, P);
						const mreal P1[2][3] =
						{
						{ p.x-ss,p.y-ss,p.z },
						{ p.x+ss,p.y+ss,p.z }
						};
						grp.addLines(m, 2, P1);
						const mreal P2[2][3] =
						{
						{ p.x+ss,p.y-ss,p.z },
						{ p.x-ss,p.y+ss,p.z }
						};
						grp.addLines(m, 2, P2);
					}
						break;
					case 'x':
					{
						const size_t m = materials.addColor(c);
						const mreal P1[2][3] =
						{
						{ p.x-ss,p.y-ss,p.z },
						{ p.x+ss,p.y+ss,p.z }
						};
						grp.addLines(m, 2, P1);
						const mreal P2[2][3] =
						{
						{ p.x+ss,p.y-ss,p.z },
						{ p.x-ss,p.y+ss,p.z }
						};
						grp.addLines(m, 2, P2);
					}
						break;
					case 'S':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x-ss,p.y-ss,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x+ss,p.y-ss,p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x-ss,p.y+ss,p.z);
						const size_t pi4 = vertexcoords.addVertexCoords(p.x+ss,p.y+ss,p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
						grp.addTriangle(pi4, ti, pi3, ti, pi2, ti);
					}
						break;
					case 's':
					{
						const size_t m = materials.addColor(c);
						const mreal P[5][3] =
						{
						{ p.x-ss,p.y-ss,p.z },
						{ p.x+ss,p.y-ss,p.z },
						{ p.x+ss,p.y+ss,p.z },
						{ p.x-ss,p.y+ss,p.z },
						{ p.x-ss,p.y-ss,p.z }
						};
						grp.addLines(m, 5, P);
					}
						break;
					case 'D':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x,p.y-ss,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x+ss,p.y,p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x-ss,p.y,p.z);
						const size_t pi4 = vertexcoords.addVertexCoords(p.x,p.y+ss,p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
						grp.addTriangle(pi4, ti, pi3, ti, pi2, ti);
					}
						break;
					case 'd':
					{
						const size_t m = materials.addColor(c);
						const mreal P[5][3] =
						{
						{ p.x,p.y-ss,p.z },
						{ p.x+ss,p.y,p.z },
						{ p.x,p.y+ss,p.z },
						{ p.x-ss,p.y,p.z },
						{ p.x,p.y-ss,p.z }
						};
						grp.addLines(m, 5, P);
					}
						break;
					case 'Y':
					{
						const size_t m = materials.addColor(c);
						const mreal P1[3][3] =
						{
						{ p.x,			 p.y-ss,		p.z },
						{ p.x,			 p.y,			 p.z },
						{ p.x+0.8*ss,p.y+0.6*ss,p.z }
						};
						grp.addLines(m, 3, P1);
						const mreal P2[2][3] =
						{
						{ p.x,			 p.y,			 p.z },
						{ p.x-0.8*ss,p.y+0.6*ss,p.z }
						};
						grp.addLines(m, 2, P2);
					}
						break;
					case '*':
					{
						const size_t m = materials.addColor(c);
						const mreal P1[2][3] =
						{
						{ p.x-ss,p.y,p.z },
						{ p.x+ss,p.y,p.z }
						};
						grp.addLines(m, 2, P1);
						const mreal P2[2][3] =
						{
						{ p.x-0.6*ss,p.y-0.8*ss,p.z },
						{ p.x+0.6*ss,p.y+0.8*ss,p.z }
						};
						grp.addLines(m, 2, P2);
						const mreal P3[2][3] =
						{
						{ p.x-0.6*ss,p.y+0.8*ss,p.z },
						{ p.x+0.6*ss,p.y-0.8*ss,p.z }
						};
						grp.addLines(m, 2, P3);

					}
						break;
					case 'T':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x-ss,p.y-ss/2,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x+ss,p.y-ss/2,p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x,p.y+ss,p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
					}
						break;
					case '^':
					{
						const size_t m = materials.addColor(c);
						const mreal P[4][3] =
						{
						{ p.x-ss,p.y-ss/2,p.z },
						{ p.x+ss,p.y-ss/2,p.z },
						{ p.x,	 p.y+ss,	p.z },
						{ p.x-ss,p.y-ss/2,p.z }
						};
						grp.addLines(m, 4, P);
					}
						break;
					case 'V':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x-ss,p.y+ss/2,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x,p.y-ss,p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x+ss,p.y+ss/2,p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
					}
						break;
					case 'v':
					{
						const size_t m = materials.addColor(c);
						const mreal P[4][3] =
						{
						{ p.x-ss,p.y+ss/2,p.z },
						{ p.x+ss,p.y+ss/2,p.z },
						{ p.x,	 p.y-ss,	p.z },
						{ p.x-ss,p.y+ss/2,p.z }
						};
						grp.addLines(m, 4, P);
					}
						break;
					case 'L':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x+ss/2,p.y+ss,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x-ss,	p.y,	 p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x+ss/2,p.y-ss,p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
					}
						break;
					case '<':
					{
						const size_t m = materials.addColor(c);
						const mreal P[4][3] =
						{
						{ p.x+ss/2,p.y+ss,p.z },
						{ p.x+ss/2,p.y-ss,p.z },
						{ p.x-ss,	p.y,	 p.z },
						{ p.x+ss/2,p.y+ss,p.z }
						};
						grp.addLines(m, 4, P);
					}
						break;
					case 'R':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t pi1 = vertexcoords.addVertexCoords(p.x-ss/2,p.y+ss,p.z);
						const size_t pi2 = vertexcoords.addVertexCoords(p.x-ss/2,p.y-ss,p.z);
						const size_t pi3 = vertexcoords.addVertexCoords(p.x+ss,	p.y,	 p.z);

						grp.addTriangle(pi1, ti, pi2, ti, pi3, ti);
					}
						break;
					case '>':
					{
						const size_t m = materials.addColor(c);
						const mreal P[4][3] =
						{
						{ p.x-ss/2,p.y+ss,p.z },
						{ p.x-ss/2,p.y-ss,p.z },
						{ p.x+ss,	p.y,	 p.z },
						{ p.x-ss/2,p.y+ss,p.z }
						};
						grp.addLines(m, 4, P);
					}
						break;
					case 'O':
					{
						const size_t ti = tcs[n1]; grp.addColourInfo(p);

						const size_t cpi=vertexcoords.addVertexCoords(p.x, p.y, p.z);
						size_t pnti[21];
						for(size_t k=0;k<=20;k++)
						pnti[k]=vertexcoords.addVertexCoords(p.x+ss*cos(k*M_PI/10),p.y+ss*sin(k*M_PI/10),p.z);
						for(size_t k=0;k<20;k++) {
						grp.addTriangle(pnti[k], ti, pnti[k+1], ti, cpi, ti);
						}
					}
						break;
					case 'C':
					{
						const size_t m = materials.addColor(c);
						grp.addPoint(m, vcs[n1]);
					}
					case 'o':
					{
						const size_t m = materials.addColor(c);
						mreal P[21][3];
						for(size_t k=0;k<=20;k++) {
						P[k][0] = p.x+ss*cos(k*M_PI/10);
						P[k][1] = p.y+ss*sin(k*M_PI/10);
						P[k][2] = p.z;
						}
						grp.addLines(m, 21, P);
					}
						break;
					}
				}
				break;
				case 1:
				if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a) {
				const mglPnt& p1 = gr->GetPnt(q.n1);
				const mglPnt& p2 = gr->GetPnt(q.n2);
				const size_t m = materials.addColor((p1.r+p2.r)/2, (p1.g+p2.g)/2, (p1.b+p2.b)/2, (p1.a+p2.a)/2);
				grp.addSegment(m, vcs[n1], vcs[n2]);
				}
				break;
				case 2:
				if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a || gr->GetPnt(q.n3).a > mgl_min_a) {
				grp.addTriangle(vcs[n1],tcs[n1], vcs[n2],tcs[n2], vcs[n3],tcs[n3]);
				grp.addColourInfo(gr->GetPnt(n1));
				grp.addColourInfo(gr->GetPnt(n2));
				grp.addColourInfo(gr->GetPnt(n3));
				}
				break;
				case 3:
				if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a || gr->GetPnt(q.n3).a > mgl_min_a || gr->GetPnt(q.n4).a > mgl_min_a) {
				grp.addTriangle(vcs[n1],tcs[n1], vcs[n2],tcs[n2], vcs[n3],tcs[n3]);
				grp.addTriangle(vcs[n2],tcs[n2], vcs[n4],tcs[n4], vcs[n3],tcs[n3]);
				grp.addColourInfo(gr->GetPnt(n1));
				grp.addColourInfo(gr->GetPnt(n2));
				grp.addColourInfo(gr->GetPnt(n3));
				grp.addColourInfo(gr->GetPnt(n4));
				}
				break;
				case 4:
				{
				const mglPnt p = gr->GetPnt(q.n1) - p0;

				const mreal f = q.p/2, dx=p.u/2, dy=p.v/2;
				const mreal c=q.s*cos(q.w*M_PI/180), s=-q.s*sin(q.w*M_PI/180);
				const double b[4] = {c,-s, s,c};
				long ik,il=0;

				const mglGlyph &g = gr->GetGlf(q.n4);
				const mreal dd = 0.004;
				if(q.n3&8)
				{
					const size_t p_4 = vertexcoords.addVertexCoords(p.x+b[0]*dx+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
					const size_t p_3 = vertexcoords.addVertexCoords(p.x+b[0]*dx+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);
					const size_t p_2 = vertexcoords.addVertexCoords(p.x+b[0]*(dx+f)+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
					const size_t p_1 = vertexcoords.addVertexCoords(p.x+b[0]*(dx+f)+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);

					if(!(q.n3&4))	// glyph_line(p,f,true, d);
					{
					const size_t ti = tcs[n1]; grp.addColourInfo(p);
					grp.addMonoTriangle(ti, p_1, p_3, p_2);
					grp.addMonoTriangle(ti, p_4, p_2, p_3);
					}
					else	// glyph_line(p,f,false, d);
					{
					const size_t m = materials.addColor(p.r, p.g, p.b, p.a);
					grp.addSegment(m, p_1, p_2);
					grp.addSegment(m, p_3, p_4);
					grp.addSegment(m, p_1, p_3);
					grp.addSegment(m, p_2, p_4);
					}
				}
				else
				{
					if(!(q.n3&4))	// glyph_fill(p,f,g, d);
					{
					for(ik=0;ik<g.nt;ik++)
					{
						mreal x,y;
						x = dx+f*g.trig[6*ik];		y = dy+f*g.trig[6*ik+1];
						const size_t p_3 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						x = dx+f*g.trig[6*ik+2];	y = dy+f*g.trig[6*ik+3];
						const size_t p_2 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						x = dx+f*g.trig[6*ik+4];	y = dy+f*g.trig[6*ik+5];
						const size_t p_1 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);

						const size_t ti = tcs[n1]; grp.addColourInfo(p);
						grp.addMonoTriangle(ti, p_1, p_3, p_2);
					}
					}
					else	// glyph_wire(p,f,g, d);
					{
					const size_t m = materials.addColor(p.r, p.g, p.b, p.a);
					for(ik=0;ik<g.nl;ik++)
					{
						mreal x,y;
						x = g.line[2*ik];	y = g.line[2*ik+1];
						if(x==0x3fff && y==0x3fff)	// line breakthrough
						{	il = ik+1;	continue;	}
						else if(ik==g.nl-1 || (g.line[2*ik+2]==0x3fff && g.line[2*ik+3]==0x3fff))
						{	// enclose the circle. May be in future this block should be commented
						x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
						const size_t p_2 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						x = dx+f*g.line[2*il];		y = dy+f*g.line[2*il+1];
						const size_t p_1 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						grp.addSegment(m, p_1, p_2);
						}
						else
						{	// normal line
						x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
						const size_t p_2 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						x = dx+f*g.line[2*ik+2];	y = dy+f*g.line[2*ik+3];
						const size_t p_1 = vertexcoords.addVertexCoords(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
						grp.addSegment(m, p_1, p_2);
						}

					}
					}
				}

				}
				break;
			}
		}
		if (!grp.triangles.empty() || !grp.lines.empty() || !grp.points.empty())
		fprintf(fp,"g %s\n",gr->Grp[i].Lbl.c_str());

		if (!grp.triangles.empty()) {
		if (grp.samecolor) {
			fprintf(fp,"usemtl Material%" PRIuS "\n", materials.addColor(grp.commoncolor));
			grp.writeTriangles();
		} else {
			fprintf(fp,"usemtl Material\n");
			grp.writeTexturedTriangles();
		}
		}
		grp.writeLines();
		grp.writePoints();

		gr->Grp[i].p.clear();	// we don't need indexes anymore
	}
	fclose(fp);
	fclose(fpmat);
	delete []tname;
}

void MGL_EXPORT mgl_write_obj_(uintptr_t *gr, const char *fname,const char *descr, int *use_png,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]='\0';
	char *d=new char[n+1];	memcpy(d,descr,n);	d[n]='\0';
	mgl_write_obj(_GR_,s,d,*use_png);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
