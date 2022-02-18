/***************************************************************************
 * prc.cpp is part of Math Graphic Library
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
#include <png.h>
#include <stdio.h>
#include <time.h>
#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"

#include "prc/oPRCFile.h"
#include <map>
#include <utility>
#include <string.h>
#include <iostream>
#include <iomanip>
#include <fstream>

#if MGL_HAVE_PDF
#include <setjmp.h>
#include <hpdf.h>
#include <hpdf_u3d.h>
#include <hpdf_annotation.h>
#endif // MGL_HAVE_PDF


#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
#define _Gr_	((mglCanvas *)(gr))

//-----------------------------------------------------------------------------
void mglTexture::GetRGBAPRC(unsigned char *f) const
{
	for(size_t i=0;i<256;i++)
	{
		mglColor c1 = col[2*i], c2 = col[2*i+1], c;
		for(size_t j=0;j<256;j++)
		{
			size_t i0 = 4*(j+256*(255-i));
			c = c1 + (c2-c1)*(j/255.);
			f[i0]   = int(255*c.r);
			f[i0+1] = int(255*c.g);
			f[i0+2] = int(255*c.b);
			f[i0+3] = int(255*c.a);
		}
	}
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_tga_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_pnga_save(const char *fname, int w, int h, unsigned char **p);
void MGL_NO_EXPORT mgl_printf(void *fp, bool gz, const char *str, ...);
//-----------------------------------------------------------------------------
struct prctriangle {
	uint32_t pi[3];
	uint32_t ti[3];
};
//-----------------------------------------------------------------------------
struct prctriangles {
	prctriangles(const HMGL g) : samecolour(true), samealpha(true),
	gr(g), ntxt(g->GetTxtNum()), vertexcolor(g->get(MGL_PREFERVC)) {}
	std::map<PRCVector3d,uint32_t> points;
	std::map<PRCVector2d,uint32_t> texturecoords;
	std::map<RGBAColour,uint32_t> colours;
	std::vector<prctriangle> triangles;
	RGBAColour commoncolour;
	bool samecolour;
	float commonalpha;
	bool samealpha;
	const HMGL gr;
	const size_t ntxt;
	bool vertexcolor;


	uint32_t addPoint(const mglPnt& p)
	{
		const PRCVector3d point(p.x,p.y,p.z);

		std::map<PRCVector3d,uint32_t>::iterator pPoint = points.find(point);
		if(pPoint!=points.end())
			return pPoint->second;
		else
		{
			const uint32_t point_index = (uint32_t)points.size();
			points.insert(std::make_pair(point,point_index));
			return point_index;
		}
	}

	uint32_t addPoint(float x, float y, float z)
	{
		const PRCVector3d point(x,y,z);

		std::map<PRCVector3d,uint32_t>::iterator pPoint = points.find(point);
		if(pPoint!=points.end())
			return pPoint->second;
		else
		{
			const uint32_t point_index = (uint32_t)points.size();
			points.insert(std::make_pair(point,point_index));
			return point_index;
		}
	}

	void writePoints(double (*P)[3])
	{
		for(std::map<PRCVector3d,uint32_t>::const_iterator pPoint = points.begin(); pPoint != points.end(); pPoint++)
		{
			P[pPoint->second][0] = pPoint->first.x;
			P[pPoint->second][1] = pPoint->first.y;
			P[pPoint->second][2] = pPoint->first.z;
		}
	}

	void addTriangle(uint32_t ti, uint32_t pi1, uint32_t pi2, uint32_t pi3)
	{
		prctriangle triangle;
		triangle.pi[0] = pi1;
		triangle.pi[1] = pi2;
		triangle.pi[2] = pi3;
		triangle.ti[0] = ti;
		triangle.ti[1] = ti;
		triangle.ti[2] = ti;
		triangles.push_back(triangle);
	}
	void addTriangle(uint32_t pi1, uint32_t ti1, uint32_t pi2, uint32_t ti2, uint32_t pi3, uint32_t ti3)
	{
		prctriangle triangle;
		triangle.pi[0] = pi1;
		triangle.pi[1] = pi2;
		triangle.pi[2] = pi3;
		triangle.ti[0] = ti1;
		triangle.ti[1] = ti2;
		triangle.ti[2] = ti3;
		triangles.push_back(triangle);
	}
	uint32_t addColourInfo(const mglPnt& p)
	{
		const RGBAColour colour(p.r,p.g,p.b,p.a);

		if (colours.empty() && texturecoords.empty()) {
			commoncolour = colour;
			commonalpha = p.a;
		}
		if (samecolour) {
			if (commoncolour != colour)
				samecolour = false;
		}
		if (samealpha) {
			if (commonalpha != p.a)
				samealpha = false;
		}

		if (vertexcolor) {
			std::map<RGBAColour,uint32_t>::iterator pColour = colours.find(colour);
			if(pColour!=colours.end())
				return pColour->second;
			else
			{
				const uint32_t colour_index = (uint32_t)colours.size();
				colours.insert(std::make_pair(colour,colour_index));
				return colour_index;
			}
		} else {
			const mreal gap = 1./512;
			const double u = p.ta*(1-2*gap)+gap;
			const double v = ((p.c-floor(p.c))*(1-2*gap) + gap + floor(p.c))/ntxt;

			const PRCVector2d point(u, v);
			std::map<PRCVector2d,uint32_t>::iterator pPoint = texturecoords.find(point);
			if(pPoint!=texturecoords.end())
				return pPoint->second;
			else
			{
				const uint32_t point_index = (uint32_t)texturecoords.size();
				texturecoords.insert(std::make_pair(point,point_index));
				return point_index;
			}
		}
	}

	void writeTextureCoords(double (*T)[2])
	{
		for(std::map<PRCVector2d,uint32_t>::const_iterator pPoint = texturecoords.begin(); pPoint != texturecoords.end(); pPoint++)
		{
			T[pPoint->second][0] = pPoint->first.x;
			T[pPoint->second][1] = pPoint->first.y;
		}
	}

	void writeColours(RGBAColour *C)
	{
		for(std::map<RGBAColour,uint32_t>::const_iterator pColour = colours.begin(); pColour != colours.end(); pColour++)
		{
			C[pColour->second] = pColour->first;
		}
	}
};

//-----------------------------------------------------------------------------
/* structure to store PNG image bytes */
struct png_buf
{
	uint8_t *data;
	size_t size;
};
//-----------------------------------------------------------------------------
void my_png_write_data(png_structp png_ptr, png_bytep data, png_size_t length)
{
	struct png_buf* p=(struct png_buf*)png_get_io_ptr(png_ptr);
	size_t nsize = p->size + length;

	/* allocate or grow buffer */
	if(p->data)
		p->data = (uint8_t*)realloc(p->data, nsize);
	else
		p->data = (uint8_t*)malloc(nsize);

	if(!p->data)
		png_error(png_ptr, "Write Error - no mem");

	/* copy new bytes to end of buffer */
	memcpy(p->data + p->size, data, length);
	p->size += length;
}
//-----------------------------------------------------------------------------
void my_png_flush(png_structp /*png_ptr*/)
{
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_prc(HMGL gr, const char *fname,const char* /*descr*/, int make_pdf)
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
	char * const tname = new char[len+9];	strcpy(tname,fname);
	if (strncmp(tname+len-4, ".prc", 4)!=0)
	{
		tname[len]='.';	tname[len+1]='p';	tname[len+2]='r';	tname[len+3]='c'; tname[len+4]='\0';
	}
	oPRCFile file(tname);
	PRCoptions grpopt;
	grpopt.tess = true;
	grpopt.closed = gr->get(MGL_ONESIDED); // set to true to make only front side visible
	// grpopt.no_break = true;
	// grpopt.do_break = false;
	grpopt.crease_angle = 80;

	uint32_t materialMathGLid = m1;
	if (gr->get(MGL_PREFERVC)) {
		const PRCmaterial materialMathGL(
			RGBAColour(0.1,0.1,0.1,1), // ambient
			RGBAColour(1.0,1.0,1.0,1), // diffuse
			RGBAColour(0.1,0.1,0.1,1), // emissive
			RGBAColour(0.0,0.0,0.0,1), // spectral
			1.0,0.1 // alpha, shininess
			);
		materialMathGLid = file.addMaterial(materialMathGL);
	}
	else
	{
		png_buf buffer;
		buffer.data = (uint8_t*)malloc(1024);;
		buffer.size = 0;
		const size_t ntxt = gr->GetTxtNum();

		// prepare texture file (PNG)
		const png_uint_32 width=256, height=256*png_uint_32(ntxt);
		png_bytep buf = new png_byte[4*width*height];
		png_bytepp pbuf= new png_bytep[height];
		for(size_t i=0;i<height;i++)
			pbuf[i] = buf+4*width*i;
		for(size_t i=0;i<ntxt;i++)
			gr->GetTxt(i).GetRGBAPRC(buf+(ntxt-1-i)*256*width*4);

		png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,0,0,0);
		png_infop info_ptr = png_create_info_struct(png_ptr);

		png_set_write_fn(png_ptr, &buffer, my_png_write_data, my_png_flush);
		png_set_filter(png_ptr, 0, PNG_ALL_FILTERS);
		png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);
		png_set_IHDR(png_ptr, info_ptr, width, height, 8,
				PNG_COLOR_TYPE_RGB_ALPHA,
				PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
				PNG_FILTER_TYPE_DEFAULT);
		png_set_rows(png_ptr, info_ptr, pbuf);
		png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, 0);
		png_write_end(png_ptr, info_ptr);

		png_destroy_write_struct(&png_ptr, &info_ptr);
		delete []pbuf;	delete []buf;

		PRCtexture* t = new PRCtexture();
		t->mapping = PRC_TEXTURE_MAPPING_DIFFUSE;
		t->components = PRC_TEXTURE_MAPPING_COMPONENTS_RGBA;
		// Modulate for OBJ compatibilty, Replace is a better setting
		t->function = KEPRCTextureFunction_Replace;
		// Repeat for OBJ compatibilty, ClampToEdge is a better setting
		t->wrapping_mode_S = KEPRCTextureWrappingMode_ClampToEdge;
		t->wrapping_mode_T = KEPRCTextureWrappingMode_ClampToEdge;
		t->data = buffer.data;
		t->size	= buffer.size;
		t->format = KEPRCPicture_PNG;

		PRCmaterial m(
									RGBAColour(0.0,0.0,0.0,1), // ambient
									RGBAColour(1.0,1.0,1.0,1), // diffuse
									RGBAColour(0.0,0.0,0.0,1), // emissive
									RGBAColour(0.0,0.0,0.0,1), // spectral
									1.0,0.1); // alpha, shininess

		materialMathGLid = file.addTexturedMaterial(m,1,&t);
		delete t;

		// char * const pngname = new char[len+100];
		// strcpy(pngname, "test_texture_");
		// strcat(pngname,tname);
		// const size_t tlen=strlen(pngname)-4;
		// pngname[tlen+1]='p';	pngname[tlen+2]='n';	pngname[tlen+3]='g';
		// FILE *fp = fopen(pngname, "wb");
		// fwrite(buffer.data, 1, buffer.size, fp);
		// fclose(fp);
		// delete[] pngname;

		free(buffer.data); buffer.data = NULL;
	}

	// primitive definition in groups

	mglPnt p0;
	const double width  = gg->GetWidth();
	const double height = gg->GetHeight();
	const double depth  = sqrt(width*height);

	p0.x = width/2.;
	p0.y = height/2.;
	p0.z = (1.f-sqrt(width*height)/(2*depth))*depth;

	for(size_t i=0;i<gr->Grp.size();i++)
	{
		mglGroup& grp = gr->Grp[i];
		std::vector<long>& prm = grp.p;
		prctriangles group(gr);
		file.begingroup(grp.Lbl.c_str(),&grpopt);
		for(size_t j=0;j<prm.size();j++)
		{
			const mglPrim &q=gr->GetPrm(prm[j],false);
			const double w = (q.w>1)?(q.w*sqrt(gr->FontFactor()/400.)):1;

			const mglPnt p = gr->GetPnt(q.n1) - p0;
			const mreal size = q.s;
			{
				switch(q.type)
				{
					case 0:
					if (gr->GetPnt(q.n1).a > mgl_min_a) {
						const char type = q.n4;
						float ss=size;
						const RGBAColour c(p.r, p.g, p.b, p.a);

						if(!strchr("xsSoO",type))	ss *= 1.1;
						if(type=='.' || ss==0)
						{
							const double P[3] = {p.x, p.y, p.z};
							file.addPoint(P, c, w);
						}
						else
							switch(type)
							{
								case 'P':
								{
									const double P[5][3] =
									{
										{ p.x-ss,p.y-ss,p.z },
										{ p.x+ss,p.y-ss,p.z },
										{ p.x+ss,p.y+ss,p.z },
										{ p.x-ss,p.y+ss,p.z },
										{ p.x-ss,p.y-ss,p.z }
									};
									file.addLine(5, P, c, w);
								}
								case '+':
								{
									const double P1[2][3] =
									{
										{ p.x-ss,p.y,p.z },
										{ p.x+ss,p.y,p.z }
									};
									file.addLine(2, P1, c, w);
									const double P2[2][3] =
									{
										{ p.x,p.y-ss,p.z },
										{ p.x,p.y+ss,p.z }
									};
									file.addLine(2, P2, c, w);
								}
								break;
								case 'X':
								{
									const double P[5][3] =
									{
										{ p.x-ss,p.y-ss,p.z },
										{ p.x+ss,p.y-ss,p.z },
										{ p.x+ss,p.y+ss,p.z },
										{ p.x-ss,p.y+ss,p.z },
										{ p.x-ss,p.y-ss,p.z }
									};
									file.addLine(5, P, c, w);
									const double P1[2][3] =
									{
										{ p.x-ss,p.y-ss,p.z },
										{ p.x+ss,p.y+ss,p.z }
									};
									file.addLine(2, P1, c, w);
									const double P2[2][3] =
									{
										{ p.x+ss,p.y-ss,p.z },
										{ p.x-ss,p.y+ss,p.z }
									};
									file.addLine(2, P2, c, w);
								}
								break;
								case 'x':
								{
									const double P1[2][3] =
									{
										{ p.x-ss,p.y-ss,p.z },
										{ p.x+ss,p.y+ss,p.z }
									};
									file.addLine(2, P1, c, w);
									const double P2[2][3] =
									{
										{ p.x+ss,p.y-ss,p.z },
										{ p.x-ss,p.y+ss,p.z }
									};
									file.addLine(2, P2, c, w);
								}
								break;
								case 'S':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x-ss,p.y-ss,p.z);
									const uint32_t pi2 = group.addPoint(p.x+ss,p.y-ss,p.z);
									const uint32_t pi3 = group.addPoint(p.x-ss,p.y+ss,p.z);
									const uint32_t pi4 = group.addPoint(p.x+ss,p.y+ss,p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
									group.addTriangle(ti, pi4, pi3, pi2);
								}
								break;
								case 's':
								{
									const double P[5][3] =
									{
										{ p.x-ss,p.y-ss,p.z },
										{ p.x+ss,p.y-ss,p.z },
										{ p.x+ss,p.y+ss,p.z },
										{ p.x-ss,p.y+ss,p.z },
										{ p.x-ss,p.y-ss,p.z }
									};
									file.addLine(5, P, c, w);
								}
								break;
								case 'D':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x,p.y-ss,p.z);
									const uint32_t pi2 = group.addPoint(p.x+ss,p.y,p.z);
									const uint32_t pi3 = group.addPoint(p.x-ss,p.y,p.z);
									const uint32_t pi4 = group.addPoint(p.x,p.y+ss,p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
									group.addTriangle(ti, pi4, pi3, pi2);
								}
								break;
								case 'd':
								{
									const double P[5][3] =
									{
										{ p.x,p.y-ss,p.z },
										{ p.x+ss,p.y,p.z },
										{ p.x,p.y+ss,p.z },
										{ p.x-ss,p.y,p.z },
										{ p.x,p.y-ss,p.z }
									};
									file.addLine(5, P, c, w);
								}
								break;
								case 'Y':
								{
									const double P1[3][3] =
									{
										{ p.x,			 p.y-ss,		p.z },
										{ p.x,			 p.y,			 p.z },
										{ p.x+0.8*ss,p.y+0.6*ss,p.z }
									};
									file.addLine(3, P1, c, w);
									const double P2[2][3] =
									{
										{ p.x,			 p.y,			 p.z },
										{ p.x-0.8*ss,p.y+0.6*ss,p.z }
									};
									file.addLine(2, P2, c, w);
								}
								break;
								case '*':
								{
									const double P1[2][3] =
									{
										{ p.x-ss,p.y,p.z },
										{ p.x+ss,p.y,p.z }
									};
									file.addLine(2, P1, c, w);
									const double P2[2][3] =
									{
										{ p.x-0.6*ss,p.y-0.8*ss,p.z },
										{ p.x+0.6*ss,p.y+0.8*ss,p.z }
									};
									file.addLine(2, P2, c, w);
									const double P3[2][3] =
									{
										{ p.x-0.6*ss,p.y+0.8*ss,p.z },
										{ p.x+0.6*ss,p.y-0.8*ss,p.z }
									};
									file.addLine(2, P3, c, w);

								}
								break;
								case 'T':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x-ss,p.y-ss/2,p.z);
									const uint32_t pi2 = group.addPoint(p.x+ss,p.y-ss/2,p.z);
									const uint32_t pi3 = group.addPoint(p.x,p.y+ss,p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
								}
								break;
								case '^':
								{
									const double P[4][3] =
									{
										{ p.x-ss,p.y-ss/2,p.z },
										{ p.x+ss,p.y-ss/2,p.z },
										{ p.x,	 p.y+ss,	p.z },
										{ p.x-ss,p.y-ss/2,p.z }
									};
									file.addLine(4, P, c, w);
								}
								break;
								case 'V':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x-ss,p.y+ss/2,p.z);
									const uint32_t pi2 = group.addPoint(p.x,p.y-ss,p.z);
									const uint32_t pi3 = group.addPoint(p.x+ss,p.y+ss/2,p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
								}
								break;
								case 'v':
								{
									const double P[4][3] =
									{
										{ p.x-ss,p.y+ss/2,p.z },
										{ p.x+ss,p.y+ss/2,p.z },
										{ p.x,	 p.y-ss,	p.z },
										{ p.x-ss,p.y+ss/2,p.z }
									};
									file.addLine(4, P, c, w);
								}
								break;
								case 'L':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x+ss/2,p.y+ss,p.z);
									const uint32_t pi2 = group.addPoint(p.x-ss,	p.y,	 p.z);
									const uint32_t pi3 = group.addPoint(p.x+ss/2,p.y-ss,p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
								}
								break;
								case '<':
								{
									const double P[4][3] =
									{
										{ p.x+ss/2,p.y+ss,p.z },
										{ p.x+ss/2,p.y-ss,p.z },
										{ p.x-ss,	p.y,	 p.z },
										{ p.x+ss/2,p.y+ss,p.z }
									};
									file.addLine(4, P, c, w);
								}
								break;
								case 'R':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t pi1 = group.addPoint(p.x-ss/2,p.y+ss,p.z);
									const uint32_t pi2 = group.addPoint(p.x-ss/2,p.y-ss,p.z);
									const uint32_t pi3 = group.addPoint(p.x+ss,	p.y,	 p.z);

									group.addTriangle(ti, pi1, pi2, pi3);
								}
								break;
								case '>':
								{
									const double P[4][3] =
									{
										{ p.x-ss/2,p.y+ss,p.z },
										{ p.x-ss/2,p.y-ss,p.z },
										{ p.x+ss,	p.y,	 p.z },
										{ p.x-ss/2,p.y+ss,p.z }
									};
									file.addLine(4, P, c, w);
								}
								break;
								case 'O':
								{
									const uint32_t ti = group.addColourInfo(p);

									const uint32_t cpi=group.addPoint(p);
									uint32_t pnti[21];
									for(size_t k=0;k<=20;k++)
										pnti[k]=group.addPoint(p.x+ss*cos(k*M_PI/10),p.y+ss*sin(k*M_PI/10),p.z);
									for(size_t k=0;k<20;k++) {
										group.addTriangle(ti, pnti[k], pnti[k+1], cpi);
									}
								}
								break;
								case 'C':
								{
									const double P[3] = {p.x, p.y, p.z};
									file.addPoint(P, c, w);
								}
								case 'o':
								{
									double P[21][3];
									for(size_t k=0;k<=20;k++) {
										P[k][0] = p.x+ss*cos(k*M_PI/10);
										P[k][1] = p.y+ss*sin(k*M_PI/10);
										P[k][2] = p.z;
									}
									file.addLine(21, P, c, w);
								}
								break;
						}
					}
					break;

					case 1:
					if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a) {
						const mglPnt p1 = gr->GetPnt(q.n1) - p0, p2 = gr->GetPnt(q.n2) - p0;

						const uint32_t n = 2;
						double P[2][3];
						P[0][0] = p1.x;
						P[0][1] = p1.y;
						P[0][2] = p1.z;
						P[1][0] = p2.x;
						P[1][1] = p2.y;
						P[1][2] = p2.z;
						const RGBAColour colour((p1.r+p2.r)/2, (p1.g+p2.g)/2, (p1.b+p2.b)/2, (p1.a+p2.a)/2);
						file.addLine(n, P, colour, w);
					}
					break;

					case 2:
					if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a || gr->GetPnt(q.n3).a > mgl_min_a) {
						const mglPnt p1 = gr->GetPnt(q.n1)	- p0, p2 = gr->GetPnt(q.n2) - p0, p3 = gr->GetPnt(q.n3) - p0;

						prctriangle triangle;
						triangle.pi[0] = group.addPoint(p1);
						triangle.pi[1] = group.addPoint(p2);
						triangle.pi[2] = group.addPoint(p3);
						triangle.ti[0] = group.addColourInfo(p1);
						triangle.ti[1] = group.addColourInfo(p2);
						triangle.ti[2] = group.addColourInfo(p3);
						group.triangles.push_back(triangle);
					}
					break;
					case 3:
					if (gr->GetPnt(q.n1).a > mgl_min_a || gr->GetPnt(q.n2).a > mgl_min_a || gr->GetPnt(q.n3).a > mgl_min_a || gr->GetPnt(q.n4).a > mgl_min_a) {
						const mglPnt p1 = gr->GetPnt(q.n1) - p0;
						const uint32_t pi1 = group.addPoint(p1);
						const uint32_t ti1 = group.addColourInfo(p1);

						const mglPnt p2 = gr->GetPnt(q.n2) - p0;
						const uint32_t pi2 = group.addPoint(p2);
						const uint32_t ti2 = group.addColourInfo(p2);

						const mglPnt p3 = gr->GetPnt(q.n3) - p0;
						const uint32_t pi3 = group.addPoint(p3);
						const uint32_t ti3 = group.addColourInfo(p3);

						const mglPnt p4 = gr->GetPnt(q.n4) - p0;
						const uint32_t pi4 = group.addPoint(p4);
						const uint32_t ti4 = group.addColourInfo(p4);

						prctriangle triangle1, triangle2;
						triangle1.pi[0] = pi1;
						triangle1.pi[1] = pi2;
						triangle1.pi[2] = pi3;
						triangle1.ti[0] = ti1;
						triangle1.ti[1] = ti2;
						triangle1.ti[2] = ti3;
						group.triangles.push_back(triangle1);
						triangle2.pi[0] = pi4;
						triangle2.pi[1] = pi3;
						triangle2.pi[2] = pi2;
						triangle2.ti[0] = ti4;
						triangle2.ti[1] = ti3;
						triangle2.ti[2] = ti2;
						group.triangles.push_back(triangle2);
					}
					break;
					case 4:
					if (gr->GetPnt(q.n1).a > mgl_min_a) {
						const mglPnt p = gr->GetPnt(q.n1) - p0;

						const mreal f = q.p/2, dx=p.u/2, dy=p.v/2;
						const mreal c=q.s*cos(q.w*M_PI/180), s=-q.s*sin(q.w*M_PI/180);
						const double b[4] = {c,-s, s,c};
						long ik,il=0;

						const mglGlyph &g = gr->GetGlf(q.n4);
						const mreal dd = 0.004;
						if(q.n3&8)
						{
							if(!(q.n3&4))	// glyph_line(p,f,true, d);
							{
								const uint32_t ti = group.addColourInfo(p);
								const uint32_t p_4 = group.addPoint(p.x+b[0]*dx+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
								const uint32_t p_3 = group.addPoint(p.x+b[0]*dx+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);
								const uint32_t p_2 = group.addPoint(p.x+b[0]*(dx+f)+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z);
								const uint32_t p_1 = group.addPoint(p.x+b[0]*(dx+f)+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z);

								group.addTriangle(ti, p_1, p_3, p_2);
								group.addTriangle(ti, p_4, p_2, p_3);
							}
							else	// glyph_line(p,f,false, d);
							{
								const RGBAColour c(p.r, p.g, p.b, p.a);
								const double p_4[3] = {p.x+b[0]*dx+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z};
								const double p_3[3] = {p.x+b[0]*dx+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z};
								const double p_2[3] = {p.x+b[0]*(dx+f)+b[1]*(dy-dd),p.y+b[2]*dx+b[3]*(dy-dd),p.z};
								const double p_1[3] = {p.x+b[0]*(dx+f)+b[1]*(dy+dd),p.y+b[2]*dx+b[3]*(dy+dd),p.z};

								file.addSegment(p_1, p_2, c, w);
								file.addSegment(p_3, p_4, c, w);
								file.addSegment(p_1, p_3, c, w);
								file.addSegment(p_2, p_4, c, w);
							}
						}
						else
						{
							if(!(q.n3&4))	// glyph_fill(p,f,g, d);
							{
								for(ik=0;ik<g.nt;ik++)
								{
									const uint32_t ti = group.addColourInfo(p);
									mreal x,y;
									x = dx+f*g.trig[6*ik];		y = dy+f*g.trig[6*ik+1];
									const uint32_t p_3 = group.addPoint(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
									x = dx+f*g.trig[6*ik+2];	y = dy+f*g.trig[6*ik+3];
									const uint32_t p_2 = group.addPoint(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);
									x = dx+f*g.trig[6*ik+4];	y = dy+f*g.trig[6*ik+5];
									const uint32_t p_1 = group.addPoint(p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z);

									group.addTriangle(ti, p_1, p_3, p_2);
								}
							}
							else	// glyph_wire(p,f,g, d);
							{
								const RGBAColour c(p.r, p.g, p.b, p.a);
								for(ik=0;ik<g.nl;ik++)
								{
									mreal x,y;
									x = g.line[2*ik];	y = g.line[2*ik+1];
									if(x==0x3fff && y==0x3fff)	// line breakthrough
									{	il = ik+1;	continue;	}
									else if(ik==g.nl-1 || (g.line[2*ik+2]==0x3fff && g.line[2*ik+3]==0x3fff))
									{	// enclose the circle. May be in future this block should be commented
										x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
										const double p_2[3] = {p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z};
										x = dx+f*g.line[2*il];		y = dy+f*g.line[2*il+1];
										const double p_1[3] = {p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z};
										file.addSegment(p_1, p_2, c, w);
									}
									else
									{	// normal line
										x = dx+f*g.line[2*ik];		y = dy+f*g.line[2*ik+1];
										const double p_2[3] = {p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z};
										x = dx+f*g.line[2*ik+2];	y = dy+f*g.line[2*ik+3];
										const double p_1[3] = {p.x+b[0]*x+b[1]*y,p.y+b[2]*x+b[3]*y,p.z};
										file.addSegment(p_1, p_2, c, w);
									}

								}
							}
						}

					}
						break;

				}
			}
		}
		if (!group.triangles.empty()) {
			const uint32_t nP = (uint32_t)group.points.size();
			double (*P)[3] = new double[nP][3];
			group.writePoints(P);
			const uint32_t nI = (uint32_t)group.triangles.size();
			uint32_t (*PI)[3] = new uint32_t[nI][3];
			for(uint32_t k = 0; k<nI; k++)
			{
				PI[k][0] = group.triangles[k].pi[0];
				PI[k][1] = group.triangles[k].pi[1];
				PI[k][2] = group.triangles[k].pi[2];
			}
			if (!group.samecolour) {
				if (gr->get(MGL_PREFERVC)) {
					const uint32_t nC = (uint32_t)group.colours.size();
					RGBAColour *C = new RGBAColour[nC];
					group.writeColours(C);
					uint32_t (*CI)[3] = new uint32_t[nI][3];
					for(uint32_t k = 0; k<nI; k++)
					{
						CI[k][0] = group.triangles[k].ti[0];
						CI[k][1] = group.triangles[k].ti[1];
						CI[k][2] = group.triangles[k].ti[2];
					}
					const uint32_t tess_index = file.createTriangleMesh(nP, P, nI, PI, m1, 0, NULL, NULL, 0, NULL, NULL, nC, C, CI, 0, NULL, NULL, grpopt.crease_angle);
					uint32_t materialid = materialMathGLid;
					if (group.samealpha) { // workaround for transparency ignored in vertex colors, may not work in OpenGL
						const double a = group.commonalpha;
						const PRCmaterial materialTransparent(
							RGBAColour(0.1,0.1,0.1,a), // ambient
							RGBAColour(1.0,1.0,1.0,a), // diffuse
							RGBAColour(0.1,0.1,0.1,a), // emissive
							RGBAColour(0.0,0.0,0.0,a), // spectral
							a,0.1 // alpha, shininess
							);
						materialid = file.addMaterial(materialTransparent);
					}
					file.useMesh(tess_index, materialid);
					delete [] CI;
					delete [] C;
				} else {
					const uint32_t nT = (uint32_t)group.texturecoords.size();
					double (*T)[2] = new double[nT][2];
					group.writeTextureCoords(T);
					uint32_t (*TI)[3] = new uint32_t[nI][3];
					for(uint32_t k = 0; k<nI; k++)
					{
						TI[k][0] = group.triangles[k].ti[0];
						TI[k][1] = group.triangles[k].ti[1];
						TI[k][2] = group.triangles[k].ti[2];
					}
					const uint32_t tess_index = file.createTriangleMesh(nP, P, nI, PI, m1, 0, NULL, NULL, nT, T, TI, 0, NULL, NULL, 0, NULL, NULL, grpopt.crease_angle);
					file.useMesh(tess_index, materialMathGLid);
					delete [] TI;
					delete [] T;
				}
			} else {
				const uint32_t tess_index = file.createTriangleMesh(nP, P, nI, PI, m1, 0, NULL, NULL, 0, NULL, NULL, 0, NULL, NULL, 0, NULL, NULL, grpopt.crease_angle);

				const PRCmaterial material(
					RGBAColour(0.1,0.1,0.1,1), // ambient
					group.commoncolour,        // diffuse
					RGBAColour(0.1,0.1,0.1,1), // emissive
					RGBAColour(0.0,0.0,0.0,1), // spectral
					group.commoncolour.A,0.1); // alpha, shininess
				file.useMesh(tess_index, file.addMaterial(material));
			}
			delete [] PI;
			delete [] P;
		}
		file.endgroup();
		prm.clear();	// we don't need indexes anymore
	}
	file.finish();

	if (make_pdf) {
#if MGL_HAVE_PDF
//		const HPDF_REAL width  = dynamic_cast<mglCanvas *>(gr)->GetWidth();
//		const HPDF_REAL height = dynamic_cast<mglCanvas *>(gr)->GetHeight();
//		const HPDF_REAL depth  = sqrt(width*height);

		const HPDF_Rect rect = {0, 0, HPDF_REAL(width), HPDF_REAL(height)};

		HPDF_Doc	pdf;
		HPDF_Page page;
		HPDF_Annotation annot;
		HPDF_U3D u3d;

		HPDF_Dict view;
		pdf = HPDF_New (NULL, NULL);

		pdf->pdf_version = HPDF_VER_17;

		page = HPDF_AddPage (pdf);

		HPDF_Page_SetWidth (page, width);
		HPDF_Page_SetHeight (page, height);

		u3d = HPDF_LoadU3DFromFile (pdf, tname);

		//	Default view
		view = HPDF_Create3DView (u3d->mmgr, "DefaultView");

		//	Position camera
		HPDF_3DView_SetCamera (view, 0, 0, 0, 0, 0, 1, depth, 0);

		//	Set ortho projection
		HPDF_3DView_SetOrthogonalProjection (view, 1);

		//	Background color
		HPDF_3DView_SetBackgroundColor (view, 0.9, 0.9, 0.9);

		//	Lighting
		HPDF_3DView_SetLighting (view, "CAD");

		//	Add views
		HPDF_U3D_Add3DView (u3d, view);
		HPDF_U3D_SetDefault3DView(u3d, "DefaultView");

		//	Create annotation
		annot = HPDF_Page_Create3DAnnot (page, rect, u3d );

		//  Enable toolbar
		HPDF_Dict action = (HPDF_Dict)HPDF_Dict_GetItem (annot, "3DA", HPDF_OCLASS_DICT);
		HPDF_Dict_AddBoolean (action, "TB", HPDF_TRUE);

		/* save the document to a file */
		const size_t tlen = strlen(tname);
		tname[tlen-3]='p';	tname[tlen-2]='d';	tname[tlen-1]='f';
		HPDF_SaveToFile (pdf, tname);

		/* clean up */
		HPDF_Free (pdf);
#else
		const size_t tlen = strlen(tname);
		tname[tlen-2]='p';	tname[tlen-2]='d';	tname[tlen-1]='f';
		tname[tlen+0]='.';	tname[tlen+1]='t';	tname[tlen+2]='x';	tname[tlen+3]='t'; tname[tlen+4]='\0';
		FILE *fp=fopen(tname,"wt");
		fputs("Can not produce PDF file, MathGL compiled without PDF output support\n", fp);
		fclose(fp);
		mgl_set_global_warn(_("PDF support was disabled. Please, enable it and rebuild MathGL."));
#endif // MGL_HAVE_PDF
	}
	delete []tname;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_prc_(uintptr_t *gr, const char *fname,const char *descr, int *make_pdf,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_prc(_GR_,s,f,*make_pdf);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
