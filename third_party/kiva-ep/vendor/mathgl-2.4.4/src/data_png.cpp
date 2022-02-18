/***************************************************************************
 * data_png.cpp is part of Math Graphic Library
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
#include "mgl2/base.h"
#include "mgl2/data.h"
#if MGL_HAVE_PNG
#include <png.h>
#endif
#if MGL_HAVE_JPEG
#include <jpeglib.h>
#endif
//-----------------------------------------------------------------------------
unsigned MGL_LOCAL_PURE mgl_col_dif(unsigned char *c1,unsigned char *c2)
{
	unsigned d1=abs(int(c1[0])-c2[0]), d2=abs(int(c1[1])-c2[1]), d3=abs(int(c1[2])-c2[2]);
	d2 = d2>d3?d2:d3;	d2 = d2>d1?d2:d1;	return d2;
}
//-----------------------------------------------------------------------------
MGL_NO_EXPORT unsigned char *mgl_create_scheme(const char *scheme,long &num)
{
	unsigned char *c=0,*cc=new unsigned char[3*strlen(scheme)+3],*c1,*c2;
	size_t nc=1,np=0,l=strlen(scheme);
	mglColor col;
	for(size_t i=0;i<l;i++)
	{
		col.Set(scheme[i]);
		if(col.Valid())
		{	cc[3*np]=255*col.r;	cc[3*np+1]=255*col.g;	cc[3*np+2]=255*col.b;	np++;	}
	}
	if(np<2)	{	num=0;	delete []cc;	return 0;	}
	for(size_t i=0;i<np-1;i++)	nc+=mgl_col_dif(cc+3*i,cc+3*i+3);
	c = new unsigned char[3*nc+3];
	size_t pos=0;
	for(size_t i=0;i<np-1;i++)
	{
		size_t dd=mgl_col_dif(cc+3*i,cc+3*i+3);
		for(size_t j=0;j<dd;j++)
		{
			c1 = c+3*(pos+j);	c2 = cc+3*i;
			c1[0] = c2[0]+(c2[3]-c2[0])*j/dd;
			c1[1] = c2[1]+(c2[4]-c2[1])*j/dd;
			c1[2] = c2[2]+(c2[5]-c2[2])*j/dd;
		}
		pos += dd;
	}
	memcpy(c+3*nc-3,cc+3*np-3,3);	delete []cc;
	num=nc;	return c;
}
//-----------------------------------------------------------------------------
bool MGL_NO_EXPORT mgl_read_image(unsigned char **g, int &w, int &h, const char *fname)
{
	const char *ext = fname+strlen(fname)-1;	// rindex(fname,'.');
	while(*ext!='.' && ext!=fname)	ext--;
	if(!strcmp(ext,".png"))
	{
#if MGL_HAVE_PNG
		FILE *fp = fopen(fname, "rb");
		if(!fp)	return false;
		png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
		if (!png_ptr)	{	fclose(fp);	return false;	}
		png_infop info_ptr = png_create_info_struct(png_ptr);
		if (!info_ptr)
		{	png_destroy_read_struct(&png_ptr,0,0);	fclose(fp);	return false;	}
		png_infop end_info = png_create_info_struct(png_ptr);
		if (!end_info)
		{	png_destroy_read_struct(&png_ptr,&info_ptr,0);	fclose(fp);	return false;	}

		png_init_io(png_ptr, fp);
		png_read_png(png_ptr, info_ptr, PNG_TRANSFORM_PACKING|PNG_TRANSFORM_STRIP_16|PNG_TRANSFORM_EXPAND,0);
		unsigned char **rows = png_get_rows(png_ptr, info_ptr);

		long wi=png_get_image_width(png_ptr, info_ptr);
		long hi=png_get_image_height(png_ptr, info_ptr);
		int type = png_get_color_type(png_ptr, info_ptr);
		if(*g)
		{
			if(wi>w)	wi = w;
			if(hi>h)	hi = h;
		}
		else	{	w = wi;	h = hi;	*g = new unsigned char[4*w*h];	}
		if(type==PNG_COLOR_TYPE_RGB_ALPHA)
#pragma omp parallel for
			for(long i=0;i<hi;i++)	memcpy(*g+4*w*i,rows[i],4*wi);
		else if(type==PNG_COLOR_TYPE_RGB)
#pragma omp parallel for collapse(2)
			for(long i=0;i<hi;i++)	for(long j=0;j<wi;j++)
				memcpy(*g+4*(w*i+j),rows[i]+3*j,3);
		else if(type==PNG_COLOR_TYPE_GRAY)
#pragma omp parallel for collapse(2)
			for(long i=0;i<hi;i++)	for(long j=0;j<wi;j++)
				(*g)[4*(w*i+j)] = (*g)[4*(w*i+j)+1] = (*g)[4*(w*i+j)+2] = rows[i][j];
		else if(type==PNG_COLOR_TYPE_GRAY_ALPHA)
#pragma omp parallel for collapse(2)
			for(long i=0;i<hi;i++)	for(long j=0;j<wi;j++)
			{
				(*g)[4*(w*i+j)] = (*g)[4*(w*i+j)+1] = (*g)[4*(w*i+j)+2] = rows[i][2*j];
				(*g)[4*(w*i+j)+3] = rows[i][2*j+1];
			}
		png_destroy_read_struct(&png_ptr, &info_ptr,&end_info);
		fclose(fp);
#else
		mgl_set_global_warn(_("PNG support was disabled. Please, enable it and rebuild MathGL."));
#endif
	}
	else if(!strcmp(ext,".jpg") || !strcmp(ext,".jpeg"))
	{
#if MGL_HAVE_JPEG
		FILE *fp = fopen(fname, "rb");
		if(!fp)	return false;

		jpeg_decompress_struct info;
		jpeg_error_mgr err;
		info.err = jpeg_std_error(&err);
		jpeg_create_decompress(&info);

		jpeg_stdio_src(&info, fp);
		jpeg_read_header(&info, TRUE);	// read jpeg file header
		jpeg_start_decompress(&info);	// decompress the file

		long wi = info.output_width;	//set width and height
		long hi = info.output_height;
		int channels = info.num_components;	// == 4 for RGBA else for RGB
		unsigned char *buf = new unsigned char[(channels==4?4:3)*wi];
		if(*g)
		{
			if(hi>h)	hi = h;
			if(wi>w)	wi = w;
		}
		else	{	w = wi;	h = hi;	*g = new unsigned char[4*w*h];	}
		for(long i=0;i<hi;i++)
		{
			jpeg_read_scanlines(&info, &buf, 1);
			if(channels==4)
				memcpy(*g+4*i*w,buf,4*wi);
			else
#pragma omp parallel for
				for(long j=0;j<wi;j++)
					memcpy(*g+4*i*w+4*j,buf+3*j,3);
		}
		delete []buf;
#else
		mgl_set_global_warn(_("JPEG support was disabled. Please, enable it and rebuild MathGL."));
#endif
	}
	return true;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_import(HMDT d, const char *fname, const char *scheme,mreal v1,mreal v2)
{
	if(v1>=v2)	return;
	unsigned char *g = 0;
	int w=0, h=0;
	if(!mgl_read_image(&g,w,h,fname))	return;
#ifdef OLD_IMPORT	
	const mglTexture c(scheme,1);
	if(c.n<2)	return;
	d->Create(w,h,1);
	float *ll = new float[3*c.n-1];
	mglColor *lc = new mglColor[c.n-1];
	const mglColor *c0=c.c0;
	for(long i=0;i<c.n-1;i++)
	{
		lc[i] = c.c0[2*i+2]-c.c0[2*i];
		float tmp = lc[i]*lc[i];
		ll[3*i] = tmp?1/tmp:0;
		ll[3*i+1] = c.val[i];
		ll[3*i+2] = c.val[i+1]-c.val[i];
	}
#pragma omp parallel for collapse(2)
	for(long i=0;i<h;i++)	for(long j=0;j<w;j++)
	{
		mglColor cc(g+4*w*(d->ny-i-1)+4*j);
		float pos=NAN, mval=256;
		for(long k=0;k<c.n-1;k++)
		{
			mglColor tc(cc-c0[2*k]);
			float u = (tc*lc[k])*ll[3*k];	tc -= lc[k]*u;
			float v = tc*tc, u0=ll[3*k+1], du=ll[3*k+2];
			if(v==0)
			{
				if(u>=0 && u<=1)	{	pos=u*du+u0;mval=0;	break;	}
//				else if(u>-v && u<0){	pos=u0;	mval=0;	break;	}
//				else if(u<1+v)	{	pos=du+u0;	mval=0;	break;	}
			}
			else if(v<mval)
			{
				if(u>=0 && u<=1)	{	pos=u*du+u0;	mval=v;	}
//				else if(u>-v && u<0){	pos=u0;	mval=v;	}
//				else if(u<1+v)	{	pos=du+u0;	mval=v;	}
			}
		}
		long K=c.n-2;
		float uF = (mglColor(cc-c0[0])*lc[0])*ll[0];
		float uL = (mglColor(cc-c0[2*K])*lc[K])*ll[3*K];
		if(mgl_isnan(pos) && uF<0)	pos = 0;
		if(mgl_isnan(pos) && uL>1)	pos = 1;
		d->a[j+d->nx*i] = v1 + pos*(v2-v1);
	}
printf("\n");
	delete []g;	delete []ll;	delete []lc;
#else
	long num=0;
	unsigned char *c = mgl_create_scheme(scheme,num);
	if(num<2)	return;
	d->Create(w,h,1);
#pragma omp parallel for collapse(2)
	for(long i=0;i<h;i++)	for(long j=0;j<w;j++)
	{
		unsigned pos=0,mval=256*256;
		const unsigned char *c2=g+4*w*(d->ny-i-1)+4*j;
		for(long k=0;k<num;k++)
		{
			const unsigned char *c1=c+3*k;
			int v0=int(c1[0])-c2[0];
			int v1=int(c1[1])-c2[1];
			int v2=int(c1[2])-c2[2];
			unsigned val = v0*v0+v1*v1+v2*v2;
			if(val==0)		{	pos=k;	break;	}
			if(val<mval)	{	pos=k;	mval=val;	}
		}
		d->a[j+d->nx*i] = v1 + pos*(v2-v1)/(num-1);
	}
	delete []c;	delete []g;
#endif
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_png_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_bmp_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_tga_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_jpeg_save(const char *fname, int w, int h, unsigned char **p);
int MGL_NO_EXPORT mgl_bps_save(const char *fname, int w, int h, unsigned char **p);
void MGL_EXPORT mgl_data_export(HCDT dd, const char *fname, const char *scheme, double v1, double v2, long ns)
{
	long nx=dd->GetNx(), ny=dd->GetNy(), nz=dd->GetNz();
	if(v1>v2)	return;
	if(ns<0 || ns>=nz)	ns=0;
	if(v1==v2)
	{
		v1 = INFINITY;	v2=-INFINITY;
		for(long i=0;i<nx*ny*nz;i++)
		{	mreal vv = dd->vthr(i);	if(vv<v1)	v1=vv;	if(vv>v2)	v2=vv;	}
	}
	if(v1==v2)	return;
	long num=0;
	unsigned char *c = mgl_create_scheme(scheme,num);
	if(num<2)	{	delete []c;		return;		}

	unsigned char **p = new unsigned char*[ny];
	unsigned char *d = new unsigned char[3*nx*ny];
	#pragma omp parallel for
	for(long i=0;i<ny;i++)	p[i] = d+3*nx*(ny-1-i);
	#pragma omp parallel for collapse(2)
	for(long i=0;i<ny;i++)	for(long j=0;j<nx;j++)
	{
		long k = long(num*(dd->v(j,i,ns)-v1)/(v2-v1));
		if(k<0)		k=0;
		if(k>=num) k=num-1;
		memcpy(d+3*(j+i*nx),c+3*k,3);
	}
	delete []c;

	int len=strlen(fname);
	if(!strcmp(fname+len-4,".jpg") || !strcmp(fname+len-5,".jpeg"))	mgl_jpeg_save(fname, nx,ny,p);
	if(!strcmp(fname+len-4,".bmp")) 	mgl_bmp_save(fname, nx,ny,p);
	if(!strcmp(fname+len-4,".png")) 	mgl_png_save(fname, nx,ny,p);
	if(!strcmp(fname+len-4,".eps") || !strcmp(fname+len-4,".bps")) 	mgl_bps_save(fname, nx,ny,p);
	delete []p;	delete []d;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_export_(uintptr_t *d, const char *fname, const char *scheme,mreal *v1,mreal *v2,int *ns,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,scheme,n);	f[n]=0;
	mgl_data_export(_DT_,s,f,*v1,*v2,*ns);	delete []s;		delete []f;	}
void MGL_EXPORT mgl_data_import_(uintptr_t *d, const char *fname, const char *scheme,mreal *v1,mreal *v2,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,scheme,n);	f[n]=0;
	mgl_data_import(_DT_,s,f,*v1,*v2);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
