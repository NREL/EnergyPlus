/***************************************************************************
 * export.cpp is part of Math Graphic Library
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

#if defined(WIN32) || defined(_MSC_VER) || defined(__BORLANDC__)
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"

#if MGL_HAVE_PNG
#include <png.h>
#endif

#if MGL_HAVE_JPEG
extern "C" {
#include <jpeglib.h>
}
#endif

#if MGL_HAVE_GIF
#include <gif_lib.h>
#endif
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_pnga_save(const char *fname, int w, int h, unsigned char **p)
{
#if MGL_HAVE_PNG
	bool fl = strcmp(fname,"-");
	FILE *fp = fl ? fopen(fname, "wb") : stdout;
	if (!fp)	return 1;

	png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,0,0,0);
	if (png_ptr)
	{
		png_infop info_ptr = png_create_info_struct(png_ptr);
		if (info_ptr)
		{
			png_init_io(png_ptr, fp);
			png_set_filter(png_ptr, 0, PNG_ALL_FILTERS);
			png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);
			png_set_IHDR(png_ptr, info_ptr, w, h, 8,
						PNG_COLOR_TYPE_RGB_ALPHA,
						PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
						PNG_FILTER_TYPE_DEFAULT);
			png_set_rows(png_ptr, info_ptr, p);
			png_write_png(png_ptr, info_ptr,  PNG_TRANSFORM_IDENTITY, 0);
			png_write_end(png_ptr, info_ptr);
		}
		png_destroy_write_struct(&png_ptr, &info_ptr);
	}
	if(fl)	fclose(fp);
	return 0;
#else
	mgl_set_global_warn(_("PNG support was disabled. Please, enable it and rebuild MathGL."));
	return 1;
#endif
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_png_save(const char *fname, int w, int h, unsigned char **p)
{
#if MGL_HAVE_PNG
	bool fl = strcmp(fname,"-");
	FILE *fp = fl ? fopen(fname, "wb") : stdout;
	if (!fp)	return 1;

	png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,0,0,0);
	if (png_ptr)
	{
		png_infop info_ptr = png_create_info_struct(png_ptr);
		if (info_ptr)
		{
			png_init_io(png_ptr, fp);
			png_set_filter(png_ptr, 0, PNG_ALL_FILTERS);
			png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);
			png_set_IHDR(png_ptr, info_ptr, w, h, 8,
						PNG_COLOR_TYPE_RGB,
						PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
						PNG_FILTER_TYPE_DEFAULT);
			png_set_rows(png_ptr, info_ptr, p);
			png_write_png(png_ptr, info_ptr,  PNG_TRANSFORM_IDENTITY, 0);
			png_write_end(png_ptr, info_ptr);
		}
		png_destroy_write_struct(&png_ptr, &info_ptr);
	}
	if(fl)	fclose(fp);
	return 0;
#else
	mgl_set_global_warn(_("PNG support was disabled. Please, enable it and rebuild MathGL."));
	return 1;
#endif
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_bmp_save(const char *fname, int w, int h, unsigned char **p)
{
	bool fl = strcmp(fname,"-");
	FILE *fp = fl ? fopen(fname, "wb") : stdout;
	if (!fp)	return 1;

	char z[4] = {0,0,0,0};
	unsigned u = w*h*3 + 54;
	// BITMAPFILEHEADER
	fwrite("BM",2,1,fp);	fwrite(&u,4,1,fp);
	fwrite(z,4,1,fp);	u=54;	fwrite(&u,4,1,fp);
	// BITMAPINFOHEADER
	u=40;	fwrite(&u,4,1,fp);	fwrite(&w,4,1,fp);	fwrite(&h,4,1,fp);
	unsigned short pp=1;
	fwrite(&pp,2,1,fp);	pp=24;	fwrite(&pp,2,1,fp);	u = w*h*3;
	fwrite(z,4,1,fp);	fwrite(&u,4,1,fp);
	fwrite(z,4,1,fp);	fwrite(z,4,1,fp);
	fwrite(z,4,1,fp);	fwrite(z,4,1,fp);
	// image
	for(long i=h-1;i>=0;i--)	for(long j=0;j<w;j++)
	{
		const unsigned char *q = p[i]+3*j;
		fwrite(q+2,1,1,fp);
		fwrite(q+1,1,1,fp);
		fwrite(q+0,1,1,fp);
	}
	if(fl)	fclose(fp);
	return 0;
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_tga_save(const char *fname, int w, int h, unsigned char **p)
{
	bool fl = strcmp(fname,"-");
	FILE *fp = fl ? fopen(fname, "wb") : stdout;
	if (!fp)	return 1;
	// header
	char head[14]={0,0,2, 0,0,0,0,0, 0,0,0,0, 32,0};
	fwrite(head,12,1,fp);
	fwrite(&w,2,1,fp);	fwrite(&h,2,1,fp);
	fwrite(head+12,2,1,fp);
	// image
	for(long i=h-1;i>=0;i--)	for(long j=0;j<w;j++)
	{
		const unsigned char *q = p[i]+4*j;
		fwrite(q+2,1,1,fp);
		fwrite(q+1,1,1,fp);
		fwrite(q+0,1,1,fp);
		fwrite(q+3,1,1,fp);
	}
	if(fl)	fclose(fp);
	return 0;
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_jpeg_save(const char *fname, int w, int h, unsigned char **p)
{
#if MGL_HAVE_JPEG
	struct jpeg_compress_struct cinfo;
	struct jpeg_error_mgr jerr;

	bool fl = strcmp(fname,"-");
	FILE *fp = fl ? fopen(fname, "wb") : stdout;
	if (!fp)	return 1;

	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
	jpeg_stdio_dest(&cinfo, fp);
	cinfo.image_width = w;
	cinfo.image_height = h;
	cinfo.input_components = 3;
	cinfo.in_color_space = JCS_RGB;
	jpeg_set_defaults(&cinfo);
	jpeg_start_compress(&cinfo, TRUE);
	jpeg_write_scanlines(&cinfo, p, h);
	jpeg_finish_compress(&cinfo);
	jpeg_destroy_compress(&cinfo);
	if(fl)	fclose(fp);
	return 0;
#else
	mgl_set_global_warn(_("JPEG support was disabled. Please, enable it and rebuild MathGL."));
	return 1;
#endif
}
//-----------------------------------------------------------------------------
void MGL_NO_EXPORT mgl_printf(void *fp, bool gz, const char *str, ...)	// NOTE This function is not thread-safe
{
	static char buf[1024];
	va_list lst;
	va_start(lst,str);
	vsnprintf(buf,1023,str,lst);	buf[1023]=0;
	va_end(lst);
	if(gz)	gzprintf((gzFile)fp, "%s", buf);
	else	fprintf((FILE *)fp, "%s", buf);
}
//---------------------------------------------------------------------------
std::string MGL_NO_EXPORT mgl_sprintf(const char *str, ...)
{
	char *buf=new char[1024];
	va_list lst;
	va_start(lst,str);
	vsnprintf(buf,1023,str,lst);	buf[1023]=0;
	va_end(lst);
	std::string res = buf;	delete []buf;
	return res;
}
//---------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_bps_save(const char *fname, int w, int h, unsigned char **p)
{
	time_t now;	time(&now);
	bool gz = fname[strlen(fname)-1]=='z';

	void *fp;
	if(!strcmp(fname,"-"))	fp = stdout;		// allow to write in stdout
	else
	{
		fp = gz ? (void*)gzopen(fname,"wt") : (void*)fopen(fname,"wt");
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
			fprintf(fb, "%%%%BoundingBox: 0 0 %d %d\n", w, h);
			fclose(fb);	delete []buf;
		}
	}
	mgl_printf(fp, gz, "%%!PS-Adobe-3.0 EPSF-3.0\n%%%%BoundingBox: 0 0 %d %d\n",w,h);
	mgl_printf(fp, gz, "%%%%Created by MathGL library\n%%%%Title: %s\n", fname);
	mgl_printf(fp, gz, "%%%%CreationDate: %s\n",ctime(&now));
	mgl_printf(fp, gz, "%d %d 8 [1 0 0 1 0 0] {currentfile %d string readhexstring pop} false 3 colorimage\n",
			w,h,w*h/40);
	for(long j=0;j<h;j++)	for(long i=0;i<w;i++)
	{
		if((i+w*j)%40==0 && i+j>0)	mgl_printf(fp, gz, "\n");
		long jj=h-1-j;
		mgl_printf(fp, gz, "%02x%02x%02x",p[jj][3*i],p[jj][3*i+1],p[jj][3*i+2]);
	}
	mgl_printf(fp, gz, "\n\nshowpage\n%%%%EOF\n");
	if(strcmp(fname,"-"))	{	if(gz)	gzclose((gzFile)fp);	else	fclose((FILE *)fp);	}
	return 0;
}
//-----------------------------------------------------------------------------
int MGL_NO_EXPORT mgl_gif_save(const char *fname, int w, int h, unsigned char **l)
{
#if MGL_HAVE_GIF
#if GIFLIB_MAJOR>=5
	GifFileType *fg = EGifOpenFileName(fname, 0, 0);
#else
	GifFileType *fg = EGifOpenFileName(fname, 0);
#endif
	// define colormap
	GifColorType col[256];
	memset(col,0,256*sizeof(GifColorType));
	for(int i=0;i<6;i++)	for(int j=0;j<6;j++)	for(int k=0;k<6;k++)
	{
		long m = i+6*(j+6*k);		// part 1
		col[m].Red = 51*i;
		col[m].Green=51*j;
		col[m].Blue =51*k;
	}
	// write header
#if GIFLIB_MAJOR>=5
	ColorMapObject *gmap = GifMakeMapObject(256, col);
	EGifPutScreenDesc(fg, w, h, 256,0,gmap);
	GifFreeMapObject(gmap);
#else
	ColorMapObject *gmap = MakeMapObject(256, col);
	EGifPutScreenDesc(fg, w, h, 256,0,gmap);
	FreeMapObject(gmap);
#endif
	// write frame
	EGifPutImageDesc(fg, 0, 0, w, h, 0, 0);
	GifPixelType *line = new GifPixelType[w*h];
	for(long m=0;m<w*h;m++)
	{
		long ii = 3*(m%w), k = m/w;
		int i = (l[k][ii]+25)/51;
		int j = (l[k][ii+1]+25)/51;
		k = (l[k][ii+2]+25)/51;
		line[m] = i+6*(j+6*k);
	}
	EGifPutLine(fg, line, w*h);
#if GIFLIB_MAJOR>5 || (GIFLIB_MAJOR==5 && GIFLIB_MINOR>0)
	EGifCloseFile(fg,0);
#else
	EGifCloseFile(fg);
#endif
	delete []line;	return 0;
#else
	mgl_set_global_warn(_("GIF support was disabled. Please, enable it and rebuild MathGL."));
	return 1;
#endif
}
//-----------------------------------------------------------------------------
//
//		Save animation
//
//-----------------------------------------------------------------------------
void mglCanvas::StartGIF(const char *fname, int ms)
{
#if MGL_HAVE_GIF
	std::string fn=fname;
	if(fn.empty())	{	fn=PlotId+".gif";	fname = fn.c_str();	}
#if GIFLIB_MAJOR>5 || (GIFLIB_MAJOR==5 && GIFLIB_MINOR>0)
	if(gif)	EGifCloseFile(gif,0);
#else
	if(gif)	EGifCloseFile(gif);
#endif
#if GIFLIB_MAJOR>=5
	gif = EGifOpenFileName(fname, 0, 0);
	EGifSetGifVersion(gif,true);
#else
	EGifSetGifVersion("89a");
	gif = EGifOpenFileName(fname, 0);
#endif
	// get picture sizes
	// NOTE: you shouldn't call SetSize() after StartGIF() !!!
	long width, height;
	unsigned char *f=0, **l=GetRGBLines(width, height, f);
	if(f)	free(f);
	if(l)	free(l);
	// define colormap
	GifColorType col[256];
	memset(col,0,256*sizeof(GifColorType));
	for(int i=0;i<6;i++)	for(int j=0;j<6;j++)	for(int k=0;k<6;k++)
	{
		long m = i+6*(j+6*k);		// part 1
		col[m].Red = 51*i;
		col[m].Green=51*j;
		col[m].Blue =51*k;
	}
	// write header
#if GIFLIB_MAJOR>=5
	ColorMapObject *gmap = GifMakeMapObject(256, col);
	EGifPutScreenDesc(gif, width, height, 256,0,gmap);
	GifFreeMapObject(gmap);
#else
	ColorMapObject *gmap = MakeMapObject(256, col);
	EGifPutScreenDesc(gif, width, height, 256,0,gmap);
	FreeMapObject(gmap);
#endif
	// put animation parameters
	ms /= 10;
	unsigned char ext1[11] = {0x4E, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45, 0x32, 0x2E, 0x30};
	unsigned char ext2[9] = {0x08, (unsigned char)(ms%256), (unsigned char)(ms/256), 0xff};
	unsigned char ext3[3] = {0x01, 0xff, 0xff};
#if GIFLIB_MAJOR>=5
	EGifPutExtensionLeader(gif,0xff);
	EGifPutExtensionBlock(gif,11,ext1);
	EGifPutExtensionBlock(gif,3,ext3);
	EGifPutExtensionTrailer(gif);
	EGifPutExtension(gif,0xf9,4,ext2);
#else
	EGifPutExtensionFirst(gif,0xff,11,ext1);
	EGifPutExtensionLast(gif,0xff,3,ext3);
	EGifPutExtension(gif,0xf9,4,ext2);
#endif
#else
	mgl_set_global_warn(_("GIF support was disabled. Please, enable it and rebuild MathGL."));
#endif
}
//-----------------------------------------------------------------------------
void mglCanvas::CloseGIF()
{
#if MGL_HAVE_GIF
#if GIFLIB_MAJOR>5 || (GIFLIB_MAJOR==5 && GIFLIB_MINOR>0)
	if(gif)	EGifCloseFile(gif,0);
#else
	if(gif)	EGifCloseFile(gif);
#endif
#else
	mgl_set_global_warn(_("GIF support was disabled. Please, enable it and rebuild MathGL."));
#endif
	gif = 0;
}
//-----------------------------------------------------------------------------
int mglCanvas::NewFrame()
{
	Clf();	Identity();	CurFrameId++;
	return CurFrameId-1;
}
//-----------------------------------------------------------------------------
void mglCanvas::EndFrame()
{
	Finish();
	if(get(MGL_VECT_FRAME))	PushDrwDat();
#if MGL_HAVE_GIF
	if(!gif)	return;
	long width, height, n;
	unsigned char *f=0, **l=GetRGBLines(width, height, f);
	n = width*height;
	if(!l)	return;
	EGifPutImageDesc(gif, 0, 0, width, height, 0, 0);
	GifPixelType *line = new GifPixelType[n];
	for(long m=0;m<n;m++)
	{
		long ii = 3*(m%width), k = m/width;
		int i = (l[k][ii]+25)/51;
		int j = (l[k][ii+1]+25)/51;
		k = (l[k][ii+2]+25)/51;
		line[m] = i+6*(j+6*k);
	}
	EGifPutLine(gif, line, n);
	delete []line;	free(l);
	if(f)	free(f);
#endif
}
//-----------------------------------------------------------------------------
void mglCanvas::DelFrame(long i)
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexDrw);
	if(get(MGL_VECT_FRAME))	DrwDat.erase(DrwDat.begin()+i);
	pthread_mutex_unlock(&mutexDrw);
#else
#pragma omp critical(drw)
	if(get(MGL_VECT_FRAME))	DrwDat.erase(DrwDat.begin()+i);
#endif
	CurFrameId--;
}
//-----------------------------------------------------------------------------
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
#define _Gr_	((mglCanvas *)(gr))
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_png(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f,true);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".png";	fname = fn.c_str();	}
		if(mgl_pnga_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_png_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_png(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_png_solid(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".png";	fname = fn.c_str();	}
		if(mgl_png_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_png_solid_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_png_solid(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_jpg(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".jpg";	fname = fn.c_str();	}
		if(mgl_jpeg_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_jpg_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_jpg(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_tga(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f,true);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".tga";	fname = fn.c_str();	}
		if(mgl_tga_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_tga_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_tga(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_bmp(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".bmp";	fname = fn.c_str();	}
		if(mgl_bmp_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_bmp_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_bmp(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_bps(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".eps";	fname = fn.c_str();	}
		if(mgl_bps_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_bps_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_bps(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_gif(HMGL gr, const char *fname,const char *)
{
	long w,h;	unsigned char *f=0, **p=0;
	p =_Gr_->GetRGBLines(w,h,f);
	if(p)
	{
		std::string fn=fname;
		if(fn.empty())	{	fn=gr->PlotId+".gif";	fname = fn.c_str();	}
		if(mgl_gif_save(fname,w,h,p))	gr->SetWarn(mglWarnOpen,fname);
		free(p);	if(f)	free(f);
	}
}
void MGL_EXPORT mgl_write_gif_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_gif(_GR_,s,f);	delete []s;		delete []f;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_start_gif(HMGL gr, const char *fname,int ms)
{	_Gr_->StartGIF(fname,ms);	}
void MGL_EXPORT mgl_start_gif_(uintptr_t *gr, const char *fname,int *ms,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	mgl_start_gif(_GR_,s,*ms);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_close_gif(HMGL gr)			{	_Gr_->CloseGIF();	}
void MGL_EXPORT mgl_close_gif_(uintptr_t *gr)	{	mgl_close_gif(_GR_);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_write_frame(HMGL gr, const char *fname,const char *descr)
{
	char buf[64];
	if(!fname || !fname[0])
	{	snprintf(buf,64,"%s%04d.jpg",_Gr_->PlotId.c_str(),_Gr_->GetNumFrame());	buf[63]=0;	fname = buf;	}
	int len=strlen(fname);
	if(!strcmp(fname+len-4,".jpg")) 	mgl_write_jpg(gr,fname,descr);
	else if(!strcmp(fname+len-5,".jpeg"))	mgl_write_jpg(gr,fname,descr);
	else if(!strcmp(fname+len-4,".prc")) 	mgl_write_prc(gr,fname,descr,1);
	else if(!strcmp(fname+len-4,".pdf")) 	mgl_write_prc(gr,fname,descr,1);
	else if(!strcmp(fname+len-4,".png")) 	mgl_write_png(gr,fname,descr);
	else if(!strcmp(fname+len-4,".eps")) 	mgl_write_eps(gr,fname,descr);
	else if(!strcmp(fname+len-5,".epsz"))	mgl_write_eps(gr,fname,descr);
	else if(!strcmp(fname+len-7,".eps.gz"))	mgl_write_eps(gr,fname,descr);
	else if(!strcmp(fname+len-4,".bps")) 	mgl_write_bps(gr,fname,descr);
	else if(!strcmp(fname+len-5,".bpsz"))	mgl_write_bps(gr,fname,descr);
	else if(!strcmp(fname+len-7,".bps.gz"))	mgl_write_bps(gr,fname,descr);
	else if(!strcmp(fname+len-4,".svg")) 	mgl_write_svg(gr,fname,descr);
	else if(!strcmp(fname+len-5,".svgz"))	mgl_write_svg(gr,fname,descr);
	else if(!strcmp(fname+len-7,".svg.gz"))	mgl_write_svg(gr,fname,descr);
	else if(!strcmp(fname+len-4,".gif")) 	mgl_write_gif(gr,fname,descr);
	else if(!strcmp(fname+len-4,".bmp")) 	mgl_write_bmp(gr,fname,descr);
	else if(!strcmp(fname+len-4,".tga")) 	mgl_write_tga(gr,fname,descr);
	else if(!strcmp(fname+len-5,".mgld"))	mgl_export_mgld(gr,fname,descr);
	else if(!strcmp(fname+len-5,".json"))	mgl_write_json(gr,fname,descr);
	else if(!strcmp(fname+len-6,".jsonz"))	mgl_write_json(gr,fname,descr);
	else if(!strcmp(fname+len-4,".obj")) 	mgl_write_obj(gr,fname,descr,1);
	else if(!strcmp(fname+len-4,".tex")) 	mgl_write_tex(gr,fname,descr);
	else if(!strcmp(fname+len-4,".xyz")) 	mgl_write_xyz(gr,fname,descr);
	else if(!strcmp(fname+len-4,".stl")) 	mgl_write_stl(gr,fname,descr);
	else if(!strcmp(fname+len-4,".off")) 	mgl_write_off(gr,fname,descr,0);
//	else if(!strcmp(fname+len-4,".x3d")) 	mgl_write_x3d(gr,fname,descr,1);
}
void MGL_EXPORT mgl_write_frame_(uintptr_t *gr, const char *fname,const char *descr,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,descr,n);	f[n]=0;
	mgl_write_frame(_GR_,s,f);	delete []s;		delete []f;}
//-----------------------------------------------------------------------------
#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif
void MGL_EXPORT mgl_show_image(HMGL gr, const char *viewer, int keep)
{
	static size_t counter=size_t(0xffffffff*mgl_rnd());
	char *fname = new char[256], *cmd = new char [288];
#if defined(_MSC_VER)
	snprintf(fname,128,"%s.png", tmpnam(NULL));
#else
	snprintf(fname,256,"%s/mathgl%lu.png", P_tmpdir, counter);
#endif
	fname[255]=0;	counter++;
	mgl_write_png_solid(gr,fname,"MathGL ShowImage file");
	if(!viewer || !viewer[0])
		viewer = MGL_DEF_VIEWER;
#ifdef WIN32
		if(keep)
		{
			snprintf(cmd,288,"%s %s &", viewer,fname);	cmd[287]=0;
			if(system(cmd)==-1)	printf(_("Error to call external viewer\n"));
			Sleep(2000);
			snprintf(cmd,288,"del %s", fname);
		}
		else	snprintf(cmd,288,"%s %s; del %s", viewer,fname,fname);
#else
		if(keep)
		{
			snprintf(cmd,288,"%s %s &", viewer,fname);	cmd[287]=0;
			if(system(cmd)==-1)	printf(_("Error to call external viewer\n"));
			sleep(2);
			snprintf(cmd,288,"rm %s", fname);
		}
		else	snprintf(cmd,288,"%s %s; rm %s", viewer,fname,fname);
#endif
		cmd[287] = 0;
		if(system(cmd)==-1)	printf(_("Error to call external viewer\n"));
		delete []cmd;	delete []fname;
}
void MGL_EXPORT mgl_show_image_(uintptr_t *gr, const char *viewer, int *keep, int l)
{	char *s=new char[l+1];	memcpy(s,viewer,l);	s[l]=0;
	mgl_show_image(_GR_,s,*keep);	delete []s;	}
//-----------------------------------------------------------------------------
