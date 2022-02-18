/***************************************************************************
 * define.h is part of Math Graphic Library
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
#ifndef _MGL_DEFINE_H_
#define _MGL_DEFINE_H_
//-----------------------------------------------------------------------------
// Disable warnings for MSVC:
// 4190 - C-linkage of std::complex,
// 4996 - deprecated abi functions
// 4786 - disable warnings on 255 char debug symbols
// 4231 - disable warnings on extern before template instantiation
// 4800	- "int,uint32_t,etc" forcing value to bool 'true' or 'false' (performance warning)
// 4244 - conversion from 'mreal,double' to 'float', possible loss of data
// 4267	- conversion from 'size_t' to 'long,int,etc', possible loss of data
// 4305	- truncation from 'double' to 'float'
// 4251 - class 'type' needs to have dll-interface to be used by clients of class 'type2'
#if defined(_MSC_VER)
#pragma warning(disable: 4190 4996 4786 4800 4244 4267 4305 4251)
#endif

#if defined(_WIN32) && !defined(WIN32)
#define WIN32 1
#endif

#include "mgl2/config.h"
#ifndef SWIG

#if MGL_HAVE_PTHR_WIDGET|MGL_HAVE_PTHREAD
#include <pthread.h>
#endif

#include "mgl2/dllexport.h"
#if defined(MGL_LIB_MSVC)
#define MGL_EXTERN
#else
#define MGL_EXTERN extern
#endif

#if defined(_MSC_VER)
#define MGL_OBSOLETE	MGL_NO_EXPORT
#else
#define MGL_OBSOLETE	MGL_EXPORT
#endif

#if MGL_HAVE_ATTRIBUTE
#define MGL_FUNC_CONST	__attribute__((const))
#define MGL_FUNC_PURE	__attribute__((pure))
#else
#define MGL_FUNC_CONST
#define MGL_FUNC_PURE
#endif
#define MGL_EXPORT_CONST	MGL_EXPORT MGL_FUNC_CONST
#define MGL_EXPORT_PURE		MGL_EXPORT MGL_FUNC_PURE
#define MGL_LOCAL_CONST		MGL_NO_EXPORT MGL_FUNC_CONST
#define MGL_LOCAL_PURE		MGL_NO_EXPORT MGL_FUNC_PURE

#if MGL_HAVE_RVAL	// C++11 don't support register keyword
#endif

#endif
//-----------------------------------------------------------------------------
#ifdef MGL_SRC

#if MGL_USE_GETTEXT
	#include <libintl.h>
	#define _(x)	gettext(x)
#else
	#define _(x)	(x)
#endif


#if MGL_HAVE_ZLIB
#include <zlib.h>
#ifndef Z_BEST_COMPRESSION
#define Z_BEST_COMPRESSION 9
#endif
#else
#define gzFile	FILE*
#define gzread(fp,buf,size)	fread(buf,1,size,fp)
#define gzopen	fopen
#define gzclose	fclose
#define gzprintf	fprintf
#define gzgets(fp,str,size)	fgets(str,size,fp)
#define gzgetc	fgetc
#endif
#endif

#if (defined(_MSC_VER) && (_MSC_VER<1600)) || defined(__BORLANDC__)
typedef signed char int8_t;
typedef signed short int16_t;
typedef signed long int32_t;
typedef signed long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned long uint32_t;
typedef unsigned long long uint64_t;
#else
#include <stdint.h>
#endif
#if defined(__BORLANDC__)
typedef unsigned long uintptr_t;
#endif

#if ((defined(_MSC_VER) || defined(__BORLANDC__)) && !defined(M_PI))	//_MSC_VER needs this before math.h
#define	_USE_MATH_DEFINES
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#if defined(_MSC_VER)
typedef long msize;
#define collapse(a)	// MSVS don't support OpenMP 3.*
#if (_MSC_VER<=1800)
#define strtoull _strtoui64
//#define hypot	_hypot
#define getcwd	_getcwd
#define chdir	_chdir // BORLAND has chdir
#endif
#define snprintf _snprintf
#if (_MSC_VER<1600) // based on https://hg.python.org/cpython/rev/9aedb876c2d7
#define hypot	_hypot
#endif
#else
typedef size_t msize;
#endif

#if !MGL_SYS_NAN
#include <float.h>
#include <math.h>
const unsigned long long mgl_nan[2] = {0x7fffffffffffffff, 0x7fffffff};
const unsigned long long mgl_inf[2] = {0x7ff0000000000000, 0x7f800000};
#define NANd    (*(double*)mgl_nan)
#define NANf    (*(float*)(mgl_nan+1))
#define INFd    (*(double*)mgl_inf)
#define INFf    (*(float*)(mgl_inf+1))

#if !defined(NAN)
#if MGL_USE_DOUBLE
#define NAN		NANd
#else
#define NAN		NANf
#endif
#endif

#if !defined(INFINITY)
#if MGL_USE_DOUBLE
#define INFINITY	INFd
#else
#define INFINITY	INFf
#endif
#endif
#endif	// !MGL_SYS_NAN

#ifndef M_PI
#define M_PI	3.14159265358979323846  /* pi */
#endif
//-----------------------------------------------------------------------------
#ifdef WIN32
#define mglprintf    _snwprintf
#else
#define mglprintf    swprintf
#endif
//#define FLT_EPS	1.1920928955078125e-07
//-----------------------------------------------------------------------------
#if MGL_USE_DOUBLE
typedef double mreal;
#define MGL_EPSILON	(1.+1e-10)
#define MGL_MIN_VAL 1e-307
#else
typedef float mreal;
#define MGL_EPSILON	(1.+1e-5)
#define MGL_MIN_VAL 1e-37
#endif
#define MGL_FEPSILON	(1.+1e-5)
//-----------------------------------------------------------------------------
#ifndef MGL_CMAP_COLOR
#define MGL_CMAP_COLOR	32
#endif
//-----------------------------------------------------------------------------
#ifndef MGL_DEF_VIEWER
#define MGL_DEF_VIEWER "evince"
#endif
//-----------------------------------------------------------------------------
enum{	// types of predefined curvelinear coordinate systems
	mglCartesian = 0,	// no transformation
	mglPolar,
	mglSpherical,
	mglParabolic,
	mglParaboloidal,
	mglOblate,
	mglProlate,
	mglElliptic,
	mglToroidal,
	mglBispherical,
	mglBipolar,
	mglLogLog,
	mglLogX,
	mglLogY
};
//-----------------------------------------------------------------------------
// types of drawing
#define MGL_DRAW_WIRE	0	// fastest, no faces
#define MGL_DRAW_FAST	1	// fast, no color interpolation
#define MGL_DRAW_NORM	2	// high quality, slower
#define MGL_DRAW_LMEM	4	// low memory usage (direct to pixel)
#define MGL_DRAW_DOTS	8	// draw dots instead of primitives
#define MGL_DRAW_NONE	9	// no ouput (for testing only)
//-----------------------------------------------------------------------------
enum{	// Codes for warnings/messages
	mglWarnNone = 0,// Everything OK
	mglWarnDim,		// Data dimension(s) is incompatible
	mglWarnLow,		// Data dimension(s) is too small
	mglWarnNeg,	 	// Minimal data value is negative
	mglWarnFile, 	// No file or wrong data dimensions
	mglWarnMem,		// Not enough memory
	mglWarnZero, 	// Data values are zero
	mglWarnLeg,		// No legend entries
	mglWarnSlc,		// Slice value is out of range
	mglWarnCnt,		// Number of contours is zero or negative
	mglWarnOpen, 	// Couldn't open file
	mglWarnLId,		// Light: ID is out of range
	mglWarnSize, 	// Setsize: size(s) is zero or negative
	mglWarnFmt,		// Format is not supported for that build
	mglWarnTern, 	// Axis ranges are incompatible
	mglWarnNull, 	// Pointer is NULL
	mglWarnSpc,		// Not enough space for plot
	mglScrArg,		// Wrong argument(s) in MGL script
	mglScrCmd,		// Wrong command in MGL script
	mglScrLong,		// Too long line in MGL script
	mglScrStr,		// Unbalanced ' in MGL script
	mglScrTemp,		// Change temporary data in MGL script
	mglWarnEnd		// Maximal number of warnings (must be last)
};
//-----------------------------------------------------------------------------
#define MGL_DEF_PAL	"bgrcmyhlnqeupH"	// default palette
#define MGL_DEF_SCH	"BbcyrR"	// default palette
#define MGL_COLORS	"kwrgbcymhWRGBCYMHlenpquLENPQU"
//-----------------------------------------------------------------------------
/// Brushes for mask with symbol "-+=;oOsS~<>jdD*^" correspondingly
extern MGL_EXPORT uint64_t mgl_mask_val[16];
#define MGL_MASK_ID		"-+=;oOsS~<>jdD*^"
#define MGL_SOLID_MASK	0xffffffffffffffff
//-----------------------------------------------------------------------------
#define MGL_TRANSP_NORM		0x00000000
#define MGL_TRANSP_GLASS 	0x00000001
#define MGL_TRANSP_LAMP		0x00000002
#define MGL_ENABLE_CUT		0x00000004 	///< Flag which determines how points outside bounding box are drown.
#define MGL_ENABLE_RTEXT 	0x00000008 	///< Use text rotation along axis
#define MGL_AUTO_FACTOR		0x00000010 	///< Enable autochange PlotFactor
#define MGL_ENABLE_ALPHA 	0x00000020 	///< Flag that Alpha is used
#define MGL_ENABLE_LIGHT 	0x00000040 	///< Flag of using lightning
#define MGL_TICKS_ROTATE 	0x00000080 	///< Allow ticks rotation
#define MGL_TICKS_SKIP		0x00000100 	///< Allow ticks rotation
// flags for internal use only
#define MGL_DISABLE_SCALE	0x00000200 	///< Temporary flag for disable scaling (used for axis)
#define MGL_FINISHED 		0x00000400 	///< Flag that final picture (i.e. mglCanvas::G) is ready
#define MGL_USE_GMTIME		0x00000800 	///< Use gmtime instead of localtime
#define MGL_SHOW_POS		0x00001000 	///< Switch to show or not mouse click position
#define MGL_CLF_ON_UPD		0x00002000 	///< Clear plot before Update()
#define MGL_NOSUBTICKS		0x00004000 	///< Disable subticks drawing (for bounding box)
#define MGL_LOCAL_LIGHT		0x00008000 	///< Keep light sources for each inplot
#define MGL_VECT_FRAME		0x00010000 	///< Use DrwDat to remember all data of frames
#define MGL_REDUCEACC		0x00020000 	///< Reduce accuracy of points (to reduce size of output files)
#define MGL_PREFERVC 		0x00040000 	///< Prefer vertex color instead of texture if output format supports
#define MGL_ONESIDED 		0x00080000 	///< Render only front side of surfaces if output format supports (for debugging)
#define MGL_NO_ORIGIN 		0x00100000 	///< Don't draw tick labels at axis origin
#define MGL_GRAY_MODE 		0x00200000 	///< Convert all colors to gray ones
#define MGL_FULL_CURV 		0x00400000 	///< Disable omitting points in straight-line part(s).
#define MGL_NO_SCALE_REL 	0x00800000 	///< Disable font scaling in relative inplots
//-----------------------------------------------------------------------------
#ifdef __cplusplus
#include <string>
#include <vector>
#include <complex>
typedef std::complex<mreal> dual;
//-----------------------------------------------------------------------------
inline bool mgl_isrange(double a, double b)
{	return fabs(a-b)>MGL_MIN_VAL && a-a==0. && b-b==0.;	}
inline bool mgl_isbad(double a)	{	return a-a!=0;	}
inline bool mgl_isbad(dual a)	{	return a-a!=mreal(0);	}
inline bool mgl_isfin(double a)	{	return a-a==0;	}
inline bool mgl_isfin(dual a)	{	return a-a==mreal(0);	}
inline bool mgl_isnum(double a)	{	return a==a;	}
inline bool mgl_isnum(dual a)	{	return a==a;	}
inline bool mgl_isnan(double a)	{	return a!=a;	}
inline bool mgl_isnan(dual a)	{	return a!=a;	}
inline int mgl_sign(double a)	{	return a<0?-1:1;	}
inline long mgl_int(double a)	{	return long(a+(a>=0?0.5:-0.5));	}
inline double mgl_min(double a, double b)	{	return a>b?b:a;	}
inline double mgl_max(double a, double b)	{	return a>b?a:b;	}
inline long mgl_imin(long a, long b)	{	return a>b?b:a;	}
inline long mgl_imax(long a, long b)	{	return a>b?a:b;	}
inline void mgl_strncpy(char *a, const char *b, size_t s)	{	strncpy(a,b,s);	a[s-1]=0;	}
//-----------------------------------------------------------------------------
extern "C" {
#endif
//-----------------------------------------------------------------------------
struct cmdual	// complex number (bypass C/C++ incompatibility)
{
	mreal re,im;	// real and imaginary parts
#ifdef __cplusplus
	operator dual() const	{	return dual(re,im);	}
	mreal real() const	{	return re;	}
	mreal imag() const	{	return im;	}
#endif
};
#ifdef __cplusplus
struct mdual : public cmdual
{
	mdual(const cmdual &c)	{	re=c.re;	im=c.im;	}
	mdual(const std::complex<float> &c)	{	re=c.real();	im=c.imag();	}
	mdual(const std::complex<double> &c){	re=c.real();	im=c.imag();	}
	mdual(mreal r=0, mreal i=0)	{	re=r;	im=i;	}
	mdual &operator=(const cmdual &c)	{	re=c.re;	im=c.im;	return *this;	}
	mdual &operator=(const std::complex<float> &c)	{	re=c.real();	im=c.imag();	return *this;	}
	mdual &operator=(const std::complex<double> &c)	{	re=c.real();	im=c.imag();	return *this;	}
	mdual &operator=(mreal r)	{	re=r;	im=0;	return *this;	}
};
#else
typedef struct cmdual cmdual;
typedef cmdual mdual;
#if MGL_HAVE_C99_COMPLEX
	#include <complex.h>
	#if MGL_USE_DOUBLE
		typedef double _Complex dual;
	#else
		typedef float _Complex dual;
	#endif
	MGL_EXPORT dual mdual2c(cmdual c);
	MGL_EXPORT cmdual c2mdual(dual c);
#endif
#endif
//-----------------------------------------------------------------------------
extern float mgl_cos[360];	///< contain cosine with step 1 degree
//-----------------------------------------------------------------------------
/// Calculate sqrt(x*x+y*y)
double MGL_EXPORT_CONST mgl_hypot(double x, double y);
/// Find length of wchar_t string (bypass standard wcslen bug)
size_t MGL_EXPORT_PURE mgl_wcslen(const wchar_t *str);
/// Get RGB values for given color id or fill by -1 if no one found
void MGL_EXPORT mgl_chrrgb(char id, float rgb[3]);
/// Get number of colors in the string
size_t MGL_EXPORT_PURE mgl_get_num_color(const char *s, int smooth);
/// Check if string contain color id and return its number
long MGL_EXPORT_PURE mgl_have_color(const char *stl);
/// Find symbol in string excluding {} and return its position or NULL
MGL_EXPORT_PURE const char *mglchr(const char *str, char ch);
/// Find any symbol from chr in string excluding {} and return its position or NULL
MGL_EXPORT_PURE const char *mglchrs(const char *str, const char *chr);
/// Set number of thread for plotting and data handling (for pthread version only)
void MGL_EXPORT mgl_set_num_thr(int n);
void MGL_EXPORT mgl_set_num_thr_(int *n);
void MGL_EXPORT mgl_test_txt(const char *str, ...);
void MGL_EXPORT mgl_set_test_mode(int enable);
/// Remove spaces at begining and at the end of the string
void MGL_EXPORT mgl_strtrim(char *str);
void MGL_EXPORT mgl_wcstrim(wchar_t *str);
/** Change register to lowercase (only for ANSI symbols) */
void MGL_EXPORT mgl_strlwr(char *str);
void MGL_EXPORT mgl_wcslwr(wchar_t *str);
/// Convert wchar_t* string into char* one
void MGL_EXPORT mgl_wcstombs(char *dst, const wchar_t *src, int size);
/// Clear internal data for speeding up FFT and Hankel transforms
void MGL_EXPORT mgl_clear_fft();
/// Set global warning message
void MGL_EXPORT mgl_set_global_warn(const char *text);
void MGL_EXPORT mgl_set_global_warn_(const char *text,int);
/// Get text of global warning message(s)
MGL_EXPORT_PURE const char *mgl_get_global_warn();
int MGL_EXPORT mgl_get_global_warn_(char *out, int len);
/// Setup gettext usage. NOTE: Russian translation MUST be installed.
void MGL_EXPORT mgl_textdomain(const char *argv0, const char *locale);
void MGL_EXPORT mgl_textdomain_(const char *locale, int);
/// size of var array
const int MGL_VS = 'z'-'a'+1;
#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
