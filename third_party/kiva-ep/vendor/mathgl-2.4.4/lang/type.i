/***************************************************************************
 * type.i is part of Math Graphic Library
 * Copyright (C) 2007-2012 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
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
#include "mgl2/config.h"
#if MGL_USE_DOUBLE
typedef double mreal;
#else
typedef float mreal;
#endif
//-----------------------------------------------------------------------------
const mreal All = -1;
const mreal Pi = M_PI;
const mreal NaN = NAN;
const mreal Inf = INFINITY;
const mreal mgl_min_a = 1./256;
//-----------------------------------------------------------------------------
/// Class for point in 3D space
struct mglPoint
{
	mreal x,y,z,c;
	mglPoint(mreal X=0,mreal Y=0,mreal Z=0,mreal C=0):x(X),y(Y),z(Z),c(C) {}
	mglPoint(const mglPoint &d):x(d.x),y(d.y),z(d.z),c(d.c) {}
	inline bool IsNAN()		{	return (x!=x || y!=y || z!=z || c!=c);	}
	inline mreal val(int i)	{	return (i<2 ? (i==0 ? x:y) : (i==2 ? z:c));	}
	inline mreal norm()		{	return sqrt(x*x+y*y+z*z);	}
	inline void Normalize()	{	mreal v=norm();	x/=v;	y/=v;	z/=v;	}

	inline const mglPoint &operator=(const mglPoint &p)
	{	x=p.x;	y=p.y;	z=p.z;	c=p.c;	return p;	}
	inline void operator+=(const mglPoint &a)	{	x+=a.x;	y+=a.y;	z+=a.z;	c+=a.c;	}
	inline void operator-=(const mglPoint &a)	{	x-=a.x;	y-=a.y;	z-=a.z;	c-=a.c;	}
	inline void operator+=(mreal a)	{	x+=a;	y+=a;	z+=a;	}
	inline void operator-=(mreal a)	{	x-=a;	y-=a;	z-=a;	}
	inline void operator*=(mreal a)	{	x*=a;	y*=a;	z*=a;	}
	inline void operator/=(mreal a)	{	x/=a;	y/=a;	z/=a;	}
};
//-----------------------------------------------------------------------------
/// Class for RGBA color
struct mglColor
{
	float r;	///< Red component of color
	float g;	///< Green component of color
	float b;	///< Blue component of color
	float a;	///< Alpha component of color

	/// Constructor for RGB components manualy
	mglColor(float R,float G,float B, float A=1):r(R),g(G),b(B),a(A)	{}
	/// Constructor for RGB components manualy
	mglColor(const unsigned char *c, float A=1):r(c[0]/255.f),g(c[1]/255.f),b(c[3]/255.f),a(A)	{}
	/// Constructor set default color
	mglColor():r(0),g(0),b(0),a(1)	{}
	/// Constructor set color from character id
	mglColor(char c, float bright=1)		{	Set(c,bright);	}
	/// Copy constructor
	mglColor(const mglColor &d):r(d.r),g(d.g),b(d.b),a(d.a)	{}
	/// Set color as Red, Green, Blue values
	void Set(float R,float G,float B,float A=1)	{	r=R;	g=G;	b=B;	a=A;	}
	/// Set color as Red, Green, Blue values
	void Set(mglColor c, float bright=1)
	{
		if(bright<0)	bright=0;
		if(bright>2.f)	bright=2.f;
		r = bright<=1 ? c.r*bright : 1 - (1-c.r)*(2-bright);
		g = bright<=1 ? c.g*bright : 1 - (1-c.g)*(2-bright);
		b = bright<=1 ? c.b*bright : 1 - (1-c.b)*(2-bright);	a = 1;
	}
	/// Check if color is valid
	inline bool Valid()
	{	return (r>=0 && r<=1 && g>=0 && g<=1 && b>=0 && b<=1 && a>=0 && a<=1);	}
	/// Get maximal spectral component
	inline float Norm()
	{	return r>g ? r : (g>b ? g : b);	}
	inline float NormS()
	{	return r*r+g*g+b*b;	}
	/// Set color from symbolic id
	inline void Set(char p, float bright=1)
	{
		float rgb[3];	mgl_chrrgb(p,rgb);
		Set(mglColor(rgb[0],rgb[1],rgb[2]),bright);
	}
	inline const mglColor &operator=(const mglColor &p)
	{	r=p.r;	g=p.g;	b=p.b;	a=p.a;	return p;	}
	/// Copy color from other one
	inline bool operator==(const mglColor &c) const	// NOTE: exact comparison is used here
	{	return (r-c.r)*(r-c.r)+(g-c.g)*(g-c.g)+(b-c.b)*(b-c.b)+(a-c.a)*(a-c.a)==0;	}
//	{	return !memcmp(this, &c, sizeof(mglColor));	}
	inline bool operator!=(const mglColor &c) const	// NOTE: exact comparison is used here
	{	return (r-c.r)*(r-c.r)+(g-c.g)*(g-c.g)+(b-c.b)*(b-c.b)+(a-c.a)*(a-c.a)!=0;	}
//	{	return memcmp(this, &c, sizeof(mglColor));		}
	inline bool operator<(const mglColor &c) const
	{	return memcmp(this, &c, sizeof(mglColor))<0;	}
	// transparency still the same
	inline void operator*=(float v)				{	r*=v;	g*=v;	b*=v;	a*=v;	}
	inline void operator+=(const mglColor &c)	{	r+=c.r;	g+=c.g;	b+=c.b;	a+=c.a;	}
	inline void operator-=(const mglColor &c)	{	r-=c.r;	g-=c.g;	b-=c.b;	a-=c.a;	}
};
//-----------------------------------------------------------------------------

