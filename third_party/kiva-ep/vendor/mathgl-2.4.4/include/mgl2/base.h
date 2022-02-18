/***************************************************************************
 * base.h is part of Math Graphic Library
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
#ifndef _MGL_BASE_H_
#define _MGL_BASE_H_
//#if !defined(_MSC_VER) && !defined(__BORLANDC__)
#include "mgl2/abstract.h"

#ifdef __cplusplus

#if (MGL_HAVE_PTHREAD|MGL_HAVE_PTHR_WIDGET)
#include <pthread.h>
#endif

#if MGL_HAVE_PTHREAD
#define MGL_PUSH(a,v,m)	{pthread_mutex_lock(&m);	a.push_back(v);	pthread_mutex_unlock(&m);}
#else
#define MGL_PUSH(a,v,m)	a.push_back(v);
#endif

#if MGL_HAVE_PTHREAD
#define MGL_PUSHs(func,m)	{pthread_mutex_lock(&m);	func;	pthread_mutex_unlock(&m);}
#else
#define MGL_PUSHs(func,m)	{func;}
#endif
//-----------------------------------------------------------------------------
inline mreal mgl_d(mreal v,mreal v1,mreal v2) { return v2!=v1?(v-v1)/(v2-v1):NAN; }
//-----------------------------------------------------------------------------
mglPoint GetX(HCDT x, int i, int j, int k=0);
mglPoint GetY(HCDT y, int i, int j, int k=0);
mglPoint GetZ(HCDT z, int i, int j, int k=0);
//-----------------------------------------------------------------------------
/// Class for replacement of std::vector
// NOTE memcpy is used --> no memory allocation in T
template <class T> class mglStack
{
	T **dat;
	unsigned MGL_PB;	///< size of buffer (real size is 2^pb == 1L<<pb). 
	/** NOTE This limit the number of maximal points and primitives as (1<<MGL_PB)^2, 
	  * i.e. as 10^12 for MGL_PB=20 or 4*10^9 for MGL_PB=16. See mgl_bsize().*/
	size_t nb;	///< used blocks
	size_t n;	///< used cells
	void *mutex;
public:
	mglStack(const mglStack<T> &st)
	{	mutex = 0;	n=0;	nb=1;	MGL_PB = mgl_bsize(0);
		dat = new T*[(size_t)1<<MGL_PB];
		*dat = new T[(size_t)1<<MGL_PB];	reserve(st.n);
		for(size_t i=0;i<nb;i++)
			memcpy(dat[i],st.dat[i],((size_t)1<<MGL_PB)*sizeof(T));
		n=st.n;	}
	mglStack()
	{	mutex = 0;	n=0;	nb=1;	MGL_PB = mgl_bsize(0);
		dat = new T*[(size_t)1<<MGL_PB];
		*dat = new T[(size_t)1<<MGL_PB];	}
	~mglStack()	{	clear();	delete [](*dat);	delete []dat;	}
	inline void set_mutex(void *mtx)	{	mutex=mtx;	}
	inline size_t allocate(size_t num)
	{	reserve(num);	size_t r=n;	n+=num;	return r;	}
	void reserve(size_t num)
	{
		num = num?num+n:n+1;	// final required size
		if(num>(nb<<MGL_PB))
		{
			num = 1 + (num>>MGL_PB);
			for(;nb<num;nb++)	dat[nb] = new T[(size_t)1<<MGL_PB];
		}
	}
	void clear()
	{
		if(mutex)	mgl_mutex_lock(mutex);
		for(size_t i=1;i<nb;i++)	delete [](dat[i]);
		n=0;	nb=1;
		if(mutex)	mgl_mutex_unlock(mutex);
	}
	T &operator[](size_t i)
	{	size_t d=i>>MGL_PB;	return dat[d][i-(d<<MGL_PB)];	}
	const T &operator[](size_t i)	const
	{	size_t d=i>>MGL_PB;	return dat[d][i-(d<<MGL_PB)];	}
	void push_back(const T &t)
	{
		if(n>=(nb<<MGL_PB))	reserve(1);
		size_t d=n>>MGL_PB;	dat[d][n-(d<<MGL_PB)] = t;	n++;
	}
	void push_back(size_t num, const T *t)
	{
		if(n+num>=(nb<<MGL_PB))	reserve(num);
		for(size_t i=0;i<num;i++)
		{	size_t d=n>>MGL_PB;	dat[d][n-(d<<MGL_PB)] = t[i];	n++;	}
	}
	size_t size()	const	{	return n;	}
	const mglStack<T> &operator=(const mglStack<T> &st)
	{
		clear();	reserve(st.n);
		for(size_t i=0;i<nb;i++)
			memcpy(dat[i],st.dat[i],((size_t)1<<MGL_PB)*sizeof(T));
		n = st.n;	return st;
	}
};
//-----------------------------------------------------------------------------
/// Structure for transformation matrix
struct MGL_EXPORT mglMatrix
{
	float x,y,z,pf;
	float b[9];
	bool norot;	// flag to disable pnts rotation
	mglMatrix()	{	clear();	}
	mglMatrix(const mglMatrix &aa) : x(aa.x),y(aa.y),z(aa.z),pf(aa.pf),norot(aa.norot) 	{	memcpy(b,aa.b,9*sizeof(float));	}
	void Rotate(mreal tetz,mreal tetx,mreal tety);
	void RotateN(mreal Tet,mreal x,mreal y,mreal z);
	inline void clear()	{	x=y=z=pf=0;	memset(b,0,9*sizeof(float));	b[0]=b[4]=b[8]=1;	norot=false;	}
	inline const mglMatrix &operator=(const mglMatrix &a)
	{	x=a.x;	y=a.y;	z=a.z;	pf=a.pf;	memcpy(b,a.b,9*sizeof(float));	norot=false;	return a;	}
};
inline bool operator==(const mglMatrix &a, const mglMatrix &b)
{	return ((a.x-b.x)*(a.x-b.x)+(a.y-b.y)*(a.y-b.y)+(a.z-b.z)*(a.z-b.z)+(a.pf-b.pf)*(a.pf-b.pf)==0)&&!memcmp(b.b,a.b,9*sizeof(float));}
inline bool operator!=(const mglMatrix &a, const mglMatrix &b)
{	return ((a.x-b.x)*(a.x-b.x)+(a.y-b.y)*(a.y-b.y)+(a.z-b.z)*(a.z-b.z)+(a.pf-b.pf)*(a.pf-b.pf)!=0)||memcmp(b.b,a.b,9*sizeof(float));	}
//-----------------------------------------------------------------------------
/// Structure for simplest primitives
struct MGL_EXPORT mglPrim	// NOTE: use float for reducing memory size
{
	// NOTE: n4 is used as mark; n3 -- as pen style for type=0,1,4
	// NOTE: n3 is used as position of txt,font in Ptxt for type=6
	long n1,n2,n3,n4;	///< coordinates of corners
	short type;	///< primitive type (0-point, 1-line, 2-trig, 3-quad, 4-glyph, 6-text)
	short angl;	///< rotation angle for mask
	int id;		///< object id
	float z;	///< z-position
	float w;	///< width (if applicable) or ftet
	union
	{
		struct
		{
			float s;		///< size (if applicable) or fscl
			float p;
		};
		uint64_t m;
	};
	mglPrim():n1(0),n2(0),n3(0),n4(0),type(0),angl(0),id(0),z(0),w(0),m(0)	{}
	explicit mglPrim(int t):n1(0),n2(0),n3(0),n4(0),type(t),angl(0),id(0),z(0),w(0),m(0)	{}
	mglPrim(const mglPrim &aa) : n1(aa.n1),n2(aa.n2),n3(aa.n3),n4(aa.n4),type(aa.type),angl(aa.angl),id(aa.id),z(aa.z),w(aa.w),m(aa.m) 	{}
	const mglPrim &operator=(const mglPrim &aa)
	{	n1=aa.n1;	n2=aa.n2;	n3=aa.n3;	n4=aa.n4;	type=aa.type;	angl=aa.angl;	id=aa.id;	z=aa.z;	w=aa.w;	m=aa.m;	return aa;	}
};
bool operator<(const mglPrim &a,const mglPrim &b);
bool operator>(const mglPrim &a,const mglPrim &b);
//-----------------------------------------------------------------------------
/// Structure for light source
struct MGL_EXPORT mglLight
{
	mglLight():a(0),b(0),n(false)	{}
	mglLight(const mglLight &aa) : d(aa.d),r(aa.r),q(aa.q),p(aa.p),c(aa.c),a(aa.a),b(aa.b),n(aa.n)	{}
	const mglLight &operator=(const mglLight &aa)
	{	d=aa.d;	r=aa.r;	q=aa.q;	p=aa.p;	c=aa.c;	a=aa.a;	b=aa.b;	n=aa.n;	return aa;	}

	mglPoint d;		///< Direction of light sources
	mglPoint r;		///< Position of light sources (NAN for infinity)
	mglPoint q;		///< Actual position of light sources (filled by LightScale() function)
	mglPoint p;		///< Actual direction of light sources (filled by LightScale() function)
	mglColor c;		///< Color of light sources
	float a;		///< Aperture of light sources
	float b;		///< Brightness of light sources
	bool n;			///< Availability of light sources
};
//-----------------------------------------------------------------------------
/// Structure for inplot
struct MGL_EXPORT mglBlock
{
	long n1,n2,n3,n4;	///< coordinates of corners {n1=x1,n2=x2,n3=y1,n4=y2}

	mglLight light[10];	///< Light sources
	float AmbBr;		///< Default ambient light brightness
	float DifBr;		///< Default diffusive light brightness
	mglMatrix B;		///< Transformation matrix
	int id;		///< object id

	mglBlock():n1(0),n2(0),n3(0),n4(0),AmbBr(0.5),DifBr(0.5),id(0)	{}
	mglBlock(const mglBlock &aa)	{	memcpy(this, &aa, sizeof(mglBlock));	}
	const mglBlock &operator=(const mglBlock &aa)
	{	n1=aa.n1;	n2=aa.n2;	n3=aa.n3;	n4=aa.n4;	for(int i=0;i<10;i++)	light[i]=aa.light[i];
		AmbBr=aa.AmbBr;	DifBr=aa.DifBr;	B=aa.B;	id=aa.id;	return aa;	}
};
//-----------------------------------------------------------------------------
/// Structure for group of primitives
struct MGL_EXPORT mglGroup
{
	std::vector<long> p;	///< list of primitives (not filled!!!)
	int Id;					///< Current list of primitives
	std::string Lbl;		///< Group label
	mglGroup(const char *lbl="", int id=0) : Id(id), Lbl(lbl)	{}
	mglGroup(const mglGroup &aa) : p(aa.p),Id(aa.Id),Lbl(aa.Lbl)	{}
#if MGL_HAVE_RVAL
	mglGroup(mglGroup &&aa) : p(aa.p),Id(aa.Id),Lbl(aa.Lbl)	{}
#endif
	inline const mglGroup &operator=(const mglGroup &aa)	{	Lbl = aa.Lbl;	Id = aa.Id;	p = aa.p;	return aa;	}
};
//-----------------------------------------------------------------------------
/// Structure for text label
struct MGL_EXPORT mglText
{
	std::wstring text;
	std::string stl;
	float val;
	mglText(const wchar_t *txt=L"", const char *fnt="", float v=0) : text(txt), stl(fnt), val(v) {}
	mglText(const std::wstring &txt, float v=0): text(txt), val(v)	{}
	mglText(const mglText &aa) : text(aa.text),stl(aa.stl),val(aa.val)	{}
#if MGL_HAVE_RVAL
	mglText(mglText &&aa) : text(aa.text),stl(aa.stl),val(aa.val)	{}
#endif
	const mglText&operator=(const mglText &aa)	{ text=aa.text;	stl=aa.stl;	val=aa.val;	return aa;	}
};
//-----------------------------------------------------------------------------
/// Structure for internal point representation
struct MGL_EXPORT mglPnt	// NOTE: use float for reducing memory size
{
	union {	float dat[15];	struct {
		float x,y,z;	// coordinates
		float u,v,w;	// normales
		float r,g,b,a;	// RGBA color
		float xx,yy,zz;	// original coordinates
		float c,ta;	// index in color scheme
	}; };
	int sub;		// subplot id and rotation information (later will be in subplot)
	mglPnt(float X=0, float Y=0, float Z=0, float U=0, float V=0, float W=0, float R=0, float G=0, float B=0, float A=0, short s=0) :x(X),y(Y),z(Z),u(U),v(V),w(W),r(R),g(G),b(B),a(A),xx(X),yy(Y),zz(Z),c(0),ta(0),sub(s)	{}
	mglPnt(const mglPnt &aa) : sub(aa.sub)	{	memcpy(dat,aa.dat,15*sizeof(float));	}
	inline const mglPnt&operator=(const mglPnt &aa)	{ sub=aa.sub;	memcpy(dat,aa.dat,15*sizeof(float));	return aa;	}
	inline bool same(const mglPnt &p, mreal d)	const {	return fabs(x-p.x)<d && fabs(y-p.y)<d;	}
};
inline mglPnt operator+(const mglPnt &a, const mglPnt &b)
//{	mglPnt p;	for(long i=0;i<10;i++)	p.dat[i] = a.dat[i]+b.dat[i];	p.sub=a.sub;	return p;	}
{	return mglPnt(a.x+b.x,a.y+b.y,a.z+b.z, a.u+b.u,a.v+b.v,a.w+b.w, a.r+b.r,a.g+b.g,a.b+b.b,a.a+b.a, a.sub);	}
inline mglPnt operator-(const mglPnt &a, const mglPnt &b)
//{	mglPnt p;	for(long i=0;i<10;i++)	p.dat[i] = a.dat[i]-b.dat[i];	p.sub=a.sub;	return p;	}
{	return mglPnt(a.x-b.x,a.y-b.y,a.z-b.z, a.u-b.u,a.v-b.v,a.w-b.w, a.r-b.r,a.g-b.g,a.b-b.b,a.a-b.a, a.sub);	}
inline mglPnt operator*(const mglPnt &a, float b)
//{	mglPnt p;	for(long i=0;i<10;i++)	p.dat[i] = a.dat[i]*b;	p.sub=a.sub;	return p;	}
{	return mglPnt(a.x*b,a.y*b,a.z*b, a.u*b,a.v*b,a.w*b, a.r*b,a.g*b,a.b*b,a.a*b, a.sub);	}
inline mglPnt operator*(float b, const mglPnt &a)
//{	mglPnt p;	for(long i=0;i<10;i++)	p.dat[i] = a.dat[i]*b;	p.sub=a.sub;	return p;	}
{	return mglPnt(a.x*b,a.y*b,a.z*b, a.u*b,a.v*b,a.w*b, a.r*b,a.g*b,a.b*b,a.a*b, a.sub);	}
//-----------------------------------------------------------------------------
/// Structure for glyph representation
struct MGL_EXPORT mglGlyph
{	// NOTE nt<0 is used to set char id for user-defined glyphs
	long nt, nl;		///< number of triangles and lines
	short *trig, *line;	///< vertexes of triangles and lines

	mglGlyph():nt(0),nl(0),trig(0),line(0)	{}
	mglGlyph(const mglGlyph &a):nt(0),nl(0),trig(0),line(0)	{	*this=a;	}
	mglGlyph(long Nt, long Nl):nt(0),nl(0),trig(0),line(0)	{	Create(Nt,Nl);	}
	~mglGlyph()	{	if(trig)	delete []trig;	if(line)	delete []line;	}

	void Create(long Nt, long Nl);
	bool operator==(const mglGlyph &g) const MGL_FUNC_PURE;
	inline bool operator!=(const mglGlyph &g) const MGL_FUNC_PURE
	{	return !(*this==g);	}
	inline const mglGlyph &operator=(const mglGlyph &a)
	{	Create(a.nt, a.nl);
		if(a.trig)	memcpy(trig, a.trig, 6*nt*sizeof(short));
		if(a.line)	memcpy(line, a.line, 2*nl*sizeof(short));
		return a;	}
};
//-----------------------------------------------------------------------------
#define MGL_TEXTURE_COLOURS 512
/// Structure for texture (color scheme + palette) representation
struct MGL_EXPORT mglTexture
{
	mglColor *col;	///< Colors itself
	long n;			///< Number of initial colors along u
	mglColor *c0;	///< Initial colors
	float *val;	///< Initial color positions

	char Sch[260];	///< Color scheme used
	int Smooth;		///< Type of texture (smoothing and so on)
	float Alpha;	///< Transparency

	mglTexture():n(0),c0(NULL),val(NULL),Smooth(0),Alpha(1)
	{	col = new mglColor[MGL_TEXTURE_COLOURS];	}
	mglTexture(const char *cols, int smooth=0,mreal alpha=1):n(0),c0(NULL),val(NULL)
	{	col = new mglColor[MGL_TEXTURE_COLOURS];	Set(cols,smooth,alpha);	}
	mglTexture(const mglTexture &aa) : n(aa.n),Smooth(aa.Smooth),Alpha(aa.Alpha)
	{	col = new mglColor[MGL_TEXTURE_COLOURS];	memcpy(Sch,aa.Sch,260);
		memcpy(col,aa.col,MGL_TEXTURE_COLOURS*sizeof(mglColor));
		c0 = new mglColor[2*aa.n];	memcpy(c0,aa.c0,2*aa.n*sizeof(mglColor));
		val = new float[aa.n];		memcpy(val,aa.val,aa.n*sizeof(float));	}
#if MGL_HAVE_RVAL
	mglTexture(mglTexture &&aa) : n(aa.n),c0(aa.c0),val(aa.val),Smooth(aa.Smooth),Alpha(aa.Alpha)
	{	col = aa.col;	memcpy(Sch,aa.Sch,260);	aa.col=aa.c0=0;	aa.val=0;	}
#endif
	~mglTexture()	{	if(col)	delete []col;	Clear();	}
	void Clear()
	{	if(c0)	{	delete []c0;	delete []val;	}
		n=0;	c0=NULL;	val=NULL;	}
	void Set(const char *cols, int smooth=0,mreal alpha=1);
	void Set(HCDT val, const char *cols);
	void GetC(mreal u,mreal v,mglPnt &p) const;
	mglColor GetC(mreal u,mreal v=0) const MGL_FUNC_PURE;
	inline bool IsSame(const mglTexture &t) const
	{	return n==t.n && !memcmp(col,t.col,MGL_TEXTURE_COLOURS*sizeof(mglColor));	}
	void GetRGBA(unsigned char *f) const;
	void GetRGBAPRC(unsigned char *f) const;
	void GetRGBAOBJ(unsigned char *f) const;	// Export repeating border colors, since OBJ by default wraps textures and we need an extra boundary to work around implementation quirks
	inline const mglTexture &operator=(const mglTexture &aa)
	{	n=aa.n;	Smooth=aa.Smooth;	Alpha=aa.Alpha;
		memcpy(col,aa.col,MGL_TEXTURE_COLOURS*sizeof(mglColor));
		memcpy(Sch,aa.Sch,260);	return aa;	}
};
//-----------------------------------------------------------------------------
const mglColor NC(-1,-1,-1);
const mglColor BC( 0, 0, 0);
const mglColor WC( 1, 1, 1);
const mglColor RC( 1, 0, 0);
//-----------------------------------------------------------------------------
/// Structure active points
struct MGL_EXPORT mglActivePos
{
	int x,y;	///< coordinates of active point
	int id;		///< object id for active point
	int n;		///< position of active point in command (object id)
};
//-----------------------------------------------------------------------------
#if defined(_MSC_VER)
MGL_EXTERN template class MGL_EXPORT mglStack<mglPnt>;
MGL_EXTERN template class MGL_EXPORT mglStack<mglPrim>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglGroup>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglText>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglTexture>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglGlyph>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglBlock>;
MGL_EXTERN template class MGL_EXPORT std::vector<mglMatrix>;
MGL_EXTERN template class MGL_EXPORT mglStack<mglActivePos>;
#endif
//-----------------------------------------------------------------------------
/// Base class for canvas which handle all basic drawing
class MGL_EXPORT mglBase
{
public:
	mglBase();
	virtual ~mglBase();

	mglPoint Min;		///< Lower edge of bounding box for graphics.
	mglPoint Max;		///< Upper edge of bounding box for graphics.
	mreal ZMin;			///< Adjusted minimal z-value 1D plots
	std::string Mess;	///< Buffer for receiving messages
	int ObjId;			///< object id for mglPrim
	int HighId;			///< object id to be highlited
	std::vector<mglGroup> Grp;	///< List of groups with names -- need for export
	mglStack<mglActivePos> Act;	///< Position of active points
	std::string PlotId;	///< Id of plot for saving filename (in GLUT window for example)
	int BBoxX1, BBoxY1, BBoxX2, BBoxY2;	///< BBox region for exporting 2d graphics
	std::vector<mglGlyph> UserGlf;	///< User-defined glyphs data

	mreal CDef;			///< Default (current) color in texture
	mreal AlphaDef;		///< Default value of alpha channel (transparency)
	mreal BarWidth;		///< Relative width of rectangles in Bars().
	int MeshNum;		///< Set approximate number of lines in Mesh and Grid. By default (=0) it draw all lines.
	int FaceNum;		///< Set approximate number of visible faces and lines. By default (=0) it draw everything.
	char Arrow1, Arrow2;///< Style of arrows at end and at start of curve
	long InUse;			///< Smart pointer (number of users)
	uint32_t Flag;		///< Flags for controlling drawing
	mreal size_opt;		///< Value of size option (or NAN if not specified)

	inline bool get(uint32_t fl) const	{	return Flag&fl;	}
	inline void set(uint32_t fl)	{	Flag |= fl;	}
	inline void clr(uint32_t fl)	{	Flag &=~fl;	}
	inline void set(bool v,uint32_t fl)	{	Flag = v ? Flag|fl : Flag&(~fl);	}

	/// Set axis range scaling -- simplified way to shift/zoom axis range -- need to replot whole image!
	inline void ZoomAxis(mglPoint p1=mglPoint(0,0,0,0), mglPoint p2=mglPoint(1,1,1,1))	{	AMin = p1;	AMax = p2;	}
	/// Set values of mglGraph::Min and mglGraph::Max
	inline void SetRanges(mreal x1, mreal x2, mreal y1, mreal y2, mreal z1=0, mreal z2=0, mreal c1=0, mreal c2=0)
	{	SetRanges(mglPoint(x1,y1,z1,c1),mglPoint(x2,y2,z2,c2));	}
	void SetRanges(mglPoint v1, mglPoint v2);
	/// Set values of mglGraph::Cmin and mglGraph::Cmax as minimal and maximal values of data a
	void CRange(HCDT a, bool add = false, mreal fact=0);
	void CRange(mreal v1,mreal v2,bool add=false);
	/// Set values of mglGraph::Min.x and mglGraph::Max.x as minimal and maximal values of data a
	void XRange(HCDT a, bool add = false, mreal fact=0);
	void XRange(mreal v1,mreal v2,bool add=false);
	/// Set values of mglGraph::Min.x and mglGraph::Max.x as minimal and maximal values of data a
	void YRange(HCDT a, bool add = false, mreal fact=0);
	void YRange(mreal v1,mreal v2,bool add=false);
	/// Set values of mglGraph::Min.x and mglGraph::Max.x as minimal and maximal values of data a
	void ZRange(HCDT a, bool add = false, mreal fact=0);
	void ZRange(mreal v1,mreal v2,bool add=false);
	/// Set ranges for automatic variables
	void SetAutoRanges(mreal x1, mreal x2, mreal y1=0, mreal y2=0, mreal z1=0, mreal z2=0, mreal c1=0, mreal c2=0);
	/// Set axis origin
	void SetOrigin(mreal x0, mreal y0, mreal z0=NAN, mreal c0=NAN);
	/// Save ranges into internal variable and put parsed
	mreal SaveState(const char *opt);
	/// Load ranges from internal variable
	void LoadState();
	/// Increase ZMin
	mreal AdjustZMin()	{	ZMin /= MGL_FEPSILON;	return Max.z - ZMin*(Max.z-Min.z);	}

	/// Safetly set the transformation formulas for coordinate.
	void SetFunc(const char *EqX, const char *EqY, const char *EqZ=0, const char *EqA=0);
	/// Set one of predefined transformation rule
	void SetCoor(int how);
	/// Safetly set the cutting off condition (formula).
	void CutOff(const char *EqCut);
	/// Set to draw Ternary axis (triangle like axis, grid and so on)
	void Ternary(int tern);

	/// Set cutting for points outside of bounding box
	inline void SetCut(bool val)	{	set(val, MGL_ENABLE_CUT);	}
	/// Set additional cutting box
	inline void SetCutBox(mreal x1, mreal y1, mreal z1, mreal x2, mreal y2, mreal z2)
	{	CutMin.Set(x1,y1,z1);	CutMax.Set(x2,y2,z2);	}
	inline void SetCutBox(mglPoint v1, mglPoint v2)	{	CutMin=v1;	CutMax=v2;	}
	/// Reset mask to solid state
	inline void ResetMask()	{	mask = MGL_SOLID_MASK;	MaskAn = DefMaskAn;	}
	/// Set default mask rotation angle
	inline void SetMaskAngle(int angle)	{	DefMaskAn = angle;	}

	/// Set the using of light on/off.
	virtual bool Light(bool enable)
	{	bool t=get(MGL_ENABLE_LIGHT);	set(enable,MGL_ENABLE_LIGHT);	return t;	}
	/// Set to attach light sources to inplot.
	virtual bool AttachLight(bool enable)
	{	bool t=get(MGL_LOCAL_LIGHT);	set(enable,MGL_LOCAL_LIGHT);	return t;	}
	/// Set ambient light brightness
	virtual void SetAmbient(mreal bright=0.5);
	/// Set diffusive light brightness
	virtual void SetDiffuse(mreal bright=0.5);
	/// Use diffusive light (only for local light sources)
	inline void SetDifLight(bool dif)	{	SetDiffuse(dif?0.5:0);	}
	/// Set the transparency on/off.
	virtual bool Alpha(bool enable)
	{	bool t=get(MGL_ENABLE_ALPHA);	set(enable,MGL_ENABLE_ALPHA);	return t;	}
	/// Set default value of alpha-channel
	inline void SetAlphaDef(mreal val)	{	AlphaDef=val;	}
	/// Set default palette
	inline void SetPalette(const char *colors)
	{	Txt[0].Set(mgl_have_color(colors)?colors:MGL_DEF_PAL,-1);	}
	inline void ResetPal()	{	CurrPal=0;	}
	inline long GetNumPal(long id) const	{	return Txt[labs(id)/256].n;	}
	/// Set default color scheme
	inline void SetDefScheme(const char *colors)
	{	Txt[1].Set(mgl_have_color(colors)?colors:MGL_DEF_SCH);	}

	/// Set number of mesh lines
	inline void SetMeshNum(int val)	{	MeshNum=val;	}
	/// Set relative width of rectangles in Bars, Barh, BoxPlot
	inline void SetBarWidth(mreal val)	{	BarWidth=val>0?val:-BarWidth*val;	}
	/// Set size of marks
	inline void SetMarkSize(mreal val)	{	MarkSize=val>0?0.02*val:-val*MarkSize;	}
	/// Set size of arrows
	inline void SetArrowSize(mreal val)	{	ArrowSize=val>0?0.03*val:-val*ArrowSize;	}
	/// Get unscaled arrow size
	inline mreal GetArrowSize() const	{	return ArrowSize/0.03;	}

	/// Set warning code ant fill Message
	void SetWarn(int code, const char *who);
	int inline GetWarn() const	{	return WarnCode;	}

	virtual void StartAutoGroup (const char *)=0;
	void StartGroup(const char *name, int id);
	virtual void EndGroup()=0;	//	{	LoadState();	}
	/// Highlight group
	inline void Highlight(int id)	{	HighId=id;	}

	/// Set FontSize by size in pt and picture DPI (default is 16 pt for dpi=72)
	virtual void SetFontSizePT(mreal pt, int dpi=72){	FontSize = pt*27.f/dpi;	}
	/// Set FontSize by size in centimeters and picture DPI (default is 0.56 cm = 16 pt)
	inline void SetFontSizeCM(mreal cm, int dpi=72)	{	SetFontSizePT(cm*28.45f,dpi);	}
	/// Set FontSize by size in inch and picture DPI (default is 0.22 in = 16 pt)
	inline void SetFontSizeIN(mreal in, int dpi=72)	{	SetFontSizePT(in*72.27f,dpi);	}
	/// Set font typeface. Note that each mglFont instance can be used with ONLY ONE mglGraph instance at a moment of time!
	void SetFont(mglFont *f);
	/// Get current typeface. Note that this variable can be deleted at next SetFont() call!
	inline mglFont *GetFont()	{	return fnt;	}
	/// Restore font
	void RestoreFont();
	/// Load font from file
	void LoadFont (const char *name, const char *path=NULL);
	/// Copy font from another mglGraph instance
	void CopyFont(mglBase *gr);
	/// Set default font size
	inline void SetFontSize(mreal val)	{	FontSize=val>0 ? val:-FontSize*val;	}
	inline mreal GetFontSize() const	{	return FontSize;	}
	mreal TextWidth(const char *text, const char *font, mreal size) const MGL_FUNC_PURE;
	mreal TextWidth(const wchar_t *text, const char *font, mreal size) const MGL_FUNC_PURE;
	mreal TextHeight(const char *text, const char *font, mreal size) const MGL_FUNC_PURE;
	mreal TextHeight(const wchar_t *text, const char *font, mreal size) const MGL_FUNC_PURE;
	mreal TextHeight(const char *font, mreal size) const MGL_FUNC_PURE;
	inline mreal FontFactor() const		{	return font_factor;	}
	virtual mreal GetRatio() const MGL_FUNC_CONST;
	virtual int GetWidth() const MGL_FUNC_CONST;
	virtual int GetHeight() const MGL_FUNC_CONST;
	/// Add user-defined glyph
	void DefineGlyph(HCDT x, HCDT y, unsigned char id=0);

	/// Set to use or not text rotation
	inline void SetRotatedText(bool val)	{	set(val,MGL_ENABLE_RTEXT);	}
	/// Set default font style and color
	inline void SetFontDef(const char *font){	mgl_strncpy(FontDef, font, 31);	}
	/// Set to use or not text rotation
	inline void SetTickRotate(bool val)	{	set(val,MGL_TICKS_ROTATE);	}
	/// Set to use or not text rotation
	inline void SetTickSkip(bool val)	{	set(val,MGL_TICKS_SKIP);	}

	/// Add string to legend
	void AddLegend(const char *text,const char *style);
	void AddLegend(const wchar_t *text,const char *style);
	/// Clear saved legend string
	inline void ClearLegend()	{	Leg.clear();	}

	/// Set plot quality
	virtual void SetQuality(int qual=MGL_DRAW_NORM)	{	Quality=qual;	}
	inline int GetQuality() const	{	return Quality;	}
	inline void SetDrawReg(long nx=1, long ny=1, long m=0)	{	dr_x=nx;	dr_y=ny;	dr_p=m;	}

	// ~~~~~~~~~~~~~~~~~~~~~~ Developer functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/// Add point to the Pnt and return its position
	inline long AddPnt(mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1)
	{	return AddPnt(&B,p,c,n,a,scl);	}
	long AddPnt(const mglMatrix *M, mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1);
	bool AddPntQ(mglPnt &q, const mglMatrix *M, mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1);
	inline bool AddPntQ(mglPnt &q, mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1)
	{	return AddPntQ(q,&B,p,c,n,a,scl);	}
	inline bool AddPntQ(long id, const mglMatrix *M, mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1)
	{	return AddPntQ(Pnt[id],M,p,c,n,a,scl);	}
	inline bool AddPntQ(long id, mglPoint p, mreal c=-1, mglPoint n=mglPoint(NAN), mreal a=-1, int scl=1)
	{	return AddPntQ(Pnt[id],&B,p,c,n,a,scl);	}
	inline void SetPntOff(size_t id)	{	Pnt[id].x=NAN;	}
	long AllocPnts(size_t num);
	long PushPnts(size_t num, const mglPnt *qq);
	long CopyNtoC(long k, mreal c);
	bool CopyNtoC(mglPnt &q, long k, mreal c);
	inline bool CopyNtoC(long id, long k, mreal c)
	{	return (id>=0)?CopyNtoC(Pnt[id],k,c):false;	}
	long CopyProj(long from, mglPoint p, mglPoint n, short sub=0);
	bool CopyProj(mglPnt &q, long from, mglPoint p, mglPoint n, short sub=0);
	void CopyProj(long id, long from, mglPoint p, mglPoint n, short sub=0)
	{	if(id>=0)	CopyProj(Pnt[id],from,p,n,sub);	}
	void DisablePnt(long id)	{	Pnt[id].x = NAN;	}
	void SetRGBA(long k, const mglColor &c)
	{	if(k>=0)	{mglPnt &p=Pnt[k];	p.r = c.r;	p.g = c.g;	p.b = c.b;	p.a = c.a;}	}
	virtual void Reserve(long n);	///< Allocate n-cells for Pnt and return current position
	/// Set to reduce accuracy of points (to reduce size of output files)
	inline void SetReduceAcc(bool val)	{	set(val, MGL_REDUCEACC);	}
	/// Add glyph of current font to the Glf and return its position
	long AddGlyph(int s, long j);
	/// Add glyph to the Glf and return its position
	long AddGlyph(unsigned char id);
	/// Add active point as k-th element of Pnt
	void AddActive(long k,int n=0);
	/// Clear unused points and primitives
	void ClearUnused();

	inline mreal GetPenWidth()	{	return PenWidth;	}

	inline const mglMatrix *GetB()	const	{	return &B;	}
	inline mglPoint GetPntP(long i) const
	{	const mglPnt &p=Pnt[i];	return mglPoint(p.x,p.y,p.z);	}
	inline mglPoint GetPntN(long i) const
	{	const mglPnt &p=Pnt[i];	return mglPoint(p.u,p.v,p.w);	}
	inline mglColor GetPntC(long i) const
	{	const mglPnt &p=Pnt[i];	return mglColor(p.r,p.g,p.b,p.a);	}
	inline float GetClrC(long i) const	{	return Pnt[i].c;	}
	inline const mglGlyph &GetGlf(long i) const	{	return Glf[i];	}
	inline long GetGlfNum() const		{	return Glf.size();	}
	inline const mglPnt &GetPnt(long i) const	{	return Pnt[i];		}
	inline long GetPntNum() const		{	return Pnt.size();	}
	inline bool SamePnt(long i, long j) const
	{
		if(i<0 || j<0)	return true;
		const mglPnt &p=Pnt[i], &q=Pnt[j];
		return mgl_isnan(p.x) || mgl_isnan(q.x) || (p.x==q.x && p.y==q.y);
	}
	inline bool ValidPnt(size_t i)	{	return mgl_isnum(Pnt[i].x);	}
//	inline mglPrim &GetPrm(long i)		{	return Prm[i];		}
	inline mglPrim &GetPrm(long i, bool sort=true)
	{	return (sort && PrmInd) ? Prm[PrmInd[i]]:Prm[i];	}
	inline const mglPrim &GetPrm(long i, bool sort=true) const
	{	return (sort && PrmInd) ? Prm[PrmInd[i]]:Prm[i];	}
	inline long GetPrmNum() const		{	return Prm.size();	}
	inline const mglText &GetPtx(long i) const	{	return Ptx[i];		}
	inline long GetPtxNum() const		{	return Ptx.size();	}
	inline const mglTexture &GetTxt(long i) const	{	return Txt[i];	}
	inline long GetTxtNum() const		{	return Txt.size();	}
	/// Scale coordinates and cut off some points
	virtual bool ScalePoint(const mglMatrix *M, mglPoint &p, mglPoint &n, bool use_nan=true) const;

	inline const mglBlock &GetSub(size_t i) const	{	return Sub[i];	}
	inline const mglBlock &GetSub() const	{	return Sub.back();	}

	virtual mreal GetOrgX(char dir, bool inv=false) const=0;	///< Get Org.x (parse NAN value)
	virtual mreal GetOrgY(char dir, bool inv=false) const=0;	///< Get Org.y (parse NAN value)
	virtual mreal GetOrgZ(char dir, bool inv=false) const=0;	///< Get Org.z (parse NAN value)

	/// Get color depending on single variable z, which should be scaled if scale=true
	inline mreal GetC(long s,mreal z,bool scale = true) const
	{	return s+(scale?GetA(z):(z<0?0:z/MGL_FEPSILON));	}
//	{	return s+(scale?GetA(z):(z>0?z/MGL_FEPSILON:0));	}
	/// Get alpha value depending on single variable a
	mreal GetA(mreal a) const MGL_FUNC_PURE;
	/// Set pen/palette
	char SetPenPal(const char *stl, long *id=0, bool pal=true);
	/// Add texture (like color scheme) and return the position of first color
	long AddTexture(const char *cols, int smooth=0);
//	inline mreal AddTexture(char col)	{	return AddTexture(mglColor(col));	}
	mreal AddTexture(mglColor col);
	inline void DefColor(mglColor col)	{	CDef = AddTexture(col);	}
	/// Set mask for face coloring
	void SetMask(const char *mask);
	/// Set next color from palette
	mreal NextColor(long &id);
	mreal NextColor(long id, long sh);

	virtual void mark_plot(long p, char type, mreal size=1)=0;
	virtual void arrow_plot(long p1, long p2, char st)=0;
	virtual void line_plot(long p1, long p2)=0;
	virtual void trig_plot(long p1, long p2, long p3)=0;
	virtual void quad_plot(long p1, long p2, long p3, long p4)=0;
	virtual void smbl_plot(long p1, char id, double size)=0;
	void curve_plot(size_t n, size_t kq, size_t step=1);
	virtual void Glyph(mreal x, mreal y, mreal f, int style, long icode, mreal col)=0;
	virtual float GetGlyphPhi(const mglPnt &q, float phi)=0;
	virtual mreal text_plot(long p,const wchar_t *text,const char *fnt,mreal size=-1,mreal sh=0,mreal  col=-('k'),bool rot=true)=0;
	void vect_plot(long p1, long p2, mreal s=1);

	// check if visible
	virtual bool trig_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3) const =0;
	virtual bool quad_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4) const =0;

	inline mreal mark_size()	{	return MarkSize*font_factor;	}
//	inline char last_color()	{	return last_style[1];	}
	inline const char *last_line()	{	return last_style;	}
	int PrmCmp(size_t i, size_t j) const MGL_FUNC_PURE;	// compare 2 primitives with indexes i,j
	/// Check if plot termination is asked
	bool NeedStop()	{	if(event_cb)	event_cb(event_par);	return Stop;	}
	/// Ask to stop drawing
	void AskStop(bool stop=true)	{	Stop = stop;	}
	/// Set callback function for event processing
	void SetEventFunc(void (*func)(void *), void *par)	{	event_cb=func;	event_par=par;	}

protected:
	volatile bool Stop;	///< Flag that execution should be terminated.
	void (*event_cb)(void *);	///< Function to be called for event processing
	void *event_par;	///< Parameter for event processing function

	mglPoint OMin;		///< Lower edge for original axis (before scaling)
	mglPoint OMax;		///< Upper edge for original axis (before scaling)
	mglPoint AMin;		///< Lower edge for axis scaling
	mglPoint AMax;		///< Upper edge for axis scaling
	mglPoint FMin;		///< Actual lower edge after transformation formulas.
	mglPoint FMax;		///< Actual upper edge after transformation formulas.
	mglPoint Org;		///< Center of axis cross section.
	int WarnCode;		///< Warning code
	size_t *PrmInd;		///< Indexes of sorted primitives
	mglStack<mglPnt> Pnt; 	///< Internal points
	mglStack<mglPrim> Prm;	///< Primitives (lines, triangles and so on) -- need for export
	std::vector<mglBlock> Sub;	///< InPlot regions
	std::vector<mglText> Ptx;	///< Text labels for mglPrim
	std::vector<mglText> Leg;	///< Text labels for legend
	std::vector<mglGlyph> Glf;	///< Glyphs data
	std::vector<mglTexture> Txt;	///< Pointer to textures
#if MGL_HAVE_PTHREAD
	pthread_mutex_t mutexPnt, mutexTxt, mutexLeg, mutexGlf, mutexAct, mutexDrw;
	pthread_mutex_t mutexSub, mutexPrm, mutexPtx, mutexStk, mutexGrp, mutexClf;
#endif
	void *lockClf;		///< pointer to mutex for mglStack

	int TernAxis;		///< Flag that Ternary axis is used
	unsigned PDef;		///< Pen bit mask
	mreal pPos;			///< Current position in pen mask
	mreal PenWidth;		///< Pen width for further line plotting (must be >0 !!!)
//	long numT;			///< Number of textures
	mreal AmbBr;		///< Default ambient light brightness
	mreal DifBr;		///< Default diffusive light brightness

	mreal persp;		///< Original value for perspective
	mglMatrix Bp;		///< Transformation matrix for View() and Zoom()
	mglMatrix B;		///< Transformation matrix
	mglMatrix B1;		///< Transformation matrix for colorbar

	mglFont *fnt;		///< Class for printing vector text
	mreal FontSize;		///< The size of font for tick and axis labels
	char FontDef[32];	///< Font specification (see mglGraph::Puts). Default is Roman with align at center.
	int Quality;		///< Quality of plot (0x0-pure, 0x1-fast; 0x2-fine; 0x4 - low memory)

	HMEX fx;			///< Transformation formula for x direction.
	HMEX fy;			///< Transformation formula for y direction.
	HMEX fz;			///< Transformation formula for z direction.
	HMEX fa;			///< Transformation formula for coloring.
	HMEX fc;			///< Cutting off condition (formula).

	long CurrPal;		///< Current palette index
	mreal MarkSize;		///< The size of marks for 1D plots.
	mreal ArrowSize;	///< The size of arrows.
	char last_style[64];///< Last pen style	TODO: replace by std::string
	mreal font_factor;	///< Font scaling factor

	long dr_x, dr_y, dr_p;	///< default drawing region for quality&4 mode

	virtual void LightScale(const mglMatrix *M)=0;			///< Scale positions of light sources
	void ClearPrmInd();

	// block for SaveState()
	mglPoint MinS;		///< Saved lower edge of bounding box for graphics.
	mglPoint MaxS;		///< Saved upper edge of bounding box for graphics.
	mreal MSS, ASS, FSS, ADS, MNS, LSS;	///< Saved state
	mreal PrevState;		///< Previous value of SaveState()
	long CSS;			///< Saved flags
	bool saved;			///< State is saved
	std::string leg_str;///< text to be save in legend

	union
	{
		uint64_t mask;	///< Mask to be used for coloring
		unsigned char mask_ch[8];
	};
	int MaskAn;		///< Mask rotation angle in degrees
	int DefMaskAn;	///< Default mask rotation angle in degrees

private:
	mglBase(const mglBase &){}	// copying is not allowed
	const mglBase &operator=(const mglBase &t){return t;}	// copying is not allowed

	mglPoint CutMin;	///< Lower edge of bounding box for cut off.
	mglPoint CutMax;	///< Upper edge of bounding box for cut off.

	bool RecalcCRange();	///< Recalculate internal parameter for correct coloring.
	void RecalcBorder();	///< Recalculate internal parameter for correct transformation rules.
	bool SetFBord(mreal x,mreal y,mreal z);	///< Set internal boundng box depending on transformation formula
	void ClearEq();			///< Clear the used variables for axis transformation
};
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_dim0(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *name, bool less=false);
bool MGL_EXPORT mgl_check_dim1(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *name, bool less=false);
bool MGL_EXPORT mgl_check_dim2(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *name, bool less=false);
bool MGL_EXPORT mgl_check_dim3(HMGL gr, bool both, HCDT x, HCDT y, HCDT z, HCDT a, HCDT b, const char *name);
bool MGL_EXPORT mgl_check_vec3(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *name);
bool MGL_EXPORT mgl_check_trig(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *name, int d=3);
bool MGL_EXPORT mgl_isnboth(HCDT x, HCDT y, HCDT z, HCDT a);
bool MGL_EXPORT mgl_isboth(HCDT x, HCDT y, HCDT z, HCDT a);
inline bool mgl_islog(mreal a,mreal b)	{	return a*b>0 && (b/a+a/b)>=10.1;	}
//-----------------------------------------------------------------------------
#endif
#endif
