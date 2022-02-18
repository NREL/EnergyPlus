/***************************************************************************
 * canvas.h is part of Math Graphic Library
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
#ifndef MGL_CANVAS_H
#define MGL_CANVAS_H
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
struct GifFileType;
//-----------------------------------------------------------------------------
/// Structure for drawing axis and ticks
struct MGL_EXPORT mglAxis
{
	mglAxis() : dv(0),ds(0),d(0),ns(0),	v0(0),v1(0),v2(0),o(NAN),	f(0),	ch(0),	pos('t'),sh(0),inv(false),angl(NAN)	{}
	mglAxis(const mglAxis &aa) : dv(aa.dv),ds(aa.ds),d(aa.d),ns(aa.ns),	t(aa.t),fact(aa.fact),stl(aa.stl),	dir(aa.dir),a(aa.a),b(aa.b),org(aa.org), v0(aa.v0),v1(aa.v1),v2(aa.v2),o(aa.o),	f(aa.f),txt(aa.txt),	ch(aa.ch),	pos(aa.pos),sh(aa.sh),inv(aa.inv),angl(aa.angl)	{}
#if MGL_HAVE_RVAL
	mglAxis(mglAxis &&aa) : dv(aa.dv),ds(aa.ds),d(aa.d),ns(aa.ns),	t(aa.t),fact(aa.fact),stl(aa.stl),	dir(aa.dir),a(aa.a),b(aa.b),org(aa.org), v0(aa.v0),v1(aa.v1),v2(aa.v2),o(aa.o),	f(aa.f),txt(aa.txt),	ch(aa.ch),	pos(aa.pos),sh(aa.sh),inv(aa.inv),angl(aa.angl)	{}
#endif

	const mglAxis &operator=(const mglAxis &aa)
	{	dv=aa.dv; ds=aa.ds; d=aa.d; ns=aa.ns; t=aa.t; fact=aa.fact; stl=aa.stl;
		dir=aa.dir; a=aa.a; b=aa.b; org=aa.org; v0=aa.v0; v1=aa.v1; v2=aa.v2; o=aa.o;
		f=aa.f; txt=aa.txt; 	ch=aa.ch; pos=aa.pos; sh=aa.sh; inv=aa.inv;	return aa;	}
	inline void AddLabel(const std::wstring &lbl, mreal v)
	{	if(mgl_isfin(v))	txt.push_back(mglText(L' '+lbl+L' ',v));	}
	inline void Clear()
	{	dv=ds=d=v0=v1=v2=sh=0;	o=NAN;	ns=f=0;	pos = 't';	inv=false;
		fact.clear();	stl.clear();	t.clear();	txt.clear();	}

	mreal dv,ds;	///< Actual step for ticks and subticks.
	mreal d;		///< Step for axis ticks (if positive) or its number (if negative).
	int ns;			///< Number of axis subticks.
	std::wstring t;	///< Tick template (set "" to use default one ("%.2g" in simplest case))
	std::wstring fact;	///< Factor which should be placed after number (like L"\pi")
	std::string stl;	///< Tick styles (default is ""=>"3m")
	mglPoint dir;	///< Axis direction
	mglPoint a,b;	///< Directions of over axis
	mglPoint org;
	mreal v0;		///< Center of axis cross section
	mreal v1;		///< Minimal axis range.
	mreal v2;		///< Maximal axis range.
	mreal o;		///< Point of starting ticks numbering (if NAN then Org is used).
	int f;			///< Flag 0x1 - time, 0x2 - manual, 0x4 - fixed dv
	std::vector<mglText> txt;	///< Axis labels
	char ch;		///< Character of axis (like 'x','y','z','c')
	char pos;		///< Text position ('t' by default, or 'T' for opposite)
	mreal sh;		///< Extra shift of ticks and axis labels
	bool inv;		///< Inverse automatic origin position
	mreal angl;		///< Manual for ticks rotation (if not NAN)
};
//-----------------------------------------------------------------------------
class mglCanvas;
/// Structure for drawing region
struct MGL_EXPORT mglDrawReg
{
	mglDrawReg() {	memset(this,0,sizeof(mglDrawReg));	}
	mglDrawReg(const mglDrawReg &aa) : PDef(aa.PDef),angle(aa.angle),ObjId(aa.ObjId),PenWidth(aa.PenWidth),pPos(aa.pPos) ,x1(aa.x1),x2(aa.x2),y1(aa.y1),y2(aa.y2)	{}
#if MGL_HAVE_RVAL
	mglDrawReg(mglDrawReg &&aa) : PDef(aa.PDef),angle(aa.angle),ObjId(aa.ObjId),PenWidth(aa.PenWidth),pPos(aa.pPos) ,x1(aa.x1),x2(aa.x2),y1(aa.y1),y2(aa.y2)	{}
#endif
	inline void copy(const mglPrim &p)
	{	PDef = p.n3;	pPos = p.s;	ObjId = p.id;	PenWidth=p.w;	angle = p.angl;
		if(p.type==2 || p.type==3) PDef = p.m;	}
	inline const mglDrawReg &operator=(const mglDrawReg &aa)
	{	PDef=aa.PDef;	angle=aa.angle;	ObjId=aa.ObjId;	PenWidth=aa.PenWidth;	pPos=aa.pPos;	x1=aa.x1;	x2=aa.x2;	y1=aa.y1;	y2=aa.y2;	return aa;	}
	union
	{
		uint64_t PDef;
		unsigned char m[8];
	};
	int angle;	///< mask rotation values in degrees
	int ObjId;
	mreal PenWidth, pPos;
	int x1,x2,y1,y2;
	void set(mglCanvas *gr, int nx, int ny, int m);
};
//-----------------------------------------------------------------------------
/// Structure contains everything for drawing
struct MGL_EXPORT mglDrawDat
{
	mglDrawDat() {}
	mglDrawDat(const mglDrawDat &aa) : Pnt(aa.Pnt),Prm(aa.Prm),Sub(aa.Sub),Ptx(aa.Ptx),Glf(aa.Glf),Txt(aa.Txt)	{}
#if MGL_HAVE_RVAL
	mglDrawDat(mglDrawDat &&aa) : Pnt(aa.Pnt),Prm(aa.Prm),Sub(aa.Sub),Ptx(aa.Ptx),Glf(aa.Glf),Txt(aa.Txt)	{}
#endif
	inline const mglDrawDat&operator=(const mglDrawDat &aa)
	{	Pnt=aa.Pnt;	Prm=aa.Prm;	Ptx=aa.Ptx;	Glf=aa.Glf;	Txt=aa.Txt;	Sub=aa.Sub;	return aa;	}
	mglStack<mglPnt>  Pnt;	///< Internal points
	mglStack<mglPrim> Prm;	///< Primitives (lines, triangles and so on) -- need for export
	std::vector<mglBlock> Sub;	///< InPlot regions
	std::vector<mglText> Ptx;	///< Text labels for mglPrim
	std::vector<mglGlyph> Glf;	///< Glyphs data
	std::vector<mglTexture> Txt;	///< Pointer to textures
};
#if defined(_MSC_VER)
MGL_EXTERN template class MGL_EXPORT std::vector<mglDrawDat>;
#endif
//-----------------------------------------------------------------------------
union mglRGBA	{	uint32_t c; unsigned char r[4];	};
//-----------------------------------------------------------------------------
/// Class contains all functionality for creating different mathematical plots
class MGL_EXPORT mglCanvas : public mglBase
{
friend struct mglPrim;
friend struct mglDrawReg;
public:
using mglBase::Light;

	mglCanvas(int w=800, int h=600);
	virtual ~mglCanvas();

	/// Set default parameter for plotting
	void DefaultPlotParam();

	/// Set angle of view indepently from mglCanvas::Rotate()
	virtual void View(mreal tetx,mreal tetz,mreal tety=0);
	/// Zoom in or zoom out (if Zoom(0, 0, 1, 1)) a part of picture
	virtual void Zoom(mreal x1, mreal y1, mreal x2, mreal y2);
	/// Restore image after View() and Zoom()
	inline void Restore()	{	Zoom(0,0,1,1);	}

	/// Clear transformation matrix.
	inline void Identity(bool rel=false)	{	InPlot(0,1,0,1,rel);	}
	inline void Identity(mglMatrix &M, bool rel=false)	{	InPlot(M,0,1,0,1,rel);	}
	/// Push transformation matrix into stack
	void Push();
	/// Set PlotFactor
	inline void SetPlotFactor(mreal val)
	{	if(val<=0)	{B.pf=1.55;	set(MGL_AUTO_FACTOR);}	else {B.pf=val;	clr(MGL_AUTO_FACTOR);}	}
	/// Get PlotFactor
	inline mreal GetPlotFactor()	{	return B.pf;	}
	/// Pop transformation matrix from stack
	void Pop();
	/// Clear up the frame
	virtual void Clf(mglColor back=NC);
	virtual void Clf(const char *col);

	/// Put further plotting in cell of stick rotated on angles tet, phi
	void StickPlot(int num, int i, mreal tet, mreal phi);
	/// Put further plotting in cell of stick sheared on sx, sy
	void ShearPlot(int num, int i, mreal sx, mreal sy, mreal xd, mreal yd);
	/// Put further plotting in some region of whole frame surface.
	inline void InPlot(mreal x1,mreal x2,mreal y1,mreal y2,bool rel=true)
	{	InPlot(B,x1,x2,y1,y2,rel);	}
	void InPlot(mreal x1,mreal x2,mreal y1,mreal y2, const char *style);
	void InPlot(mglMatrix &M,mreal x1,mreal x2,mreal y1,mreal y2,bool rel=true);
	/// Add title for current subplot/inplot
	void Title(const char *title,const char *stl="#",mreal size=-2);
	void Title(const wchar_t *title,const char *stl="#",mreal size=-2);
	/// Set aspect ratio for further plotting.
	void Aspect(mreal Ax,mreal Ay,mreal Az);
	/// Shear a further plotting.
	void Shear(mreal Sx,mreal Sy);
	/// Rotate a further plotting.
	void Rotate(mreal TetX,mreal TetZ,mreal TetY=0);
	/// Rotate a further plotting around vector {x,y,z}.
	void RotateN(mreal Tet,mreal x,mreal y,mreal z);
	/// Set perspective (in range [0,1)) for plot. Set to zero for switching off. Return the current perspective.
	void Perspective(mreal a, bool req=true)
	{	if(req)	persp = Bp.pf = a;	else	Bp.pf = persp?persp:fabs(a);	}
	/// Save parameters of current inplot
	inline void SaveInPlot()
	{	sB=B;	sW=inW, sH=inH, sZ=ZMin, sX=inX, sY=inY, sFF=font_factor;	}
	/// Use saved parameters as current inplot
	inline void LoadInPlot()
	{	B=sB;	inW=sW, inH=sH, ZMin=sZ, inX=sX, inY=sY, font_factor=sFF;	}

	/// Set size of frame in pixels. Normally this function is called internaly.
	virtual void SetSize(int w,int h,bool clf=true);
	/// Get ratio (mreal width)/(mreal height).
	mreal GetRatio() const MGL_FUNC_PURE;
	/// Get bitmap data prepared for saving to file
	virtual unsigned char **GetRGBLines(long &w, long &h, unsigned char *&f, bool alpha=false);
	/// Get RGB bitmap of current state image.
	virtual const unsigned char *GetBits();
	/// Get RGBA bitmap of background image.
	const unsigned char *GetBackground()	{	return GB;	};
	/// Get RGBA bitmap of current state image.
	const unsigned char *GetRGBA()	{	Finish();	return G4;	}
	/// Get width of the image
	int GetWidth() const	{	return Width;	}
	/// Get height of the image
	int GetHeight() const	{	return Height;	}
	/// Combine plots from 2 canvases. Result will be saved into this.
	void Combine(const mglCanvas *gr);
	/// Set boundary box for export graphics into 2D file formats
	void SetBBox(int x1=0, int y1=0, int x2=-1, int y2=-1)
	{	BBoxX1=x1;	BBoxY1=y1;	BBoxX2=x2;	BBoxY2=y2;	}

	/// Rasterize current plot and set it as background image
	void Rasterize();
	/// Load image for background from file
	void LoadBackground(const char *fname, double alpha=1);
	/// Fill background image by specified color
	void FillBackground(const mglColor &cc);

	inline mreal GetDelay() const	{	return Delay;	}
	inline void SetDelay(mreal d)	{	Delay=d;	}

	/// Calculate 3D coordinate {x,y,z} for screen point {xs,ys}
	mglPoint CalcXYZ(int xs, int ys, bool real=false) const MGL_FUNC_PURE;
	/// Calculate screen point {xs,ys} for 3D coordinate {x,y,z}
	void CalcScr(mglPoint p, int *xs, int *ys) const;
	mglPoint CalcScr(mglPoint p) const;
	/// Set object/subplot id
	inline void SetObjId(long id)	{	ObjId = id;	}
	/// Get object id
	inline int GetObjId(long xs,long ys) const
	{	long i=xs+Width*ys;	return (i>=0 && i<Width*Height)?OI[i]:-1;	}
	/// Get subplot id
	int GetSplId(long xs,long ys) const MGL_FUNC_PURE;
	/// Check if there is active point or primitive (n=-1)
	int IsActive(int xs, int ys,int &n);

	/// Create new frame.
	virtual int NewFrame();
	/// Finish frame drawing
	virtual void EndFrame();
	/// Get the number of created frames
	inline int GetNumFrame() const	{	return CurFrameId;	}
	/// Reset frames counter (start it from zero)
	virtual void ResetFrames();
	/// Delete primitives for i-th frame
	virtual void DelFrame(long i);
	/// Get drawing data for i-th frame.
	void GetFrame(long i);
	/// Set drawing data for i-th frame. This work as EndFrame() but don't add frame to GIF image.
	virtual void SetFrame(long i);
	/// Add drawing data from i-th frame to the current drawing
	void ShowFrame(long i);
	/// Clear list of primitives for current drawing
	void ClearFrame();

	/// Start write frames to cinema using GIF format
	void StartGIF(const char *fname, int ms=100);
	/// Stop writing cinema using GIF format
	void CloseGIF();
	/// Finish plotting. Normally this function is called internaly.
	virtual void Finish();
	/// Export points and primitives in file using MGLD format
	bool ExportMGLD(const char *fname, const char *descr=0);
	/// Import points and primitives from file using MGLD format
	bool ImportMGLD(const char *fname, bool add=false);
	/// Export in JSON format suitable for later drawing by JavaScript
	bool WriteJSON(const char *fname, bool force_zlib=false);
	std::string GetJSON();

	/// Set the transparency type
	inline void SetTranspType(int val)
	{	Flag=(Flag&(~3)) + (val&3);	SetAxisStl(val==2?"w-":"k-");	}
	/// Set the fog distance or switch it off (if d=0).
	virtual void Fog(mreal d, mreal dz=0.25);
	/// Switch on/off the specified light source.
	virtual void Light(int n, bool enable);
	/// Add a light source.
	virtual void AddLight(int n,mglPoint r, mglPoint d, char c='w', mreal bright=0.5, mreal ap=0);
	inline void AddLight(int n,mglPoint d, char c='w', mreal bright=0.5, mreal ap=0)
	{	AddLight(n,mglPoint(NAN),d,c,bright,ap);	}

	/// Set ticks position and text (\n separated). Use n=0 to disable this feature.
	void SetTicksVal(char dir, const char *lbl, bool add=false);
	void SetTicksVal(char dir, HCDT v, const char *lbl, bool add=false);
	void SetTicksVal(char dir, HCDT v, const char **lbl, bool add=false);
	void SetTicksVal(char dir, const wchar_t *lbl, bool add=false);
	void SetTicksVal(char dir, HCDT v, const wchar_t *lbl, bool add=false);
	void SetTicksVal(char dir, HCDT v, const wchar_t **lbl, bool add=false);
		/// Add manual tick at given position. Use "" to disable this feature.
	void AddTick(char dir, double val, const char *lbl);
	void AddTick(char dir, double val, const wchar_t *lbl);

	/// Set templates for ticks
	void SetTickTempl(char dir, const wchar_t *t);
	void SetTickTempl(char dir, const char *t);
	/// Set time templates for ticks
	void SetTickTime(char dir, mreal d=0, const char *t="");
	/// Set the ticks parameters
	void SetTicks(char dir, mreal d=0, int ns=0, mreal org=NAN, const wchar_t *lbl=0);
	/// Auto adjust ticks
	void AdjustTicks(const char *dir="xyzc", bool force=false, std::string stl="");
	/// Tune ticks
	inline void SetTuneTicks(int tune, mreal pos=1.15)
	{	TuneTicks = tune;	FactorPos = pos;	}
	/// Set ticks styles
	void SetAxisStl(const char *stl="k", const char *tck=0, const char *sub=0);
	/// Set ticks length
	void SetTickLen(mreal tlen, mreal stt=1.);

	/// Draws bounding box outside the plotting volume with color c.
	void Box(const char *col=0, bool ticks=true);
	/// Draw axises with ticks in directions determined by string parameter dir.
	void Axis(const char *dir="xyzt", const char *stl="", const char *opt="");
	/// Draw grid lines perpendicular to direction determined by string parameter dir.
	void Grid(const char *dir="xyzt",const char *pen="B-", const char *opt="");
	/// Print the label text for axis dir.
	void Label(char dir, const char *text, mreal pos=0, const char *opt="");
	void Labelw(char dir, const wchar_t *text, mreal pos=0, const char *opt="");

	/// Draw colorbar at edge of axis
	void Colorbar(const char *sch=0);
	void Colorbar(const char *sch, mreal x, mreal y, mreal w, mreal h);
	/// Draw colorbar at edge of axis for manual colors
	void Colorbar(HCDT v, const char *sch=0);
	void Colorbar(HCDT v, const char *sch, mreal x, mreal y, mreal w, mreal h);

	/// Draw legend of accumulated strings at position (x, y) by font with size
	inline void Legend(mreal x, mreal y, const char *font="#", const char *opt="")
	{	Legend(Leg,x,y,font,opt);	}
	/// Draw legend of accumulated strings by font with size
	inline void Legend(int where=0x3, const char *font="#", const char *opt="")
	{	Legend(Leg,(where&1)?1:0,(where&2)?1:0,font,opt);	}
	/// Draw legend of accumulated strings by font with size
	inline void Legend(const std::vector<mglText> &leg, int where=3, const char *font="#", const char *opt="")
	{	Legend(leg,(where&1)?1:0,(where&2)?1:0,font,opt);	}
	/// Draw legend strings text at position (x, y) by font with size
	void Legend(const std::vector<mglText> &leg, mreal x, mreal y, const char *font="#", const char *opt="");
	/// Number of marks in legend sample
	inline void SetLegendMarks(int num=1)	{	LegendMarks = num>0?num:1;	}

	/// Draw table for values val along given direction with row labels text at given position
	void Table(mreal x, mreal y, HCDT val, const wchar_t *text, const char *fnt, const char *opt);

	void StartAutoGroup (const char *);
	void EndGroup();
	/// Set extra shift for tick and axis labels
	inline void SetTickShift(mglPoint p)
	{	ax.sh = p.x;	ay.sh = p.y;	az.sh = p.z;	ac.sh = p.c;	}
	/// Get rotation angle for glyph
	float GetGlyphPhi(const mglPnt &q, float phi);

	// Following arrays are open for advanced users only. It is not recommended to change them directly
	float *Z;			///< Height for given level in Z-direction (size 3*width*height)
	unsigned char *C;	///< Picture for given level in Z-direction (size 3*4*width*height)
	int *OI;			///< ObjId arrays (size width*height)
	/// Plot point p with color c
	void pnt_plot(long x,long y,mreal z,const unsigned char c[4], int obj_id);
	void pnt_fast(long x,long y,mreal z,const unsigned char c[4], int obj_id);
	/// preparing primitives for 2d export or bitmap drawing (0 default, 1 for 2d vector, 2 for 3d vector)
	void PreparePrim(int fast);
	inline uint32_t GetPntCol(long i) const	{	return pnt_col[i];	}
	inline uint32_t GetPrmCol(long i, bool sort=true) const	{	return GetColor(GetPrm(i, sort));	}
	/// Set the size of semi-transparent area around lines, marks, ...
	inline void SetPenDelta(float d)	{	pen_delta = 1.5*fabs(d);	}

protected:
	mreal Delay;		///< Delay for animation in seconds
	// NOTE: Z should be float for reducing space and for compatibility reasons
	unsigned char *G4;	///< Final picture in RGBA format. Prepared in Finish().
	unsigned char *G;	///< Final picture in RGB format. Prepared in Finish().
	unsigned char *GB;	///< Background picture in RGBA format.
	std::vector<mglDrawDat> DrwDat;	///< Set of ALL drawing data for each frames

	int LegendMarks;	///< Number of marks in the Legend
	unsigned char BDef[4];	///< Background color
	mglAxis ax,ay,az,ac;///< Axis parameters

	int TuneTicks;		///< Draw tuned ticks with extracted common component
	mreal FactorPos;	///< Position of axis ticks factor (0 at Min, 1 at Max, 1.1 is default)
	mreal TickLen;		///< Length of tiks (subticks length is sqrt(1+st_t)=1.41... times smaller)
	char AxisStl[32];	///< Axis line style. Default is "k"
	char TickStl[32];	///< Tick line style. Default is "k"
	char SubTStl[32];	///< Subtick line style. Default is "k"
	mreal st_t;			///< Subtick-to-tick ratio (ls=lt/sqrt(1+st_t)). Default is 1.

	int CurFrameId;		///< Number of automaticle created frames
	int Width;			///< Width of the image
	int Height;			///< Height of the image
	int Depth;			///< Depth of the image
	mreal inW, inH;		///< Width and height of last InPlot
	mreal inX, inY;		///< Coordinates of last InPlot
	mglLight light[10];	///< Light sources
	mreal FogDist;		///< Inverse fog distance (fog ~ exp(-FogDist*Z))
	mreal FogDz;		///< Relative shift of fog

	inline mglAxis &GetAxis(unsigned ch)
	{	mglAxis *aa[3]={&ax,&ay,&az};	ch-='x';	return ch<3?*(aa[ch]):ac;	}
	inline HMEX GetFormula(unsigned ch)
	{	HMEX aa[3]={fx,fy,fz};	ch-='x';	return ch<3?aa[ch]:fa;	}
	/// Auto adjust ticks
	void AdjustTicks(mglAxis &aa, bool ff);
	/// Prepare labels for ticks
	void LabelTicks(mglAxis &aa);
	/// Draw axis
	void DrawAxis(mglAxis &aa, int text=1, char arr=0,const char *stl="",mreal angl=NAN);
	/// Draw axis grid lines
	void DrawGrid(mglAxis &aa, bool at_tick=false);
	/// Update axis ranges
	inline void UpdateAxis()
	{	ax.v0=Org.x;	ay.v0=Org.y;	az.v0=Org.z;	ac.v0=Org.c;
		ax.v1=Min.x;	ay.v1=Min.y;	az.v1=Min.z;	ac.v1=Min.c;
		ax.v2=Max.x;	ay.v2=Max.y;	az.v2=Max.z;	ac.v2=Max.c;	}

	/// Clear ZBuffer only
	void ClfZB(bool force=false);
	/// Scale coordinates and cut off some points
	bool ScalePoint(const mglMatrix *M, mglPoint &p, mglPoint &n, bool use_nan=true) const;
	void LightScale(const mglMatrix *M);	///< Additionally scale positions of light sources
	void LightScale(const mglMatrix *M, mglLight &l);	///< Additionally scale positions of light
	/// Push drawing data (for frames only). NOTE: can be VERY large
	long PushDrwDat();
	/// Retur color for primitive depending lighting
	uint32_t GetColor(const mglPrim &p) const MGL_FUNC_PURE;

	mreal GetOrgX(char dir, bool inv=false) const MGL_FUNC_PURE;	///< Get Org.x (parse NAN value)
	mreal GetOrgY(char dir, bool inv=false) const MGL_FUNC_PURE;	///< Get Org.y (parse NAN value)
	mreal GetOrgZ(char dir, bool inv=false) const MGL_FUNC_PURE;	///< Get Org.z (parse NAN value)

	void mark_plot(long p, char type, mreal size=1);
	void arrow_plot(long p1, long p2, char st);
	void line_plot(long p1, long p2);
	void trig_plot(long p1, long p2, long p3);
	void quad_plot(long p1, long p2, long p3, long p4);
	void Glyph(mreal x, mreal y, mreal f, int style, long icode, mreal col);
	void smbl_plot(long p1, char id, double size);
	mreal text_plot(long p,const wchar_t *text,const char *fnt,mreal size=-1,mreal sh=0,mreal  col=-('k'), bool rot=true);

	void add_prim(mglPrim &a);	///< add primitive to list
	void arrow_draw(const mglPnt &p1, const mglPnt &p2, char st, mreal size, const mglDrawReg *d);
	virtual void mark_draw(const mglPnt &p, char type, mreal size, mglDrawReg *d);
	virtual void line_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *d);
	virtual void trig_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool anorm, const mglDrawReg *d);
	virtual void quad_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *d);
	virtual void pnt_draw(const mglPnt &p, const mglDrawReg *d);
	void arrow_draw(long n1, long n2, char st, float ll);
	void arrow_plot_3d(long n1, long n2, char st, float ll);
	void glyph_draw(const mglPrim &P, mglDrawReg *d);
	void glyph_draw_new(const mglPrim &P, mglDrawReg *d);
	bool IsSame(const mglPrim &pr,mreal wp,mglColor cp,int st);

	// check if visible
	bool trig_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3) const;
	bool quad_vis(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4) const;

	// functions for glyph drawing
	virtual void glyph_fill(mreal phi, const mglPnt &p, mreal f, const mglGlyph &g, const mglDrawReg *d);
	void glyph_wire(mreal phi, const mglPnt &p, mreal f, const mglGlyph &g, const mglDrawReg *d);
	void glyph_line(mreal phi, const mglPnt &p, mreal f, bool solid, const mglDrawReg *d);

	// restore normalized coordinates from screen ones
	mglPoint RestorePnt(mglPoint ps, bool norm=false) const MGL_FUNC_PURE;

	// functions for multi-threading
	void pxl_pntcol(long id, long n, const void *);
	void pxl_combine(long id, long n, const void *);
	void pxl_memcpy(long id, long n, const void *);
	void pxl_backgr(long id, long n, const void *);
	void pxl_primdr(long id, long n, const void *);
	void pxl_dotsdr(long id, long n, const void *);
	void pxl_primpx(long id, long n, const void *);
	void pxl_transform(long id, long n, const void *);
	void pxl_setz(long id, long n, const void *);
	void pxl_setz_adv(long id, long n, const void *);
	void pxl_other(long id, long n, const void *p);
	/// Put drawing from other mglCanvas (for multithreading, like subplots)
	void PutDrawReg(mglDrawReg *d, const mglCanvas *gr);

private:
    mglCanvas(const mglCanvas &){}	// copying is not allowed
	const mglCanvas &operator=(const mglCanvas &t){return t;}	// copying is not allowed

	mglMatrix sB;	// parameters of saved inplot
	mreal sW, sH, sZ, sX, sY, sFF;

	uint32_t *pnt_col;
//	mreal _tetx,_tety,_tetz;		// extra angles
	std::vector<mglMatrix> stack;	///< stack for transformation matrices
	GifFileType *gif;
	mreal fscl,ftet;	///< last scale and rotation for glyphs
	long forg;			///< original point (for directions)
	size_t grp_counter;	///< Counter for StartGroup(); EndGroup();
	mglMatrix Bt;		///< temporary matrix for text
	float pen_delta;	///< delta pen width (dpw) -- the size of semi-transparent region for lines, marks, ...

	/// Draw generic colorbar
	void colorbar(HCDT v, const mreal *s, int where, mreal x, mreal y, mreal w, mreal h, bool text);
	/// Draw labels for ticks
	void DrawLabels(mglAxis &aa, bool inv=false, const mglMatrix *M=0);
	/// Get label style
	char GetLabelPos(mreal c, long kk, mglAxis &aa);
	/// Draw tick
	void tick_draw(mglPoint o, mglPoint d1, mglPoint d2, int f);
	mreal FindOptOrg(char dir, int ind) const MGL_FUNC_PURE;
	/// Transform mreal color and alpha to bits format
	void col2int(const mglPnt &p, unsigned char *r, int obj_id) const;
	/// Combine colors in 2 plane.
	void combine(unsigned char *c1, const unsigned char *c2) const;
	/// Fast drawing of line between 2 points
	void fast_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *d);

	/// Additionally scale points p for positioning in image
	void PostScale(const mglMatrix *M, mglPoint &p) const;
	/// Scale points p for projection to the face number nface in image
	long ProjScale(int nface, long p, bool text=false);
	/// Set coordinate and add the point, return its id
	long setPp(mglPnt &q, const mglPoint &p);

	// fill pixel for given primitive
	void mark_pix(long i,long j,const mglPnt &p, char type, mreal size, mglDrawReg *d);
	void arrow_pix(long i,long j,const mglPnt &p1, const mglPnt &p2, char st, mreal size, const mglDrawReg *d);
	void line_pix(long i,long j,const mglPnt &p1, const mglPnt &p2, const mglDrawReg *d);
	void trig_pix(long i,long j,const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool anorm, const mglDrawReg *d);
	void quad_pix(long i,long j,const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *d);
	void glyph_pix(long i,long j,const mglPrim &P, mglDrawReg *d);
	void pnt_pix(long i,long j,const mglPnt &p, const mglDrawReg *d);
	void glyph_fpix(long i,long j,const mglMatrix *M, const mglPnt &p, mreal f, const mglGlyph &g, const mglDrawReg *d);
	void glyph_wpix(long i,long j,const mglMatrix *M, const mglPnt &p, mreal f, const mglGlyph &g, const mglDrawReg *d);
	void glyph_lpix(long i,long j,const mglMatrix *M, const mglPnt &p, mreal f, bool solid, const mglDrawReg *d);
};
//-----------------------------------------------------------------------------
struct mglThreadG
{
	mglCanvas *gr;		// grapher
	void (mglCanvas::*f)(long i, long n, const void *);
	unsigned id;		// thread id
	long n;	// total number of iteration
	const void *p;		// external parameter
};
/// Start several thread for the task
void mglStartThread(void (mglCanvas::*func)(long i, long n), mglCanvas *gr, long n);
//-----------------------------------------------------------------------------
inline mreal get_persp(float pf, float z, float Depth)
//{	return (1-pf)/(1-pf*z/Depth);	}
{	return (1-pf/1.37)/(1-pf*z/Depth);	}
inline mreal get_pfact(float pf, float Depth)
//{	return pf/(1-pf)/Depth;	}
{	return pf/(1-pf/1.37)/Depth;	}
//-----------------------------------------------------------------------------
#endif
