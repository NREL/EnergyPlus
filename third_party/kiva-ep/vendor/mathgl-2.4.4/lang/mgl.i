/***************************************************************************
 * mgl.h is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//-----------------------------------------------------------------------------
/// Wrapper class for all graphics
class mglGraph
{
	mglGraph(const mglGraph &t) {}	// copying is not allowed
	const mglGraph &operator=(const mglGraph &t)	{	return t;	}
protected:
	HMGL gr;
public:
	inline mglGraph(int kind=0, int width=600, int height=400)
	{
		if(kind==-1)	gr=NULL;
#if MGL_HAVE_OPENGL
		else if(kind==1)	gr=mgl_create_graph_gl();
#else
		else if(kind==1)
		{	gr=mgl_create_graph(width, height);
			SetGlobalWarn("OpenGL support was disabled. Please, enable it and rebuild MathGL.");	}
#endif
		else	gr=mgl_create_graph(width, height);
	}
	inline mglGraph(HMGL graph)
	{	gr = graph;		mgl_use_graph(gr,1);	}
	virtual ~mglGraph()
	{	if(mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);	}
	/// Get pointer to internal HMGL object
	inline HMGL Self()	{	return gr;	}
	/// Set default parameters for plotting
	inline void DefaultPlotParam()			{	mgl_set_def_param(gr);	}
	/// Set name of plot for saving filename
	inline void SetPlotId(const char *id)	{	mgl_set_plotid(gr,id);	}
	/// Get name of plot for saving filename
	inline const char *GetPlotId()	{	return mgl_get_plotid(gr);	}

	/// Ask to stop drawing
	inline void Stop(bool stop=true)	{	mgl_ask_stop(gr, stop);	}
	/// Check if plot termination is asked
	inline bool NeedStop()	{	return mgl_need_stop(gr);	}

	/// Set the transparency on/off.
	inline void Alpha(bool enable)			{	mgl_set_alpha(gr, enable);	}
	/// Set the gray-scale mode on/off.
	inline void Gray(bool enable)			{	mgl_set_gray(gr, enable);	}
	/// Set default value of alpha-channel
	inline void SetAlphaDef(double alpha)	{	mgl_set_alpha_default(gr, alpha);	}
	/// Set the transparency type (0 - usual, 1 - glass, 2 - lamp)
	inline void SetTranspType(int type)		{	mgl_set_transp_type(gr, type);	}
	/// Set the size of semi-transparent area around lines, marks, glyphs, ... Default is 1.
	inline void SetPenDelta(double d)	{	mgl_pen_delta(gr,d);	}

	/// Set the using of light on/off.
	inline void Light(bool enable)			{	mgl_set_light(gr, enable);	}
	/// Switch on/off the specified light source.
	inline void Light(int n,bool enable)	{	mgl_set_light_n(gr, n, enable);	}
	/// Use diffusive light (only for local light sources) -- OBSOLETE
	inline void SetDifLight(bool dif)		{	mgl_set_light_dif(gr, dif);	}
	/// Set to attach light settings to inplot.
	inline void AttachLight(bool enable)		{	mgl_set_attach_light(gr, enable);	}
	/// Add a light source.
	inline void AddLight(int n, mglPoint p, char col='w', double bright=0.5, double ap=0)
	{	mgl_add_light_ext(gr, n, p.x, p.y, p.z, col, bright, ap);	}
	inline void AddLight(int n, mglPoint r, mglPoint p, char col='w', double bright=0.5, double ap=0)
	{	mgl_add_light_loc(gr, n, r.x, r.y, r.z, p.x, p.y, p.z, col, bright, ap);	}
	/// Set ambient light brightness
	inline void SetAmbient(double i)			{	mgl_set_ambbr(gr, i);	}
	/// Set diffusive light brightness
	inline void SetDiffuse(double i)			{	mgl_set_difbr(gr, i);	}
	/// Set the fog distance or switch it off (if d=0).
	inline void Fog(double d, double dz=0.25)	{	mgl_set_fog(gr, d, dz);		}

	/// Set relative width of rectangles in Bars, Barh, BoxPlot, Candle, OHLC (default is 0.7)
	inline void SetBarWidth(double width)	{	mgl_set_bar_width(gr, width);	}
	/// Set default size of marks (locally you can use "size" option)
	inline void SetMarkSize(double size)		{	mgl_set_mark_size(gr, size);	}
	/// Set default size of arrows (locally you can use "size" option)
	inline void SetArrowSize(double size)	{	mgl_set_arrow_size(gr, size);	}
	/// Set number of mesh lines (use 0 to draw all of them)
	inline void SetMeshNum(int num)			{	mgl_set_meshnum(gr, num);	}
	/// Set number of visible faces (use 0 to draw all of them)
	inline void SetFaceNum(int num)			{	mgl_set_facenum(gr, num);	}

	/// Set cutting for points outside of bounding box
	inline void SetCut(bool cut)				{	mgl_set_cut(gr, cut);	}
	/// Set additional cutting box
	inline void SetCutBox(mglPoint p1, mglPoint p2)
	{	mgl_set_cut_box(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z);	}
	/// Set the cutting off condition (formula)
	inline void CutOff(const char *EqC)		{	mgl_set_cutoff(gr, EqC);	}

	/// Set default font size
	inline void SetFontSize(double size)	{	mgl_set_font_size(gr, size);	}
	/// Set default font style and color
	inline void SetFontDef(const char *fnt)	{	mgl_set_font_def(gr, fnt);	}
	/// Set FontSize by size in pt and picture DPI (default is 16 pt for dpi=72)
	virtual void SetFontSizePT(double pt, int dpi=72)	{	SetFontSize(pt*27.f/dpi);	}
	/// Set FontSize by size in centimeters and picture DPI (default is 0.56 cm = 16 pt)
	inline void SetFontSizeCM(double cm, int dpi=72)	{	SetFontSizePT(cm*28.45f,dpi);	}
	/// Set FontSize by size in inch and picture DPI (default is 0.22 in = 16 pt)
	inline void SetFontSizeIN(double in, int dpi=72)	{	SetFontSizePT(in*72.27f,dpi);	}
	/// Load font from file
	inline void LoadFont(const char *name, const char *path=NULL)
	{	mgl_load_font(gr, name, path);	}
	/// Copy font from another mglGraph instance
	inline void CopyFont(const mglGraph *GR)	{	mgl_copy_font(gr, GR->gr);}
	/// Restore font (load default font for new HMGL objects)
	inline void RestoreFont()				{	mgl_restore_font(gr);	}
	/// Set to use or not text rotation
	inline void SetRotatedText(bool enable)	{	mgl_set_rotated_text(gr, enable);	}
	/// Set to scale text in relative subplots too
	inline void SetScaleText(bool enable)	{	mgl_set_scale_text(gr, enable);	}
	/// Set default font for all new HMGL and mglGraph objects
	static inline void SetDefFont(const char *name, const char *path="")	{	mgl_def_font(name,path);	}
	/// Add user-defined glyph for symbol and set its optional id
	inline void DefineSymbol(char id, const mglData &x, const mglData &y)
	{	mgl_define_symbol(gr, id, &x, &y);	}

	/// Set default palette
	inline void SetPalette(const char *colors)	{	mgl_set_palette(gr, colors);	}
	/// Set default color scheme
	inline void SetDefScheme(const char *sch)	{	mgl_set_def_sch(gr, sch);	}

	/// Sets RGB values for color with given id
	static inline void SetColor(char id, double r, double g, double b)	{	mgl_set_color(id, r, g, b);	}
	/// Set mask for face coloring as array of type 'unsigned char[8]'
	static inline void SetMask(char id, const char *mask)	{	mgl_set_mask(id, mask);	}
	/// Set mask for face coloring as uint64_t number
	static inline void SetMask(char id, uint64_t mask)	{	mgl_set_mask_val(id, mask);	}
	/// Set default mask rotation angle
	inline void SetMaskAngle(int angle)	{	mgl_set_mask_angle(gr, angle);	}

	/// Get last warning code
	inline int  GetWarn()	{	return mgl_get_warn(gr);}
	/// Set warning code ant fill message
	inline void SetWarn(int code, const char *info)	{	mgl_set_warn(gr,code,info);	}
	/// Get text of warning message(s)
	inline const char *Message()	{	return mgl_get_mess(gr);	}
	/// Set global warning message
	static inline void SetGlobalWarn(const char *text)	{	mgl_set_global_warn(text);	}
	/// Get text of global warning message(s)
	static inline const char *GlobalWarn()	{	return mgl_get_global_warn();	}
	/// Suppress printing warnings to stderr
	static inline void SuppressWarn(bool on)	{	mgl_suppress_warn(on);	}
	/// Check if MathGL version is valid (return false) or not (return true)
	static inline bool CheckVersion(const char *ver)	{	return mgl_check_version(ver);	}
	/// Display progress of something.
	inline void Progress(int value, int maximal)	{	mgl_progress(value, maximal, gr);	}

	/// Set axis range scaling -- simplified way to shift/zoom axis range -- need to replot whole image!
	inline void ZoomAxis(mglPoint p1=mglPoint(0,0,0,0), mglPoint p2=mglPoint(1,1,1,1))
	{	mgl_zoom_axis(gr, p1.x,p1.y,p1.z,p1.c, p2.x,p2.y,p2.z,p2.c);	}
	/// Add [v1, v2] to the current range in direction dir
	inline void AddRange(char dir, double v1, double v2)
	{	mgl_add_range_val(gr, dir, v1, v2);	}
	/// Set range in direction dir as [v1, v2]
	inline void SetRange(char dir, double v1, double v2)
	{	mgl_set_range_val(gr, dir, v1, v2);	}
	/// Set range in direction dir as minimal and maximal values of data a
	inline void SetRange(char dir, const mglData &dat, bool add=false)
	{	mgl_set_range_dat(gr, dir, &dat, add);	}
	/// Set values of axis range as minimal and maximal values of corresponding data
	inline void SetRanges(const mglData &xx, const mglData &yy, const mglData &zz, const mglData &cc)
	{	mgl_set_range_dat(gr,'x',&xx,0);	mgl_set_range_dat(gr,'y',&yy,0);
		mgl_set_range_dat(gr,'z',&zz,0);	mgl_set_range_dat(gr,'c',&cc,0);	}
	/// Set values of axis range as minimal and maximal values of corresponding data
	inline void SetRanges(const mglData &xx, const mglData &yy, const mglData &zz)
	{	mgl_set_range_dat(gr,'x',&xx,0);	mgl_set_range_dat(gr,'y',&yy,0);
		mgl_set_range_dat(gr,'z',&zz,0);	mgl_set_range_dat(gr,'c',&zz,0);	}
	/// Set values of axis range as minimal and maximal values of corresponding data
	inline void SetRanges(const mglData &xx, const mglData &yy)
	{	mgl_set_range_dat(gr,'x',&xx,0);	mgl_set_range_dat(gr,'y',&yy,0);	}
	/// Set values of axis ranges
	inline void SetRanges(double x1, double x2, double y1, double y2, double z1=0, double z2=0)
	{	mgl_set_ranges(gr, x1, x2, y1, y2, z1, z2);	}
	/// Set values of axis ranges
	inline void SetRanges(mglPoint p1, mglPoint p2)
	{	mgl_set_ranges(gr, p1.x, p2.x, p1.y, p2.y, p1.z, p2.z);	}
	/// Set ranges for automatic variables
	inline void SetAutoRanges(double x1, double x2, double y1=0, double y2=0, double z1=0, double z2=0, double c1=0, double c2=0)
	{	mgl_set_auto_ranges(gr, x1, x2, y1, y2, z1, z2, c1, c2);	}
	/// Set ranges for automatic variables
	inline void SetAutoRanges(mglPoint p1, mglPoint p2)
	{	mgl_set_auto_ranges(gr, p1.x, p2.x, p1.y, p2.y, p1.z, p2.z, p1.c, p2.c);	}
	/// Set axis origin
	inline void SetOrigin(mglPoint p)
	{	mgl_set_origin(gr, p.x, p.y, p.z);	}
	inline void SetOrigin(double x0, double y0, double z0=NaN)
	{	mgl_set_origin(gr, x0, y0, z0);	}

	/// Set the transformation formulas for coordinate. Use "" for built-in ones
	inline void SetFunc(const char *EqX, const char *EqY, const char *EqZ=NULL, const char *EqA=NULL)
	{	mgl_set_func(gr, EqX, EqY, EqZ, EqA);	}
	/// Set one of predefined transformation rule
	inline void SetCoor(int how)		{	mgl_set_coor(gr, how);	}
	/// Set to draw Ternary axis (triangle like axis, grid and so on)
	/** val=1 for Ternary axis (a+b+c=1, z=z),
	 *  val=2 for Quaternary axis (a+b+c+d=1),
	 *  val|4 for projections. */
	inline void Ternary(int val)		{	mgl_set_ternary(gr, val);	}

	/// Set to use or not tick labels rotation
	inline void SetTickRotate(bool val)	{	mgl_set_tick_rotate(gr,val);	}
	/// Set to use or not tick labels skipping
	inline void SetTickSkip(bool val)	{	mgl_set_tick_skip(gr,val);	}
	/// Set tick length
	inline void SetTickLen(double len, double stt=1)
	{	mgl_set_tick_len(gr, len, stt);	}
	/// Set axis and ticks style
	inline void SetAxisStl(const char *stl="k", const char *tck=0, const char *sub=0)
	{	mgl_set_axis_stl(gr, stl, tck, sub);	}

	/// Set time templates for ticks
	inline void SetTicksTime(char dir, double d=0, const char *t="")
	{	mgl_set_ticks_time(gr,dir,d,t);	}
	/// Set ticks text (\n separated). Use "" to disable this feature.
	inline void SetTicksVal(char dir, const char *lbl, bool add=false)
	{	mgl_set_ticks_str(gr,dir,lbl,add);	}
	inline void SetTicksVal(char dir, const wchar_t *lbl, bool add=false)
	{	mgl_set_ticks_wcs(gr,dir,lbl,add);	}
	/// Set ticks position and text (\n separated). Use "" to disable this feature.
	inline void SetTicksVal(char dir, const mglData &v, const char *lbl, bool add=false)
	{	mgl_set_ticks_val(gr,dir,&v,lbl,add);	}
	inline void SetTicksVal(char dir, const mglData &v, const wchar_t *lbl, bool add=false)
	{	mgl_set_ticks_valw(gr,dir,&v,lbl,add);	}
	/// Add manual tick at given position. Use "" to disable this feature.
	inline void AddTick(char dir, double val, const char *lbl)
	{	mgl_add_tick(gr,dir,val,lbl);	}
	inline void AddTick(char dir, double val, const wchar_t *lbl)
	{	mgl_add_tickw(gr,dir,val,lbl);	}
	/// Set the ticks parameters and string for its factor
	inline void SetTicks(char dir, double d=0, int ns=0, double org=NaN, const char *factor="")
	{	mgl_set_ticks_fact(gr, dir, d, ns, org, factor);	}
	inline void SetTicks(char dir, double d, int ns, double org, const wchar_t *factor)
	{	mgl_set_ticks_factw(gr, dir, d, ns, org, factor);	}
	/// Auto adjust ticks
	inline void Adjust(const char *dir="xyzc")
	{	mgl_adjust_ticks(gr, dir);	}
	/// Set templates for ticks
	inline void SetTickTempl(char dir, const char *t)
	{	mgl_set_tick_templ(gr,dir,t);	}
	inline void SetTickTempl(char dir, const wchar_t *t)
	{	mgl_set_tick_templw(gr,dir,t);	}
	/// Tune ticks (tune|1 for common multiplier, tune|2 for common component)
	inline void SetTuneTicks(int tune, double fact_pos=1.15)
	{	mgl_tune_ticks(gr, tune, fact_pos);	}
	/// Set additional shift of tick labels
	inline void SetTickShift(mglPoint p)
	{	mgl_set_tick_shift(gr,p.x,p.y,p.z,p.c);	}
	/// Set to use UTC time instead of local time
	inline void SetTimeUTC(bool enable)
	{	mgl_set_flag(gr,enable, MGL_USE_GMTIME);	}
	/// Set to draw tick labels at axis origin
	inline void SetOriginTick(bool enable=true)
	{	mgl_set_flag(gr,!enable, MGL_NO_ORIGIN);	}
	/// Set bit-value flag of HMGL state (for advanced users only)
	inline void SetFlagAdv(int val, uint32_t flag)
	{	mgl_set_flag(gr, val, flag);	}

	/// Put further plotting in m-th cell of nx*ny grid of the image.
	/** String \a style may contain:
	 *  '<' for reserving space at left
	 *  '>' for reserving space at right
	 *  '^' for reserving space at top
	 *  '_' for reserving space at bottom
	 *  '#' for using whole region. */
	inline void SubPlot(int nx,int ny,int m,const char *style="<>_^", double dx=0, double dy=0)
	{	mgl_subplot_d(gr, nx, ny, m, style, dx, dy);	}
	/// Put further plotting in rectangle of dx*dy cells starting from m-th cell of nx*ny grid of the image and shift it by distance {sx,sy}.
	/** String \a style may contain:
	 *  '<' for reserving space at left
	 *  '>' for reserving space at right
	 *  '^' for reserving space at top
	 *  '_' for reserving space at bottom
	 *  '#' for using whole region. */
	inline void MultiPlot(int nx,int ny,int m, int dx, int dy, const char *style="<>_^", double sx=0, double sy=0)
	{	mgl_multiplot_d(gr, nx, ny, m, dx, dy, style, sx, sy);	}
	/// Put further plotting in a region [x1,x2]*[y1,y2] of the image or subplot (x1,x2,y1,y2 in range [0, 1]).
	inline void InPlot(double x1,double x2,double y1,double y2, bool rel=true)
	{	if(rel)	mgl_relplot(gr, x1, x2, y1, y2);
		else	mgl_inplot(gr, x1, x2, y1, y2);	}
	/// Put further plotting in column cell of previous subplot
	inline void ColumnPlot(int num, int ind, double d=0)
	{	mgl_columnplot(gr,num,ind,d);	}
	/// Put further plotting in matrix cell of previous subplot
	inline void GridPlot(int nx, int ny, int ind, double d=0)
	{	mgl_gridplot(gr,nx,ny,ind,d);	}
	/// Put further plotting in cell of stick rotated on angles tet, phi
	inline void StickPlot(int num, int i, double tet, double phi)
	{	mgl_stickplot(gr,num,i,tet,phi);	}
	/// Put further plotting in cell of stick sheared on sx, sy.
	inline void ShearPlot(int num, int i, mreal sx, mreal sy, mreal xd=1, mreal yd=0)
	{	mgl_shearplot(gr,num,i,sx,sy,xd,yd);	}

	/// Set factor of plot size
	inline void SetPlotFactor(double val)
	{	mgl_set_plotfactor(gr,val);	}
	/// Push transformation matrix into stack
	inline void Push()	{	mgl_mat_push(gr);	}
	/// Pop transformation matrix from stack
	inline void Pop()	{	mgl_mat_pop(gr);	}

	/// Add title for current subplot/inplot
	/** Style '#' draw box around the title. */
	inline 	void Title(const char *title,const char *stl="",double size=-2)
	{	mgl_title(gr,title,stl,size);	}
	/// Add title for current subplot/inplot
	/** Style '#' draw box around the title. */
	inline 	void Title(const wchar_t *title,const char *stl="",double size=-2)
	{	mgl_titlew(gr,title,stl,size);	}
	/// Set aspect ratio for further plotting.
	inline void Aspect(double Ax,double Ay,double Az=1)
	{	mgl_aspect(gr, Ax, Ay, Az);		}
	/// Shear a further plotting.
	inline void Shear(double Sx,double Sy)
	{	mgl_shear(gr, Sx, Sy);		}
	/// Rotate a further plotting.
	inline void Rotate(double TetX,double TetZ=0,double TetY=0)
	{	mgl_rotate(gr, TetX, TetZ, TetY);	}
	/// Rotate a further plotting around vector {x,y,z}.
	inline void RotateN(double Tet,double x,double y,double z)
	{	mgl_rotate_vector(gr, Tet, x, y, z);	}
	/// Set perspective (in range [0,1)) for plot. Set to zero for switching off.
	inline void Perspective(double val)
	{	mgl_perspective(gr, val);	}
	/// Set angle of view independently from Rotate().
	inline void View(double TetX,double TetZ=0,double TetY=0)
	{	mgl_view(gr, TetX, TetZ, TetY);	}
	/// Set angle of view independently from Rotate().
	inline void ViewAsRotate(double TetZ,double TetX,double TetY=0)
	{	mgl_view(gr, -TetX, -TetZ, -TetY);	}
	/// Zoom in/out a part of picture (use Zoom(0, 0, 1, 1) for restore default)
	inline void Zoom(double x1, double y1, double x2, double y2)
	{	mgl_zoom(gr, x1, y1, x2, y2);	}

	/// Set size of frame in pixels. Normally this function is called internally.
	inline void SetSize(int width, int height, bool clf=true)
	{	if(clf)	mgl_set_size(gr, width, height);
		else	mgl_scale_size(gr, width, height);	}
	/// Scaling for all further set size calls.
	static inline void SetSizeScl(double scl)	{	mgl_set_size_scl(scl);	}
	/// Set plot quality
	/** qual=0 -- no face drawing (fastest),
	 *  qual=1 -- no color interpolation (fast),
	 *  qual=2 -- high quality (normal),
	 *  qual|4 -- direct bitmap drawing (low memory usage);
	 *  qual|8 for dots drawing instead of primitives (extremely fast). */
	inline void SetQuality(int qual=MGL_DRAW_NORM)	{	mgl_set_quality(gr, qual);	}
	/// Get plot quality
	inline int GetQuality()	{	return mgl_get_quality(gr);	}
	/// Set drawing region for Quality&4
	inline void SetDrawReg(long nx=1, long ny=1, long m=0)	{	mgl_set_draw_reg(gr,nx,ny,m);	}
	/// Start group of objects
	inline void StartGroup(const char *name)	{	mgl_start_group(gr, name);	}
	/// End group of objects
	inline void EndGroup()	{	mgl_end_group(gr);	}
	/// Highlight objects with given id
	inline void Highlight(int id)	{	mgl_highlight(gr, id);	}
	/// Set boundary box for export graphics into 2D file formats.
	/** If x2<0 (y2<0) then full width (height) will be used.
	 *  If x1<0 or y1<0 or x1>=x2|Width or y1>=y2|Height then cropping will be disabled. */
	inline void SetBBox(int x1=0, int y1=0, int x2=-1, int y2=-1)
	{	mgl_set_bbox(gr,x1,y1,x2,y2);	}

	/// Show current image
	inline void ShowImage(const char *viewer, bool keep=0)
	{	mgl_show_image(gr, viewer, keep);	}
	/// Write the frame in file (depending extension, write current frame if fname is empty)
	inline void WriteFrame(const char *fname=0,const char *descr="")
	{	mgl_write_frame(gr, fname, descr);	}
	/// Write the frame in file using JPEG format
	inline void WriteJPEG(const char *fname,const char *descr="")
	{	mgl_write_jpg(gr, fname, descr);	}
	/// Write the frame in file using PNG format with transparency
	inline void WritePNG(const char *fname,const char *descr="", bool alpha=true)
	{	if(alpha)	mgl_write_png(gr, fname, descr);
		else	mgl_write_png_solid(gr, fname, descr);	}
	/// Write the frame in file using BMP format
	inline void WriteBMP(const char *fname,const char *descr="")
	{	mgl_write_bmp(gr, fname, descr);	}
	/// Write the frame in file using BMP format
	inline void WriteTGA(const char *fname,const char *descr="")
	{	mgl_write_tga(gr, fname, descr);	}
	/// Write the frame in file using PostScript format
	inline void WriteEPS(const char *fname,const char *descr="")
	{	mgl_write_eps(gr, fname, descr);	}
	/// Write the frame in file using LaTeX format
	inline void WriteTEX(const char *fname,const char *descr="")
	{	mgl_write_tex(gr, fname, descr);	}
	/// Write the frame in file using PostScript format as bitmap
	inline void WriteBPS(const char *fname,const char *descr="")
	{	mgl_write_bps(gr, fname, descr);	}
	/// Write the frame in file using SVG format
	inline void WriteSVG(const char *fname,const char *descr="")
	{	mgl_write_svg(gr, fname, descr);	}
	/// Write the frame in file using GIF format (only for current frame!)
	inline void WriteGIF(const char *fname,const char *descr="")
	{	mgl_write_gif(gr, fname, descr);	}

	/// Write the frame in file using OBJ format
	inline void WriteOBJ(const char *fname,const char *descr="",bool use_png=true)
	{	mgl_write_obj(gr, fname, descr, use_png);	}
	/// Write the frame in file using OBJ format - Balakin way
	inline void WriteOBJold(const char *fname,const char *descr="",bool use_png=true)
	{	mgl_write_obj_old(gr, fname, descr, use_png);	}
	/// Write the frame in file using XYZ format
	inline void WriteXYZ(const char *fname,const char *descr="")
	{	mgl_write_xyz(gr, fname, descr);	}
	/// Write the frame in file using STL format (faces only)
	inline void WriteSTL(const char *fname,const char *descr="")
	{	mgl_write_stl(gr, fname, descr);	}
	/// Write the frame in file using OFF format
	inline void WriteOFF(const char *fname,const char *descr="", bool colored=false)
	{	mgl_write_off(gr, fname, descr,colored);	}
//	/// Write the frame in file using X3D format
//	inline void WriteX3D(const char *fname,const char *descr="")
//	{	mgl_write_x3d(gr, fname, descr);	}
	/// Write the frame in file using PRC format
	inline void WritePRC(const char *fname,const char *descr="",bool make_pdf=true)
	{	mgl_write_prc(gr, fname, descr, make_pdf);	}
	/// Export in JSON format suitable for later drawing by JavaScript
	inline void WriteJSON(const char *fname,const char *descr="",bool force_z=false)
	{	if(force_z)	mgl_write_json_z(gr, fname, descr);
		else 	mgl_write_json(gr, fname, descr);	}
	/// Return string of JSON data suitable for later drawing by JavaScript
	inline const char *GetJSON()	{	return mgl_get_json(gr);	}

	/// Force preparing the image. It can be useful for OpenGL mode mostly.
	inline void Finish()			{	mgl_finish(gr);	}
	/// Create new frame.
	inline void NewFrame()		{	mgl_new_frame(gr);	}
	/// Finish frame drawing
	inline void EndFrame()		{	mgl_end_frame(gr);	}
	/// Get the number of created frames
	inline int GetNumFrame()	{	return mgl_get_num_frame(gr);	}
	/// Reset frames counter (start it from zero)
	inline void ResetFrames()	{	mgl_reset_frames(gr);	}
	/// Delete primitives for i-th frame (work if MGL_VECT_FRAME is set on)
	inline void DelFrame(int i)	{	mgl_del_frame(gr, i);	}
	/// Get drawing data for i-th frame (work if MGL_VECT_FRAME is set on)
	inline void GetFrame(int i)	{	mgl_get_frame(gr, i);	}
	/// Set drawing data for i-th frame (work if MGL_VECT_FRAME is set on). Work as EndFrame() but don't add frame to GIF image.
	inline void SetFrame(int i)	{	mgl_set_frame(gr, i);	}
	/// Append drawing data from i-th frame (work if MGL_VECT_FRAME is set on)
	inline void ShowFrame(int i){	mgl_show_frame(gr, i);	}
	/// Clear list of primitives for current drawing
	inline void ClearFrame()	{	mgl_clear_frame(gr);	}

	/// Start write frames to cinema using GIF format
	inline void StartGIF(const char *fname, int ms=100)
	{	mgl_start_gif(gr, fname,ms);	}
	/// Stop writing cinema using GIF format
	inline void CloseGIF()		{	mgl_close_gif(gr);	}
	/// Export points and primitives in file using MGLD format
	inline void ExportMGLD(const char *fname, const char *descr=0)
	{	mgl_export_mgld(gr, fname, descr);	}
	/// Import points and primitives from file using MGLD format
	inline void ImportMGLD(const char *fname, bool add=false)
	{	mgl_import_mgld(gr, fname, add);	}

	/// Copy RGB values into array which is allocated by user
	/** Position of element {i,j} is [3*i + 3*Width*j]. */
	inline bool GetRGB(char *imgdata, int imglen)
	{
		long w=mgl_get_width(gr), h=mgl_get_height(gr);
		if(imglen>=3*w*h)	memcpy(imgdata, mgl_get_rgb(gr),3*w*h);
		return imglen>=3*w*h;
	}
	/// Get RGB values of current bitmap
	/** Position of element {i,j} is [3*i + 3*Width*j]. */
	inline const unsigned char *GetRGB()		{	return mgl_get_rgb(gr);	}
	/// Copy RGBA values into array which is allocated by user
	/** Position of element {i,j} is [4*i + 4*Width*j]. */
	inline bool GetRGBA(char *imgdata, int imglen)
	{
		long w=mgl_get_width(gr), h=mgl_get_height(gr);
		if(imglen>=4*w*h)	memcpy(imgdata, mgl_get_rgba(gr),4*w*h);
		return imglen>=4*w*h;
	}
	/// Get RGBA values of current bitmap
	/** Position of element {i,j} is [4*i + 4*Width*j]. */
	inline const unsigned char *GetRGBA()	{	return mgl_get_rgba(gr);	}
	/// Copy BGRN values into array which is allocated by user
	inline bool GetBGRN(unsigned char *imgdata, int imglen)
	{
		long w=mgl_get_width(gr), h=mgl_get_height(gr), i;
		const unsigned char *buf=mgl_get_rgb(gr);
		if(imglen>=4*w*h)	for(i=0;i<w*h;i++)
		{
			imgdata[4*i]   = buf[3*i+2];
			imgdata[4*i+1] = buf[3*i+1];
			imgdata[4*i+2] = buf[3*i];
			imgdata[4*i+3] = 255;
		}
		return imglen>=4*w*h;
	}
	/// Copy RGBA values of background image into array which is allocated by user
	/** Position of element {i,j} is [4*i + 4*Width*j]. */
	inline bool GetBackground(char *imgdata, int imglen)
	{
		long w=mgl_get_width(gr), h=mgl_get_height(gr);
		if(imglen>=4*w*h)	memcpy(imgdata, mgl_get_background(gr),4*w*h);
		return imglen>=4*w*h;
	}
	/// Get RGBA values of background image
	/** Position of element {i,j} is [4*i + 4*Width*j]. */
	inline const unsigned char *GetBackground()	{	return mgl_get_background(gr);	}
	/// Get width of the image
	inline int GetWidth()	{	return mgl_get_width(gr);	}
	/// Get height of the image
	inline int GetHeight()	{	return mgl_get_height(gr);}
	/// Calculate 3D coordinate {x,y,z} for screen point {xs,ys}
	inline mglPoint CalcXYZ(int xs, int ys)
	{
		mreal x,y,z;
		mgl_calc_xyz(gr,xs,ys,&x,&y,&z);
		return mglPoint(x,y,z);
	}
	/// Calculate screen point {xs,ys} for 3D coordinate {x,y,z}
	inline mglPoint CalcScr(mglPoint p)
	{
		int xs,ys;
		mgl_calc_scr(gr,p.x,p.y,p.z,&xs,&ys);
		return mglPoint(xs,ys);
	}
	/// Set object/subplot id
	inline void SetObjId(int id)		{	mgl_set_obj_id(gr,id);	}
	/// Get object id
	inline int GetObjId(long x,long y)	{	return mgl_get_obj_id(gr,x,y);	}
	/// Get subplot id
	inline int GetSplId(long x,long y)	{	return mgl_get_spl_id(gr,x,y);	}
	/// Check if {\a xs,\a ys} is close to active point with accuracy d, and return its position or -1
	inline long IsActive(int xs, int ys, int d=1)	{	return mgl_is_active(gr,xs,ys,d);	}

	/// Combine plots from 2 canvases. Result will be saved into this
	inline void Combine(const mglGraph *g)	{	mgl_combine_gr(gr,g->gr);	}

	/// Clear up the frame and fill background by specified color
	inline void Clf(double r, double g, double b)	{	mgl_clf_rgb(gr, r, g, b);	}
	/// Clear up the frame and fill background by specified color with manual transparency
	inline void Clf(const char *col)	{	mgl_clf_str(gr, col);	}
	/// Clear up the frame and fill background by specified color
	inline void Clf(char col)	{	mgl_clf_chr(gr, col);	}
	/// Clear up the frame
	inline void Clf()	{	mgl_clf(gr);	}
	/// Clear unused points and primitives. Useful only in combination with SetFaceNum().
	inline void ClearUnused()	{	mgl_clear_unused(gr);	}

	/// Load background image
	inline void LoadBackground(const char *fname, double alpha=1)
	{	mgl_load_background(gr,fname,alpha);	}
	/// Force drawing the image and use it as background one
	inline void Rasterize()			{	mgl_rasterize(gr);	}

	/// Draws the point (ball) at position {x,y,z} with color c
	inline void Ball(mglPoint p, char c='r')
	{	char s[3]={'.',c,0};	mgl_mark(gr, p.x, p.y, p.z, s);	}
	/// Draws the mark at position p
	inline void Mark(mglPoint p, const char *mark)
	{	mgl_mark(gr, p.x, p.y, p.z, mark);	}
	/// Draws the line between points by specified pen
	/** Large \a n (for example, n=100) should be used for geodesic line in curved coordinates */
	inline void Line(mglPoint p1, mglPoint p2, const char *pen="B",int n=2)
	{	mgl_line(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, pen, n);	}
	/// Draws the spline curve between points by specified pen
	inline void Curve(mglPoint p1, mglPoint d1, mglPoint p2, mglPoint d2, const char *pen="B", int n=100)
	{	mgl_curve(gr, p1.x, p1.y, p1.z, d1.x, d1.y, d1.z, p2.x, p2.y, p2.z, d2.x, d2.y, d2.z, pen, n);	}
	/// Draws the 3d error box e for point p
	inline void Error(mglPoint p, mglPoint e, const char *pen="k")
	{	mgl_error_box(gr, p.x, p.y, p.z, e.x, e.y, e.z, pen);	}

	/// Draws Lamerey diagram for mapping x_new = f(x_old)
	/** String \a stl may contain: ‘v’ for drawing arrows; ‘~’ for disable 1st segment.
	 *	Option value set the number of segments (default is 20).*/
	inline void Lamerey(double x0, const mglData &f, const char *stl="", const char *opt="")
	{	mgl_lamerey_dat(gr,x0,&f,stl,opt);	}
	inline void Lamerey(double x0, const char *func, const char *stl="", const char *opt="")
	{	mgl_lamerey_str(gr,x0,func,stl,opt);	}
	/// Draws Bifurcation diagram for mapping x_new = f(x_old) in x-axis range
	/** Option value set the number of stationary points (default is 1024).*/
	inline void Bifurcation(double dx, const mglData &f, const char *stl="", const char *opt="")
	{	mgl_bifurcation_dat(gr,dx,&f,stl,opt);	}
	inline void Bifurcation(double dx, const char *func, const char *stl="", const char *opt="")
	{	mgl_bifurcation_str(gr,dx,func,stl,opt);	}

	/// Draws Iris plots for determining cross-dependences of data arrays
	/** NOTE: using the same ranges and empty ids will not draw axis. This will add data to existing Iris plot.
	 * 	Option value set the size of data labels ids, separated by ';'.*/
	inline void Iris(mglData &dats, const char *ids, const char *stl="", const char *opt="")
	{	mgl_iris_1(gr,&dats,ids,stl,opt);	}
	inline void Iris(mglData &dats, const wchar_t *ids, const char *stl="", const char *opt="")
	{	mgl_irisw_1(gr,&dats,ids,stl,opt);	}
	inline void Iris(mglData &dats, mglData &ranges, const char *ids, const char *stl="", const char *opt="")
	{	mgl_iris(gr,&dats,&ranges,ids,stl,opt);	}
	inline void Iris(mglData &dats, mglData &ranges, const wchar_t *ids, const char *stl="", const char *opt="")
	{	mgl_irisw(gr,&dats,&ranges,ids,stl,opt);	}

	/// Draws the face between points with color stl (include interpolation up to 4 colors).
	inline void Face(mglPoint p1, mglPoint p2, mglPoint p3, mglPoint p4, const char *stl="r")
	{	mgl_face(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z, stl);	}
	/// Draws the face in y-z plane at point p with color stl (include interpolation up to 4 colors).
	inline void FaceX(mglPoint p, double wy, double wz, const char *stl="w", double dx=0, double dy=0)
	{	mgl_facex(gr, p.x, p.y, p.z, wy, wz, stl, dx, dy);	}
	/// Draws the face in x-z plane at point p with color stl (include interpolation up to 4 colors).
	inline void FaceY(mglPoint p, double wx, double wz, const char *stl="w", double dx=0, double dy=0)
	{	mgl_facey(gr, p.x, p.y, p.z, wx, wz, stl, dx, dy);	}
	/// Draws the face in x-y plane at point p with color stl (include interpolation up to 4 colors).
	inline void FaceZ(mglPoint p, double wx, double wy, const char *stl="w", double dx=0, double dy=0)
	{	mgl_facez(gr, p.x, p.y, p.z, wx, wy, stl, dx, dy);	}
	/// Draws the drop at point p in direction d with color col and radius r
	/** Parameter \a shift set the degree of drop oblongness: ‘0’ is sphere, ‘1’ is maximally oblongness drop. Parameter \a ap set relative width of the drop (this is analogue of “ellipticity” for the sphere).*/
	inline void Drop(mglPoint p, mglPoint d, double r, const char *col="r", double shift=1, double ap=1)
	{	mgl_drop(gr, p.x, p.y, p.z, d.x, d.y, d.z, r, col, shift, ap);	}
	/// Draws the sphere at point p with color col and radius r
	inline void Sphere(mglPoint p, double r, const char *col="r")
	{	mgl_sphere(gr, p.x, p.y, p.z, r, col);	}
	/// Draws the cone between points p1,p2 with radius r1,r2 and with style stl
	/** Parameter \a stl can contain:
	 * ‘@’ for drawing edges;
	 * ‘#’ for wired cones;
	 * ‘t’ for drawing tubes/cylinder instead of cones/prisms;
	 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones.*/
	inline void Cone(mglPoint p1, mglPoint p2, double r1, double r2=-1, const char *stl="r@")
	{	mgl_cone(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,r1,r2,stl);	}
	/// Draws the ellipse between points p1,p2 with color stl and width r
	/** Parameter \a stl can contain:
	 * ‘#’ for wired figure (boundary only);
	 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
	inline void Ellipse(mglPoint p1, mglPoint p2, double r, const char *stl="r")
	{	mgl_ellipse(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,stl);	}
	/// Draws the circle at point p with color stl and radius r
	/** Parameter \a stl can contain:
	 * ‘#’ for wired figure (boundary only);
	 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
	inline void Circle(mglPoint p, double r, const char *stl="r")
	{	mgl_ellipse(gr, p.x, p.y, p.z, p.x, p.y, p.z, r,stl);	}
	/// Draws the rhomb between points p1,p2 with color stl and width r
	/** Parameter \a stl can contain:
	 * ‘#’ for wired figure (boundary only);
	 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
	inline void Rhomb(mglPoint p1, mglPoint p2, double r, const char *stl="r")
	{	mgl_rhomb(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,stl);	}
	/// Draws the polygon based on points p1,p2 with color stl
	/** Parameter \a stl can contain:
	 * ‘#’ for wired figure (boundary only);
	 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
	inline void Polygon(mglPoint p1, mglPoint p2, int n, const char *stl="r")
	{	mgl_polygon(gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, n,stl);	}
	/// Draws the arc around axis pr with center at p0 and starting from p1, by color stl and angle a (in degrees)
	inline void Arc(mglPoint p0, mglPoint pa, mglPoint p1, double a, const char *stl="r")
	{	mgl_arc_ext(gr, p0.x,p0.y,p0.z, pa.x,pa.y,pa.z, p1.x,p1.y,p1.z, a,stl);	}
	/// Draws the arc around axis 'z' with center at p0 and starting from p1, by color stl and angle a (in degrees)
	inline void Arc(mglPoint p0, mglPoint p1, double a, const char *stl="r")
	{	mgl_arc_ext(gr, p0.x,p0.y,p0.z, 0,0,1, p1.x,p1.y,p0.z, a,stl);	}
	/// Draws bitmap (logo) which is stretched along whole axis range
	inline void Logo(long w, long h, const unsigned char *rgba, bool smooth=false, const char *opt="")
	{	mgl_logo(gr, w, h, rgba, smooth, opt);	}
	inline void Logo(const char *fname, bool smooth=false, const char *opt="")
	{	mgl_logo_file(gr, fname, smooth, opt);	}

	/// Draw user-defined symbol in position p
	inline void Symbol(mglPoint p, char id, const char *how="", double size=-1)
	{	mgl_symbol(gr, p.x, p.y, p.z, id, how, size);	}
	/// Draw user-defined symbol in position p along direction d
	inline void Symbol(mglPoint p, mglPoint d, char id, const char *how="", double size=-1)
	{	mgl_symbol_dir(gr, p.x, p.y, p.z, d.x, d.y, d.z, id, how, size);	}
	/// Print text in position p with specified font
	inline void Putsw(mglPoint p,const wchar_t *text,const char *font=":C",double size=-1)
	{	mgl_putsw(gr, p.x, p.y, p.z, text, font, size);	}
	/// Print text in position p with specified font
	inline void Puts(mglPoint p,const char *text,const char *font=":C",double size=-1)
	{	mgl_puts(gr, p.x, p.y, p.z, text, font, size);	}
	/// Print text in position p with specified font
	inline void Putsw(double x, double y,const wchar_t *text,const char *font=":AC",double size=-1)
	{	mgl_putsw(gr, x, y, 0, text, font, size);	}
	/// Print text in position p with specified font
	inline void Puts(double x, double y,const char *text,const char *font=":AC",double size=-1)
	{	mgl_puts(gr, x, y, 0, text, font, size);	}
	/// Print text in position p along direction d with specified font
	inline void Putsw(mglPoint p, mglPoint d, const wchar_t *text, const char *font=":L", double size=-1)
	{	mgl_putsw_dir(gr, p.x, p.y, p.z, d.x, d.y, d.z, text, font, size);	}
	/// Print text in position p along direction d with specified font
	inline void Puts(mglPoint p, mglPoint d, const char *text, const char *font=":L", double size=-1)
	{	mgl_puts_dir(gr, p.x, p.y, p.z, d.x, d.y, d.z, text, font, size);	}

	/// Print text along the curve
	inline void Text(const mglData &x, const mglData &y, const mglData &z, const char *text, const char *font="", const char *opt="")
	{	mgl_text_xyz(gr, &x, &y, &z, text, font, opt);	}
	/// Print text along the curve
	inline void Text(const mglData &x, const mglData &y, const char *text, const char *font="", const char *opt="")
	{	mgl_text_xy(gr, &x, &y, text, font, opt);	}
	/// Print text along the curve
	inline void Text(const mglData &y, const char *text, const char *font="", const char *opt="")
	{	mgl_text_y(gr, &y, text, font, opt);	}
	/// Print text along the curve
	inline void Text(const mglData &x, const mglData &y, const mglData &z, const wchar_t *text, const char *font="", const char *opt="")
	{	mgl_textw_xyz(gr, &x, &y, &z, text, font, opt);	}
	/// Print text along the curve
	inline void Text(const mglData &x, const mglData &y, const wchar_t *text, const char *font="", const char *opt="")
	{	mgl_textw_xy(gr, &x, &y, text, font, opt);	}
	/// Print text along the curve
	inline void Text(const mglData &y, const wchar_t *text, const char *font="", const char *opt="")
	{	mgl_textw_y(gr, &y, text, font, opt);	}

	/// Draws bounding box outside the plotting volume with color c.
	/** Style ‘@’ produce filled back faces. */
	inline void Box(const char *col="", bool ticks=true)
	{	mgl_box_str(gr, col, ticks);	}
	/// Draw axises with ticks in direction(s) dir.
	/** Parameter \a dir may contain:
	 *	‘xyzt’for drawing axis in corresponding direction;
	 *	‘XYZT’ for drawing axis in corresponding direction but with inverted positions of labels;
	 *	‘~’, ‘_’ for disabling tick labels;
	 *	‘U’ for disabling rotation of tick labels;
	 *	‘^’ for inverting default axis origin;
	 *	‘!’ for disabling ticks tuning;
	 *	‘AKDTVISO’ for drawing arrow at the end of axis;
	 *	‘a’ for forced adjusting of axis ticks;
	 *	‘f’ for printing ticks labels in fixed format;
	 *	‘E’ for using ‘E’ instead of ‘e’ in ticks labels;
	 *	‘F’ for printing ticks labels in LaTeX format;
	 *	‘+’ for printing ‘+’ for positive ticks;
	 *	‘-’ for printing usual ‘-’ in ticks labels;
	 *	‘0123456789’ for precision at printing ticks labels.
	 *	 Option "value" set the manual rotation angle for the ticks. */
	inline void Axis(const char *dir="xyzt", const char *stl="", const char *opt="")
	{	mgl_axis(gr, dir,stl,opt);	}
	/// Draw grid lines perpendicular to direction(s) dir.
	inline void Grid(const char *dir="xyzt",const char *pen="B", const char *opt="")
	{	mgl_axis_grid(gr, dir, pen, opt);	}
	/// Print the label text for axis dir.
	/** Option "value" set additional shifting of the label. */
	inline void Label(char dir, const char *text, double pos=+1, const char *opt="")
	{	mgl_label(gr, dir, text, pos, opt);	}
	/// Print the label text for axis dir.
	/** Option "value" set additional shifting of the label. */
	inline void Label(char dir, const wchar_t *text, double pos=+1, const char *opt="")
	{	mgl_labelw(gr, dir, text, pos, opt);	}

	/// Draw colorbar at edge of axis
	/** Parameter \a sch may contain:
	 *	 ‘<>^_’ for positioning at left, at right, at top or at bottom correspondingly;
	 *	 ‘I’ for positioning near bounding (by default, at edges of subplot);
	 *	 ‘A’ for using absolute coordinates;
	 *	 ‘~’ for disabling tick labels.
	 *	 ‘!’ for disabling ticks tuning;
	 *	 ‘f’ for printing ticks labels in fixed format;
	 *	 ‘E’ for using ‘E’ instead of ‘e’ in ticks labels;
	 *	 ‘F’ for printing ticks labels in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive ticks;
	 *	 ‘-’ for printing usual ‘-’ in ticks labels;
	 *	 ‘0123456789’ for precision at printing ticks labels.*/
	inline void Colorbar(const char *sch="")
	{	mgl_colorbar(gr, sch);	}
	/// Draw colorbar at manual position
	/** Parameter \a sch may contain:
	 *	 ‘<>^_’ for positioning at left, at right, at top or at bottom correspondingly;
	 *	 ‘I’ for positioning near bounding (by default, at edges of subplot);
	 *	 ‘A’ for using absolute coordinates;
	 *	 ‘~’ for disabling tick labels.
	 *	 ‘!’ for disabling ticks tuning;
	 *	 ‘f’ for printing ticks labels in fixed format;
	 *	 ‘E’ for using ‘E’ instead of ‘e’ in ticks labels;
	 *	 ‘F’ for printing ticks labels in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive ticks;
	 *	 ‘-’ for printing usual ‘-’ in ticks labels;
	 *	 ‘0123456789’ for precision at printing ticks labels.*/
	inline void Colorbar(const char *sch,double x,double y,double w=1,double h=1)
	{	mgl_colorbar_ext(gr, sch, x,y,w,h);	}
	/// Draw colorbar with manual colors at edge of axis
	/** Parameter \a sch may contain:
	 *	 ‘<>^_’ for positioning at left, at right, at top or at bottom correspondingly;
	 *	 ‘I’ for positioning near bounding (by default, at edges of subplot);
	 *	 ‘A’ for using absolute coordinates;
	 *	 ‘~’ for disabling tick labels.
	 *	 ‘!’ for disabling ticks tuning;
	 *	 ‘f’ for printing ticks labels in fixed format;
	 *	 ‘E’ for using ‘E’ instead of ‘e’ in ticks labels;
	 *	 ‘F’ for printing ticks labels in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive ticks;
	 *	 ‘-’ for printing usual ‘-’ in ticks labels;
	 *	 ‘0123456789’ for precision at printing ticks labels.*/
	inline void Colorbar(const mglData &val, const char *sch="")
	{	mgl_colorbar_val(gr, &val, sch);	}
	/// Draw colorbar with manual colors at manual position
	/** Parameter \a sch may contain:
	 *	 ‘<>^_’ for positioning at left, at right, at top or at bottom correspondingly;
	 *	 ‘I’ for positioning near bounding (by default, at edges of subplot);
	 *	 ‘A’ for using absolute coordinates;
	 *	 ‘~’ for disabling tick labels.
	 *	 ‘!’ for disabling ticks tuning;
	 *	 ‘f’ for printing ticks labels in fixed format;
	 *	 ‘E’ for using ‘E’ instead of ‘e’ in ticks labels;
	 *	 ‘F’ for printing ticks labels in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive ticks;
	 *	 ‘-’ for printing usual ‘-’ in ticks labels;
	 *	 ‘0123456789’ for precision at printing ticks labels.*/
	inline void Colorbar(const mglData &val, const char *sch,double x,double y,double w=1,double h=1)
	{	mgl_colorbar_val_ext(gr, &val, sch, x,y,w,h);	}

	/// Add string to legend
	inline void AddLegend(const char *text,const char *style)
	{	mgl_add_legend(gr, text, style);	}
	inline void AddLegend(const wchar_t *text,const char *style)
	{	mgl_add_legendw(gr, text, style);	}
	/// Clear saved legend string
	inline void ClearLegend()
	{	mgl_clear_legend(gr);	}
	/// Draw legend of accumulated strings at position {x,y}
	/** Parameter fnt may contain:
	 *	 font style for legend text;
	 *	 colors for background (first one), border (second one) and text (last one);
	 *	 ‘A’ for positioning in absolute coordinates;
	 *	 ‘^’ for positioning outside of specified point;
	 *	 ‘-’ for arranging entries horizontally;
	 *	 ‘#’ for drawing box around legend.
	 * Option value set the space between line samples and text (default is 0.1).*/
	inline void Legend(double x, double y, const char *font="#", const char *opt="")
	{	mgl_legend_pos(gr, x, y, font, opt);	}
	/// Draw legend of accumulated strings
	/** Parameter fnt may contain:
	 *	 font style for legend text;
	 *	 colors for background (first one), border (second one) and text (last one);
	 *	 ‘A’ for positioning in absolute coordinates;
	 *	 ‘^’ for positioning outside of specified point;
	 *	 ‘-’ for arranging entries horizontally;
	 *	 ‘#’ for drawing box around legend.
	 * Option value set the space between line samples and text (default is 0.1).
	 * Parameter \a where sets position: 0 at bottom-left, 1 at bottom-right, 2 at top-left, 3 at top-right (default).*/
	inline void Legend(int where=3, const char *font="#", const char *opt="")
	{	mgl_legend(gr, where, font, opt);	}
	/// Set number of marks in legend sample
	inline void SetLegendMarks(int num)		{	mgl_set_legend_marks(gr, num);	}

	/// Draw usual curve {x,y,z}
	inline void Plot(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_plot_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw usual curve {x,y}
	inline void Plot(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_plot_xy(gr, &x, &y, pen,opt);	}
	/// Draw usual curve {x,y} with x in x-axis range
	inline void Plot(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_plot(gr, &y, pen,opt);	}
	/// Draw tapes which rotates as (bi-)normales of curve {x,y,z}
	/** The width of tape is proportional to barwidth and can be changed by option "value".*/
	inline void Tape(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_tape_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw tapes which rotates as (bi-)normales of curve {x,y}
	/** The width of tape is proportional to barwidth and can be changed by option "value".*/
	inline void Tape(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_tape_xy(gr, &x, &y, pen,opt);	}
	/// Draw tapes which rotates as (bi-)normales of curve {x,y} with x in x-axis range
	/** The width of tape is proportional to barwidth and can be changed by option "value".*/
	inline void Tape(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_tape(gr, &y, pen,opt);	}
	/// Draw radar chart (plot in curved coordinates)
	/** Option "value" set the additional shift of data (i.e. the data a+value is used instead of a).*/
	inline void Radar(const mglData &a, const char *pen="", const char *opt="")
	{	mgl_radar(gr, &a, pen, opt);	}

	/// Draw stairs for points in arrays {x,y,z}
	inline void Step(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_step_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw stairs for points in arrays {x,y}
	inline void Step(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_step_xy(gr, &x, &y, pen, opt);	}
	/// Draw stairs for points in arrays {x,y} with x in x-axis range
	inline void Step(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_step(gr, &y, pen, opt);	}

	/// Draw curve {x,y,z} which is colored by c (like tension plot)
	inline void Tens(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *pen="", const char *opt="")
	{	mgl_tens_xyz(gr, &x, &y, &z, &c, pen, opt);	}
	/// Draw curve {x,y} which is colored by c (like tension plot)
	inline void Tens(const mglData &x, const mglData &y, const mglData &c, const char *pen="", const char *opt="")
	{	mgl_tens_xy(gr, &x, &y, &c, pen, opt);	}
	/// Draw curve {x,y} with x in x-axis range which is colored by c (like tension plot)
	inline void Tens(const mglData &y, const mglData &c, const char *pen="", const char *opt="")
	{	mgl_tens(gr, &y, &c, pen, opt);	}

	/// Fill area between curve {x,y,z} and axis plane
	/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Area(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_area_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Fill area between curve {x,y} and axis plane
	/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Area(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_area_xy(gr, &x, &y, pen, opt);	}
	/// Fill area between curve {x,y} with x in x-axis range and axis plane
	/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Area(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_area(gr, &y, pen, opt);	}

	/// Fill area between curves {x,y1} and {x,y2} with x in x-axis range
	/** Style 'i' will fill area only if y1 < y2.
	  * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Region(const mglData &y1, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_region(gr, &y1, &y2, pen, opt);	}
	/// Fill area between curves {x,y1} and {x,y2}
	/** Style 'i' will fill area only if y1 < y2.
	  * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Region(const mglData &x, const mglData &y1, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_region_xy(gr, &x, &y1, &y2, pen, opt);	}
	/// Fill area (draw ribbon) between curves {x1,y1,z1} and {x2,y2,z2}
	/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Region(const mglData &x1, const mglData &y1, const mglData &z1, const mglData &x2, const mglData &y2, const mglData &z2, const char *pen="", const char *opt="")
	{	mgl_region_3d(gr, &x1, &y1, &z1, &x2, &y2, &z2, pen, opt);	}
	/// Fill area (draw ribbon) between curves {x1,y1} and {x2,y2}
	/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Region(const mglData &x1, const mglData &y1, const mglData &x2, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_region_3d(gr, &x1, &y1, NULL, &x2, &y2, NULL, pen, opt);	}

	/// Draw vertical lines from points {x,y,z} to axis plane
	inline void Stem(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_stem_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw vertical lines from points {x,y} to axis plane
	inline void Stem(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_stem_xy(gr, &x, &y, pen, opt);	}
	/// Draw vertical lines from points {x,y} with x in x-axis range to axis plane
	inline void Stem(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_stem(gr, &y, pen, opt);	}

	/// Draw vertical bars from points {x,y,z} to axis plane
	/** String \a pen may contain:
	 * ‘a’ for drawing boxes one above another (like summation);
	 * ‘f’ for waterfall chart;
	 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Bars(const mglData &x, const mglData &y, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_bars_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw vertical bars from points {x,y} to axis plane
	/** String \a pen may contain:
	 * ‘a’ for drawing boxes one above another (like summation);
	 * ‘f’ for waterfall chart;
	 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Bars(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_bars_xy(gr, &x, &y, pen, opt);	}
	/// Draw vertical bars from points {x,y} with x in x-axis range to axis plane
	/** String \a pen may contain:
	 * ‘a’ for drawing boxes one above another (like summation);
	 * ‘f’ for waterfall chart;
	 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Bars(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_bars(gr, &y, pen, opt);	}
	/// Draw horizontal bars from points {x,y} to axis plane
	/** String \a pen may contain:
	 * ‘a’ for drawing boxes one above another (like summation);
	 * ‘f’ for waterfall chart;
	 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Barh(const mglData &y, const mglData &v, const char *pen="", const char *opt="")
	{	mgl_barh_yx(gr, &y, &v, pen, opt);	}
	/// Draw horizontal bars from points {x,y} with y in y-axis range to axis plane
	/** String \a pen may contain:
	 * ‘a’ for drawing boxes one above another (like summation);
	 * ‘f’ for waterfall chart;
	 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Barh(const mglData &v, const char *pen="", const char *opt="")
	{	mgl_barh(gr, &v, pen, opt);	}
	/// Draw chart for data a
	/** Space denote transparent color. Style '#' draw black borders. */
	inline void Chart(const mglData &a, const char *colors="", const char *opt="")
	{	mgl_chart(gr, &a, colors,opt);	}

	/// Draw Open-High-Low-Close (OHLC) diagram
	/**  Different colors for up and down values are used if number of specified colors is equal to 2*number of curves. */
	inline void OHLC(const mglData &x, const mglData &open, const mglData &high, const mglData &low, const mglData &close, const char *pen="", const char *opt="")
	{	mgl_ohlc_x(gr, &x, &open,&high,&low,&close,pen,opt);	}
	/// Draw Open-High-Low-Close (OHLC) diagram with x in x-axis range
	/**  Different colors for up and down values are used if number of specified colors is equal to 2*number of curves. */
	inline void OHLC(const mglData &open, const mglData &high, const mglData &low, const mglData &close, const char *pen="", const char *opt="")
	{	mgl_ohlc(gr, &open,&high,&low,&close,pen,opt);	}

	/// Draw box-plot (special 5-value plot used in statistic)
	/** String \a pen may contain ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.*/
	inline void BoxPlot(const mglData &x, const mglData &y, const char *pen="", const char *opt="")
	{	mgl_boxplot_xy(gr, &x, &y, pen,opt);	}
	/// Draw box-plot (special 5-value plot used in statistic) with x in x-axis range
	/** String \a pen may contain ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.*/
	inline void BoxPlot(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_boxplot(gr, &y, pen,opt);	}

	/// Draw candle plot
	/** Different colors are used for up and down values if 2 colors are specified.
	 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
	inline void Candle(const mglData &x, const mglData &v1, const mglData &v2, const mglData &y1, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_candle_xyv(gr, &x, &v1, &v2, &y1, &y2, pen, opt);	}
	/// Draw candle plot with x in x-axis range
	/** Different colors are used for up and down values if 2 colors are specified.
	 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
	inline void Candle(const mglData &v1, const mglData &v2, const mglData &y1, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_candle_yv(gr, &v1, &v2, &y1, &y2, pen, opt);	}
	inline void Candle(const mglData &v1, const mglData &v2, const char *pen="", const char *opt="")
	{	mgl_candle_yv(gr, &v1, &v2, NULL, NULL, pen, opt);	}
	/// Draw candle plot with v1=v[i], v2=v[i+1]
	/** Different colors are used for up and down values if 2 colors are specified.
	 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
	inline void Candle(const mglData &y, const mglData &y1, const mglData &y2, const char *pen="", const char *opt="")
	{	mgl_candle(gr, &y, &y1, &y2, pen, opt);	}
	/// Draw candle plot with v1=v[i], v2=v[i+1]
	/** Different colors are used for up and down values if 2 colors are specified.
	 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
	inline void Candle(const mglData &y, const char *pen="", const char *opt="")
	{	mgl_candle(gr, &y, NULL, NULL, pen, opt);	}

	/// Draw cones from points {x,y,z} to axis plane
	/** String \a pen may contain:
	 * ‘@’ for drawing edges;
	 * ‘#’ for wired cones;
	 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
	 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
	 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Cones(const mglData &x, const mglData &y, const mglData &z, const char *pen="@", const char *opt="")
	{	mgl_cones_xyz(gr, &x, &y, &z, pen, opt);	}
	/// Draw cones from points {x,z} to axis plane
	/** String \a pen may contain:
	 * ‘@’ for drawing edges;
	 * ‘#’ for wired cones;
	 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
	 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
	 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Cones(const mglData &x, const mglData &z, const char *pen="@", const char *opt="")
	{	mgl_cones_xz(gr, &x, &z, pen, opt);	}
	/// Draw cones from points {x,z} with x in x-axis range to axis plane
	/** String \a pen may contain:
	 * ‘@’ for drawing edges;
	 * ‘#’ for wired cones;
	 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
	 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
	 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
	 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
	inline void Cones(const mglData &z, const char *pen="@", const char *opt="")
	{	mgl_cones(gr, &z, pen, opt);	}

	/// Draw error boxes {ey} at points {x,y} with x in x-axis range
	/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
	inline void Error(const mglData &y, const mglData &ey, const char *pen="", const char *opt="")
	{	mgl_error(gr, &y, &ey, pen, opt);	}
	/// Draw error boxes {ey} at points {x,y}
	/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
	inline void Error(const mglData &x, const mglData &y, const mglData &ey, const char *pen="", const char *opt="")
	{	mgl_error_xy(gr, &x, &y, &ey, pen, opt);	}
	/// Draw error boxes {ex,ey} at points {x,y}
	/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
	inline void Error(const mglData &x, const mglData &y, const mglData &ex, const mglData &ey, const char *pen="", const char *opt="")
	{	mgl_error_exy(gr, &x, &y, &ex, &ey, pen, opt);	}

	/// Draw marks with size r at points {x,y,z}
	inline void Mark(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const char *pen, const char *opt="")
	{	mgl_mark_xyz(gr, &x, &y, &z, &r, pen, opt);	}
	/// Draw marks with size r at points {x,y}
	inline void Mark(const mglData &x, const mglData &y, const mglData &r, const char *pen, const char *opt="")
	{	mgl_mark_xy(gr, &x, &y, &r, pen, opt);	}
	/// Draw marks with size r at points {x,y} with x in x-axis range
	inline void Mark(const mglData &y, const mglData &r, const char *pen, const char *opt="")
	{	mgl_mark_y(gr, &y, &r, pen, opt);	}

	/// Draw Poincare map at condition s==0 for curve {x,y,z}
	inline void Pmap(const mglData &x, const mglData &y, const mglData &z, const mglData &s, const char *pen, const char *opt="")
	{	mgl_pmap_xyz(gr, &x, &y, &z, &s, pen, opt);	}
	/// Draw Poincare map at condition s==0 for curve {x,y}
	inline void Pmap(const mglData &x, const mglData &y, const mglData &s, const char *pen, const char *opt="")
	{	mgl_pmap_xy(gr, &x, &y, &s, pen, opt);	}
	/// Draw Poincare map at condition s==0 for curve {x,y} with x in x-axis range
	inline void Pmap(const mglData &y, const mglData &s, const char *pen, const char *opt="")
	{	mgl_pmap(gr, &y, &s, pen, opt);	}

	/// Draw textual marks with size r at points {x,y,z}
	inline void TextMark(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const char *text, const char *fnt="", const char *opt="")
	{	mgl_textmark_xyzr(gr, &x, &y, &z, &r, text, fnt, opt);	}
	/// Draw textual marks with size r at points {x,y}
	inline void TextMark(const mglData &x, const mglData &y, const mglData &r, const char *text, const char *fnt="", const char *opt="")
	{	mgl_textmark_xyr(gr, &x, &y, &r, text, fnt, opt);	}
	/// Draw textual marks with size r at points {x,y} with x in x-axis range
	inline void TextMark(const mglData &y, const mglData &r, const char *text, const char *fnt="", const char *opt="")
	{	mgl_textmark_yr(gr, &y, &r, text, fnt, opt);	}
	/// Draw textual marks at points {x,y} with x in x-axis range
	inline void TextMark(const mglData &y, const char *text, const char *fnt="", const char *opt="")
	{	mgl_textmark(gr, &y, text, fnt, opt);	}
	/// Draw textual marks with size r at points {x,y,z}
	inline void TextMark(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_textmarkw_xyzr(gr, &x, &y, &z, &r, text, fnt, opt);	}
	/// Draw textual marks with size r at points {x,y}
	inline void TextMark(const mglData &x, const mglData &y, const mglData &r, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_textmarkw_xyr(gr, &x, &y, &r, text, fnt, opt);	}
	/// Draw textual marks with size r at points {x,y} with x in x-axis range
	inline void TextMark(const mglData &y, const mglData &r, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_textmarkw_yr(gr, &y, &r, text, fnt, opt);	}
	/// Draw textual marks at points {x,y} with x in x-axis range
	inline void TextMark(const mglData &y, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_textmarkw(gr, &y, text, fnt, opt);	}

	/// Draw labels for points coordinate(s) at points {x,y,z}
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &x, const mglData &y, const mglData &z, const char *text, const char *fnt="", const char *opt="")
	{	mgl_label_xyz(gr, &x, &y, &z, text, fnt, opt);	}
	/// Draw labels for points coordinate(s) at points {x,y}
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &x, const mglData &y, const char *text, const char *fnt="", const char *opt="")
	{	mgl_label_xy(gr, &x, &y, text, fnt, opt);	}
	/// Draw labels for points coordinate(s) at points {x,y} with x in x-axis range
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &y, const char *text, const char *fnt="", const char *opt="")
	{	mgl_label_y(gr, &y, text, fnt, opt);	}
	/// Draw labels for points coordinate(s) at points {x,y,z}
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &x, const mglData &y, const mglData &z, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_labelw_xyz(gr, &x, &y, &z, text, fnt, opt);	}
	/// Draw labels for points coordinate(s) at points {x,y}
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &x, const mglData &y, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_labelw_xy(gr, &x, &y, text, fnt, opt);	}
	/// Draw labels for points coordinate(s) at points {x,y} with x in x-axis range
	/** String \a fnt may contain:
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.*/
	inline void Label(const mglData &y, const wchar_t *text, const char *fnt="", const char *opt="")
	{	mgl_labelw_y(gr, &y, text, fnt, opt);	}

	/// Draw table for values val along given direction with row labels text
	/** String \a fnt may contain:
	 *	 ‘#’ for drawing cell borders;
	 *	 ‘|’ for limiting table widh by subplot one (equal to option ‘value 1’);
	 *	 ‘=’ for equal width of all cells;
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.
	 * Option value set the width of the table (default is 1).*/
	inline void Table(const mglData &val, const char *text, const char *fnt="#|", const char *opt="")
	{	mgl_table(gr, 0, 0, &val, text, fnt, opt);	}
	/// Draw table for values val along given direction with row labels text
	/** String \a fnt may contain:
	 *	 ‘#’ for drawing cell borders;
	 *	 ‘|’ for limiting table widh by subplot one (equal to option ‘value 1’);
	 *	 ‘=’ for equal width of all cells;
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.
	 * Option value set the width of the table (default is 1).*/
	inline void Table(const mglData &val, const wchar_t *text, const char *fnt="#|", const char *opt="")
	{	mgl_tablew(gr, 0, 0, &val, text, fnt, opt);	}
	/// Draw table for values val along given direction with row labels text at given position
	/** String \a fnt may contain:
	 *	 ‘#’ for drawing cell borders;
	 *	 ‘|’ for limiting table widh by subplot one (equal to option ‘value 1’);
	 *	 ‘=’ for equal width of all cells;
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.
	 * Option value set the width of the table (default is 1).*/
	inline void Table(double x, double y, const mglData &val, const char *text, const char *fnt="#|", const char *opt="")
	{	mgl_table(gr, x, y, &val, text, fnt, opt);	}
	/// Draw table for values val along given direction with row labels text at given position
	/** String \a fnt may contain:
	 *	 ‘#’ for drawing cell borders;
	 *	 ‘|’ for limiting table widh by subplot one (equal to option ‘value 1’);
	 *	 ‘=’ for equal width of all cells;
	 *	 ‘f’ for fixed format of printed numbers;
	 *	 ‘E’ for using ‘E’ instead of ‘e’;
	 *	 ‘F’ for printing in LaTeX format;
	 *	 ‘+’ for printing ‘+’ for positive numbers;
	 *	 ‘-’ for printing usual ‘-’;
	 *	 ‘0123456789’ for precision at printing numbers.
	 * Option value set the width of the table (default is 1).*/
	inline void Table(double x, double y, const mglData &val, const wchar_t *text, const char *fnt="#|", const char *opt="")
	{	mgl_tablew(gr, x, y, &val, text, fnt, opt);	}

	/// Draw tube with radius r around curve {x,y,z}
	inline void Tube(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const char *pen="", const char *opt="")
	{	mgl_tube_xyzr(gr, &x, &y, &z, &r, pen, opt);	}
	/// Draw tube with radius r around curve {x,y,z}
	inline void Tube(const mglData &x, const mglData &y, const mglData &z, double r, const char *pen="", const char *opt="")
	{	mgl_tube_xyz(gr, &x, &y, &z, r, pen, opt);	}
	/// Draw tube with radius r around curve {x,y}
	inline void Tube(const mglData &x, const mglData &y, const mglData &r, const char *pen="", const char *opt="")
	{	mgl_tube_xyr(gr, &x, &y, &r, pen, opt);	}
	/// Draw tube with radius r around curve {x,y}
	inline void Tube(const mglData &x, const mglData &y, double r, const char *pen="", const char *opt="")
	{	mgl_tube_xy(gr, &x, &y, r, pen, opt);	}
	/// Draw tube with radius r around curve {x,y} with x in x-axis range
	inline void Tube(const mglData &y, const mglData &r, const char *pen="", const char *opt="")
	{	mgl_tube_r(gr, &y, &r, pen, opt);	}
	/// Draw tube with radius r around curve {x,y} with x in x-axis range
	inline void Tube(const mglData &y, double r, const char *pen="", const char *opt="")
	{	mgl_tube(gr, &y, r, pen, opt);	}
	/// Draw surface of curve {r,z} rotation around axis
	/** Style ‘#’ produce wire plot. Style ‘.’ produce plot by dots.*/
	inline void Torus(const mglData &r, const mglData &z, const char *pen="", const char *opt="")
	{	mgl_torus(gr, &r, &z, pen,opt);	}

	/// Draw mesh lines for 2d data specified parametrically
	inline void Mesh(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_mesh_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw mesh lines for 2d data
	inline void Mesh(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_mesh(gr, &z, stl, opt);	}

	/// Draw waterfall plot for 2d data specified parametrically
	/** Style 'x' draw lines in x-direction. */
	inline void Fall(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_fall_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw waterfall plot for 2d data
	/** Style 'x' draw lines in x-direction. */
	inline void Fall(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_fall(gr, &z, stl, opt);	}

	/// Draw belts for 2d data specified parametrically
	/** Style 'x' draw belts in x-direction. */
	inline void Belt(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_belt_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw belts for 2d data
	/** Style 'x' draw belts in x-direction. */
	inline void Belt(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_belt(gr, &z, stl, opt);	}
	/// Draw belts for 2d data specified parametrically with color proportional to c
	/** Style 'x' draw belts in x-direction. */
	inline void BeltC(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_beltc_xy(gr, &x, &y, &z, &c, stl, opt);	}
	/// Draw belts for 2d data with color proportional to c
	/** Style 'x' draw belts in x-direction. */
	inline void BeltC(const mglData &z, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_beltc(gr, &z, &c, stl, opt);	}

	/// Draw surface for 2d data specified parametrically with color proportional to z
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void Surf(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_surf_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw surface for 2d data with color proportional to z
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void Surf(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_surf(gr, &z, stl, opt);	}

	/// Draw grid lines for density plot of 2d data specified parametrically
	inline void Grid(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_grid_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw grid lines for density plot of 2d data
	inline void Grid(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_grid(gr, &z, stl, opt);	}

	/// Draw vertical tiles with manual colors c for 2d data specified parametrically
	inline void Tile(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_tile_xyc(gr, &x, &y, &z, &c, stl, opt);	}
	/// Draw vertical tiles for 2d data specified parametrically
	inline void Tile(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_tile_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw vertical tiles for 2d data
	inline void Tile(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_tile(gr, &z, stl, opt);	}

	/// Draw density plot for 2d data specified parametrically
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void Dens(const mglData &x, const mglData &y, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_dens_xy(gr, &x, &y, &c, stl, opt);	}
	/// Draw density plot for 2d data
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void Dens(const mglData &c, const char *stl="", const char *opt="")
	{	mgl_dens(gr, &c, stl, opt);	}

	/// Draw vertical boxes for 2d data specified parametrically
	/** Style ‘#’ draw filled boxes. */
	inline void Boxs(const mglData &x, const mglData &y, const mglData &z, const char *stl="", const char *opt="")
	{	mgl_boxs_xy(gr, &x, &y, &z, stl, opt);	}
	/// Draw vertical boxes for 2d data
	/** Style ‘#’ draw filled boxes. */
	inline void Boxs(const mglData &z, const char *stl="", const char *opt="")
	{	mgl_boxs(gr, &z, stl, opt);	}

	/// Draw contour lines on parametric surface at manual levels for 2d data specified parametrically
	/** Style ‘f’ to draw solid contours.
	 * Style 't'/'T' draw contour labels below/above contours.*/
	inline void ContP(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_contp_val(gr, &v, &x, &y, &z, &a, sch, opt);	}
	/// Draw contour lines on parametric surface at manual levels for 2d data specified parametrically
	/** Style ‘f’ to draw solid contours.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContP(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_contp(gr, &x, &y, &z, &a, sch, opt);	}
	/// Draw contour lines at manual levels for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style 't'/'T' draw contour labels below/above contours.*/
	inline void Cont(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_cont_xy_val(gr, &v, &x, &y, &z, sch, opt);	}
	/// Draw contour lines for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style 't'/'T' draw contour labels below/above contours.*/
	inline void Cont(const mglData &v, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_cont_val(gr, &v, &z, sch, opt);	}
	/// Draw contour lines at manual levels for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void Cont(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_cont_xy(gr, &x, &y, &z, sch, opt);	}
	/// Draw contour lines for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void Cont(const mglData &z, const char *sch="", const char *opt="")
	{	mgl_cont(gr, &z, sch, opt);	}

	/// Draw solid contours at manual levels for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContF(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contf_xy_val(gr, &v, &x, &y, &z, sch, opt);	}
	/// Draw solid contours at manual levels for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContF(const mglData &v, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contf_val(gr, &v, &z, sch, opt);	}
	/// Draw solid contours for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContF(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contf_xy(gr, &x, &y, &z, sch, opt);	}
	/// Draw solid contours for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContF(const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contf(gr, &z, sch, opt);	}

	/// Draw solid contours at manual levels for 2d data specified parametrically with specified colors
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContD(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contd_xy_val(gr, &v, &x, &y, &z, sch, opt);	}
	/// Draw solid contours at manual levels for 2d data with specified colors
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContD(const mglData &v, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contd_val(gr, &v, &z, sch, opt);	}
	/// Draw solid contours for 2d data specified parametrically with specified colors
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContD(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contd_xy(gr, &x, &y, &z, sch, opt);	}
	/// Draw solid contours for 2d data with specified colors
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContD(const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contd(gr, &z, sch, opt);	}

	/// Draw contour tubes between manual levels for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContV(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contv_xy_val(gr, &v, &x, &y, &z, sch, opt);	}
	/// Draw contour tubes between manual levels for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box. */
	inline void ContV(const mglData &v, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contv_val(gr, &v, &z, sch, opt);	}
	/// Draw contour tubes for 2d data specified parametrically
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContV(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contv_xy(gr, &x, &y, &z, sch, opt);	}
	/// Draw contour tubes for 2d data
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContV(const mglData &z, const char *sch="", const char *opt="")
	{	mgl_contv(gr, &z, sch, opt);	}

	/// Draw axial-symmetric isosurfaces at manual levels for 2d data specified parametrically
	/** String \a sch may contain:
	 * ‘#’ for wired plot;
	 * ‘.’ for plot by dots;
	 * ‘x’, ‘z’ for rotation around x-, z-axis correspondingly (default is y-axis). */
	inline void Axial(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_axial_xy_val(gr, &v, &x, &y, &z, sch,opt);	}
	/// Draw axial-symmetric isosurfaces at manual levels for 2d data
	/** String \a sch may contain:
	 * ‘#’ for wired plot;
	 * ‘.’ for plot by dots;
	 * ‘x’, ‘z’ for rotation around x-, z-axis correspondingly (default is y-axis). */
	inline void Axial(const mglData &v, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_axial_val(gr, &v, &z, sch, opt);	}
	/// Draw axial-symmetric isosurfaces for 2d data specified parametrically
	/** String \a sch may contain:
	 * ‘#’ for wired plot;
	 * ‘.’ for plot by dots;
	 * ‘x’, ‘z’ for rotation around x-, z-axis correspondingly (default is y-axis).
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Axial(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_axial_xy(gr, &x, &y, &z, sch, opt);	}
	/// Draw axial-symmetric isosurfaces for 2d data
	/** String \a sch may contain:
	 * ‘#’ for wired plot;
	 * ‘.’ for plot by dots;
	 * ‘x’, ‘z’ for rotation around x-, z-axis correspondingly (default is y-axis).
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Axial(const mglData &z, const char *sch="", const char *opt="")
	{	mgl_axial(gr, &z, sch, opt);	}

	/// Draw grid lines for density plot at slice for 3d data specified parametrically
	/** Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.*/
	inline void Grid3(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_grid3_xyz(gr, &x, &y, &z, &a, stl, sVal, opt);	}
	/// Draw grid lines for density plot at slice for 3d data
	/** Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.*/
	inline void Grid3(const mglData &a, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_grid3(gr, &a, stl, sVal, opt);	}

	/// Draw density plot at slice for 3d data specified parametrically
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.*/
	inline void Dens3(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_dens3_xyz(gr, &x, &y, &z, &a, stl, sVal, opt);	}
	/// Draw density plot at slice for 3d data
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.*/
	inline void Dens3(const mglData &a, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_dens3(gr, &a, stl, sVal, opt);	}

	/// Draw isosurface for 3d data specified parametrically
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.*/
	inline void Surf3(double Val, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *stl="", const char *opt="")
	{	mgl_surf3_xyz_val(gr, Val, &x, &y, &z, &a, stl, opt);	}
	/// Draw isosurface for 3d data
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.*/
	inline void Surf3(double Val, const mglData &a, const char *stl="", const char *opt="")
	{	mgl_surf3_val(gr, Val, &a, stl, opt);	}
	/// Draw isosurfaces for 3d data specified parametrically
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *stl="", const char *opt="")
	{	mgl_surf3_xyz(gr, &x, &y, &z, &a, stl, opt);	}
	/// Draw isosurfaces for 3d data
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3(const mglData &a, const char *stl="", const char *opt="")
	{	mgl_surf3(gr, &a, stl, opt);	}

	/// Draw a semi-transparent cloud for 3d data specified parametrically
	/** Style ‘.’ produce plot by dots. Style ‘i’ use inverted values for transparency. */
	inline void Cloud(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *stl="", const char *opt="")
	{	mgl_cloud_xyz(gr, &x, &y, &z, &a, stl, opt);	}
	/// Draw a semi-transparent cloud for 3d data
	/** Style ‘.’ produce plot by dots. Style ‘i’ use inverted values for transparency. */
	inline void Cloud(const mglData &a, const char *stl="", const char *opt="")
	{	mgl_cloud(gr, &a, stl, opt);	}

	/// Draw contour lines at manual levels along slice for 3d data specified parametrically
	/** Style ‘#’ draw grid lines.
	 * Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 * Style ‘t’/‘T’ draw contour labels below/above contours. */
	inline void Cont3(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_cont3_xyz_val(gr, &v, &x, &y, &z, &a, sch, sVal, opt);	}
	/// Draw contour lines at manual levels along slice for 3d data
	/** Style ‘#’ draw grid lines.
	 * Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 * Style ‘t’/‘T’ draw contour labels below/above contours. */
	inline void Cont3(const mglData &v, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_cont3_val(gr, &v, &a, sch, sVal, opt);	}
	/// Draw contour lines along slice for 3d data specified parametrically
	/** Style ‘#’ draw grid lines.
	 * Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void Cont3(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_cont3_xyz(gr, &x, &y, &z, &a, sch, sVal, opt);	}
	/// Draw contour lines along slice for 3d data
	/** Style ‘#’ draw grid lines.
	 * Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void Cont3(const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_cont3(gr, &a, sch, sVal, opt);	}

	/// Draw solid contours at manual levels along slice for 3d data specified parametrically
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly. */
	inline void ContF3(const mglData &v, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_contf3_xyz_val(gr, &v, &x, &y, &z, &a, sch, sVal, opt);	}
	/// Draw solid contours at manual levels along slice for 3d data
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly. */
	inline void ContF3(const mglData &v, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_contf3_val(gr, &v, &a, sch, sVal, opt);	}
	/// Draw solid contours along slice for 3d data specified parametrically
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 *  Option "value" set the number of contour levels (default is 7).*/
	inline void ContF3(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_contf3_xyz(gr, &x, &y, &z, &a, sch, sVal, opt);	}
	/// Draw solid contours along slice for 3d data
	/** Style ‘#’ draw grid lines. Style ‘x’ or ‘z’ produce plot perpendicular to x- or z-direction correspondingly.
	 *  Option "value" set the number of contour levels (default is 7).*/
	inline void ContF3(const mglData &a, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_contf3(gr, &a, sch, sVal, opt);	}

	/// Draw several isosurfaces for 3d beam in curvilinear coordinates
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.
	 *  Variable \a flag is bitwise:
	 * ‘0x1’ - draw in accompanied (not laboratory) coordinates;
	 * ‘0x2’ - draw projection to \rho-z plane;
	 * ‘0x4’ - draw normalized in each slice field.*/
	inline void Beam(const mglData &tr, const mglData &g1, const mglData &g2, const mglData &a, double r, const char *stl=0, int flag=0, int num=3)
	{	mgl_beam(gr, &tr,&g1,&g2,&a,r,stl,flag,num);	}
	/// Draw isosurface at value \a val for 3d beam in curvilinear coordinates
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.
	 *  Variable \a flag is bitwise:
	 * ‘0x1’ - draw in accompanied (not laboratory) coordinates;
	 * ‘0x2’ - draw projection to \rho-z plane;
	 * ‘0x4’ - draw normalized in each slice field.*/
	inline void Beam(double val, const mglData &tr, const mglData &g1, const mglData &g2, const mglData &a, double r, const char *stl=NULL, int flag=0)
	{	mgl_beam_val(gr,val,&tr,&g1,&g2,&a,r,stl,flag);	}

	/// Draw vertical tiles with variable size r and manual colors c for 2d data specified parametrically
	inline void TileS(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_tiles_xyc(gr, &x, &y, &z, &r, &c, stl, opt);	}
	/// Draw vertical tiles with variable size r for 2d data specified parametrically
	inline void TileS(const mglData &x, const mglData &y, const mglData &z, const mglData &r, const char *stl="", const char *opt="")
	{	mgl_tiles_xy(gr, &x, &y, &z, &r, stl, opt);	}
	/// Draw vertical tiles with variable size r for 2d data
	inline void TileS(const mglData &z, const mglData &r, const char *stl="", const char *opt="")
	{	mgl_tiles(gr, &z, &r, stl, opt);	}

	/// Draw surface for 2d data specified parametrically with color proportional to c
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfC(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_surfc_xy(gr, &x, &y, &z, &c, sch,opt);	}
	/// Draw surface for 2d data with color proportional to c
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfC(const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_surfc(gr, &z, &c, sch,opt);	}

	/// Draw surface for 2d data specified parametrically with alpha proportional to c
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfA(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_surfa_xy(gr, &x, &y, &z, &c, sch,opt);	}
	/// Draw surface for 2d data with alpha proportional to c
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfA(const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_surfa(gr, &z, &c, sch,opt);	}

	/// Draw surface for 2d data specified parametrically with color proportional to c and alpha proportional to a
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfCA(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_surfca_xy(gr, &x, &y, &z, &c, &a, sch,opt);	}
	/// Draw surface for 2d data with color proportional to c and alpha proportional to a
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void SurfCA(const mglData &z, const mglData &c, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_surfca(gr, &z, &c, &a, sch,opt);	}

	/// Color map of matrix a to matrix b, both matrix can parametrically depend on coordinates
	/** Style ‘.’ produce plot by dots. */
	inline void Map(const mglData &x, const mglData &y, const mglData &a, const mglData &b, const char *sch="", const char *opt="")
	{	mgl_map_xy(gr, &x, &y, &a, &b, sch, opt);	}
	/// Color map of matrix a to matrix b
	/** Style ‘.’ produce plot by dots. */
	inline void Map(const mglData &a, const mglData &b, const char *sch="", const char *opt="")
	{	mgl_map(gr, &a, &b, sch, opt);	}

	/// Draw density plot for spectra-gramm specified parametrically
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void STFA(const mglData &x, const mglData &y, const mglData &re, const mglData &im, int dn, const char *sch="", const char *opt="")
	{	mgl_stfa_xy(gr, &x, &y, &re, &im, dn, sch, opt);	}
	/// Draw density plot for spectra-gramm
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void STFA(const mglData &re, const mglData &im, int dn, const char *sch="", const char *opt="")
	{	mgl_stfa(gr, &re, &im, dn, sch, opt);	}

	/// Draw isosurface for 3d data specified parametrically with alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3A(double Val, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3a_xyz_val(gr, Val, &x, &y, &z, &a, &b, stl, opt);	}
	/// Draw isosurface for 3d data with alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3A(double Val, const mglData &a, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3a_val(gr, Val, &a, &b, stl, opt);	}
	/// Draw isosurfaces for 3d data specified parametrically with alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3A(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3a_xyz(gr, &x, &y, &z, &a, &b, stl, opt);	}
	/// Draw isosurfaces for 3d data with alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3A(const mglData &a, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3a(gr, &a, &b, stl, opt);	}

	/// Draw isosurface for 3d data specified parametrically with color proportional to c
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3C(double Val, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_surf3c_xyz_val(gr, Val, &x, &y, &z, &a, &c, stl,opt);	}
	/// Draw isosurface for 3d data with color proportional to c
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3C(double Val, const mglData &a, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_surf3c_val(gr, Val, &a, &c, stl, opt);	}
	/// Draw isosurfaces for 3d data specified parametrically with color proportional to c
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3C(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_surf3c_xyz(gr, &x, &y, &z, &a, &c, stl, opt);	}
	/// Draw isosurfaces for 3d data specified parametrically with color proportional to c
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3C(const mglData &a, const mglData &c, const char *stl="", const char *opt="")
	{	mgl_surf3c(gr, &a, &c, stl, opt);	}

	/// Draw isosurface for 3d data specified parametrically with color proportional to c and alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3CA(double Val, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &c, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3ca_xyz_val(gr, Val, &x, &y, &z, &a, &c, &b, stl,opt);	}
	/// Draw isosurface for 3d data with color proportional to c and alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots. */
	inline void Surf3CA(double Val, const mglData &a, const mglData &c, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3ca_val(gr, Val, &a, &c, &b, stl, opt);	}
	/// Draw isosurfaces for 3d data specified parametrically with color proportional to c and alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3CA(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &c, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3ca_xyz(gr, &x, &y, &z, &a, &c, &b, stl, opt);	}
	/// Draw isosurfaces for 3d data with color proportional to c and alpha proportional to b
	/** Style ‘#’ draw wired plot. Style ‘.’ produce plot by dots.
	 * Option "value" set the number of isosurfaces (default is 3). */
	inline void Surf3CA(const mglData &a, const mglData &c, const mglData &b, const char *stl="", const char *opt="")
	{	mgl_surf3ca(gr, &a, &c, &b, stl, opt);	}

	/// Plot dew drops for vector field {ax,ay} parametrically depended on coordinate {x,y}
	inline void Dew(const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_dew_xy(gr, &x, &y, &ax, &ay, sch, opt);	}
	/// Plot dew drops for vector field {ax,ay}
	inline void Dew(const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_dew_2d(gr, &ax, &ay, sch, opt);	}

	/// Plot vectors at position {x,y} along {ax,ay} with length/color proportional to |a|
	/** Option value set the vector length factor (if non-zero) or vector length to be proportional the distance between curve points (if value=0). */
	inline void Traj(const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_traj_xy(gr, &x, &y, &ax, &ay, sch, opt);	}
	/// Plot vectors at position {x,y,z} along {ax,ay,az} with length/color proportional to |a|
	/** Option value set the vector length factor (if non-zero) or vector length to be proportional the distance between curve points (if value=0). */
	inline void Traj(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_traj_xyz(gr, &x, &y, &z, &ax, &ay, &az, sch, opt);	}

	/// Plot vector field {ax,ay} parametrically depended on coordinate {x,y} with length/color proportional to |a|
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows. */
	inline void Vect(const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_vect_xy(gr, &x, &y, &ax, &ay, sch, opt);	}
	/// Plot vector field {ax,ay} with length/color proportional to |a|
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows. */
	inline void Vect(const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_vect_2d(gr, &ax, &ay, sch, opt);	}
	/// Plot vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with length/color proportional to |a|
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows. */
	inline void Vect(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_vect_xyz(gr, &x, &y, &z, &ax, &ay, &az, sch, opt);	}
	/// Plot vector field {ax,ay,az} with length/color proportional to |a|
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows. */
	inline void Vect(const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_vect_3d(gr, &ax, &ay, &az, sch, opt);	}

	/// Draw vector plot along slice for 3d data specified parametrically
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows,
	 * ‘ x’, ‘z’ for producing plot perpendicular to x- or z-direction correspondingly. */
	inline void Vect3(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_vect3_xyz(gr, &x, &y, &z, &ax,&ay,&az, stl, sVal, opt);	}
	/// Draw vector plot along slice for 3d data
	/** String \a sch may contain:
	 * ‘f’ for drawing arrows with fixed lengths,
	 * ‘ >’, ‘<’ for drawing arrows to or from the ce*ll point (default is centering),
	 * ‘.’ for drawing hachures with dots instead of arrows,
	 * ‘=’ for enabling color gradient along arrows,
	 * ‘ x’, ‘z’ for producing plot perpendicular to x- or z-direction correspondingly. */
	inline void Vect3(const mglData &ax, const mglData &ay, const mglData &az, const char *stl="", double sVal=-1, const char *opt="")
	{	mgl_vect3(gr, &ax,&ay,&az, stl, sVal, opt);	}

	/// Plot flows for vector field {ax,ay} parametrically depended on coordinate {x,y} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Flow(const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_flow_xy(gr, &x, &y, &ax, &ay, sch, opt);	}
	/// Plot flows for vector field {ax,ay} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Flow(const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_flow_2d(gr, &ax, &ay, sch, opt);	}
	/// Plot flows for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Flow(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_flow_xyz(gr, &x, &y, &z, &ax, &ay, &az, sch, opt);	}
	/// Plot flows for vector field {ax,ay,az} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Flow(const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_flow_3d(gr, &ax, &ay, &az, sch, opt);	}

	/// Plot flow from point p for vector field {ax,ay} parametrically depended on coordinate {x,y} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads. */
	inline void FlowP(mglPoint p, const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_flowp_xy(gr, p.x, p.y, p.z, &x, &y, &ax, &ay, sch, opt);	}
	/// Plot flow from point p for vector field {ax,ay} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads. */
	inline void FlowP(mglPoint p, const mglData &ax, const mglData &ay, const char *sch="", const char *opt="")
	{	mgl_flowp_2d(gr, p.x, p.y, p.z, &ax, &ay, sch, opt);	}
	/// Plot flow from point p for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly. */
	inline void FlowP(mglPoint p, const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_flowp_xyz(gr, p.x, p.y, p.z, &x, &y, &z, &ax, &ay, &az, sch, opt);	}
	/// Plot flow from point p for vector field {ax,ay,az} with color proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly. */
	inline void FlowP(mglPoint p, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", const char *opt="")
	{	mgl_flowp_3d(gr, p.x, p.y, p.z, &ax, &ay, &az, sch, opt);	}

	/// Plot flows from given plain for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color proportional to |a|
	/** String \a sch may contain:
	* color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	* 'v' for drawing arrows on the threads;
	* 't' for drawing tapes of normals in x-y and y-z planes.
	* Option "value" sets the number of threads (default is 5). */
	inline void Flow3(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_flow3_xyz(gr, &x, &y, &z, &ax, &ay, &az, sch, sVal, opt);	}
	/// Plot flows from given plain for vector field {ax,ay,az} with color proportional to |a|
	/** String \a sch may contain:
	* color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	* 'v' for drawing arrows on the threads;
	* 't' for drawing tapes of normals in x-y and y-z planes.
	* Option "value" sets the number of threads (default is 5). */
	inline void Flow3(const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", double sVal=-1, const char *opt="")
	{	mgl_flow3(gr, &ax, &ay, &az, sch, sVal, opt);	}
	/// Plot flows for gradient of scalar field phi parametrically depended on coordinate {x,y,z}
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Grad(const mglData &x, const mglData &y, const mglData &z, const mglData &phi, const char *sch="", const char *opt="")
	{	mgl_grad_xyz(gr,&x,&y,&z,&phi,sch,opt);	}
	/// Plot flows for gradient of scalar field phi parametrically depended on coordinate {x,y}
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Grad(const mglData &x, const mglData &y, const mglData &phi, const char *sch="", const char *opt="")
	{	mgl_grad_xy(gr,&x,&y,&phi,sch,opt);	}
	/// Plot flows for gradient of scalar field phi
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘v’ for drawing arrows on the threads;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Grad(const mglData &phi, const char *sch="", const char *opt="")
	{	mgl_grad(gr,&phi,sch,opt);	}

	/// Plot flow pipes for vector field {ax,ay} parametrically depended on coordinate {x,y} with color and radius proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘i’ for pipe radius to be inverse proportional to amplitude.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Pipe(const mglData &x, const mglData &y, const mglData &ax, const mglData &ay, const char *sch="", double r0=0.05, const char *opt="")
	{	mgl_pipe_xy(gr, &x, &y, &ax, &ay, sch, r0, opt);	}
	/// Plot flow pipes for vector field {ax,ay} with color and radius proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘i’ for pipe radius to be inverse proportional to amplitude.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Pipe(const mglData &ax, const mglData &ay, const char *sch="", double r0=0.05, const char *opt="")
	{	mgl_pipe_2d(gr, &ax, &ay, sch, r0, opt);	}
	/// Plot flow pipes for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color and radius proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘i’ for pipe radius to be inverse proportional to amplitude;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Pipe(const mglData &x, const mglData &y, const mglData &z, const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", double r0=0.05, const char *opt="")
	{	mgl_pipe_xyz(gr, &x, &y, &z, &ax, &ay, &az, sch, r0, opt);	}
	/// Plot flow pipes for vector field {ax,ay,az} with color and radius proportional to |a|
	/** String \a sch may contain:
	 * color scheme: up-half (warm) corresponds to normal flow (like attractor), bottom-half (cold) corresponds to inverse flow (like source);
	 * ‘#’ for starting threads from edges only;
	 * ‘i’ for pipe radius to be inverse proportional to amplitude;
	 * ‘x’, ‘z’ for drawing tapes of normals in x-y and y-z planes correspondingly.
	 * Option "value" sets the number of threads (default is 5). */
	inline void Pipe(const mglData &ax, const mglData &ay, const mglData &az, const char *sch="", double r0=0.05, const char *opt="")
	{	mgl_pipe_3d(gr, &ax, &ay, &az, sch, r0, opt);	}

	/// Draw density plot for data at x = sVal
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void DensX(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_dens_x(gr, &a, stl, sVal, opt);	}
	/// Draw density plot for data at y = sVal
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void DensY(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_dens_y(gr, &a, stl, sVal, opt);	}
	/// Draw density plot for data at z = sVal
	/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
	inline void DensZ(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_dens_z(gr, &a, stl, sVal, opt);	}

	/// Draw contour lines for data at x = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContX(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_x(gr, &a, stl, sVal, opt);	}
	/// Draw contour lines at manual levels for data at x = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours. */
	inline void ContX(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_x_val(gr, &v, &a, stl, sVal, opt);	}
	/// Draw contour lines for data at y = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContY(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_y(gr, &a, stl, sVal, opt);	}
	/// Draw contour lines at manual levels for data at y = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours. */
	inline void ContY(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_y_val(gr, &v, &a, stl, sVal, opt);	}
	/// Draw contour lines for data at z = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void ContZ(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_z(gr, &a, stl, sVal, opt);	}
	/// Draw contour lines at manual levels for data at z = sVal
	/** Style ‘t’/‘T’ draw contour labels below/above contours. */
	inline void ContZ(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_cont_z_val(gr, &v, &a, stl, sVal, opt);	}

	/// Draw solid contours for data at x = sVal
	/** Option "value" set the number of contour levels (default is 7). */
	inline void ContFX(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_x(gr, &a, stl, sVal, opt);	}
	/// Draw solid contours at manual levels for data at x = sVal
	inline void ContFX(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_x_val(gr, &v, &a, stl, sVal, opt);	}
	/// Draw solid contours for data at y = sVal
	/** Option "value" set the number of contour levels (default is 7). */
	inline void ContFY(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_y(gr, &a, stl, sVal, opt);	}
	/// Draw solid contours at manual levels for data at y = sVal
	inline void ContFY(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_y_val(gr, &v, &a, stl, sVal, opt);	}
	/// Draw solid contours for data at z = sVal
	/** Option "value" set the number of contour levels (default is 7). */
	inline void ContFZ(const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_z(gr, &a, stl, sVal, opt);	}
	/// Draw solid contours at manual levels for data at z = sVal
	inline void ContFZ(const mglData &v, const mglData &a, const char *stl="", double sVal=mglNaN, const char *opt="")
	{	mgl_contf_z_val(gr, &v, &a, stl, sVal, opt);	}

	/// Draw curve for formula with x in x-axis range
	/** Option "value" set initial number of points. */
	inline void FPlot(const char *fy, const char *stl="", const char *opt="")
	{	mgl_fplot(gr, fy, stl, opt);	}
	/// Draw curve for formulas parametrically depended on t in range [0,1]
	/** Option "value" set initial number of points. */
	inline void FPlot(const char *fx, const char *fy, const char *fz, const char *stl, const char *opt="")
	{	mgl_fplot_xyz(gr, fx, fy, fz, stl, opt);	}
	/// Draw surface by formula with x,y in axis range
	/** Option "value" set initial number of points. */
	inline void FSurf(const char *fz, const char *stl="", const char *opt="")
	{	mgl_fsurf(gr, fz, stl, opt);	}
	/// Draw surface by formulas parametrically depended on u,v in range [0,1]
	/** Option "value" set initial number of points. */
	inline void FSurf(const char *fx, const char *fy, const char *fz, const char *stl, const char *opt="")
	{	mgl_fsurf_xyz(gr, fx, fy, fz, stl, opt);	}

	/// Draw triangle mesh for points in arrays {x,y,z} with specified color c.
	/** Style ‘#’ produce wire plot. If id.ny=c.nx then c set the triangle colors, else vertex colors. */
	inline void TriPlot(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_triplot_xyzc(gr, &nums, &x, &y, &z, &c, sch, opt);	}
	/// Draw triangle mesh for points in arrays {x,y,z}
	/** Style ‘#’ produce wire plot. */
	inline void TriPlot(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_triplot_xyz(gr, &nums, &x, &y, &z, sch, opt);	}
	/// Draw triangle mesh for points in arrays {x,y}
	/** Style ‘#’ produce wire plot. */
	inline void TriPlot(const mglData &nums, const mglData &x, const mglData &y, const char *sch="", const char *opt="")
	{	mgl_triplot_xy(gr, &nums, &x, &y, sch, opt);	}

	/// Draw quad mesh for points in arrays {x,y,z} with specified color c
	/** Style ‘#’ produce wire plot. If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void QuadPlot(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &c, const char *sch="", const char *opt="")
	{	mgl_quadplot_xyzc(gr, &nums, &x, &y, &z, &c, sch, opt);	}
	/// Draw quad mesh for points in arrays {x,y,z}
	/** Style ‘#’ produce wire plot. */
	inline void QuadPlot(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_quadplot_xyz(gr, &nums, &x, &y, &z, sch, opt);	}
	/// Draw quad mesh for points in arrays {x,y}
	/** Style ‘#’ produce wire plot. */
	inline void QuadPlot(const mglData &nums, const mglData &x, const mglData &y, const char *sch="", const char *opt="")
	{	mgl_quadplot_xy(gr, &nums, &x, &y, sch, opt);	}

	/// Draw contour lines for triangle mesh for points in arrays {x,y,z}
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void TriCont(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_tricont_xyc(gr, &nums, &x, &y, &z, sch, opt);	}
	/// Draw contour lines for triangle mesh for points in arrays {x,y,z}
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors.
	 * Option "value" set the number of contour levels (default is 7). */
	inline void TriContV(const mglData &v, const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_tricont_xycv(gr, &v, &nums, &x, &y, &z, sch, opt);	}
	/// Draw contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void TriCont(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_tricont_xyzc(gr, &nums, &x, &y, &z, &a, sch, opt);	}
	/// Draw contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void TriContV(const mglData &v, const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_tricont_xyzcv(gr, &v, &nums, &x, &y, &z, &a, sch, opt);	}
	/// Draw contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
	/** Style ‘_’ to draw contours at bottom of axis box.
	 * Style ‘t’/‘T’ draw contour labels below/above contours.
	 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void TriCont(const mglData &v, const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_tricont_xyzcv(gr, &v, &nums, &x, &y, &z, &a, sch, opt);	}

	/// Draw contour tubes for triangle mesh for points in arrays {x,y,z}
	/** Option "value" set the number of contour levels (default is 7). */
	inline void TriContVt(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_tricontv_xyc(gr, &nums, &x, &y, &z, sch, opt);	}
	/// Draw contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c
	/** Option "value" set the number of contour levels (default is 7). */
	inline void TriContVt(const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_tricontv_xyzc(gr, &nums, &x, &y, &z, &a, sch, opt);	}
	/// Draw contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c
	/** If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
	inline void TriContVt(const mglData &v, const mglData &nums, const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_tricontv_xyzcv(gr, &v, &nums, &x, &y, &z, &a, sch, opt);	}

	/// Draw dots in points {x,y,z}.
	inline void Dots(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_dots(gr, &x, &y, &z, sch, opt);	}
	/// Draw semitransparent dots in points {x,y,z} with specified alpha a.
	inline void Dots(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_dots_a(gr, &x, &y, &z, &a, sch, opt);	}
	/// Draw semitransparent dots in points {x,y,z} with specified color c and alpha a.
	inline void Dots(const mglData &x, const mglData &y, const mglData &z, const mglData &c, const mglData &a, const char *sch="", const char *opt="")
	{	mgl_dots_ca(gr, &x, &y, &z, &c, &a, sch, opt);	}
	/// Draw surface reconstructed for points in arrays {x,y,z}.
	/** Style ‘#’ produce wired plot. */
	inline void Crust(const mglData &x, const mglData &y, const mglData &z, const char *sch="", const char *opt="")
	{	mgl_crust(gr, &x, &y, &z, sch, opt);	}

	/// Fit data along x-direction for each data row. Return array with values for found formula.
	inline mglData Fit(const mglData &y, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_1(gr, &y, eq,vars,0, opt));	}
	/// Fit data along x-direction for each data row starting from \a ini values. Return array with values for found formula.
	inline mglData Fit(const mglData &y, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_1(gr, &y, eq, vars, &ini, opt));	}
	/// Fit data along x-, y-directions for each data slice. Return array with values for found formula.
	inline mglData Fit2(const mglData &z, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_2(gr, &z, eq, vars,0, opt));	}
	/// Fit data along x-, y-direction for each data slice starting from \a ini values. Return array with values for found formula.
	inline mglData Fit2(const mglData &z, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_2(gr, &z, eq, vars, &ini, opt));	}
	/// Fit data along along all directions. Return array with values for found formula.
	inline mglData Fit3(const mglData &a, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_3(gr, &a, eq, vars,0, opt));	}
	/// Fit data along all directions starting from \a ini values. Return array with values for found formula.
	inline mglData Fit3(const mglData &a, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_3(gr, &a, eq, vars, &ini, opt));	}

	/// Fit data along x-direction for each data row. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xy(gr, &x, &y, eq, vars,0, opt));	}
	/// Fit data along x-direction for each data row starting from \a ini values. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xy(gr, &x, &y, eq, vars, &ini, opt));	}
	/// Fit data along x-, y-directions for each data slice. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const mglData &z, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xyz(gr, &x, &y, &z, eq, vars,0, opt));	}
	/// Fit data along x-, y-directions for each data slice starting from \a ini values. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const mglData &z, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xyz(gr, &x, &y, &z, eq, vars, &ini, opt));	}
	/// Fit data along along all directions. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xyza(gr, &x, &y, &z, &a, eq, vars,0, opt));	}
	/// Fit data along along all directions starting from \a ini values. Return array with values for found formula.
	inline mglData Fit(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xyza(gr, &x, &y, &z, &a, eq,vars, &ini, opt));	}

	/// Fit data with dispersion s along x-direction for each data row. Return array with values for found formula.
	inline mglData FitS(const mglData &y, const mglData &s, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_ys(gr, &y, &s, eq, vars,0, opt));	}
	/// Fit data with dispersion s along x-direction for each data row starting from \a ini values. Return array with values for found formula.
	inline mglData FitS(const mglData &y, const mglData &s, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_ys(gr, &y, &s, eq, vars, &ini, opt));	}
	/// Fit data with dispersion s along x-direction for each data row. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &s, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xys(gr, &x, &y, &s, eq, vars,0, opt));	}
	/// Fit data with dispersion s along x-direction for each data row starting from \a ini values. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &s, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xys(gr, &x, &y, &s, eq, vars, &ini, opt));	}
	/// Fit data with dispersion s along x-, y-directions for each data slice. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &z, const mglData &s, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xyzs(gr, &x, &y, &z, &s, eq, vars,0, opt));	}
	/// Fit data with dispersion s along x-, y-directions for each data slice starting from \a ini values. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &z, const mglData &s, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xyzs(gr, &x, &y, &z, &s, eq, vars, &ini, opt));	}
	/// Fit data with dispersion s along all directions. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &s, const char *eq, const char *vars, const char *opt="")
	{	return mglData(true,mgl_fit_xyzas(gr, &x, &y, &z, &a, &s, eq, vars,0, opt));	}
	/// Fit data with dispersion s along all directions starting from \a ini values. Return array with values for found formula.
	inline mglData FitS(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const mglData &s, const char *eq, const char *vars, mglData &ini, const char *opt="")
	{	return mglData(true,mgl_fit_xyzas(gr, &x, &y, &z, &a, &s, eq, vars, &ini, opt));	}

	/// Print fitted last formula (with coefficients)
	inline void PutsFit(mglPoint p, const char *prefix=0, const char *font="", double size=-1)
	{	mgl_puts_fit(gr, p.x, p.y, p.z, prefix, font, size);	}
	/// Get last fitted formula
	inline const char *GetFit()	const
	{	return mgl_get_fit(gr);	}
	/// Get chi for last fitted formula
	static inline mreal GetFitChi()
	{	return mgl_get_fit_chi();	}
	/// Get covariance matrix for last fitted formula
	static inline mglData GetFitCovar()
	{	return mglData(mgl_get_fit_covar());	}

	/// Solve PDE with x,y,z in range axis range
	inline mglData PDE(const char *ham, const mglData &ini_re, const mglData &ini_im, double dz=0.1, double k0=100, const char *opt="")
	{	return mglData(true,mgl_pde_solve(gr,ham,&ini_re,&ini_im,dz,k0, opt));	}
	/// Solve PDE with x,y,z in range axis range
	inline mglDataC PDEc(const char *ham, const mglData &ini_re, const mglData &ini_im, double dz=0.1, double k0=100, const char *opt="")
	{	return mglDataC(true,mgl_pde_solve_c(gr,ham,&ini_re,&ini_im,dz,k0, opt));	}

	/// Solve PDE with x,y,z in range axis range using advanced (slow!!!) method (2d only)
	inline mglData APDE(const char *ham, const mglData &ini_re, const mglData &ini_im, double dz=0.1, double k0=100, const char *opt="")
	{	return mglData(true,mgl_pde_adv(gr,ham,&ini_re,&ini_im,dz,k0, opt));	}
	/// Solve PDE with x,y,z in range axis range using advanced (slow!!!) method (2d only)
	inline mglDataC APDEc(const char *ham, const mglData &ini_re, const mglData &ini_im, double dz=0.1, double k0=100, const char *opt="")
	{	return mglDataC(true,mgl_pde_adv_c(gr,ham,&ini_re,&ini_im,dz,k0, opt));	}

	/// Fill data by formula with x,y,z in range axis range
	inline void Fill(mglData &u, const char *eq, const char *opt="")
	{	mgl_data_fill_eq(gr, &u, eq, 0, 0, opt);	}
	inline void Fill(mglData &u, const char *eq, const mglData &v, const char *opt="")
	{	mgl_data_fill_eq(gr, &u, eq, &v, 0, opt);	}
	inline void Fill(mglData &u, const char *eq, const mglData &v, const mglData &w, const char *opt="")
	{	mgl_data_fill_eq(gr, &u, eq, &v, &w, opt);	}
	/// Fill data by formula with x,y,z in range axis range
	inline void Fill(mglDataC &u, const char *eq, const char *opt="")
	{	mgl_datac_fill_eq(gr, &u, eq, 0, 0, opt);	}
	inline void Fill(mglDataC &u, const char *eq, const mglData &v, const char *opt="")
	{	mgl_datac_fill_eq(gr, &u, eq, &v, 0, opt);	}
	inline void Fill(mglDataC &u, const char *eq, const mglData &v, const mglData &w, const char *opt="")
	{	mgl_datac_fill_eq(gr, &u, eq, &v, &w, opt);	}

	/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in axis range
	inline void Refill(mglData &dat, const mglData &xdat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,&dat,&xdat,0,0,&vdat,sl,opt);	}
	/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat for x,y in axis range
	inline void Refill(mglData &dat, const mglData &xdat, const mglData &ydat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,&dat,&xdat,&ydat,0,&vdat,sl,opt);	}
	/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range
	inline void Refill(mglData &dat, const mglData &xdat, const mglData &ydat, const mglData &zdat, const mglData &vdat, const char *opt="")
	{	mgl_data_refill_gr(gr,&dat,&xdat,&ydat,&zdat,&vdat,-1,opt);	}

	/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in axis range
	inline void Refill(mglDataC &dat, const mglData &xdat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_datac_refill_gr(gr,&dat,&xdat,0,0,&vdat,sl,opt);	}
	/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat for x,y in axis range
	inline void Refill(mglDataC &dat, const mglData &xdat, const mglData &ydat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_datac_refill_gr(gr,&dat,&xdat,&ydat,0,&vdat,sl,opt);	}
	/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range
	inline void Refill(mglDataC &dat, const mglData &xdat, const mglData &ydat, const mglData &zdat, const mglData &vdat, const char *opt="")
	{	mgl_datac_refill_gr(gr,&dat,&xdat,&ydat,&zdat,&vdat,-1,opt);	}

	/// Set the data by triangulated surface values assuming x,y,z in range axis range
	inline void DataGrid(mglData &d, const mglData &x, const mglData &y, const mglData &z, const char *opt="")
	{	mgl_data_grid(gr,&d,&x,&y,&z,opt);	}

	/// Make histogram (distribution) of data. This function do not plot data.
	/** Option "value" sets the size of output array (default is mglFitPnts=100). */
	inline mglData Hist(const mglData &x, const mglData &a, const char *opt="")
	{	return mglData(true, mgl_hist_x(gr, &x, &a, opt));	}
	/// Make histogram (distribution) of data. This function do not plot data.
	/** Option "value" sets the size of output array (default is mglFitPnts=100). */
	inline mglData Hist(const mglData &x, const mglData &y, const mglData &a, const char *opt="")
	{	return mglData(true, mgl_hist_xy(gr, &x, &y, &a, opt));	}
	/// Make histogram (distribution) of data. This function do not plot data.
	/** Option "value" sets the size of output array (default is mglFitPnts=100). */
	inline mglData Hist(const mglData &x, const mglData &y, const mglData &z, const mglData &a, const char *opt="")
	{	return mglData(true, mgl_hist_xyz(gr, &x, &y, &z, &a, opt));	}

	inline void Compression(bool){}		// NOTE: Add later -- IDTF
	/// Set the preference for vertex color on/off (for formats that support it, now only PRC does).
	inline void VertexColor(bool enable)	{	mgl_set_flag(gr,enable, MGL_PREFERVC);	}
	/// Render only front side of surfaces for dubugging purposes (for formats that support it, now only PRC does).
	inline void DoubleSided(bool enable)	{	mgl_set_flag(gr,!enable, MGL_ONESIDED);	}
//	inline void TextureColor(bool){}	// NOTE: Add later -- IDTF
};
//-----------------------------------------------------------------------------
/// Wrapper class for MGL parsing
class mglParse
{
	HMPR pr;
	mglParse &operator=(mglParse &p)
	{	pr = p.pr;	mgl_use_parser(pr,1);	return p;	}
public:
	mglParse(HMPR p)		{	pr = p;		mgl_use_parser(pr,1);	}
	mglParse(mglParse &p)	{	pr = p.pr;	mgl_use_parser(pr,1);	}
	mglParse(bool setsize=false)
	{	pr=mgl_create_parser();	mgl_parser_allow_setsize(pr, setsize);	}
	~mglParse()	{	if(mgl_use_parser(pr,-1)<1)	mgl_delete_parser(pr);	}
	/// Get pointer to internal mglParser object
	inline HMPR Self()	{	return pr;	}
	/// Parse and draw single line of the MGL script
	inline int Parse(mglGraph *gr, const char *str, int pos)
	{	return mgl_parse_line(gr->Self(), pr, str, pos);	}
	inline int Parse(mglGraph *gr, const wchar_t *str, int pos)
	{	return mgl_parse_linew(gr->Self(), pr, str, pos);	}
	/// Execute MGL script text with '\n' separated lines
	inline void Execute(mglGraph *gr, const char *str)
	{	mgl_parse_text(gr->Self(), pr, str);	}
	inline void Execute(mglGraph *gr, const wchar_t *str)
	{	mgl_parse_textw(gr->Self(), pr, str);	}
	/// Execute and draw script from the file
	inline void Execute(mglGraph *gr, FILE *fp, bool print=false)
	{	mgl_parse_file(gr->Self(), pr, fp, print);	}

	/// Return type of command: 0 - not found, 1 - other data plot, 2 - func plot,
	///		3 - setup, 4 - data handle, 5 - data create, 6 - subplot, 7 - program
	///		8 - 1d plot, 9 - 2d plot, 10 - 3d plot, 11 - dd plot, 12 - vector plot
	///		13 - axis, 14 - primitives, 15 - axis setup, 16 - text/legend, 17 - data transform
	inline int CmdType(const char *name)
	{	return mgl_parser_cmd_type(pr, name);	}
	/// Return string of command format (command name and its argument[s])
	inline const char *CmdFormat(const char *name)
	{	return mgl_parser_cmd_frmt(pr, name);	}
	/// Return description of MGL command
	inline const char *CmdDesc(const char *name)
	{	return mgl_parser_cmd_desc(pr, name);	}
	/// Get name of command with number n
	inline const char *GetCmdName(long n)
	{	return mgl_parser_cmd_name(pr,n);	}
	/// Get number of defined commands
	inline long GetCmdNum()
	{	return mgl_parser_cmd_num(pr);	}
	/// Load new commands from external dynamic Library (must have "const mglCommand *mgl_cmd_extra" variable)
	inline void LoadDLL(const char *fname)
	{	mgl_parser_load(pr, fname);	}
	/// Apply one step for equation d vars[i]/dt = eqs[i] using Runge-Kutta method
	inline void RK_Step(const char *eqs, const char *vars, mreal dt=1)
	{	mgl_rk_step(pr, eqs, vars, dt);	}
	inline void RK_Step(const wchar_t *eqs, const wchar_t *vars, mreal dt=1)
	{	mgl_rk_step_w(pr, eqs, vars, dt);	}
	// Open all data arrays from HDF file and assign it as variables of parser p
	inline void OpenHDF(const char *fname)
	{	mgl_parser_openhdf(pr, fname);	}

	/// Set value for parameter $N
	inline void AddParam(int id, const char *str)
	{	mgl_parser_add_param(pr, id, str);	}
	inline void AddParam(int id, const wchar_t *str)
	{	mgl_parser_add_paramw(pr, id, str);	}
	/// Restore once flag
	inline void RestoreOnce()	{	mgl_parser_restore_once(pr);	}
	/// Allow changing size of the picture
	inline void AllowSetSize(bool allow)	{	mgl_parser_allow_setsize(pr, allow);	}
	/// Allow reading/saving files
	inline void AllowFileIO(bool allow)		{	mgl_parser_allow_file_io(pr, allow);	}
	/// Allow loading commands from external libraries
	inline void AllowDllCall(bool allow)	{	mgl_parser_allow_dll_call(pr, allow);	}
	/// Set flag to stop script parsing
	inline void Stop()	{	mgl_parser_stop(pr);	}
	/// Set variant of argument(s) separated by '?' to be used in further commands
	inline void SetVariant(int var=0)
	{	mgl_parser_variant(pr, var);	}
	/// Set starting object ID
	inline void	StartID(int id=0)
	{	mgl_parser_start_id(pr, id);	}

	/// Return result of formula evaluation
	inline mglData Calc(const char *formula)
	{	return mglData(true,mgl_parser_calc(pr,formula)); 	}
	inline mglData Calc(const wchar_t *formula)
	{	return mglData(true,mgl_parser_calcw(pr,formula));	}

	/// Find variable with given name or add a new one
	/// NOTE !!! You must not delete obtained data arrays !!!
	inline mglData *AddVar(const char *name)
	{	return mgl_parser_add_var(pr, name);	}
	inline mglData *AddVar(const wchar_t *name)
	{	return mgl_parser_add_varw(pr, name);	}
	/// Find variable with given name or return NULL if no one
	/// NOTE !!! You must not delete obtained data arrays !!!
	inline mglData *FindVar(const char *name)
	{	return mgl_parser_find_var(pr, name);	}
	inline mglData *FindVar(const wchar_t *name)
	{	return mgl_parser_find_varw(pr, name);	}
	/// Get variable with given id. Can be NULL for temporary ones.
	/// NOTE !!! You must not delete obtained data arrays !!!
	inline mglData *GetVar(unsigned long id)
	{	return mgl_parser_get_var(pr,id);	}
	/// Get number of variables
	inline long GetNumVar()
	{	return mgl_parser_num_var(pr);	}
	/// Delete variable with name
	inline void DeleteVar(const char *name)		{	mgl_parser_del_var(pr, name);		}
	inline void DeleteVar(const wchar_t *name)	{	mgl_parser_del_varw(pr, name);		}
	/// Delete all data variables
	void DeleteAll()	{	mgl_parser_del_all(pr);	}
};
//-----------------------------------------------------------------------------
