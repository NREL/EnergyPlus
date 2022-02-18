//**************************************************************************
// mgl_pas.pas is part of Math Graphic Library                             *
// Copyright (C) 2008-2013 Mikhail Barg, Alexey Balakin                    *
//                                                                         *
//   This program is free software; you can redistribute it and/or modify  *
//   it under the terms of the GNU Library General Public License as       *
//   published by the Free Software Foundation; either version 2 of the    *
//   License, or (at your option) any later version.                       *
//                                                                         *
//   This program is distributed in the hope that it will be useful,       *
//   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
//   GNU General Public License for more details.                          *
//                                                                         *
//   You should have received a copy of the GNU Library General Public     *
//   License along with this program; if not, write to the                 *
//   Free Software Foundation, Inc.,                                       *
//   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
//**************************************************************************

unit mgl_pas;

{$IFDEF FPC}
{$MODE DELPHI }
{$PACKENUM 4}    (* use 4-byte enums *)
{$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
{$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

{$IFDEF DARWIN}
{$linklib libmgl}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
Windows, Graphics,
{$ENDIF}
Math;

const MGL_VER2 = 2.2;
//* This define enables double precision in MathGL */
MGL_USE_DOUBLE = 1;

const
{$IFDEF MSWINDOWS}
//win - .dll
libmgl = 'libmgl.dll';
libmglglut = 'libmgl-glut.dll';
libmglfltk = 'libmgl-fltk.dll';
libmglqt   = 'libmgl-qt.dll';
{$ELSE}
{$IFDEF LINUX}
//linux - .so
libmgl = 'libmgl.so';
libmglglut = 'libmgl-glut.so';
libmglfltk = 'libmgl-fltk.so';
libmglqt   = 'libmgl-qt.so';
{$ELSE}
{$IFDEF DARWIN}
//darwin - .dylib
libmgl = 'libmgl.dylib';
libmglglut = 'libmgl-glut.dylib';
libmglfltk = 'libmgl-fltk.dylib';
libmglqt   = 'libmgl-qt.dylib';
{$ELSE}
// other platforms?

{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IF (MGL_USE_DOUBLE = 0)}
type mreal = double;
{$ELSE}
type mreal = real;
{$IFEND}
{$IFDEF FPC}
{$ELSE}
type QWord = Int64;
{$ENDIF}

Pmreal = ^mreal;

type TNGLDraw = record
end;
type TMGLGraph = record
end;
type TMGLData = record
end;
type TMGLParse = record
end;
type TMGLFormula = record
end;
type TMGLFormulaC = record
end;
type TMGLDataC = record
end;
type HMDR = ^TNGLDraw;
type HMGL = ^TMGLGraph;
type HMDT = ^TMGLData;
type HMPR = ^TMGLParse;
type PPChar = ^PChar;
type HMEX = ^TMGLFormula;
type HAEX = ^TMGLFormulaC;
type HADT = ^TMGLDataC;

type Preal = ^single;
type Pdouble = ^double;
type Pint = ^integer;
type dual = record
re, im: mreal;
end;
type Pdual = ^dual;
type TGSLVector = record
end;
type TGSLMatrix = record
end;
type PGSLVector = ^TGSLVector;
type PGSLMatrix = ^TGSLMatrix;

type TMglDrawFunction = function (gr: HMGL; p: pointer): integer; cdecl;
function mgl_create_graph_gl(): HMGL; cdecl; external libmgl;
function mgl_create_graph_glut(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglglut;
function mgl_create_graph_fltk(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglfltk;
procedure mgl_fltk_run(); cdecl; external libmglfltk;
function mgl_create_graph_qt(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglqt;
procedure mgl_qt_run(); cdecl; external libmglqt;

{== ../../include/mgl2/abstract.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Set seed for random numbers
procedure  mgl_srnd(seed: integer); cdecl; external libmgl;
/// Get random number
function  mgl_rnd(): double; cdecl; external libmgl;
/// Set name for data variable (can be used in mgl_formula_calc() or in MGL scripts)
procedure  mgl_data_set_name(dat: HMDT;const name: PChar); cdecl; external libmgl;
procedure  mgl_data_set_name_w(dat: HMDT;const name: PWideChar); cdecl; external libmgl;
/// Set callback function which is called at deleting variable
/// Save whole data array (for ns=-1) or only ns-th slice to text file
procedure  mgl_data_save(const dat: HMDT;const fname: PChar;ns: integer); cdecl; external libmgl;
/// Export data array (for ns=-1) or only ns-th slice to PNG file according color scheme
procedure  mgl_data_export(const dat: HMDT;const fname: PChar;const scheme: PChar;v1: mreal;v2: mreal;ns: integer); cdecl; external libmgl;
/// Save data to HDF file
procedure  mgl_data_save_hdf(const d: HMDT;const fname: PChar;const data: PChar;rewrite: integer); cdecl; external libmgl;
/// Get information about the data (sizes and momentum) to string
function mgl_data_info(const dat: HMDT): PChar; cdecl; external libmgl;
/// Put HDF data names into buf as '\t' separated.
function  mgl_datas_hdf(const fname: PChar;buf: PChar;size: integer): integer; cdecl; external libmgl;
/// Get maximal value of the data
function  mgl_data_max(const dat: HMDT): mreal; cdecl; external libmgl;
/// Get maximal value of the data which is less than 0
function  mgl_data_neg_max(const dat: HMDT): mreal; cdecl; external libmgl;
/// Get minimal value of the data
function  mgl_data_min(const dat: HMDT): mreal; cdecl; external libmgl;
/// Get minimal value of the data which is larger than 0
function  mgl_data_pos_min(const dat: HMDT): mreal; cdecl; external libmgl;
/// Find position (after specified in i,j,k) of first nonzero value of formula
function  mgl_data_first(const dat: HMDT;const cond: PChar;i: Pint;j: Pint;k: Pint): mreal; cdecl; external libmgl;
/// Find position (before specified in i,j,k) of last nonzero value of formula
function  mgl_data_last(const dat: HMDT;const cond: PChar;i: Pint;j: Pint;k: Pint): mreal; cdecl; external libmgl;
/// Find position of first in direction 'dir' nonzero value of formula
function  mgl_data_find(const dat: HMDT;const cond: PChar;dir: char;i: integer;j: integer;k: integer): integer; cdecl; external libmgl;
/// Find if any nonzero value of formula
function  mgl_data_find_any(const dat: HMDT;const cond: PChar): integer; cdecl; external libmgl;
/// Get maximal value of the data and its position
function  mgl_data_max_int(const dat: HMDT;i: Pint;j: Pint;k: Pint): mreal; cdecl; external libmgl;
/// Get maximal value of the data and its approximated position
function  mgl_data_max_real(const dat: HMDT;x: Pmreal;y: Pmreal;z: Pmreal): mreal; cdecl; external libmgl;
/// Get minimal value of the data and its position
function  mgl_data_min_int(const dat: HMDT;i: Pint;j: Pint;k: Pint): mreal; cdecl; external libmgl;
/// Get minimal value of the data and its approximated position
function  mgl_data_min_real(const dat: HMDT;x: Pmreal;y: Pmreal;z: Pmreal): mreal; cdecl; external libmgl;
/// Get "energy and find 4 momenta of data: median, width, skewness, kurtosis
function  mgl_data_momentum_val(const d: HMDT;dir: char;m: Pmreal;w: Pmreal;s: Pmreal;k: Pmreal): mreal; cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Callback function for asking user a question. Result shouldn't exceed 1024.
//-----------------------------------------------------------------------------
/// Abstract class for data array
//	{	return i>0 ? (i<GetNx()-1 ? (v(i+1,j,k)-v(i-1,j,k))/2 : v(i,j,k)-v(i-1,j,k)) : v(1,j,k)-v(0,j,k) 	}
//	{	return j>0 ? (j<GetNy()-1 ? (v(i,j+1,k)-v(i,j-1,k))/2 : v(i,j,k)-v(i,j-1,k)) : v(i,1,k)-v(i,0,k) 	}
//	{	return k>0 ? (k<GetNz()-1 ? (v(i,j,k+1)-v(i,j,k-1))/2 : v(i,j,k)-v(i,j,k-1)) : v(i,j,1)-v(i,j,0) 	}
//-----------------------------------------------------------------------------
/// Structure for color ID
//-----------------------------------------------------------------------------
{== ../../include/mgl2/base_cf.h ==}
//-----------------------------------------------------------------------------
/// Check if MathGL version is valid (return 0) or not (return 1)
function  mgl_check_version(const ver: PChar): integer; cdecl; external libmgl;
/// Suppress printing warnings to stderr
procedure  mgl_suppress_warn(on: integer); cdecl; external libmgl;
/// Get last warning code
function  mgl_get_warn(gr: HMGL): integer; cdecl; external libmgl;
/// Set warning code ant fill message
procedure  mgl_set_warn(gr: HMGL;code: integer;const text: PChar); cdecl; external libmgl;
/// Get text of warning message(s)
function mgl_get_mess(gr: HMGL): PChar; cdecl; external libmgl;
/// Set name of plot for saving filename
procedure  mgl_set_plotid(gr: HMGL;const id: PChar); cdecl; external libmgl;
/// Get name of plot for saving filename
function mgl_get_plotid(gr: HMGL): PChar; cdecl; external libmgl;
/// Ask to stop drawing
procedure  mgl_ask_stop(gr: HMGL;stop: integer); cdecl; external libmgl;
/// Check if plot termination is asked
function  mgl_need_stop(gr: HMGL): integer; cdecl; external libmgl;
/// Set callback function for event processing
/// Get plot quality
function  mgl_get_quality(gr: HMGL): integer; cdecl; external libmgl;
/// Set plot quality
procedure  mgl_set_quality(gr: HMGL;qual: integer); cdecl; external libmgl;
/// Set drawing region for Quality&4
procedure  mgl_set_draw_reg(gr: HMGL;nx: integer;ny: integer;m: integer); cdecl; external libmgl;
/// Check if support of frames is enabled (i.e. MGL_VECT_FRAME is set and Quality&MGL_DRAW_LMEM==0)
function  mgl_is_frames(gr: HMGL): integer; cdecl; external libmgl;
/// Get bit-value flag of HMGL state (for advanced users only)
function  mgl_get_flag(gr: HMGL;flag: LongWord): integer; cdecl; external libmgl;
/// Set bit-value flag of HMGL state (for advanced users only)
procedure  mgl_set_flag(gr: HMGL;val: integer;flag: LongWord); cdecl; external libmgl;
/// Change counter of HMGL uses (for advanced users only). Non-zero counter prevent automatic object removing.
function  mgl_use_graph(gr: HMGL;inc: integer): integer; cdecl; external libmgl;
procedure  mgl_set_rdc_acc(gr: HMGL;reduce: integer); cdecl; external libmgl;
/// Start group of objects
procedure  mgl_start_group(gr: HMGL;const name: PChar); cdecl; external libmgl;
/// End group of objects
procedure  mgl_end_group(gr: HMGL); cdecl; external libmgl;
/// Highlight objects with given id
procedure  mgl_highlight(gr: HMGL;id: integer); cdecl; external libmgl;
/// Set default palette
procedure  mgl_set_palette(gr: HMGL;const colors: PChar); cdecl; external libmgl;
/// Sets RGB values for color with given id
procedure  mgl_set_color(id: char;r: double;g: double;b: double); cdecl; external libmgl;
/// Set default color scheme
procedure  mgl_set_def_sch(gr: HMGL;const sch: PChar); cdecl; external libmgl;
/// Set mask for face coloring as array of type 'unsigned char[8]'
procedure  mgl_set_mask(id: char;const mask: PChar); cdecl; external libmgl;
/// Set mask for face coloring as unsigned long number
procedure  mgl_set_mask_val(id: char;mask: QWord); cdecl; external libmgl;
/// Set default mask rotation angle
procedure  mgl_set_mask_angle(gr: HMGL;angle: integer); cdecl; external libmgl;
/// Set default value of alpha-channel
procedure  mgl_set_alpha_default(gr: HMGL;alpha: double); cdecl; external libmgl;
/// Set relative width of rectangles in Bars, Barh, BoxPlot
procedure  mgl_set_bar_width(gr: HMGL;width: double); cdecl; external libmgl;
/// Set number of mesh lines (use 0 to draw all of them)
procedure  mgl_set_meshnum(gr: HMGL;num: integer); cdecl; external libmgl;
/// Set number of visible faces (use 0 to draw all of them)
procedure  mgl_set_facenum(gr: HMGL;num: integer); cdecl; external libmgl;
/// Clear unused points and primitives. Useful only in combination with mgl_set_facenum().
procedure  mgl_clear_unused(gr: HMGL); cdecl; external libmgl;
/// Set ambient light brightness
procedure  mgl_set_ambbr(gr: HMGL;i: double); cdecl; external libmgl;
/// Set diffusive light brightness
procedure  mgl_set_difbr(gr: HMGL;i: double); cdecl; external libmgl;
/// Use diffusive light (only for local light sources) -- OBSOLETE
procedure  mgl_set_light_dif(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Set cutting for points outside of bounding box
procedure  mgl_set_cut(gr: HMGL;cut: integer); cdecl; external libmgl;
/// Set additional cutting box
procedure  mgl_set_cut_box(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double); cdecl; external libmgl;
/// Set the cutting off condition (formula)
procedure  mgl_set_cutoff(gr: HMGL;const EqC: PChar); cdecl; external libmgl;
/// Set values of axis range
procedure  mgl_set_ranges(gr: HMGL;x1: double;x2: double;y1: double;y2: double;z1: double;z2: double); cdecl; external libmgl;
/// Set range in direction dir as [v1, v2]
procedure  mgl_set_range_val(gr: HMGL;dir: char;v1: double;v2: double); cdecl; external libmgl;
/// Add [v1, v2] to the current range in direction dir
procedure  mgl_add_range_val(gr: HMGL;dir: char;v1: double;v2: double); cdecl; external libmgl;
/// Set range in direction dir as minimal and maximal values of data a
procedure  mgl_set_range_dat(gr: HMGL;dir: char;const a: HMDT;add: integer); cdecl; external libmgl;
/// Set ranges for automatic variables
procedure  mgl_set_auto_ranges(gr: HMGL;x1: double;x2: double;y1: double;y2: double;z1: double;z2: double;c1: double;c2: double); cdecl; external libmgl;
/// Set axis range scaling -- simplified way to shift/zoom axis range -- need to redraw whole image!
procedure  mgl_zoom_axis(gr: HMGL;x1: double;y1: double;z1: double;c1: double;x2: double;y2: double;z2: double;c2: double); cdecl; external libmgl;
/// Set axis origin
procedure  mgl_set_origin(gr: HMGL;x0: double;y0: double;z0: double); cdecl; external libmgl;
/// Set the transformation formulas for coordinate
procedure  mgl_set_func(gr: HMGL;const EqX: PChar;const EqY: PChar;const EqZ: PChar;const EqA: PChar); cdecl; external libmgl;
/// Set one of predefined transformation rule
procedure  mgl_set_coor(gr: HMGL;how: integer); cdecl; external libmgl;
/// Set to draw Ternary axis (triangle like axis, grid and so on)
procedure  mgl_set_ternary(gr: HMGL;kind: integer); cdecl; external libmgl;
/// Set to use or not tick labels rotation
procedure  mgl_set_tick_rotate(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Set to use or not tick labels skipping
procedure  mgl_set_tick_skip(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Set default font for all new HMGL objects
procedure  mgl_def_font(const name: PChar;const path: PChar); cdecl; external libmgl;
/// Set default size of marks (locally you can use "size" option)
procedure  mgl_set_mark_size(gr: HMGL;size: double); cdecl; external libmgl;
/// Set default size of arrows (locally you can use "size" option)
procedure  mgl_set_arrow_size(gr: HMGL;size: double); cdecl; external libmgl;
/// Set default font size
procedure  mgl_set_font_size(gr: HMGL;size: double); cdecl; external libmgl;
/// Set default font style and color
procedure  mgl_set_font_def(gr: HMGL;const fnt: PChar); cdecl; external libmgl;
/// Set to use or not text rotation
procedure  mgl_set_rotated_text(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Load font from file
procedure  mgl_load_font(gr: HMGL;const name: PChar;const path: PChar); cdecl; external libmgl;
/// Copy font from another mglGraph instance
procedure  mgl_copy_font(gr: HMGL;gr_from: HMGL); cdecl; external libmgl;
/// Restore font (load default font for new HMGL objects)
procedure  mgl_restore_font(gr: HMGL); cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/data_cf.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Get integer power of x
function  mgl_ipow(x: double;n: integer): double; cdecl; external libmgl;
/// Get number of seconds since 1970 for given string
function  mgl_get_time(const time: PChar;const fmt: PChar): double; cdecl; external libmgl;
/// Create HMDT object
function  mgl_create_data(): HMDT; cdecl; external libmgl;
/// Create HMDT object with specified sizes
function  mgl_create_data_size(nx: integer;ny: integer;nz: integer): HMDT; cdecl; external libmgl;
/// Create HMDT object with data from file
function  mgl_create_data_file(const fname: PChar): HMDT; cdecl; external libmgl;
/// Delete HMDT object
procedure  mgl_delete_data(dat: HMDT); cdecl; external libmgl;
/// Rearange data dimensions
procedure  mgl_data_rearrange(dat: HMDT;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Link external data array (don't delete it at exit)
procedure  mgl_data_link(dat: HMDT;A: Pmreal;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (float *) array
procedure  mgl_data_set_float(dat: HMDT;const A: Preal;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (double *) array
procedure  mgl_data_set_double(dat: HMDT;const A: Pdouble;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (float **) array
/// Allocate memory and copy the data from the (double **) array
/// Allocate memory and copy the data from the (float ***) array
/// Allocate memory and copy the data from the (double ***) array
/// Import data from abstract type
procedure  mgl_data_set(dat: HMDT;const a: HMDT); cdecl; external libmgl;
/// Allocate memory and copy the data from the gsl_vector
procedure  mgl_data_set_vector(dat: HMDT;v: PGSLVector); cdecl; external libmgl;
/// Allocate memory and copy the data from the gsl_matrix
procedure  mgl_data_set_matrix(dat: HMDT;m: PGSLMatrix); cdecl; external libmgl;
/// Set value of data element [i,j,k]
procedure  mgl_data_set_value(dat: HMDT;v: mreal;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Get value of data element [i,j,k]
function  mgl_data_get_value(const dat: HMDT;i: integer;j: integer;k: integer): mreal; cdecl; external libmgl;
/// Allocate memory and scanf the data from the string
procedure  mgl_data_set_values(dat: HMDT;const val: PChar;nx: integer;ny: integer;nz: integer); cdecl; external libmgl;
/// Read data array from HDF file (parse HDF4 and HDF5 files)
function  mgl_data_read_hdf(d: HMDT;const fname: PChar;const data: PChar): integer; cdecl; external libmgl;
/// Read data from tab-separated text file with auto determining size
function  mgl_data_read(dat: HMDT;const fname: PChar): integer; cdecl; external libmgl;
/// Read data from text file with size specified at beginning of the file
function  mgl_data_read_mat(dat: HMDT;const fname: PChar;dim: integer): integer; cdecl; external libmgl;
/// Read data from text file with specifeid size
function  mgl_data_read_dim(dat: HMDT;const fname: PChar;mx: integer;my: integer;mz: integer): integer; cdecl; external libmgl;
/// Read data from tab-separated text files with auto determining size which filenames are result of sprintf(fname,templ,t) where t=from:step:to
function  mgl_data_read_range(d: HMDT;const templ: PChar;n1: double;n2: double;step: double;as_slice: integer): integer; cdecl; external libmgl;
/// Read data from tab-separated text files with auto determining size which filenames are satisfied to template (like "t_*.dat")
function  mgl_data_read_all(dat: HMDT;const templ: PChar;as_slice: integer): integer; cdecl; external libmgl;
/// Import data array from PNG file according color scheme
procedure  mgl_data_import(dat: HMDT;const fname: PChar;const scheme: PChar;v1: mreal;v2: mreal); cdecl; external libmgl;
/// Create or recreate the array with specified size and fill it by zero
procedure  mgl_data_create(dat: HMDT;nx: integer;ny: integer;nz: integer); cdecl; external libmgl;
/// Transpose dimensions of the data (generalization of Transpose)
procedure  mgl_data_transpose(dat: HMDT;const dim: PChar); cdecl; external libmgl;
/// Normalize the data to range [v1,v2]
procedure  mgl_data_norm(dat: HMDT;v1: mreal;v2: mreal;sym: integer;dim: integer); cdecl; external libmgl;
/// Normalize the data to range [v1,v2] slice by slice
procedure  mgl_data_norm_slice(dat: HMDT;v1: mreal;v2: mreal;dir: char;keep_en: integer;sym: integer); cdecl; external libmgl;
/// Get sub-array of the data with given fixed indexes
function  mgl_data_subdata(const dat: HMDT;xx: integer;yy: integer;zz: integer): HMDT; cdecl; external libmgl;
/// Get sub-array of the data with given fixed indexes (like indirect access)
function  mgl_data_subdata_ext(const dat: HMDT;const xx: HMDT;const yy: HMDT;const zz: HMDT): HMDT; cdecl; external libmgl;
/// Get column (or slice) of the data filled by formulas of named columns
function  mgl_data_column(const dat: HMDT;const eq: PChar): HMDT; cdecl; external libmgl;
/// Set names for columns (slices)
procedure  mgl_data_set_id(d: HMDT;const id: PChar); cdecl; external libmgl;
/// Equidistantly fill the data to range [x1,x2] in direction dir
procedure  mgl_data_fill(dat: HMDT;x1: mreal;x2: mreal;dir: char); cdecl; external libmgl;
/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
procedure  mgl_data_fill_eq(gr: HMGL;dat: HMDT;const eq: PChar;const vdat: HMDT;const wdat: HMDT;const opt: PChar); cdecl; external libmgl;
/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in range [x1,x2] using global spline
procedure  mgl_data_refill_gs(dat: HMDT;const xdat: HMDT;const vdat: HMDT;x1: mreal;x2: mreal;sl: integer); cdecl; external libmgl;
/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in range [x1,x2]
procedure  mgl_data_refill_x(dat: HMDT;const xdat: HMDT;const vdat: HMDT;x1: mreal;x2: mreal;sl: integer); cdecl; external libmgl;
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat for x,y in range [x1,x2]*[y1,y2]
procedure  mgl_data_refill_xy(dat: HMDT;const xdat: HMDT;const ydat: HMDT;const vdat: HMDT;x1: mreal;x2: mreal;y1: mreal;y2: mreal;sl: integer); cdecl; external libmgl;
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [x1,x2]*[y1,y2]*[z1,z2]
procedure  mgl_data_refill_xyz(dat: HMDT;const xdat: HMDT;const ydat: HMDT;const zdat: HMDT;const vdat: HMDT;x1: mreal;x2: mreal;y1: mreal;y2: mreal;z1: mreal;z2: mreal); cdecl; external libmgl;
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range
procedure  mgl_data_refill_gr(gr: HMGL;dat: HMDT;const xdat: HMDT;const ydat: HMDT;const zdat: HMDT;const vdat: HMDT;sl: integer;const opt: PChar); cdecl; external libmgl;
/// Set the data by triangulated surface values assuming x,y,z in range [r1,r2]
procedure  mgl_data_grid(gr: HMGL;d: HMDT;const xdat: HMDT;const ydat: HMDT;const zdat: HMDT;const opt: PChar); cdecl; external libmgl;
/// Set the data by triangulated surface values assuming x,y,z in range [x1,x2]*[y1,y2]
procedure  mgl_data_grid_xy(d: HMDT;const xdat: HMDT;const ydat: HMDT;const zdat: HMDT;x1: mreal;x2: mreal;y1: mreal;y2: mreal); cdecl; external libmgl;
/// Put value to data element(s)
procedure  mgl_data_put_val(dat: HMDT;val: mreal;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Put array to data element(s)
procedure  mgl_data_put_dat(dat: HMDT;const val: HMDT;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Modify the data by specified formula
procedure  mgl_data_modify(dat: HMDT;const eq: PChar;dim: integer); cdecl; external libmgl;
/// Modify the data by specified formula
procedure  mgl_data_modify_vw(dat: HMDT;const eq: PChar;const vdat: HMDT;const wdat: HMDT); cdecl; external libmgl;
/// Reduce size of the data
procedure  mgl_data_squeeze(dat: HMDT;rx: integer;ry: integer;rz: integer;smooth: integer); cdecl; external libmgl;
/// Returns pointer to data element [i,j,k]
function mgl_data_value(dat: HMDT;i: integer;j: integer;k: integer): Pmreal; cdecl; external libmgl;
/// Returns pointer to internal data array
function mgl_data_data(dat: HMDT): Pmreal; cdecl; external libmgl;
/// Gets the x-size of the data.
function  mgl_data_get_nx(const d: HMDT): integer; cdecl; external libmgl;
/// Gets the y-size of the data.
function  mgl_data_get_ny(const d: HMDT): integer; cdecl; external libmgl;
/// Gets the z-size of the data.
function  mgl_data_get_nz(const d: HMDT): integer; cdecl; external libmgl;
/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
function  mgl_data_combine(const dat1: HMDT;const dat2: HMDT): HMDT; cdecl; external libmgl;
/// Extend data dimensions
procedure  mgl_data_extend(dat: HMDT;n1: integer;n2: integer); cdecl; external libmgl;
/// Insert data rows/columns/slices
procedure  mgl_data_insert(dat: HMDT;dir: char;at: integer;num: integer); cdecl; external libmgl;
/// Delete data rows/columns/slices
procedure  mgl_data_delete(dat: HMDT;dir: char;at: integer;num: integer); cdecl; external libmgl;
/// Joind another data array
procedure  mgl_data_join(dat: HMDT;const d: HMDT); cdecl; external libmgl;
/// Smooth the data on specified direction or directions
procedure  mgl_data_smooth(d: HMDT;const dirs: PChar;delta: mreal); cdecl; external libmgl;
/// Get array which is result of summation in given direction or directions
function  mgl_data_sum(const dat: HMDT;const dir: PChar): HMDT; cdecl; external libmgl;
/// Get array which is result of maximal values in given direction or directions
function  mgl_data_max_dir(const dat: HMDT;const dir: PChar): HMDT; cdecl; external libmgl;
/// Get array which is result of minimal values in given direction or directions
function  mgl_data_min_dir(const dat: HMDT;const dir: PChar): HMDT; cdecl; external libmgl;
/// Cumulative summation the data in given direction or directions
procedure  mgl_data_cumsum(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Integrate (cumulative summation) the data in given direction or directions
procedure  mgl_data_integral(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Differentiate the data in given direction or directions
procedure  mgl_data_diff(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Differentiate the parametrically specified data along direction v1 with v2,v3=const (v3 can be NULL)
procedure  mgl_data_diff_par(dat: HMDT;const v1: HMDT;const v2: HMDT;const v3: HMDT); cdecl; external libmgl;
/// Double-differentiate (like Laplace operator) the data in given direction
procedure  mgl_data_diff2(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Swap left and right part of the data in given direction (useful for Fourier spectrum)
procedure  mgl_data_swap(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Roll data along direction dir by num slices
procedure  mgl_data_roll(dat: HMDT;dir: char;num: integer); cdecl; external libmgl;
/// Mirror the data in given direction (useful for Fourier spectrum)
procedure  mgl_data_mirror(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Sort rows (or slices) by values of specified column
procedure  mgl_data_sort(dat: HMDT;idx: integer;idy: integer); cdecl; external libmgl;
/// Apply Hankel transform
procedure  mgl_data_hankel(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Apply Sin-Fourier transform
procedure  mgl_data_sinfft(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Apply Cos-Fourier transform
procedure  mgl_data_cosfft(dat: HMDT;const dir: PChar); cdecl; external libmgl;
/// Fill data by 'x'/'k' samples for Hankel ('h') or Fourier ('f') transform
procedure  mgl_data_fill_sample(dat: HMDT;const how: PChar); cdecl; external libmgl;
/// Find correlation between 2 data arrays
function  mgl_data_correl(const dat1: HMDT;const dat2: HMDT;const dir: PChar): HMDT; cdecl; external libmgl;
/// Allocate and prepare data for Fourier transform by nthr threads
/// Free data for Fourier transform
/// Make Fourier transform of data x of size n and step s between points
/// Clear internal data for speeding up FFT and Hankel transforms
procedure  mgl_clear_fft(); cdecl; external libmgl;
/// Interpolate by cubic spline the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_data_spline(const dat: HMDT;x: mreal;y: mreal;z: mreal): mreal; cdecl; external libmgl;
/// Interpolate by linear function the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_data_linear(const dat: HMDT;x: mreal;y: mreal;z: mreal): mreal; cdecl; external libmgl;
/// Interpolate by cubic spline the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_data_spline_ext(const dat: HMDT;x: mreal;y: mreal;z: mreal;dx: Pmreal;dy: Pmreal;dz: Pmreal): mreal; cdecl; external libmgl;
/// Prepare coefficients for global spline interpolation
function  mgl_gspline_init(const x: HMDT;const v: HMDT): HMDT; cdecl; external libmgl;
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
function  mgl_gspline(const coef: HMDT;dx: mreal;d1: Pmreal;d2: Pmreal): mreal; cdecl; external libmgl;
/// Interpolate by linear function the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_data_linear_ext(const dat: HMDT;x: mreal;y: mreal;z: mreal;dx: Pmreal;dy: Pmreal;dz: Pmreal): mreal; cdecl; external libmgl;
/// Return an approximated x-value (root) when dat(x) = val
function  mgl_data_solve_1d(const dat: HMDT;val: mreal;spl: integer;i0: integer): mreal; cdecl; external libmgl;
/// Return an approximated value (root) when dat(x) = val
function  mgl_data_solve(const dat: HMDT;val: mreal;dir: char;const i0: HMDT;norm: integer): HMDT; cdecl; external libmgl;
/// Get trace of the data array
function  mgl_data_trace(const d: HMDT): HMDT; cdecl; external libmgl;
/// Resize the data to new sizes
function  mgl_data_resize(const dat: HMDT;mx: integer;my: integer;mz: integer): HMDT; cdecl; external libmgl;
/// Resize the data to new sizes of box [x1,x2]*[y1,y2]*[z1,z2]
function  mgl_data_resize_box(const dat: HMDT;mx: integer;my: integer;mz: integer;x1: mreal;x2: mreal;y1: mreal;y2: mreal;z1: mreal;z2: mreal): HMDT; cdecl; external libmgl;
/// Create n-th points distribution of this data values in range [v1, v2]
function  mgl_data_hist(const dat: HMDT;n: integer;v1: mreal;v2: mreal;nsub: integer): HMDT; cdecl; external libmgl;
/// Create n-th points distribution of this data values in range [v1, v2] with weight w
function  mgl_data_hist_w(const dat: HMDT;const weight: HMDT;n: integer;v1: mreal;v2: mreal;nsub: integer): HMDT; cdecl; external libmgl;
/// Get momentum (1D-array) of data along direction 'dir'. String looks like "x1" for median in x-direction, "x2" for width in x-dir and so on.
function  mgl_data_momentum(const dat: HMDT;dir: char;const how: PChar): HMDT; cdecl; external libmgl;
/// Get array which values is result of interpolation this for coordinates from other arrays
function  mgl_data_evaluate(const dat: HMDT;const idat: HMDT;const jdat: HMDT;const kdat: HMDT;norm: integer): HMDT; cdecl; external libmgl;
/// Set as the data envelop
procedure  mgl_data_envelop(dat: HMDT;dir: char); cdecl; external libmgl;
/// Remove phase jump
procedure  mgl_data_sew(dat: HMDT;const dirs: PChar;da: mreal); cdecl; external libmgl;
/// Crop the data
procedure  mgl_data_crop(dat: HMDT;n1: integer;n2: integer;dir: char); cdecl; external libmgl;
/// Remove rows with duplicate values in column id
procedure  mgl_data_clean(dat: HMDT;id: integer); cdecl; external libmgl;
/// Multiply the data by other one for each element
procedure  mgl_data_mul_dat(dat: HMDT;const d: HMDT); cdecl; external libmgl;
/// Divide the data by other one for each element
procedure  mgl_data_div_dat(dat: HMDT;const d: HMDT); cdecl; external libmgl;
/// Add the other data
procedure  mgl_data_add_dat(dat: HMDT;const d: HMDT); cdecl; external libmgl;
/// Subtract the other data
procedure  mgl_data_sub_dat(dat: HMDT;const d: HMDT); cdecl; external libmgl;
/// Multiply each element by the number
procedure  mgl_data_mul_num(dat: HMDT;d: mreal); cdecl; external libmgl;
/// Divide each element by the number
procedure  mgl_data_div_num(dat: HMDT;d: mreal); cdecl; external libmgl;
/// Add the number
procedure  mgl_data_add_num(dat: HMDT;d: mreal); cdecl; external libmgl;
/// Subtract the number
procedure  mgl_data_sub_num(dat: HMDT;d: mreal); cdecl; external libmgl;
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for amplitude and phase
function  mgl_transform_a(const am: HMDT;const ph: HMDT;const tr: PChar): HMDT; cdecl; external libmgl;
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for real and imaginary parts
function  mgl_transform(const re: HMDT;const im: HMDT;const tr: PChar): HMDT; cdecl; external libmgl;
/// Apply Fourier transform for the data and save result into it
procedure  mgl_data_fourier(re: HMDT;im: HMDT;const dir: PChar); cdecl; external libmgl;
/// Short time Fourier analysis for real and imaginary parts. Output is amplitude of partial Fourier (result will have size {dn, floor(nx/dn), ny} for dir='x'
function  mgl_data_stfa(const re: HMDT;const im: HMDT;dn: integer;dir: char): HMDT; cdecl; external libmgl;
/// Do something like Delone triangulation for 3d points
function  mgl_triangulation_3d(const x: HMDT;const y: HMDT;const z: HMDT): HMDT; cdecl; external libmgl;
/// Do Delone triangulation for 2d points
function  mgl_triangulation_2d(const x: HMDT;const y: HMDT): HMDT; cdecl; external libmgl;
/// Find root for nonlinear equation
/// Find root for nonlinear equation defined by textual formula
function  mgl_find_root_txt(const func: PChar;ini: mreal;var_id: char): mreal; cdecl; external libmgl;
/// Find roots for nonlinear equation defined by textual formula
function  mgl_data_roots(const func: PChar;const ini: HMDT;var_id: char): HMDT; cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Create HMEX object for expression evaluating
function  mgl_create_expr(const expr: PChar): HMEX; cdecl; external libmgl;
/// Delete HMEX object
procedure  mgl_delete_expr(ex: HMEX); cdecl; external libmgl;
/// Return value of expression for given x,y,z variables
function  mgl_expr_eval(ex: HMEX;x: double;y: double;z: double): double; cdecl; external libmgl;
/// Return value of expression for given variables
function  mgl_expr_eval_v(ex: HMEX;vars: Pmreal): double; cdecl; external libmgl;
/// Return value of expression differentiation over variable dir for given x,y,z variables
function  mgl_expr_diff(ex: HMEX;dir: char;x: double;y: double;z: double): double; cdecl; external libmgl;
/// Return value of expression differentiation over variable dir for given variables
function  mgl_expr_diff_v(ex: HMEX;dir: char;vars: Pmreal): double; cdecl; external libmgl;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
{== ../../include/mgl2/datac_cf.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Get integer power of x
function  mgl_ipowc(x: dual;n: integer): dual; cdecl; external libmgl;
/// Get exp(i*a)
function  mgl_expi(a: dual): dual; cdecl; external libmgl;
overload;
/// Create HMDT object
function  mgl_create_datac(): HADT; cdecl; external libmgl;
/// Create HMDT object with specified sizes
function  mgl_create_datac_size(nx: integer;ny: integer;nz: integer): HADT; cdecl; external libmgl;
/// Create HMDT object with data from file
function  mgl_create_datac_file(const fname: PChar): HADT; cdecl; external libmgl;
/// Delete HMDT object
procedure  mgl_delete_datac(dat: HADT); cdecl; external libmgl;
/// Rearange data dimensions
procedure  mgl_datac_rearrange(dat: HADT;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Link external data array (don't delete it at exit)
procedure  mgl_datac_link(dat: HADT;A: Pdual;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (float *) array
procedure  mgl_datac_set_float(dat: HADT;const A: Preal;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (double *) array
procedure  mgl_datac_set_double(dat: HADT;const A: Pdouble;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Allocate memory and copy the data from the (dual *) array
procedure  mgl_datac_set_complex(dat: HADT;const A: Pdual;mx: integer;my: integer;mz: integer); cdecl; external libmgl;
/// Import data from abstract type
procedure  mgl_datac_set(dat: HADT;const a: HMDT); cdecl; external libmgl;
/// Allocate memory and copy the data from the gsl_vector
procedure  mgl_datac_set_vector(dat: HADT;v: PGSLVector); cdecl; external libmgl;
/// Allocate memory and copy the data from the gsl_matrix
procedure  mgl_datac_set_matrix(dat: HADT;m: PGSLMatrix); cdecl; external libmgl;
/// Set value of data element [i,j,k]
procedure  mgl_datac_set_value(dat: HADT;v: dual;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Get value of data element [i,j,k]
function  mgl_datac_get_value(const dat: HMDT;i: integer;j: integer;k: integer): dual; cdecl; external libmgl;
/// Allocate memory and scanf the data from the string
procedure  mgl_datac_set_values(dat: HADT;const val: PChar;nx: integer;ny: integer;nz: integer); cdecl; external libmgl;
/// Returns pointer to internal data array
function mgl_datac_data(dat: HADT): PDual; cdecl; external libmgl;
/// Returns pointer to data element [i,j,k]
function mgl_datac_value(dat: HADT;i: integer;j: integer;k: integer): PDual; cdecl; external libmgl;
/// Set the data from HCDT objects for real and imaginary parts
procedure  mgl_datac_set_ri(dat: HADT;const re: HMDT;const im: HMDT); cdecl; external libmgl;
/// Set the data from HCDT objects as amplitude and phase of complex data
procedure  mgl_datac_set_ap(dat: HADT;const abs: HMDT;const phi: HMDT); cdecl; external libmgl;
/// Read data from tab-separated text file with auto determining size
function  mgl_datac_read(dat: HADT;const fname: PChar): integer; cdecl; external libmgl;
/// Read data from text file with size specified at beginning of the file
function  mgl_datac_read_mat(dat: HADT;const fname: PChar;dim: integer): integer; cdecl; external libmgl;
/// Read data from text file with specifeid size
function  mgl_datac_read_dim(dat: HADT;const fname: PChar;mx: integer;my: integer;mz: integer): integer; cdecl; external libmgl;
/// Read data from tab-separated text files with auto determining size which filenames are result of sprintf(fname,templ,t) where t=from:step:to
function  mgl_datac_read_range(d: HADT;const templ: PChar;from: double;to_: double;step: double;as_slice: integer): integer; cdecl; external libmgl;
/// Read data from tab-separated text files with auto determining size which filenames are satisfied to template (like "t_*.dat")
function  mgl_datac_read_all(dat: HADT;const templ: PChar;as_slice: integer): integer; cdecl; external libmgl;
/// Save whole data array (for ns=-1) or only ns-th slice to text file
procedure  mgl_datac_save(const dat: HMDT;const fname: PChar;ns: integer); cdecl; external libmgl;
/// Read data array from HDF file (parse HDF4 and HDF5 files)
function  mgl_datac_read_hdf(d: HADT;const fname: PChar;const data: PChar): integer; cdecl; external libmgl;
/// Save data to HDF file
procedure  mgl_datac_save_hdf(const d: HMDT;const fname: PChar;const data: PChar;rewrite: integer); cdecl; external libmgl;
/// Create or recreate the array with specified size and fill it by zero
procedure  mgl_datac_create(dat: HADT;nx: integer;ny: integer;nz: integer); cdecl; external libmgl;
/// Transpose dimensions of the data (generalization of Transpose)
procedure  mgl_datac_transpose(dat: HADT;const dim: PChar); cdecl; external libmgl;
/// Get sub-array of the data with given fixed indexes
function  mgl_datac_subdata(const dat: HMDT;xx: integer;yy: integer;zz: integer): HADT; cdecl; external libmgl;
/// Get sub-array of the data with given fixed indexes (like indirect access)
function  mgl_datac_subdata_ext(const dat: HMDT;const xx: HMDT;const yy: HMDT;const zz: HMDT): HADT; cdecl; external libmgl;
/// Get column (or slice) of the data filled by formulas of named columns
function  mgl_datac_column(const dat: HMDT;const eq: PChar): HADT; cdecl; external libmgl;
/// Get trace of the data array
function  mgl_datac_trace(const d: HMDT): HADT; cdecl; external libmgl;
/// Resize the data to new sizes
function  mgl_datac_resize(const dat: HMDT;mx: integer;my: integer;mz: integer): HADT; cdecl; external libmgl;
/// Resize the data to new sizes of box [x1,x2]*[y1,y2]*[z1,z2]
function  mgl_datac_resize_box(const dat: HMDT;mx: integer;my: integer;mz: integer;x1: mreal;x2: mreal;y1: mreal;y2: mreal;z1: mreal;z2: mreal): HADT; cdecl; external libmgl;
/// Get momentum (1D-array) of data along direction 'dir'. String looks like "x1" for median in x-direction, "x2" for width in x-dir and so on.
function  mgl_datac_momentum(const dat: HMDT;dir: char;const how: PChar): HADT; cdecl; external libmgl;
/// Get array which values is result of interpolation this for coordinates from other arrays
function  mgl_datac_evaluate(const dat: HMDT;const idat: HMDT;const jdat: HMDT;const kdat: HMDT;norm: integer): HADT; cdecl; external libmgl;
/// Get array which is result of summation in given direction or directions
function  mgl_datac_sum(const dat: HMDT;const dir: PChar): HADT; cdecl; external libmgl;
/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
function  mgl_datac_combine(const dat1: HMDT;const dat2: HMDT): HADT; cdecl; external libmgl;
/// Set names for columns (slices)
procedure  mgl_datac_set_id(d: HADT;const id: PChar); cdecl; external libmgl;
/// Equidistantly fill the data to range [x1,x2] in direction dir
procedure  mgl_datac_fill(dat: HADT;x1: dual;x2: dual;dir: char); cdecl; external libmgl;
/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
procedure  mgl_datac_fill_eq(gr: HMGL;dat: HADT;const eq: PChar;const vdat: HMDT;const wdat: HMDT;const opt: PChar); cdecl; external libmgl;
/// Modify the data by specified formula
procedure  mgl_datac_modify(dat: HADT;const eq: PChar;dim: integer); cdecl; external libmgl;
/// Modify the data by specified formula
procedure  mgl_datac_modify_vw(dat: HADT;const eq: PChar;const vdat: HMDT;const wdat: HMDT); cdecl; external libmgl;
/// Put value to data element(s)
procedure  mgl_datac_put_val(dat: HADT;val: dual;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Put array to data element(s)
procedure  mgl_datac_put_dat(dat: HADT;const val: HMDT;i: integer;j: integer;k: integer); cdecl; external libmgl;
/// Reduce size of the data
procedure  mgl_datac_squeeze(dat: HADT;rx: integer;ry: integer;rz: integer;smooth: integer); cdecl; external libmgl;
/// Extend data dimensions
procedure  mgl_datac_extend(dat: HADT;n1: integer;n2: integer); cdecl; external libmgl;
/// Insert data rows/columns/slices
procedure  mgl_datac_insert(dat: HADT;dir: char;at: integer;num: integer); cdecl; external libmgl;
/// Delete data rows/columns/slices
procedure  mgl_datac_delete(dat: HADT;dir: char;at: integer;num: integer); cdecl; external libmgl;
/// Joind another data array
procedure  mgl_datac_join(dat: HADT;const d: HMDT); cdecl; external libmgl;
/// Smooth the data on specified direction or directions
procedure  mgl_datac_smooth(d: HADT;const dirs: PChar;delta: mreal); cdecl; external libmgl;
/// Cumulative summation the data in given direction or directions
procedure  mgl_datac_cumsum(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Integrate (cumulative summation) the data in given direction or directions
procedure  mgl_datac_integral(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Differentiate the data in given direction or directions
procedure  mgl_datac_diff(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Double-differentiate (like Laplace operator) the data in given direction
procedure  mgl_datac_diff2(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Swap left and right part of the data in given direction (useful for Fourier spectrum)
procedure  mgl_datac_swap(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Roll data along direction dir by num slices
procedure  mgl_datac_roll(dat: HADT;dir: char;num: integer); cdecl; external libmgl;
/// Mirror the data in given direction (useful for Fourier spectrum)
procedure  mgl_datac_mirror(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Crop the data
procedure  mgl_datac_crop(dat: HADT;n1: integer;n2: integer;dir: char); cdecl; external libmgl;
/// Apply Hankel transform
procedure  mgl_datac_hankel(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Apply Fourier transform
procedure  mgl_datac_fft(dat: HADT;const dir: PChar); cdecl; external libmgl;
/// Find correlation between 2 data arrays
function  mgl_datac_correl(const dat1: HMDT;const dat2: HMDT;const dir: PChar): HADT; cdecl; external libmgl;
/// Calculate one step of diffraction by finite-difference method with parameter q
procedure  mgl_datac_diffr(dat: HADT;const how: PChar;q: mreal); cdecl; external libmgl;
function  mgl_datac_real(const dat: HMDT): HMDT; cdecl; external libmgl;
function  mgl_datac_imag(const dat: HMDT): HMDT; cdecl; external libmgl;
function  mgl_datac_abs(const dat: HMDT): HMDT; cdecl; external libmgl;
function  mgl_datac_arg(const dat: HMDT): HMDT; cdecl; external libmgl;
/// Interpolate by linear function the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_datac_linear(const d: HMDT;x: mreal;y: mreal;z: mreal): dual; cdecl; external libmgl;
/// Interpolate by linear function the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_datac_linear_ext(const d: HMDT;x: mreal;y: mreal;z: mreal;dx: Pdual;dy: Pdual;dz: Pdual): dual; cdecl; external libmgl;
/// Interpolate by cubic spline the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_datac_spline(const dat: HMDT;x: mreal;y: mreal;z: mreal): dual; cdecl; external libmgl;
/// Interpolate by cubic spline the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
function  mgl_datac_spline_ext(const dat: HMDT;x: mreal;y: mreal;z: mreal;dx: Pdual;dy: Pdual;dz: Pdual): dual; cdecl; external libmgl;
/// Prepare coefficients for global spline interpolation
function  mgl_gsplinec_init(const x: HMDT;const v: HMDT): HADT; cdecl; external libmgl;
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
function  mgl_gsplinec(const coef: HMDT;dx: mreal;d1: Pdual;d2: Pdual): dual; cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Create HAEX object for expression evaluating
function  mgl_create_cexpr(const expr: PChar): HAEX; cdecl; external libmgl;
/// Delete HAEX object
procedure  mgl_delete_cexpr(ex: HAEX); cdecl; external libmgl;
/// Return value of expression for given x,y,z variables
function  mgl_cexpr_eval(ex: HAEX;x: dual;y: dual;z: dual): dual; cdecl; external libmgl;
/// Return value of expression for given variables
function  mgl_cexpr_eval_v(ex: HAEX;vars: Pdual): dual; cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/cont.h ==}
//-----------------------------------------------------------------------------
/// Print text along the curve in parametric form {x,y,z}
procedure  mgl_text_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const text: PChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textw_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const text: PWideChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
/// Print text along the curve in parametric form {x,y}
procedure  mgl_text_xy(gr: HMGL;const x: HMDT;const y: HMDT;const text: PChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textw_xy(gr: HMGL;const x: HMDT;const y: HMDT;const text: PWideChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
/// Print text along the curve
procedure  mgl_text_y(gr: HMGL;const y: HMDT;const text: PChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textw_y(gr: HMGL;const y: HMDT;const text: PWideChar;const font: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_cont_gen(gr: HMGL;val: double;const a: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const stl: PChar); cdecl; external libmgl;
procedure  mgl_contf_gen(gr: HMGL;v1: double;v2: double;const a: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const stl: PChar); cdecl; external libmgl;
//void MGL_EXPORT mgl_contv_gen(HMGL gr, double v1, double v2, HCDT a, HCDT x, HCDT y, HCDT z, const char *stl)
//void MGL_EXPORT mgl_axial_gen(HMGL gr, double v1, double v2, HCDT a, HCDT x, HCDT y, HCDT z, const char *stl)
/// Draw manual contour lines for 2d data specified parametrically
procedure  mgl_cont_xy_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for 2d data
procedure  mgl_cont_val(gr: HMGL;const v: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for 2d data specified parametrically
procedure  mgl_cont_xy(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for 2d data
procedure  mgl_cont(gr: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for 2d data specified parametrically
procedure  mgl_contf_xy_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for 2d data
procedure  mgl_contf_val(gr: HMGL;const v: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for 2d data specified parametrically
procedure  mgl_contf_xy(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for 2d data
procedure  mgl_contf(gr: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for 2d data specified parametrically with manual colors
procedure  mgl_contd_xy_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for 2d data with manual colors
procedure  mgl_contd_val(gr: HMGL;const v: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for 2d data specified parametrically with manual colors
procedure  mgl_contd_xy(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for 2d data with manual colors
procedure  mgl_contd(gr: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour tubes for 2d data specified parametrically
procedure  mgl_contv_xy_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour tubes for 2d data
procedure  mgl_contv_val(gr: HMGL;const v: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour tubes for 2d data specified parametrically
procedure  mgl_contv_xy(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour tubes for 2d data
procedure  mgl_contv(gr: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual axial-symmetric isosurfaces for 2d data specified parametrically
procedure  mgl_axial_xy_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual axial-symmetric isosurfaces for 2d data
procedure  mgl_axial_val(gr: HMGL;const v: HMDT;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw axial-symmetric isosurfaces for 2d data specified parametrically
procedure  mgl_axial_xy(gr: HMGL;const x: HMDT;const y: HMDT;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw axial-symmetric isosurfaces for 2d data
procedure  mgl_axial(gr: HMGL;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface of curve {r,z} rotatation around axis
procedure  mgl_torus(gr: HMGL;const r: HMDT;const z: HMDT;const col: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw grid lines for density plot at slice for 3d data specified parametrically
procedure  mgl_grid3_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw grid lines for density plot at slice for 3d data
procedure  mgl_grid3(gr: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw density plot at slice for 3d data specified parametrically
procedure  mgl_dens3_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw density plot at slice for 3d data
procedure  mgl_dens3(gr: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines at slice for 3d data specified parametrically
procedure  mgl_cont3_xyz_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines at slice for 3d data
procedure  mgl_cont3_val(gr: HMGL;const v: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines at slice for 3d data specified parametrically
procedure  mgl_cont3_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines at slice for 3d data
procedure  mgl_cont3(gr: HMGL;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours at slice for 3d data specified parametrically
procedure  mgl_contf3_xyz_val(gr: HMGL;const v: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours at slice for 3d data
procedure  mgl_contf3_val(gr: HMGL;const v: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours at slice for 3d data specified parametrically
procedure  mgl_contf3_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours at slice for 3d data
procedure  mgl_contf3(gr: HMGL;const a: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
{== ../../include/mgl2/fit.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
function  mgl_fit_1(gr: HMGL;const y: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_2(gr: HMGL;const z: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_3(gr: HMGL;const a: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xy(gr: HMGL;const x: HMDT;const y: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xyza(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_ys(gr: HMGL;const y: HMDT;const s: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xys(gr: HMGL;const x: HMDT;const y: HMDT;const s: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xyzs(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const s: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_fit_xyzas(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const s: HMDT;const eq: PChar;const vars: PChar;ini: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function mgl_get_fit(gr: HMGL): PChar; cdecl; external libmgl;
function  mgl_hist_x(gr: HMGL;const x: HMDT;const a: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_hist_xy(gr: HMGL;const x: HMDT;const y: HMDT;const a: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
function  mgl_hist_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const opt: PChar): HMDT; cdecl; external libmgl;
procedure  mgl_puts_fit(gr: HMGL;x: double;y: double;z: double;const prefix: PChar;const font: PChar;size: double); cdecl; external libmgl;
function  mgl_get_fit_chi(): mreal; cdecl; external libmgl;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
{== ../../include/mgl2/plot.h ==}
//-----------------------------------------------------------------------------
/// Draw curve for formula with x in x-axis range
procedure  mgl_fplot(gr: HMGL;const eqY: PChar;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw curve for formulas parametrically depended on t in range [0,1]
procedure  mgl_fplot_xyz(gr: HMGL;const eqX: PChar;const eqY: PChar;const eqZ: PChar;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw radar chart (plot in curved coordinates)
procedure  mgl_radar(graph: HMGL;const a: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw usual curve {x,y,z}
procedure  mgl_plot_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw usual curve {x,y}
procedure  mgl_plot_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw usual curve {x,y} with x in x-axis range
procedure  mgl_plot(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw curve {x,y,z} which is colored by c (like tension plot)
procedure  mgl_tens_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw curve {x,y} which is colored by c (like tension plot)
procedure  mgl_tens_xy(graph: HMGL;const x: HMDT;const y: HMDT;const c: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw curve {x,y} with x in x-axis range which is colored by c (like tension plot)
procedure  mgl_tens(graph: HMGL;const y: HMDT;const c: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tape(s) which rotates as (bi-)normales of curve {x,y,z}
procedure  mgl_tape_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tape(s) which rotates as (bi-)normales of curve {x,y}
procedure  mgl_tape_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tape(s) which rotates as (bi-)normales of curve {x,y} with x in x-axis range
procedure  mgl_tape(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw box-plot (special 5-value plot used in statistic) for data specified parametrically
procedure  mgl_boxplot_xy(graph: HMGL;const x: HMDT;const a: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw box-plot (special 5-value plot used in statistic)
procedure  mgl_boxplot(graph: HMGL;const a: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area between curve {x,y,z} and axis plane
procedure  mgl_area_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area between curve {x,y} and axis plane
procedure  mgl_area_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area between curve {x,y} with x in x-axis range and axis plane
procedure  mgl_area(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area (draw ribbon) between curves {x1,y1,z1} and {x2,y2,z2}
procedure  mgl_region_3d(graph: HMGL;const x1: HMDT;const y1: HMDT;const z1: HMDT;const x2: HMDT;const y2: HMDT;const z2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area between curves {x,y1} and {x,y2}
procedure  mgl_region_xy(graph: HMGL;const x: HMDT;const y1: HMDT;const y2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Fill area between curves {x,y1} and {x,y2} with x in x-axis range
procedure  mgl_region(graph: HMGL;const y1: HMDT;const y2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical lines from points {x,y,z} to axis plane
procedure  mgl_stem_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical lines from points {x,y} to axis plane
procedure  mgl_stem_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical lines from points {x,y} with x in x-axis range to axis plane
procedure  mgl_stem(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw stairs for points in arrays {x,y,z}
procedure  mgl_step_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw stairs for points in arrays {x,y}
procedure  mgl_step_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw stairs for points in arrays {x,y} with x in x-axis range
procedure  mgl_step(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical bars from points {x,y,z} to axis plane
procedure  mgl_bars_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical bars from points {x,y} to axis plane
procedure  mgl_bars_xy(graph: HMGL;const x: HMDT;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical bars from points {x,y} with x in x-axis range to axis plane
procedure  mgl_bars(graph: HMGL;const y: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw horizontal bars from points {v,y} to axis plane
procedure  mgl_barh_yx(graph: HMGL;const y: HMDT;const v: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw horizontal bars from points {v,y} with y in y-axis range to axis plane
procedure  mgl_barh(graph: HMGL;const v: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw Open-High-Low-Close (OHLC) diagram
procedure  mgl_ohlc_x(graph: HMGL;const x: HMDT;const open: HMDT;const high: HMDT;const low: HMDT;const close: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw Open-High-Low-Close (OHLC) diagram with x in x-axis range
procedure  mgl_ohlc(graph: HMGL;const open: HMDT;const high: HMDT;const low: HMDT;const close: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw chart for data a
procedure  mgl_chart(graph: HMGL;const a: HMDT;const col: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw error boxes {ex,ey} at points {x,y}
procedure  mgl_error_exy(graph: HMGL;const x: HMDT;const y: HMDT;const ex: HMDT;const ey: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw error boxes {ey} at points {x,y}
procedure  mgl_error_xy(graph: HMGL;const x: HMDT;const y: HMDT;const ey: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw error boxes {ey} at points {x,y} with x in x-axis range
procedure  mgl_error(graph: HMGL;const y: HMDT;const ey: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw marks with size r at points {x,y,z}
procedure  mgl_mark_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw marks with size r at points {x,y}
procedure  mgl_mark_xy(graph: HMGL;const x: HMDT;const y: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw marks with size r at points {x,y} with x in x-axis range
procedure  mgl_mark_y(graph: HMGL;const y: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with variable radius r around curve {x,y,z}
procedure  mgl_tube_xyzr(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with variable radius r around curve {x,y}
procedure  mgl_tube_xyr(graph: HMGL;const x: HMDT;const y: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with variable radius r around curve {x,y} with x in x-axis range
procedure  mgl_tube_r(graph: HMGL;const y: HMDT;const r: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with constant radius r around curve {x,y,z}
procedure  mgl_tube_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;r: double;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with constant radius r around curve {x,y}
procedure  mgl_tube_xy(graph: HMGL;const x: HMDT;const y: HMDT;r: double;const penl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw tube with constant radius r around curve {x,y} with x in x-axis range
procedure  mgl_tube(graph: HMGL;const y: HMDT;r: double;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw candle plot for data specified parametrically
procedure  mgl_candle_xyv(gr: HMGL;const x: HMDT;const v1: HMDT;const v2: HMDT;const y1: HMDT;const y2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw candle plot
procedure  mgl_candle_yv(gr: HMGL;const v1: HMDT;const v2: HMDT;const y1: HMDT;const y2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw candle plot with v1=v[i], v2=v[i+1]
procedure  mgl_candle(gr: HMGL;const v: HMDT;const y1: HMDT;const y2: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/surf.h ==}
//-----------------------------------------------------------------------------
/// Draw surface by formula with x,y in axis range
procedure  mgl_fsurf(graph: HMGL;const fz: PChar;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface by formulas parametrically depended on u,v in range [0,1]
procedure  mgl_fsurf_xyz(graph: HMGL;const fx: PChar;const fy: PChar;const fz: PChar;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw grid lines for density plot of 2d data specified parametrically
procedure  mgl_grid_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw grid lines for density plot of 2d data
procedure  mgl_grid(graph: HMGL;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw mesh lines for 2d data specified parametrically
procedure  mgl_mesh_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw mesh lines for 2d data
procedure  mgl_mesh(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw mesh lines for 2d data specified parametrically
procedure  mgl_fall_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw mesh lines for 2d data
procedure  mgl_fall(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw belts for 2d data specified parametrically
procedure  mgl_belt_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw belts for 2d data
procedure  mgl_belt(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data specified parametrically with color proportional to z
procedure  mgl_surf_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data with color proportional to z
procedure  mgl_surf(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for 2d data specified parametrically
procedure  mgl_dens_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for 2d data
procedure  mgl_dens(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical boxes for 2d data specified parametrically
procedure  mgl_boxs_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical boxes for 2d data
procedure  mgl_boxs(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical tiles for 2d data specified parametrically
procedure  mgl_tile_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical tiles for 2d data
procedure  mgl_tile(graph: HMGL;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical tiles with variable size r for 2d data specified parametrically
procedure  mgl_tiles_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const r: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vertical tiles with variable size r for 2d data
procedure  mgl_tiles(graph: HMGL;const z: HMDT;const r: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data specified parametrically with color proportional to c
procedure  mgl_surfc_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data with color proportional to c
procedure  mgl_surfc(graph: HMGL;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data specified parametrically with alpha proportional to c
procedure  mgl_surfa_xy(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface for 2d data with alpha proportional to c
procedure  mgl_surfa(graph: HMGL;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for spectra-gramm specified parametrically
procedure  mgl_stfa_xy(graph: HMGL;const x: HMDT;const y: HMDT;const re: HMDT;const im: HMDT;dn: integer;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for spectra-gramm
procedure  mgl_stfa(graph: HMGL;const re: HMDT;const im: HMDT;dn: integer;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Color map of matrix a to matrix b, both matrix can parametrically depend on coordinates
procedure  mgl_map_xy(graph: HMGL;const x: HMDT;const y: HMDT;const a: HMDT;const b: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Color map of matrix a to matrix b
procedure  mgl_map(graph: HMGL;const a: HMDT;const b: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/volume.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Draw isosurface for 3d data specified parametrically
procedure  mgl_surf3_xyz_val(graph: HMGL;Val: double;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d data
procedure  mgl_surf3_val(graph: HMGL;Val: double;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data specified parametrically
procedure  mgl_surf3_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data
procedure  mgl_surf3(graph: HMGL;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d data specified parametrically with alpha proportional to b
procedure  mgl_surf3a_xyz_val(graph: HMGL;Val: double;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d data with alpha proportional to b
procedure  mgl_surf3a_val(graph: HMGL;Val: double;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data specified parametrically with alpha proportional to b
procedure  mgl_surf3a_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data with alpha proportional to b
procedure  mgl_surf3a(graph: HMGL;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d data specified parametrically with color proportional to b
procedure  mgl_surf3c_xyz_val(graph: HMGL;Val: double;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d data with color proportional to b
procedure  mgl_surf3c_val(graph: HMGL;Val: double;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data specified parametrically with color proportional to b
procedure  mgl_surf3c_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurfaces for 3d data with color proportional to b
procedure  mgl_surf3c(graph: HMGL;const a: HMDT;const b: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw a semi-transparent cloud for 3d data specified parametrically
procedure  mgl_cloud_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw a semi-transparent cloud for 3d data
procedure  mgl_cloud(graph: HMGL;const a: HMDT;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw isosurface for 3d beam in curvilinear coordinates
procedure  mgl_beam_val(graph: HMGL;Val: double;const tr: HMDT;const g1: HMDT;const g2: HMDT;const a: HMDT;r: double;const stl: PChar;norm: integer); cdecl; external libmgl;
/// Draw several isosurfaces for 3d beam in curvilinear coordinates
procedure  mgl_beam(graph: HMGL;const tr: HMDT;const g1: HMDT;const g2: HMDT;const a: HMDT;r: double;const stl: PChar;norm: integer;num: integer); cdecl; external libmgl;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
{== ../../include/mgl2/vect.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Plot vectors at position {x,y} along {ax,ay} with length/color proportional to |a|
procedure  mgl_traj_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot vectors at position {x,y,z} along {ax,ay,az} with length/color proportional to |a|
procedure  mgl_traj_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot vector field {ax,ay} parametrically depended on coordinate {x,y} with length/color proportional to |a|
procedure  mgl_vect_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot vector field {ax,ay} with length/color proportional to |a|
procedure  mgl_vect_2d(gr: HMGL;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with length/color proportional to |a|
procedure  mgl_vect_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot vector field {ax,ay,az} with length/color proportional to |a|
procedure  mgl_vect_3d(gr: HMGL;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for vector field {ax,ay} parametrically depended on coordinate {x,y} with color proportional to |a|
procedure  mgl_flow_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for vector field {ax,ay} with color proportional to |a|
procedure  mgl_flow_2d(gr: HMGL;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color proportional to |a|
procedure  mgl_flow_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for vector field {ax,ay,az} with color proportional to |a|
procedure  mgl_flow_3d(gr: HMGL;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flow from point p for vector field {ax,ay} parametrically depended on coordinate {x,y} with color proportional to |a|
procedure  mgl_flowp_xy(gr: HMGL;x0: double;y0: double;z0: double;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flow from point p for vector field {ax,ay} with color proportional to |a|
procedure  mgl_flowp_2d(gr: HMGL;x0: double;y0: double;z0: double;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flow from point p for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color proportional to |a|
procedure  mgl_flowp_xyz(gr: HMGL;x0: double;y0: double;z0: double;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flow from point p for vector field {ax,ay,az} with color proportional to |a|
procedure  mgl_flowp_3d(gr: HMGL;x0: double;y0: double;z0: double;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flow pipes for vector field {ax,ay} parametrically depended on coordinate {x,y} with color and radius proportional to |a|
procedure  mgl_pipe_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;r0: double;const opt: PChar); cdecl; external libmgl;
/// Plot flow pipes for vector field {ax,ay} with color and radius proportional to |a|
procedure  mgl_pipe_2d(gr: HMGL;const ax: HMDT;const ay: HMDT;const sch: PChar;r0: double;const opt: PChar); cdecl; external libmgl;
/// Plot flow pipes for vector field {ax,ay,az} parametrically depended on coordinate {x,y,z} with color and radius proportional to |a|
procedure  mgl_pipe_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;r0: double;const opt: PChar); cdecl; external libmgl;
/// Plot flow pipes for vector field {ax,ay,az} with color and radius proportional to |a|
procedure  mgl_pipe_3d(gr: HMGL;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;r0: double;const opt: PChar); cdecl; external libmgl;
/// Plot flows for gradient of scalar field phi parametrically depended on coordinate {x,y,z}
procedure  mgl_grad_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ph: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for gradient of scalar field phi parametrically depended on coordinate {x,y}
procedure  mgl_grad_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ph: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot flows for gradient of scalar field phi
procedure  mgl_grad(gr: HMGL;const ph: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw vector plot at slice for 3d data specified parametrically
procedure  mgl_vect3_xyz(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw vector plot at slice for 3d data
procedure  mgl_vect3(gr: HMGL;const ax: HMDT;const ay: HMDT;const az: HMDT;const sch: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
{== ../../include/mgl2/prim.h ==}
//-----------------------------------------------------------------------------
/// Draws the mark at position {x,y,z}
procedure  mgl_mark(gr: HMGL;x: double;y: double;z: double;const mark: PChar); cdecl; external libmgl;
/// Draws red point (ball) at position {x,y,z}
procedure  mgl_ball(gr: HMGL;x: double;y: double;z: double); cdecl; external libmgl;
/// Draws the line between 2 points by specified pen
procedure  mgl_line(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;const pen: PChar;n: integer); cdecl; external libmgl;
/// Draws the spline curve between 2 points by specified pen
procedure  mgl_curve(gr: HMGL;x1: double;y1: double;z1: double;dx1: double;dy1: double;dz1: double;x2: double;y2: double;z2: double;dx2: double;dy2: double;dz2: double;const pen: PChar;n: integer); cdecl; external libmgl;
/// Draws the 3d error box {ex,ey,ez} for point {x,y,z}
procedure  mgl_error_box(gr: HMGL;x: double;y: double;z: double;ex: double;ey: double;ez: double;const pen: PChar); cdecl; external libmgl;
/// Draws the face between points with color stl (include interpolation up to 4 colors).
procedure  mgl_face(gr: HMGL;x0: double;y0: double;z0: double;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;x3: double;y3: double;z3: double;const stl: PChar); cdecl; external libmgl;
/// Draws the face in y-z plane at point p with color stl (include interpolation up to 4 colors).
procedure  mgl_facex(gr: HMGL;x0: double;y0: double;z0: double;wy: double;wz: double;const stl: PChar;dx: double;dy: double); cdecl; external libmgl;
/// Draws the face in x-z plane at point p with color stl (include interpolation up to 4 colors).
procedure  mgl_facey(gr: HMGL;x0: double;y0: double;z0: double;wx: double;wz: double;const stl: PChar;dx: double;dy: double); cdecl; external libmgl;
/// Draws the face in x-y plane at point p with color stl (include interpolation up to 4 colors).
procedure  mgl_facez(gr: HMGL;x0: double;y0: double;z0: double;wx: double;wy: double;const stl: PChar;dx: double;dy: double); cdecl; external libmgl;
/// Draws the sphere at point {x,y,z} with color stl and radius r
procedure  mgl_sphere(gr: HMGL;x: double;y: double;z: double;r: double;const stl: PChar); cdecl; external libmgl;
/// Draws the drop at point {x,y,z} in direction {dx,dy,dz} with color stl and radius r
procedure  mgl_drop(gr: HMGL;x: double;y: double;z: double;dx: double;dy: double;dz: double;r: double;const stl: PChar;shift: double;ap: double); cdecl; external libmgl;
/// Draws the cone between points p1,p2 with radius r1,r2 and with style stl
procedure  mgl_cone(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;r1: double;r2: double;const stl: PChar); cdecl; external libmgl;
/// Draws the ellipse between points p1,p2 with color stl and width r
procedure  mgl_ellipse(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;r: double;const stl: PChar); cdecl; external libmgl;
/// Draws the rhomb between points p1,p2 with color stl and width r
procedure  mgl_rhomb(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;r: double;const stl: PChar); cdecl; external libmgl;
/// Draws the polygon based on points p1,p2 with color stl
procedure  mgl_polygon(gr: HMGL;x1: double;y1: double;z1: double;x2: double;y2: double;z2: double;n: integer;const stl: PChar); cdecl; external libmgl;
procedure  mgl_arc_ext(gr: HMGL;x0: double;y0: double;z0: double;xr: double;yr: double;zr: double;x1: double;y1: double;z1: double;a: double;const stl: PChar); cdecl; external libmgl;
procedure  mgl_arc(gr: HMGL;x0: double;y0: double;x1: double;y1: double;a: double;const stl: PChar); cdecl; external libmgl;
/// Draw cones from points {x,y,z} to axis plane
procedure  mgl_cones_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw cones from points {x,z} to axis plane
procedure  mgl_cones_xz(graph: HMGL;const x: HMDT;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw cones from points {x,z} with x in x-axis range to axis plane
procedure  mgl_cones(graph: HMGL;const z: HMDT;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot dew drops for vector field {ax,ay} parametrically depended on coordinate {x,y}
procedure  mgl_dew_xy(gr: HMGL;const x: HMDT;const y: HMDT;const ax: HMDT;const ay: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Plot dew drops for vector field {ax,ay}
procedure  mgl_dew_2d(gr: HMGL;const ax: HMDT;const ay: HMDT;const sch: PChar;const optl: PChar); cdecl; external libmgl;
/// Print text in position {x,y,z} with specified font
procedure  mgl_puts_dir(graph: HMGL;x: double;y: double;z: double;dx: double;dy: double;dz: double;const text: PChar;const font: PChar;size: double); cdecl; external libmgl;
procedure  mgl_putsw_dir(graph: HMGL;x: double;y: double;z: double;dx: double;dy: double;dz: double;const text: PWideChar;const font: PChar;size: double); cdecl; external libmgl;
/// Draw textual marks with size r at points {x,y,z}
procedure  mgl_textmark_xyzr(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const r: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textmarkw_xyzr(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const r: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw textual marks with size r at points {x,y}
procedure  mgl_textmark_xyr(graph: HMGL;const x: HMDT;const y: HMDT;const r: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textmarkw_xyr(graph: HMGL;const x: HMDT;const y: HMDT;const r: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw textual marks with size r at points {x,y} with x in x-axis range
procedure  mgl_textmark_yr(graph: HMGL;const y: HMDT;const r: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textmarkw_yr(graph: HMGL;const y: HMDT;const r: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw textual marks with size r=1 at points {x,y} with x in x-axis range
procedure  mgl_textmark(graph: HMGL;const y: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_textmarkw(graph: HMGL;const y: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw labels for points coordinate(s) at points {x,y,z}
procedure  mgl_label_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_labelw_xyz(graph: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw labels for points coordinate(s) at points {x,y}
procedure  mgl_label_xy(graph: HMGL;const x: HMDT;const y: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_labelw_xy(graph: HMGL;const x: HMDT;const y: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw labels for points coordinate(s) at points {x,y} with x in x-axis range
procedure  mgl_label_y(graph: HMGL;const y: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_labelw_y(graph: HMGL;const y: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw table for values val along given direction with row labels text at position {x,y}
procedure  mgl_table(gr: HMGL;x: double;y: double;const val: HMDT;const text: PChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
procedure  mgl_tablew(gr: HMGL;x: double;y: double;const val: HMDT;const text: PWideChar;const fnt: PChar;const opt: PChar); cdecl; external libmgl;
/// Draws bitmap (logo) which is stretched along whole axis range
procedure  mgl_logo(gr: HMGL;w: integer;h: integer;const rgba: PByte;smooth: integer;const opt: PChar); cdecl; external libmgl;
procedure  mgl_logo_file(gr: HMGL;const fname: PChar;smooth: integer;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/other.h ==}
//-----------------------------------------------------------------------------
/// Draw triangle mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_triplot_xyzc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw triangle mesh for points in arrays {x,y,z} with color proportional to z.
procedure  mgl_triplot_xyz(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw triangle mesh for points in arrays {x,y}
procedure  mgl_triplot_xy(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw quad mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_quadplot_xyzc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw quad mesh for points in arrays {x,y,z} with color proportional to z.
procedure  mgl_quadplot_xyz(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw quad mesh for points in arrays {x,y}.
procedure  mgl_quadplot_xy(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_tricont_xyzcv(gr: HMGL;const v: HMDT;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for triangle mesh for points in arrays {x,y,z}.
procedure  mgl_tricont_xycv(gr: HMGL;const v: HMDT;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_tricont_xyzc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for triangle mesh for points in arrays {x,y,z}.
procedure  mgl_tricont_xyc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_tricontv_xyzcv(gr: HMGL;const v: HMDT;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour tubes for triangle mesh for points in arrays {x,y,z}.
procedure  mgl_tricontv_xycv(gr: HMGL;const v: HMDT;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c.
procedure  mgl_tricontv_xyzc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw contour tubes for triangle mesh for points in arrays {x,y,z}.
procedure  mgl_tricontv_xyc(gr: HMGL;const nums: HMDT;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw dots in points {x,y,z}.
procedure  mgl_dots(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw semitransparent dots in points {x,y,z} with specified alpha a.
procedure  mgl_dots_a(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw semitransparent dots in points {x,y,z} with specified color c and alpha a.
procedure  mgl_dots_ca(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const c: HMDT;const a: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw surface reconstructed for points in arrays {x,y,z}.
procedure  mgl_crust(gr: HMGL;const x: HMDT;const y: HMDT;const z: HMDT;const sch: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for data at x = sVal
procedure  mgl_dens_x(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for data at y = sVal
procedure  mgl_dens_y(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw density plot for data at z = sVal
procedure  mgl_dens_z(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for data at x = sVal
procedure  mgl_cont_x(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for data at y = sVal
procedure  mgl_cont_y(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw contour lines for data at z = sVal
procedure  mgl_cont_z(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for data at x = sVal
procedure  mgl_cont_x_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for data at y = sVal
procedure  mgl_cont_y_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual contour lines for data at z = sVal
procedure  mgl_cont_z_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for data at x = sVal
procedure  mgl_contf_x(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for data at y = sVal
procedure  mgl_contf_y(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw solid contours for data at z = sVal
procedure  mgl_contf_z(graph: HMGL;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for data at x = sVal
procedure  mgl_contf_x_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for data at y = sVal
procedure  mgl_contf_y_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
/// Draw manual solid contours for data at z = sVal
procedure  mgl_contf_z_val(graph: HMGL;const v: HMDT;const a: HMDT;const stl: PChar;sVal: double;const opt: PChar); cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/canvas_cf.h ==}
//-----------------------------------------------------------------------------
/// Create HMGL object with specified sizes
function  mgl_create_graph(width: integer;height: integer): HMGL; cdecl; external libmgl;
/// Delete HMGL object
procedure  mgl_delete_graph(gr: HMGL); cdecl; external libmgl;
/// Set size of frame in pixels. Normally this function is called internally.
procedure  mgl_set_size(gr: HMGL;width: integer;height: integer); cdecl; external libmgl;
/// Set default parameters for plotting
procedure  mgl_set_def_param(gr: HMGL); cdecl; external libmgl;
/// Combine plots from 2 canvases. Result will be saved into gr
procedure  mgl_combine_gr(gr: HMGL;gr2: HMGL); cdecl; external libmgl;
/// Force preparing the image. It can be useful for OpenGL mode mostly.
procedure  mgl_finish(gr: HMGL); cdecl; external libmgl;
/// Force preparing the image and save result into background one.
procedure  mgl_rasterize(gr: HMGL); cdecl; external libmgl;
/// Set tick length
procedure  mgl_set_tick_len(gr: HMGL;len: double;stt: double); cdecl; external libmgl;
/// Set axis and ticks style
procedure  mgl_set_axis_stl(gr: HMGL;const stl: PChar;const tck: PChar;const sub: PChar); cdecl; external libmgl;
/// Auto adjust ticks
procedure  mgl_adjust_ticks(gr: HMGL;const dir: PChar); cdecl; external libmgl;
/// Auto adjust ticks and set ticks format ("+E0123456789-fF")
procedure  mgl_adjust_ticks_ext(gr: HMGL;const dir: PChar;const stl: PChar); cdecl; external libmgl;
/// Set the ticks parameters
procedure  mgl_set_ticks(gr: HMGL;dir: char;d: double;ns: integer;org: double); cdecl; external libmgl;
/// Set the ticks parameters and specify ticks factor string
procedure  mgl_set_ticks_fact(gr: HMGL;dir: char;d: double;ns: integer;org: double;const fact: PChar); cdecl; external libmgl;
procedure  mgl_set_ticks_factw(gr: HMGL;dir: char;d: double;ns: integer;org: double;const fact: PWideChar); cdecl; external libmgl;
/// Set manual ticks text (\n separated). Use "" to disable this feature.
procedure  mgl_set_ticks_str(gr: HMGL;dir: char;const lbl: PChar;add: integer); cdecl; external libmgl;
procedure  mgl_set_ticks_wcs(gr: HMGL;dir: char;const lbl: PWideChar;add: integer); cdecl; external libmgl;
/// Set manual ticks position and text (\n separated). Use "" to disable this feature.
procedure  mgl_set_ticks_val(gr: HMGL;dir: char;const val: HMDT;const lbl: PChar;add: integer); cdecl; external libmgl;
procedure  mgl_set_ticks_valw(gr: HMGL;dir: char;const val: HMDT;const lbl: PWideChar;add: integer); cdecl; external libmgl;
/// Add manual tick at given position. Use "" to disable this feature.
procedure  mgl_add_tick(gr: HMGL;dir: char;val: double;const lbl: PChar); cdecl; external libmgl;
procedure  mgl_add_tickw(gr: HMGL;dir: char;val: double;const lbl: PWideChar); cdecl; external libmgl;
/// Tune ticks
procedure  mgl_tune_ticks(gr: HMGL;tune: integer;fact_pos: double); cdecl; external libmgl;
/// Set templates for ticks
procedure  mgl_set_tick_templ(gr: HMGL;dir: char;const templ: PChar); cdecl; external libmgl;
procedure  mgl_set_tick_templw(gr: HMGL;dir: char;const templ: PWideChar); cdecl; external libmgl;
/// Set time templates for ticks
procedure  mgl_set_ticks_time(gr: HMGL;dir: char;d: double;const t: PChar); cdecl; external libmgl;
/// Set additional shift of tick labels
procedure  mgl_set_tick_shift(gr: HMGL;sx: double;sy: double;sz: double;sc: double); cdecl; external libmgl;
/// Draws bounding box outside the plotting volume
procedure  mgl_box(gr: HMGL); cdecl; external libmgl;
/// Draws bounding box outside the plotting volume with color c
procedure  mgl_box_str(gr: HMGL;const col: PChar;ticks: integer); cdecl; external libmgl;
/// Draw axises with ticks in direction(s) dir.
procedure  mgl_axis(gr: HMGL;const dir: PChar;const stl: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw grid lines perpendicular to direction(s) dir.
procedure  mgl_axis_grid(gr: HMGL;const dir: PChar;const pen: PChar;const opt: PChar); cdecl; external libmgl;
/// Print the label text for axis dir.
procedure  mgl_label(gr: HMGL;dir: char;const text: PChar;pos: double;const opt: PChar); cdecl; external libmgl;
procedure  mgl_labelw(gr: HMGL;dir: char;const text: PWideChar;pos: double;const opt: PChar); cdecl; external libmgl;
/// Draw colorbar at edge of axis
procedure  mgl_colorbar(gr: HMGL;const sch: PChar); cdecl; external libmgl;
/// Draw colorbar at manual position
procedure  mgl_colorbar_ext(gr: HMGL;const sch: PChar;x: double;y: double;w: double;h: double); cdecl; external libmgl;
/// Draw colorbar with manual colors at edge of axis
procedure  mgl_colorbar_val(gr: HMGL;const dat: HMDT;const sch: PChar); cdecl; external libmgl;
/// Draw colorbar with manual colors at manual position
procedure  mgl_colorbar_val_ext(gr: HMGL;const dat: HMDT;const sch: PChar;x: double;y: double;w: double;h: double); cdecl; external libmgl;
/// Add string to legend
procedure  mgl_add_legend(gr: HMGL;const text: PChar;const style: PChar); cdecl; external libmgl;
procedure  mgl_add_legendw(gr: HMGL;const text: PWideChar;const style: PChar); cdecl; external libmgl;
/// Clear saved legend string
procedure  mgl_clear_legend(gr: HMGL); cdecl; external libmgl;
/// Draw legend of accumulated strings at position {x,y}
procedure  mgl_legend_pos(gr: HMGL;x: double;y: double;const font: PChar;const opt: PChar); cdecl; external libmgl;
/// Draw legend of accumulated strings
procedure  mgl_legend(gr: HMGL;where: integer;const font: PChar;const opt: PChar); cdecl; external libmgl;
/// Set number of marks in legend sample
procedure  mgl_set_legend_marks(gr: HMGL;num: integer); cdecl; external libmgl;
/// Show current image
procedure  mgl_show_image(gr: HMGL;const viewer: PChar;keep: integer); cdecl; external libmgl;
/// Write the frame in file (depending extension, write current frame if fname is empty)
procedure  mgl_write_frame(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using BMP format
procedure  mgl_write_tga(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using BMP format
procedure  mgl_write_bmp(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using JPEG format
procedure  mgl_write_jpg(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using PNG format with transparency
procedure  mgl_write_png(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using PNG format without transparency
procedure  mgl_write_png_solid(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using PostScript format as bitmap
procedure  mgl_write_bps(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using PostScript format
procedure  mgl_write_eps(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using SVG format
procedure  mgl_write_svg(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using LaTeX format
procedure  mgl_write_tex(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using OBJ format
procedure  mgl_write_obj(gr: HMGL;const fname: PChar;const descr: PChar;use_png: integer); cdecl; external libmgl;
/// Write the frame in file using OBJ format (old version)
procedure  mgl_write_obj_old(gr: HMGL;const fname: PChar;const descr: PChar;use_png: integer); cdecl; external libmgl;
/// Write the frame in file using STL format (faces only)
procedure  mgl_write_stl(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using OFF format
procedure  mgl_write_off(gr: HMGL;const fname: PChar;const descr: PChar;colored: integer); cdecl; external libmgl;
/// Write the frame in file using XYZ format
procedure  mgl_write_xyz(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Write the frame in file using PRC format
procedure  mgl_write_prc(gr: HMGL;const fname: PChar;const descr: PChar;make_pdf: integer); cdecl; external libmgl;
/// Write the frame in file using GIF format (only for current frame!)
procedure  mgl_write_gif(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Start write frames to cinema using GIF format
procedure  mgl_start_gif(gr: HMGL;const fname: PChar;ms: integer); cdecl; external libmgl;
/// Stop writing cinema using GIF format
procedure  mgl_close_gif(gr: HMGL); cdecl; external libmgl;
/// Export points and primitives in file using MGLD format
procedure  mgl_export_mgld(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
/// Import points and primitives from file using MGLD format
procedure  mgl_import_mgld(gr: HMGL;const fname: PChar;add: integer); cdecl; external libmgl;
/// Export in JSON format suitable for later drawing by JavaScript
procedure  mgl_write_json(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
procedure  mgl_write_json_z(gr: HMGL;const fname: PChar;const descr: PChar); cdecl; external libmgl;
function mgl_get_json(gr: HMGL): PChar; cdecl; external libmgl;
/// Get RGB values of current bitmap
function mgl_get_rgb(gr: HMGL): PByte; cdecl; external libmgl;
/// Get RGBA values of current bitmap
function mgl_get_rgba(gr: HMGL): PByte; cdecl; external libmgl;
/// Get RGB values of current bitmap
function mgl_get_background(gr: HMGL): PByte; cdecl; external libmgl;
/// Set object/subplot id
procedure  mgl_set_obj_id(gr: HMGL;id: integer); cdecl; external libmgl;
/// Get object id
function  mgl_get_obj_id(gr: HMGL;x: integer;y: integer): integer; cdecl; external libmgl;
/// Get subplot id
function  mgl_get_spl_id(gr: HMGL;x: integer;y: integer): integer; cdecl; external libmgl;
/// Get width of the image
function  mgl_get_width(gr: HMGL): integer; cdecl; external libmgl;
/// Get height of the image
function  mgl_get_height(gr: HMGL): integer; cdecl; external libmgl;
/// Calculate 3D coordinate {x,y,z} for screen point {xs,ys}
procedure  mgl_calc_xyz(gr: HMGL;xs: integer;ys: integer;x: Pmreal;y: Pmreal;z: Pmreal); cdecl; external libmgl;
/// Calculate screen point {xs,ys} for 3D coordinate {x,y,z}
procedure  mgl_calc_scr(gr: HMGL;x: double;y: double;z: double;xs: Pint;ys: Pint); cdecl; external libmgl;
/// Check if {xs,ys} is close to active point with accuracy d, and return its position or -1
function  mgl_is_active(gr: HMGL;xs: integer;ys: integer;d: integer): integer; cdecl; external libmgl;
/// Create new frame.
function  mgl_new_frame(gr: HMGL): integer; cdecl; external libmgl;
/// Finish frame drawing
procedure  mgl_end_frame(gr: HMGL); cdecl; external libmgl;
/// Get the number of created frames
function  mgl_get_num_frame(gr: HMGL): integer; cdecl; external libmgl;
/// Reset frames counter (start it from zero)
procedure  mgl_reset_frames(gr: HMGL); cdecl; external libmgl;
/// Get drawing data for i-th frame (work if MGL_VECT_FRAME is set on)
procedure  mgl_get_frame(gr: HMGL;i: integer); cdecl; external libmgl;
/// Set drawing data for i-th frame (work if MGL_VECT_FRAME is set on)
procedure  mgl_set_frame(gr: HMGL;i: integer); cdecl; external libmgl;
/// Append drawing data from i-th frame (work if MGL_VECT_FRAME is set on)
procedure  mgl_show_frame(gr: HMGL;i: integer); cdecl; external libmgl;
/// Delete primitives for i-th frame (work if MGL_VECT_FRAME is set on)
procedure  mgl_del_frame(gr: HMGL;i: integer); cdecl; external libmgl;
/// Clear list of primitives for current drawing
procedure  mgl_clear_frame(gr: HMGL); cdecl; external libmgl;
/// Set the transparency type (0 - usual, 1 - glass, 2 - lamp)
procedure  mgl_set_transp_type(gr: HMGL;kind: integer); cdecl; external libmgl;
/// Set the transparency on/off.
procedure  mgl_set_alpha(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Set the fog distance or switch it off (if d=0).
procedure  mgl_set_fog(gr: HMGL;d: double;dz: double); cdecl; external libmgl;
/// Set the using of light on/off.
procedure  mgl_set_light(gr: HMGL;enable: integer); cdecl; external libmgl;
/// Switch on/off the specified light source.
procedure  mgl_set_light_n(gr: HMGL;n: integer;enable: integer); cdecl; external libmgl;
/// Add white light source at infinity.
procedure  mgl_add_light(gr: HMGL;n: integer;x: double;y: double;z: double); cdecl; external libmgl;
/// Add light source at infinity (more settings).
procedure  mgl_add_light_ext(gr: HMGL;n: integer;x: double;y: double;z: double;c: char;br: double;ap: double); cdecl; external libmgl;
/// Add local light source.
procedure  mgl_add_light_loc(gr: HMGL;n: integer;x: double;y: double;z: double;dx: double;dy: double;dz: double;c: char;br: double;ap: double); cdecl; external libmgl;
/// Pop transformation matrix from stack
procedure  mgl_mat_pop(gr: HMGL); cdecl; external libmgl;
/// Push transformation matrix into stack
procedure  mgl_mat_push(gr: HMGL); cdecl; external libmgl;
/// Clear up the frame
procedure  mgl_clf(gr: HMGL); cdecl; external libmgl;
/// Clear up the frame but keep fog settings
procedure  mgl_clf_nfog(gr: HMGL); cdecl; external libmgl;
/// Clear up the frame and fill background by specified color
procedure  mgl_clf_rgb(gr: HMGL;r: double;g: double;b: double); cdecl; external libmgl;
/// Clear up the frame and fill background by specified color
procedure  mgl_clf_chr(gr: HMGL;col: char); cdecl; external libmgl;
/// Clear up the frame and fill background by specified color with manual transparency
procedure  mgl_clf_str(gr: HMGL;const col: PChar); cdecl; external libmgl;
/// Load background image
procedure  mgl_load_background(gr: HMGL;const fname: PChar;alpha: double); cdecl; external libmgl;
/// Put further plotting in some region of whole frame.
procedure  mgl_subplot(gr: HMGL;nx: integer;ny: integer;m: integer;const style: PChar); cdecl; external libmgl;
/// Put further plotting in some region of whole frame and shift it by distance {dx,dy}.
procedure  mgl_subplot_d(gr: HMGL;nx: integer;ny: integer;m: integer;const style: PChar;dx: double;dy: double); cdecl; external libmgl;
/// Like MGL_EXPORT mgl_subplot() but "join" several cells
procedure  mgl_multiplot(gr: HMGL;nx: integer;ny: integer;m: integer;dx: integer;dy: integer;const style: PChar); cdecl; external libmgl;
/// Put further plotting in a region of whole frame.
procedure  mgl_inplot(gr: HMGL;x1: double;x2: double;y1: double;y2: double); cdecl; external libmgl;
/// Put further plotting in a region of current subplot/inplot.
procedure  mgl_relplot(gr: HMGL;x1: double;x2: double;y1: double;y2: double); cdecl; external libmgl;
/// Put further plotting in column cell of previous subplot/inplot.
procedure  mgl_columnplot(gr: HMGL;num: integer;ind: integer;d: double); cdecl; external libmgl;
/// Put further plotting in matrix cell of previous subplot/inplot.
procedure  mgl_gridplot(gr: HMGL;nx: integer;ny: integer;m: integer;d: double); cdecl; external libmgl;
/// Put further plotting in cell of stick rotated on angles tet, phi.
procedure  mgl_stickplot(gr: HMGL;num: integer;ind: integer;tet: double;phi: double); cdecl; external libmgl;
/// Add title for current subplot/inplot.
procedure  mgl_title(gr: HMGL;const title: PChar;const stl: PChar;size: double); cdecl; external libmgl;
procedure  mgl_titlew(gr: HMGL;const title: PWideChar;const stl: PChar;size: double); cdecl; external libmgl;
/// Set factor of plot size
procedure  mgl_set_plotfactor(gr: HMGL;val: double); cdecl; external libmgl;
/// Set aspect ratio for further plotting.
procedure  mgl_aspect(gr: HMGL;Ax: double;Ay: double;Az: double); cdecl; external libmgl;
/// Rotate a further plotting.
procedure  mgl_rotate(gr: HMGL;TetX: double;TetZ: double;TetY: double); cdecl; external libmgl;
/// Rotate a further plotting around vector {x,y,z}.
procedure  mgl_rotate_vector(gr: HMGL;Tet: double;x: double;y: double;z: double); cdecl; external libmgl;
/// Set perspective (in range [0,1)) for plot. Set to zero for switching off.
procedure  mgl_perspective(gr: HMGL;val: double); cdecl; external libmgl;
/// Ask to set perspective (in range [0,1)) for plot. Set to zero for switching off.
procedure  mgl_ask_perspective(gr: HMGL;val: double); cdecl; external libmgl;
/// Set angle of view independently from Rotate().
procedure  mgl_view(gr: HMGL;TetX: double;TetZ: double;TetY: double); cdecl; external libmgl;
/// Zoom in/out a part of picture (use mgl_zoom(0, 0, 1, 1) for restore default)
procedure  mgl_zoom(gr: HMGL;x1: double;y1: double;x2: double;y2: double); cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Callback function for mouse click
/// Set callback functions for drawing and data reloading
/// Set delay for animation in seconds
procedure  mgl_wnd_set_delay(gr: HMGL;dt: double); cdecl; external libmgl;
/// Get delay for animation in seconds
function  mgl_wnd_get_delay(gr: HMGL): double; cdecl; external libmgl;
/// Set window properties
procedure  mgl_setup_window(gr: HMGL;clf_upd: integer;showpos: integer); cdecl; external libmgl;
/// Switch on/off transparency (do not overwrite user settings)
procedure  mgl_wnd_toggle_alpha(gr: HMGL); cdecl; external libmgl;
/// Switch on/off lighting (do not overwrite user settings)
procedure  mgl_wnd_toggle_light(gr: HMGL); cdecl; external libmgl;
/// Switch on/off zooming by mouse
procedure  mgl_wnd_toggle_zoom(gr: HMGL); cdecl; external libmgl;
/// Switch on/off rotation by mouse
procedure  mgl_wnd_toggle_rotate(gr: HMGL); cdecl; external libmgl;
/// Switch off all zooming and rotation
procedure  mgl_wnd_toggle_no(gr: HMGL); cdecl; external libmgl;
/// Update picture by calling user drawing function
procedure  mgl_wnd_update(gr: HMGL); cdecl; external libmgl;
/// Reload user data and update picture
procedure  mgl_wnd_reload(gr: HMGL); cdecl; external libmgl;
/// Adjust size of bitmap to window size
procedure  mgl_wnd_adjust(gr: HMGL); cdecl; external libmgl;
/// Show next frame (if one)
procedure  mgl_wnd_next_frame(gr: HMGL); cdecl; external libmgl;
/// Show previous frame (if one)
procedure  mgl_wnd_prev_frame(gr: HMGL); cdecl; external libmgl;
/// Run slideshow (animation) of frames
procedure  mgl_wnd_animation(gr: HMGL); cdecl; external libmgl;
/// Get last mouse position
procedure  mgl_get_last_mouse_pos(gr: HMGL;x: Pmreal;y: Pmreal;z: Pmreal); cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Create HMPR object for parsing MGL scripts
function  mgl_create_parser(): HMPR; cdecl; external libmgl;
/// Change counter of HMPR uses (for advanced users only). Non-zero counter prevent automatic object removing.
function  mgl_use_parser(p: HMPR;inc: integer): integer; cdecl; external libmgl;
/// Delete HMPR object
procedure  mgl_delete_parser(p: HMPR); cdecl; external libmgl;
/// Set value for parameter $N
procedure  mgl_parser_add_param(p: HMPR;id: integer;const str: PChar); cdecl; external libmgl;
procedure  mgl_parser_add_paramw(p: HMPR;id: integer;const str: PWideChar); cdecl; external libmgl;
/// Find variable with given name or add a new one
/// NOTE !!! You must not delete obtained data arrays !!!
function  mgl_parser_add_var(p: HMPR;const name: PChar): HMDT; cdecl; external libmgl;
function  mgl_parser_add_varw(p: HMPR;const name: PWideChar): HMDT; cdecl; external libmgl;
/// Find variable with given name or return NULL if no one
/// NOTE !!! You must not delete obtained data arrays !!!
/// Get variable with given id
/// NOTE !!! You must not delete obtained data arrays !!!
/// Get number of variables
function  mgl_parser_num_var(p: HMPR): integer; cdecl; external libmgl;
/// Delete variable with name
procedure  mgl_parser_del_var(p: HMPR;const name: PChar); cdecl; external libmgl;
procedure  mgl_parser_del_varw(p: HMPR;const name: PWideChar); cdecl; external libmgl;
/// Delete all data variables
procedure  mgl_parser_del_all(p: HMPR); cdecl; external libmgl;
/// Load new commands from external dynamic Library (must have "const mglCommand *mgl_cmd_extra" variable)
procedure  mgl_parser_load(pr: HMPR;const dll_name: PChar); cdecl; external libmgl;
/// Parse and draw single line of the MGL script
function  mgl_parse_line(gr: HMGL;p: HMPR;const str: PChar;pos: integer): integer; cdecl; external libmgl;
function  mgl_parse_linew(gr: HMGL;p: HMPR;const str: PWideChar;pos: integer): integer; cdecl; external libmgl;
/// Execute and draw script from the file
/// Execute MGL script text with '\n' separated lines
procedure  mgl_parse_text(gr: HMGL;p: HMPR;const str: PChar); cdecl; external libmgl;
procedure  mgl_parse_textw(gr: HMGL;p: HMPR;const str: PWideChar); cdecl; external libmgl;
/// Restore once flag
procedure  mgl_parser_restore_once(p: HMPR); cdecl; external libmgl;
/// Allow changing size of the picture
procedure  mgl_parser_allow_setsize(p: HMPR;a: integer); cdecl; external libmgl;
/// Allow reading/saving files
procedure  mgl_parser_allow_file_io(p: HMPR;a: integer); cdecl; external libmgl;
/// Allow loading commands from external libraries
procedure  mgl_parser_allow_dll_call(p: HMPR;a: integer); cdecl; external libmgl;
/// Set flag to stop script parsing
procedure  mgl_parser_stop(p: HMPR); cdecl; external libmgl;
/// Return type of command: 0 - not found, 1 - data plot, 2 - other plot,
///		3 - setup, 4 - data handle, 5 - data create, 6 - subplot, 7 - program
///		8 - 1d plot, 9 - 2d plot, 10 - 3d plot, 11 - dd plot, 12 - vector plot
///		13 - axis, 14 - primitives, 15 - axis setup, 16 - text/legend, 17 - data transform
function  mgl_parser_cmd_type(pr: HMPR;const name: PChar): integer; cdecl; external libmgl;
/// Return description of MGL command
function mgl_parser_cmd_desc(pr: HMPR;const name: PChar): PChar; cdecl; external libmgl;
/// Return string of command format (command name and its argument[s])
function mgl_parser_cmd_frmt(pr: HMPR;const name: PChar): PChar; cdecl; external libmgl;
/// Get name of command with nmber n
function mgl_parser_cmd_name(pr: HMPR;id: integer): PChar; cdecl; external libmgl;
/// Get number of defined commands
function  mgl_parser_cmd_num(pr: HMPR): integer; cdecl; external libmgl;
/// Return result of formula evaluation
function  mgl_parser_calc(pr: HMPR;const formula: PChar): HMDT; cdecl; external libmgl;
function  mgl_parser_calcw(pr: HMPR;const formula: PWideChar): HMDT; cdecl; external libmgl;
//-----------------------------------------------------------------------------
{== ../../include/mgl2/addon.h ==}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/// Explicit scheme for 1 step of axial diffraction
procedure  mgl_difr_axial(a: Pdual;n: integer;step: integer;q: dual;Border: integer;tmp: Pdual;kk: integer;di: double); cdecl; external libmgl;
procedure  mgl_difr_axial_old(a: Pdual;n: integer;step: integer;q: dual;Border: integer;tmp1: Pdual;tmp2: Pdual;kk: integer;di: double); cdecl; external libmgl;
/// Explicit scheme for 1 step of plane diffraction
procedure  mgl_difr_grid(a: Pdual;n: integer;step: integer;q: dual;Border: integer;tmp: Pdual;kk: integer); cdecl; external libmgl;
procedure  mgl_difr_grid_old(a: Pdual;n: integer;step: integer;q: dual;Border: integer;tmp1: Pdual;tmp2: Pdual;kk: integer); cdecl; external libmgl;
//-----------------------------------------------------------------------------
/// Get random number with Gaussian distribution
function  mgl_gauss_rnd(): double; cdecl; external libmgl;
/// Fill frequencies for FFT
procedure  mgl_fft_freq(freq: Pdouble;nn: integer); cdecl; external libmgl;
/// Remove double spaces from the string
procedure  mgl_strcls(str: PChar); cdecl; external libmgl;
/// Get position of substring or return -1 if not found
function  mgl_strpos(const str: PChar;fnd: PChar): integer; cdecl; external libmgl;
/// Get position of symbol or return -1 if not found
function  mgl_chrpos(const str: PChar;fnd: char): integer; cdecl; external libmgl;
/// Get uncommented string from file (NOTE: it is not thread safe!!!)
/// Get parameters from uncommented strings of file (NOTE: it is not thread safe!!!)
/// Check if symbol denote true
function  mgl_istrue(ch: char): integer; cdecl; external libmgl;
/// Print test message
/// Print info message
/// Locate next data block (block started by -----)


{$IFDEF MSWINDOWS}
//*****************************************************************************/
// Delphi - specific
//*****************************************************************************/
procedure mgl_begin();
procedure mgl_end();

procedure mgl_draw_on_canvas(gr: HMGL; width, height: integer; canvas: TCanvas; switchXY: boolean = false);

implementation

var _FPUCW: word;

procedure mgl_begin();
 begin
  _FPUCW := Get8087CW();     // backup current FPU CW
  Set8087CW(_FPUCW or $3F); // masking all FPU exceptions
 end;

procedure mgl_end();
 begin
  Set8087CW(_FPUCW);         // restore old FPU CW
 end;

procedure mgl_draw_on_canvas(gr: HMGL; width, height: integer; canvas: TCanvas; switchXY: boolean = false);
  var i, j: integer;
      bytes: PByte;
      col: TColor;
 begin
  bytes := mgl_get_rgb(gr);

  if (not switchXY) then
   for j := 0 to height - 1 do
    for i := 0 to width - 1 do
     begin
      col := 0;
      col := col or (bytes^);
      inc(bytes);
      col := col or (bytes^) shl 8;
      inc(bytes);
       col := col or (bytes^) shl 16;
      inc(bytes);
      canvas.Pixels[i, j] := col;
    end
  else
   for j := height - 1 downto 0 do
    for i := 0 to width - 1 do
     begin
      col := 0;
      col := col or (bytes^);
      inc(bytes);
      col := col or (bytes^) shl 8;
      inc(bytes);
       col := col or (bytes^) shl 16;
      inc(bytes);
      canvas.Pixels[j, i] := col;
     end;
 end;

{$ENDIF}
end.

