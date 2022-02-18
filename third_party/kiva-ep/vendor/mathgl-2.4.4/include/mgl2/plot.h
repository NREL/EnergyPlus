/***************************************************************************
 * plot.h is part of Math Graphic Library
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
#ifndef _MGL_1D_H_
#define _MGL_1D_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Draw curve for formula with x in x-axis range
/** Option "value" set initial number of points. */
void MGL_EXPORT mgl_fplot(HMGL gr, const char *eqY, const char *pen, const char *opt);
void MGL_EXPORT mgl_fplot_(uintptr_t *gr, const char *fy, const char *stl, const char *opt, int ly, int ls, int lo);
/// Draw curve for formulas parametrically depended on t in range [0,1]
/** Option "value" set initial number of points. */
void MGL_EXPORT mgl_fplot_xyz(HMGL gr, const char *eqX, const char *eqY, const char *eqZ, const char *pen, const char *opt);
void MGL_EXPORT mgl_fplot_xyz_(uintptr_t *gr, const char *fx, const char *fy, const char *fz, const char *stl, const char *opt, int lx, int ly, int lz, int ls, int lo);

/// Draw radar chart (plot in curved coordinates)
/** Option "value" set the additional shift of data (i.e. the data a+value is used instead of a).*/
void MGL_EXPORT mgl_radar(HMGL graph, HCDT a, const char *pen, const char *opt);
void MGL_EXPORT mgl_radar_(uintptr_t *gr, uintptr_t *a, const char *pen, const char *opt, int l,int lo);

/// Draw usual curve {x,y,z}
void MGL_EXPORT mgl_plot_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_plot_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw usual curve {x,y}
void MGL_EXPORT mgl_plot_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_plot_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw usual curve {x,y} with x in x-axis range
void MGL_EXPORT mgl_plot(HMGL graph, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_plot_(uintptr_t *graph, uintptr_t *y,	const char *pen, const char *opt,int,int);

/// Draw curve {x,y,z} which is colored by c (like tension plot)
void MGL_EXPORT mgl_tens_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT c, const char *pen, const char *opt);
void MGL_EXPORT mgl_tens_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *pen, const char *opt,int,int);
/// Draw curve {x,y} which is colored by c (like tension plot)
void MGL_EXPORT mgl_tens_xy(HMGL graph, HCDT x, HCDT y, HCDT c, const char *pen, const char *opt);
void MGL_EXPORT mgl_tens_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *c, const char *pen, const char *opt,int,int);
/// Draw curve {x,y} with x in x-axis range which is colored by c (like tension plot)
void MGL_EXPORT mgl_tens(HMGL graph, HCDT y, HCDT c, const char *pen, const char *opt);
void MGL_EXPORT mgl_tens_(uintptr_t *graph, uintptr_t *y, uintptr_t *c, const char *pen, const char *opt,int,int);

/// Draw tapes which rotates as (bi-)normales of curve {x,y,z}
/** The width of tape is proportional to barwidth and can be changed by option "value".*/
void MGL_EXPORT mgl_tape_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_tape_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw tapes which rotates as (bi-)normales of curve {x,y}
/** The width of tape is proportional to barwidth and can be changed by option "value".*/
void MGL_EXPORT mgl_tape_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_tape_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw tapes which rotates as (bi-)normales of curve {x,y} with x in x-axis range
/** The width of tape is proportional to barwidth and can be changed by option "value".*/
void MGL_EXPORT mgl_tape(HMGL graph, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_tape_(uintptr_t *graph, uintptr_t *y,	const char *pen, const char *opt,int,int);

/// Draw box-plot (special 5-value plot used in statistic) for data specified parametrically
/** String \a pen may contain ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.*/
void MGL_EXPORT mgl_boxplot_xy(HMGL graph, HCDT x, HCDT a, const char *pen, const char *opt);
void MGL_EXPORT mgl_boxplot_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw box-plot (special 5-value plot used in statistic)
/** String \a pen may contain ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.*/
void MGL_EXPORT mgl_boxplot(HMGL graph, HCDT a, const char *pen, const char *opt);
void MGL_EXPORT mgl_boxplot_(uintptr_t *graph, uintptr_t *y, const char *pen, const char *opt,int,int);

/// Fill area between curve {x,y,z} and axis plane
/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_area_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_area_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Fill area between curve {x,y} and axis plane
/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_area_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_area_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Fill area between curve {x,y} with x in x-axis range and axis plane
/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_area(HMGL graph, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_area_(uintptr_t *graph, uintptr_t *y, const char *pen, const char *opt,int,int);

/// Fill area (draw ribbon) between curves {x1,y1,z1} and {x2,y2,z2}
/** Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_region_3d(HMGL graph, HCDT x1, HCDT y1, HCDT z1, HCDT x2, HCDT y2, HCDT z2, const char *pen, const char *opt);
void MGL_EXPORT mgl_region_3d_(uintptr_t *graph, uintptr_t *x1, uintptr_t *y1, uintptr_t *z1, uintptr_t *x2, uintptr_t *y2, uintptr_t *z2, const char *pen, const char *opt,int,int);
/// Fill area between curves {x,y1} and {x,y2}
/** Style 'i' will fill area only if y1 < y2.
  * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_region_xy(HMGL graph, HCDT x, HCDT y1, HCDT y2, const char *pen, const char *opt);
void MGL_EXPORT mgl_region_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int, int);
/// Fill area between curves {x,y1} and {x,y2} with x in x-axis range
/** Style 'i' will fill area only if y1 < y2.
  * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_region(HMGL graph, HCDT y1, HCDT y2, const char *pen, const char *opt);
void MGL_EXPORT mgl_region_(uintptr_t *graph, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int, int);

/// Draw vertical lines from points {x,y,z} to axis plane
void MGL_EXPORT mgl_stem_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_stem_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw vertical lines from points {x,y} to axis plane
void MGL_EXPORT mgl_stem_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_stem_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw vertical lines from points {x,y} with x in x-axis range to axis plane
void MGL_EXPORT mgl_stem(HMGL graph, HCDT y,	const char *pen, const char *opt);
void MGL_EXPORT mgl_stem_(uintptr_t *graph, uintptr_t *y, const char *pen, const char *opt,int,int);

/// Draw stairs for points in arrays {x,y,z}
void MGL_EXPORT mgl_step_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_step_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw stairs for points in arrays {x,y}
void MGL_EXPORT mgl_step_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_step_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw stairs for points in arrays {x,y} with x in x-axis range
void MGL_EXPORT mgl_step(HMGL graph, HCDT y,	const char *pen, const char *opt);
void MGL_EXPORT mgl_step_(uintptr_t *graph, uintptr_t *y, const char *pen, const char *opt,int,int);

/// Draw vertical bars from points {x,y,z} to axis plane
/** String \a pen may contain:
 * ‘a’ for drawing boxes one above another (like summation);
 * ‘f’ for waterfall chart;
 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_bars_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_bars_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw vertical bars from points {x,y} to axis plane
/** String \a pen may contain:
 * ‘a’ for drawing boxes one above another (like summation);
 * ‘f’ for waterfall chart;
 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_bars_xy(HMGL graph, HCDT x, HCDT y, const char *pen, const char *opt);
void MGL_EXPORT mgl_bars_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *pen, const char *opt,int,int);
/// Draw vertical bars from points {x,y} with x in x-axis range to axis plane
/** String \a pen may contain:
 * ‘a’ for drawing boxes one above another (like summation);
 * ‘f’ for waterfall chart;
 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_bars(HMGL graph, HCDT y,	const char *pen, const char *opt);
void MGL_EXPORT mgl_bars_(uintptr_t *graph, uintptr_t *y, const char *pen, const char *opt,int,int);

/// Draw horizontal bars from points {v,y} to axis plane
/** String \a pen may contain:
 * ‘a’ for drawing boxes one above another (like summation);
 * ‘f’ for waterfall chart;
 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_barh_yx(HMGL graph, HCDT y, HCDT v, const char *pen, const char *opt);
void MGL_EXPORT mgl_barh_yx_(uintptr_t *graph, uintptr_t *y, uintptr_t *v, const char *pen, const char *opt,int,int);
/// Draw horizontal bars from points {v,y} with y in y-axis range to axis plane
/** String \a pen may contain:
 * ‘a’ for drawing boxes one above another (like summation);
 * ‘f’ for waterfall chart;
 * ‘<’, ‘^’, ‘>’ for aligning boxes: at left, centered, at right.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_barh(HMGL graph, HCDT v,	const char *pen, const char *opt);
void MGL_EXPORT mgl_barh_(uintptr_t *graph, uintptr_t *v, const char *pen, const char *opt,int,int);

/// Draw Open-High-Low-Close (OHLC) diagram
/**  Different colors for up and down values are used if number of specified colors is equal to 2*number of curves. */
void MGL_EXPORT mgl_ohlc_x(HMGL graph, HCDT x, HCDT open, HCDT high, HCDT low, HCDT close, const char *pen, const char *opt);
void MGL_EXPORT mgl_ohlc_x_(uintptr_t *graph, uintptr_t *x, uintptr_t *open, uintptr_t *high, uintptr_t *low, uintptr_t *close, const char *pen, const char *opt,int,int);
/// Draw Open-High-Low-Close (OHLC) diagram with x in x-axis range
/**  Different colors for up and down values are used if number of specified colors is equal to 2*number of curves. */
void MGL_EXPORT mgl_ohlc(HMGL graph, HCDT open, HCDT high, HCDT low, HCDT close, const char *pen, const char *opt);
void MGL_EXPORT mgl_ohlc_(uintptr_t *graph, uintptr_t *open, uintptr_t *high, uintptr_t *low, uintptr_t *close, const char *pen, const char *opt,int,int);

/// Draw chart for data a
/** Space denote transparent color. Style '#' draw black borders. */
void MGL_EXPORT mgl_chart(HMGL graph, HCDT a, const char *col, const char *opt);
void MGL_EXPORT mgl_chart_(uintptr_t *graph, uintptr_t *a, const char *col, const char *opt,int,int);

/// Draw error boxes {ex,ey} at points {x,y}
/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
void MGL_EXPORT mgl_error_exy(HMGL graph, HCDT x, HCDT y, HCDT ex, HCDT ey, const char *pen, const char *opt);
void MGL_EXPORT mgl_error_exy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *ex, uintptr_t *ey, const char *pen, const char *opt,int,int);
/// Draw error boxes {ey} at points {x,y}
/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
void MGL_EXPORT mgl_error_xy(HMGL graph, HCDT x, HCDT y, HCDT ey, const char *pen, const char *opt);
void MGL_EXPORT mgl_error_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *ey, const char *pen, const char *opt,int,int);
/// Draw error boxes {ey} at points {x,y} with x in x-axis range
/** Style ‘@’ set to draw large semitransparent mark instead of error box.*/
void MGL_EXPORT mgl_error(HMGL graph, HCDT y, HCDT ey, const char *pen, const char *opt);
void MGL_EXPORT mgl_error_(uintptr_t *graph, uintptr_t *y, uintptr_t *ey, const char *pen, const char *opt,int,int);

/// Draw marks with size r at points {x,y,z}
void MGL_EXPORT mgl_mark_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_mark_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw marks with size r at points {x,y}
void MGL_EXPORT mgl_mark_xy(HMGL graph, HCDT x, HCDT y, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_mark_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw marks with size r at points {x,y} with x in x-axis range
void MGL_EXPORT mgl_mark_y(HMGL graph, HCDT y, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_mark_y_(uintptr_t *graph, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);

/// Draw Poincare map at condition s==0 for curve {x,y,z}
void MGL_EXPORT mgl_pmap_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT s, const char *pen, const char *opt);
void MGL_EXPORT mgl_pmap_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw Poincare map at condition s==0 for curve {x,y}
void MGL_EXPORT mgl_pmap_xy(HMGL graph, HCDT x, HCDT y, HCDT s, const char *pen, const char *opt);
void MGL_EXPORT mgl_pmap_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw Poincare map at condition s==0 for curve {x,y} with x in x-axis range
void MGL_EXPORT mgl_pmap(HMGL graph, HCDT y, HCDT s, const char *pen, const char *opt);
void MGL_EXPORT mgl_pmap_(uintptr_t *graph, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);

/// Draw tube with variable radius r around curve {x,y,z}
void MGL_EXPORT mgl_tube_xyzr(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_tube_xyzr_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw tube with variable radius r around curve {x,y}
void MGL_EXPORT mgl_tube_xyr(HMGL graph, HCDT x, HCDT y, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_tube_xyr_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw tube with variable radius r around curve {x,y} with x in x-axis range
void MGL_EXPORT mgl_tube_r(HMGL graph, HCDT y, HCDT r, const char *pen, const char *opt);
void MGL_EXPORT mgl_tube_r_(uintptr_t *graph, uintptr_t *y, uintptr_t *r, const char *pen, const char *opt,int,int);
/// Draw tube with constant radius r around curve {x,y,z}
void MGL_EXPORT mgl_tube_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, double r, const char *pen, const char *opt);
void MGL_EXPORT mgl_tube_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, mreal *r, const char *pen, const char *opt,int,int);
/// Draw tube with constant radius r around curve {x,y}
void MGL_EXPORT mgl_tube_xy(HMGL graph, HCDT x, HCDT y, double r, const char *penl, const char *opt);
void MGL_EXPORT mgl_tube_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, mreal *r, const char *pen, const char *opt,int,int);
/// Draw tube with constant radius r around curve {x,y} with x in x-axis range
void MGL_EXPORT mgl_tube(HMGL graph, HCDT y, double r, const char *pen, const char *opt);
void MGL_EXPORT mgl_tube_(uintptr_t *graph, uintptr_t *y, mreal *r, const char *pen, const char *opt,int,int);

/// Draw candle plot for data specified parametrically
/** Different colors are used for up and down values if 2 colors are specified.
 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
void MGL_EXPORT mgl_candle_xyv(HMGL gr, HCDT x, HCDT v1, HCDT v2, HCDT y1, HCDT y2, const char *pen, const char *opt);
void MGL_EXPORT mgl_candle_xyv_(uintptr_t *gr, uintptr_t *x, uintptr_t *v1, uintptr_t *v2, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int,int);
/// Draw candle plot
/** Different colors are used for up and down values if 2 colors are specified.
 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
void MGL_EXPORT mgl_candle_yv(HMGL gr, HCDT v1, HCDT v2, HCDT y1, HCDT y2, const char *pen, const char *opt);
void MGL_EXPORT mgl_candle_yv_(uintptr_t *gr, uintptr_t *v1, uintptr_t *v2, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int,int);
/// Draw candle plot with v1=v[i], v2=v[i+1]
/** Different colors are used for up and down values if 2 colors are specified.
 *  Style ‘#’ force drawing wire candle even for 2-color scheme. */
void MGL_EXPORT mgl_candle(HMGL gr, HCDT v, HCDT y1, HCDT y2, const char *pen, const char *opt);
void MGL_EXPORT mgl_candle_(uintptr_t *gr, uintptr_t *y, uintptr_t *y1, uintptr_t *y2, const char *pen, const char *opt,int,int);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
