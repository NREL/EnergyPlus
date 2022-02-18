/***************************************************************************
 * other.h is part of Math Graphic Library
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
#ifndef _MGL_OTHER_H_
#define _MGL_OTHER_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Draw triangle mesh for points in arrays {x,y,z} with specified color c.
/** Style ‘#’ produce wire plot. If id.ny=c.nx then c set the triangle colors, else vertex colors. */
void MGL_EXPORT mgl_triplot_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_triplot_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw triangle mesh for points in arrays {x,y,z} with color proportional to z.
/** Style ‘#’ produce wire plot. */
void MGL_EXPORT mgl_triplot_xyz(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_triplot_xyz_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw triangle mesh for points in arrays {x,y}
/** Style ‘#’ produce wire plot. */
void MGL_EXPORT mgl_triplot_xy(HMGL gr, HCDT nums, HCDT x, HCDT y, const char *sch, const char *opt);
void MGL_EXPORT mgl_triplot_xy_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, const char *sch, const char *opt,int,int);

/// Draw quad mesh for points in arrays {x,y,z} with specified color c.
/** Style ‘#’ produce wire plot. If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
void MGL_EXPORT mgl_quadplot_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_quadplot_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw quad mesh for points in arrays {x,y,z} with color proportional to z.
/** Style ‘#’ produce wire plot. */
void MGL_EXPORT mgl_quadplot_xyz(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_quadplot_xyz_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw quad mesh for points in arrays {x,y}.
/** Style ‘#’ produce wire plot. */
void MGL_EXPORT mgl_quadplot_xy(HMGL gr, HCDT nums, HCDT x, HCDT y, const char *sch, const char *opt);
void MGL_EXPORT mgl_quadplot_xy_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, const char *sch, const char *opt,int,int);

/// Draw manual contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
/** Style ‘_’ to draw contours at bottom of axis box.
 * Style ‘t’/‘T’ draw contour labels below/above contours.
 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
void MGL_EXPORT mgl_tricont_xyzcv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricont_xyzcv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw manual contour lines for triangle mesh for points in arrays {x,y,z}.
/** Style ‘_’ to draw contours at bottom of axis box.
 * Style ‘t’/‘T’ draw contour labels below/above contours.
 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
void MGL_EXPORT mgl_tricont_xycv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricont_xycv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw contour lines for triangle mesh for points in arrays {x,y,z} with specified color c.
/** Style ‘_’ to draw contours at bottom of axis box.
 * Style ‘t’/‘T’ draw contour labels below/above contours.
 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_tricont_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricont_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int, int);
/// Draw contour lines for triangle mesh for points in arrays {x,y,z}.
/** Style ‘_’ to draw contours at bottom of axis box.
 * Style ‘t’/‘T’ draw contour labels below/above contours.
 * If id.ny=c.nx then c set the quadrangle colors, else vertex colors.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_tricont_xyc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricont_xyc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int, int);

/// Draw manual contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c.
/** If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
void MGL_EXPORT mgl_tricontv_xyzcv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricontv_xyzcv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw manual contour tubes for triangle mesh for points in arrays {x,y,z}.
/** If id.ny=c.nx then c set the quadrangle colors, else vertex colors. */
void MGL_EXPORT mgl_tricontv_xycv(HMGL gr, HCDT v, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricontv_xycv_(uintptr_t *gr, uintptr_t *v, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw contour tubes for triangle mesh for points in arrays {x,y,z} with specified color c.
/** If id.ny=c.nx then c set the quadrangle colors, else vertex colors.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_tricontv_xyzc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricontv_xyzc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int, int);
/// Draw contour tubes for triangle mesh for points in arrays {x,y,z}.
/** If id.ny=c.nx then c set the quadrangle colors, else vertex colors.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_tricontv_xyc(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tricontv_xyc_(uintptr_t *gr, uintptr_t *nums, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int, int);

/// Draw dots in points {x,y,z}.
void MGL_EXPORT mgl_dots(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_dots_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw semitransparent dots in points {x,y,z} with specified alpha a.
void MGL_EXPORT mgl_dots_a(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *sch, const char *opt);
void MGL_EXPORT mgl_dots_a_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *a, const char *sch, const char *opt,int,int);
/// Draw semitransparent dots in points {x,y,z} with specified color c and alpha a.
void MGL_EXPORT mgl_dots_ca(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt);
void MGL_EXPORT mgl_dots_ca_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int,int);

/// Draw surface reconstructed for points in arrays {x,y,z}.
/** Style ‘#’ produce wired plot. */
void MGL_EXPORT mgl_crust(HMGL gr, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_crust_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw density plot for data at x = sVal
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_dens_x(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_dens_x_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw density plot for data at y = sVal
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_dens_y(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_dens_y_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw density plot for data at z = sVal
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_dens_z(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_dens_z_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);

/// Draw contour lines for data at x = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_cont_x(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_x_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw contour lines for data at y = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_cont_y(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_y_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw contour lines for data at z = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours.
 * Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_cont_z(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_z_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);

/// Draw manual contour lines for data at x = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours. */
void MGL_EXPORT mgl_cont_x_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_x_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw manual contour lines for data at y = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours. */
void MGL_EXPORT mgl_cont_y_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_y_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw manual contour lines for data at z = sVal
/** Style ‘t’/‘T’ draw contour labels below/above contours. */
void MGL_EXPORT mgl_cont_z_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_cont_z_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);

/// Draw solid contours for data at x = sVal
/** Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_contf_x(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_x_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw solid contours for data at y = sVal
/** Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_contf_y(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_y_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw solid contours for data at z = sVal
/** Option "value" set the number of contour levels (default is 7). */
void MGL_EXPORT mgl_contf_z(HMGL graph, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_z_(uintptr_t *graph, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);

/// Draw manual solid contours for data at x = sVal
void MGL_EXPORT mgl_contf_x_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_x_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw manual solid contours for data at y = sVal
void MGL_EXPORT mgl_contf_y_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_y_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);
/// Draw manual solid contours for data at z = sVal
void MGL_EXPORT mgl_contf_z_val(HMGL graph, HCDT v, HCDT a, const char *stl, double sVal, const char *opt);
void MGL_EXPORT mgl_contf_z_val_(uintptr_t *graph, uintptr_t *v, uintptr_t *a, const char *stl, mreal *sVal, const char *opt,int,int);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
