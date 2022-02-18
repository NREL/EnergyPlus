/***************************************************************************
 * prim.h is part of Math Graphic Library
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
#ifndef _MGL_PRIM_H_
#define _MGL_PRIM_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Draws the mark at position {x,y,z}
void MGL_EXPORT mgl_mark(HMGL gr, double x,double y,double z,const char *mark);
void MGL_EXPORT mgl_mark_(uintptr_t *gr, mreal *x, mreal *y, mreal *z,const char *mark,int);
/// Draws red point (ball) at position {x,y,z}
void MGL_EXPORT mgl_ball(HMGL gr, double x,double y,double z);
void MGL_EXPORT mgl_ball_(uintptr_t *gr, mreal *x, mreal *y, mreal *z);

/// Draws the line between 2 points by specified pen
/** Large \a n (for example, n=100) should be used for geodesic line in curvilinear coordinates */
void MGL_EXPORT mgl_line(HMGL gr, double x1, double y1, double z1, double x2, double y2, double z2, const char *pen,int n);
void MGL_EXPORT mgl_line_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, const char *pen,int *n,int);
/// Draws the spline curve between 2 points by specified pen
void MGL_EXPORT mgl_curve(HMGL gr, double x1, double y1, double z1, double dx1, double dy1, double dz1, double x2, double y2, double z2, double dx2, double dy2, double dz2, const char *pen,int n);
void MGL_EXPORT mgl_curve_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *dx1, mreal *dy1, mreal *dz1, mreal *x2, mreal *y2, mreal *z2, mreal *dx2, mreal *dy2, mreal *dz2, const char *pen,int *n, int l);

/// Draws the 3d error box {ex,ey,ez} for point {x,y,z}
void MGL_EXPORT mgl_error_box(HMGL gr, double x, double y, double z, double ex, double ey, double ez, const char *pen);
void MGL_EXPORT mgl_error_box_(uintptr_t* gr, mreal *x, mreal *y, mreal *z, mreal *ex, mreal *ey, mreal *ez, const char *pen, int);

/// Draws the face between points with color stl (include interpolation up to 4 colors).
void MGL_EXPORT mgl_face(HMGL gr, double x0, double y0, double z0, double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3, double z3, const char *stl);
void MGL_EXPORT mgl_face_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *z0, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, mreal *x3, mreal *y3, mreal *z3, const char *stl, int);
/// Draws the face in y-z plane at point p with color stl (include interpolation up to 4 colors).
void MGL_EXPORT mgl_facex(HMGL gr, double x0, double y0, double z0, double wy, double wz, const char *stl, double dx, double dy);
void MGL_EXPORT mgl_facex_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *z0, mreal *wy, mreal *wz, const char *stl, mreal *dx, mreal *dy, int l);
/// Draws the face in x-z plane at point p with color stl (include interpolation up to 4 colors).
void MGL_EXPORT mgl_facey(HMGL gr, double x0, double y0, double z0, double wx, double wz, const char *stl, double dx, double dy);
void MGL_EXPORT mgl_facey_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *z0, mreal *wx, mreal *wz, const char *stl, mreal *dx, mreal *dy, int l);
/// Draws the face in x-y plane at point p with color stl (include interpolation up to 4 colors).
void MGL_EXPORT mgl_facez(HMGL gr, double x0, double y0, double z0, double wx, double wy, const char *stl, double dx, double dy);
void MGL_EXPORT mgl_facez_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *z0, mreal *wx, mreal *wy, const char *stl, mreal *dx, mreal *dy, int l);

/// Draws the sphere at point {x,y,z} with color stl and radius r
void MGL_EXPORT mgl_sphere(HMGL gr, double x, double y, double z, double r, const char *stl);
void MGL_EXPORT mgl_sphere_(uintptr_t* gr, mreal *x, mreal *y, mreal *z, mreal *r, const char *stl, int);
/// Draws the drop at point {x,y,z} in direction {dx,dy,dz} with color stl and radius r
/** Parameter \a shift set the degree of drop oblongness: ‘0’ is sphere, ‘1’ is maximally oblongness drop. Parameter \a ap set relative width of the drop (this is analogue of “ellipticity” for the sphere).*/
void MGL_EXPORT mgl_drop(HMGL gr, double x, double y, double z, double dx, double dy, double dz, double r, const char *stl, double shift, double ap);
void MGL_EXPORT mgl_drop_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, mreal *r, const char *stl, mreal *shift, mreal *ap, int);
/// Draws the cone between points p1,p2 with radius r1,r2 and with style stl
/** Parameter \a stl can contain:
 * ‘@’ for drawing edges;
 * ‘#’ for wired cones;
 * ‘t’ for drawing tubes/cylinder instead of cones/prisms;
 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones.*/
void MGL_EXPORT mgl_cone(HMGL gr, double x1, double y1, double z1, double x2, double y2, double z2, double r1, double r2, const char *stl);
void MGL_EXPORT mgl_cone_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, mreal *r1, mreal *r2, const char *stl, int);
/// Draws the ellipse between points p1,p2 with color stl and width r
/** Parameter \a stl can contain:
 * ‘#’ for wired figure (boundary only);
 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
void MGL_EXPORT mgl_ellipse(HMGL gr, double x1, double y1, double z1, double x2, double y2, double z2, double r, const char *stl);
void MGL_EXPORT mgl_ellipse_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, mreal *r, const char *stl, int);
/// Draws the rhomb between points p1,p2 with color stl and width r
/** Parameter \a stl can contain:
 * ‘#’ for wired figure (boundary only);
 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
void MGL_EXPORT mgl_rhomb(HMGL gr, double x1, double y1, double z1, double x2, double y2, double z2, double r, const char *stl);
void MGL_EXPORT mgl_rhomb_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, mreal *r, const char *stl, int);
/// Draws the polygon based on points p1,p2 with color stl
/** Parameter \a stl can contain:
 * ‘#’ for wired figure (boundary only);
 * ‘@’ for filled figure and with boundary (second color or black one is used for boundary).*/
void MGL_EXPORT mgl_polygon(HMGL gr, double x1, double y1, double z1, double x2, double y2, double z2, int n, const char *stl);
void MGL_EXPORT mgl_polygon_(uintptr_t* gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2, int *n, const char *stl, int);
	/// Draws the arc around axis pr with center at p0 and starting from p1, by color stl and angle a (in degrees)
void MGL_EXPORT mgl_arc_ext(HMGL gr, double x0, double y0, double z0, double xr, double yr, double zr, double x1, double y1, double z1, double a, const char *stl);
void MGL_EXPORT mgl_arc_ext_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *z0, mreal *xr, mreal *yr, mreal *zr, mreal *x1, mreal *y1, mreal *z1, mreal *a, const char *stl, int);
	/// Draws the arc around axis 'z' with center at p0 and starting from p1, by color stl and angle a (in degrees)
void MGL_EXPORT mgl_arc(HMGL gr, double x0, double y0, double x1, double y1, double a, const char *stl);
void MGL_EXPORT mgl_arc_(uintptr_t* gr, mreal *x0, mreal *y0, mreal *x1, mreal *y1, mreal *a, const char *stl,int l);


/// Draw cones from points {x,y,z} to axis plane
/** String \a pen may contain:
 * ‘@’ for drawing edges;
 * ‘#’ for wired cones;
 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_cones_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_cones_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw cones from points {x,z} to axis plane
/** String \a pen may contain:
 * ‘@’ for drawing edges;
 * ‘#’ for wired cones;
 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_cones_xz(HMGL graph, HCDT x, HCDT z, const char *pen, const char *opt);
void MGL_EXPORT mgl_cones_xz_(uintptr_t *graph, uintptr_t *x, uintptr_t *z, const char *pen, const char *opt,int,int);
/// Draw cones from points {x,z} with x in x-axis range to axis plane
/** String \a pen may contain:
 * ‘@’ for drawing edges;
 * ‘#’ for wired cones;
 * ‘t’ for drawing tubes/cylinders instead of cones/prisms;
 * ‘4’, ‘6’, ‘8’ for drawing square, hex- or octo-prism instead of cones;
 * ‘<’, ‘^’ or ‘>’ for aligning cones left, right or centering them at its x-coordinates.
 * Gradient filling is used if number of specified colors is equal to 2*number of curves.*/
void MGL_EXPORT mgl_cones(HMGL graph, HCDT z,	const char *pen, const char *opt);
void MGL_EXPORT mgl_cones_(uintptr_t *graph, uintptr_t *z,	const char *pen, const char *opt,int,int);

/// Plot dew drops for vector field {ax,ay} parametrically depended on coordinate {x,y}
void MGL_EXPORT mgl_dew_xy(HMGL gr, HCDT x, HCDT y, HCDT ax, HCDT ay, const char *sch, const char *opt);
void MGL_EXPORT mgl_dew_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int,int);
/// Plot dew drops for vector field {ax,ay}
void MGL_EXPORT mgl_dew_2d(HMGL gr, HCDT ax, HCDT ay, const char *sch, const char *optl);
void MGL_EXPORT mgl_dew_2d_(uintptr_t *gr, uintptr_t *ax, uintptr_t *ay, const char *sch, const char *opt,int,int l);

/// Print text in position {x,y,z} with specified font
/* NOTE: Function don't have options because it can be part of group.*/
void MGL_EXPORT mgl_puts(HMGL graph, double x, double y, double z,const char *text, const char *font, double size);
void MGL_EXPORT mgl_puts_(uintptr_t *graph, mreal *x, mreal *y, mreal *z,const char *text, const char *font, mreal *size, int, int);
void MGL_EXPORT mgl_putsw(HMGL graph, double x, double y, double z,const wchar_t *text, const char *font, double size);

/// Print text in position {x,y,z} along direction {dx,dy,dz} with specified font
/* NOTE: Function don't have options because it can be part of group.*/
void MGL_EXPORT mgl_puts_dir(HMGL graph, double x, double y, double z, double dx, double dy, double dz, const char *text, const char *font, double size);
void MGL_EXPORT mgl_puts_dir_(uintptr_t *graph, mreal *x, mreal *y, mreal *z, mreal *dx, mreal *dy, mreal *dz, const char *text, const char *font, mreal *size, int, int);
void MGL_EXPORT mgl_putsw_dir(HMGL graph, double x, double y, double z, double dx, double dy, double dz, const wchar_t *text, const char *font, double size);

/// Draw user-defined symbol in position p
void MGL_EXPORT mgl_symbol(HMGL gr, double x, double y, double z, char id, const char *how, double size);
void MGL_EXPORT mgl_symbol_(uintptr_t *gr, double *x, double *y, double *z, char *id, const char *how, double *size,int,int);
/// Draw user-defined symbol in position p along direction d
void MGL_EXPORT mgl_symbol_dir(HMGL gr, double x, double y, double z, double dx, double dy, double dz, char id, const char *how, double size);
void MGL_EXPORT mgl_symbol_dir_(uintptr_t *gr, double *x, double *y, double *z, double *dx, double *dy, double *dz, char *id, const char *how, double *size,int,int);

/// Draw textual marks with size r at points {x,y,z}
void MGL_EXPORT mgl_textmark_xyzr(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_textmark_xyzr_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_textmarkw_xyzr(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, const wchar_t *text, const char *fnt, const char *opt);
/// Draw textual marks with size r at points {x,y}
void MGL_EXPORT mgl_textmark_xyr(HMGL graph, HCDT x, HCDT y, HCDT r, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_textmark_xyr_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *r, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_textmarkw_xyr(HMGL graph, HCDT x, HCDT y, HCDT r, const wchar_t *text, const char *fnt, const char *opt);
/// Draw textual marks with size r at points {x,y} with x in x-axis range
void MGL_EXPORT mgl_textmark_yr(HMGL graph, HCDT y, HCDT r, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_textmark_yr_(uintptr_t *graph, uintptr_t *y, uintptr_t *r, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_textmarkw_yr(HMGL graph, HCDT y, HCDT r, const wchar_t *text, const char *fnt, const char *opt);
/// Draw textual marks with size r=1 at points {x,y} with x in x-axis range
void MGL_EXPORT mgl_textmark(HMGL graph, HCDT y, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_textmarkw(HMGL graph, HCDT y, const wchar_t *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_textmark_(uintptr_t *graph, uintptr_t *y, const char *text, const char *fnt, const char *opt,int,int,int);

/// Draw labels for points coordinate(s) at points {x,y,z}
/** String \a fnt may contain:
 *	 ‘f’ for fixed format of printed numbers;
 *	 ‘E’ for using ‘E’ instead of ‘e’;
 *	 ‘F’ for printing in LaTeX format;
 *	 ‘+’ for printing ‘+’ for positive numbers;
 *	 ‘-’ for printing usual ‘-’;
 *	 ‘0123456789’ for precision at printing numbers.*/
void MGL_EXPORT mgl_label_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_label_xyz_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_labelw_xyz(HMGL graph, HCDT x, HCDT y, HCDT z, const wchar_t *text, const char *fnt, const char *opt);
/// Draw labels for points coordinate(s) at points {x,y}
/** String \a fnt may contain:
 *	 ‘f’ for fixed format of printed numbers;
 *	 ‘E’ for using ‘E’ instead of ‘e’;
 *	 ‘F’ for printing in LaTeX format;
 *	 ‘+’ for printing ‘+’ for positive numbers;
 *	 ‘-’ for printing usual ‘-’;
 *	 ‘0123456789’ for precision at printing numbers.*/
void MGL_EXPORT mgl_label_xy(HMGL graph, HCDT x, HCDT y, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_label_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_labelw_xy(HMGL graph, HCDT x, HCDT y, const wchar_t *text, const char *fnt, const char *opt);
/// Draw labels for points coordinate(s) at points {x,y} with x in x-axis range
/** String \a fnt may contain:
 *	 ‘f’ for fixed format of printed numbers;
 *	 ‘E’ for using ‘E’ instead of ‘e’;
 *	 ‘F’ for printing in LaTeX format;
 *	 ‘+’ for printing ‘+’ for positive numbers;
 *	 ‘-’ for printing usual ‘-’;
 *	 ‘0123456789’ for precision at printing numbers.*/
void MGL_EXPORT mgl_label_y(HMGL graph, HCDT y, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_labelw_y(HMGL graph, HCDT y, const wchar_t *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_label_y_(uintptr_t *graph, uintptr_t *y, const char *text, const char *fnt, const char *opt,int,int,int);

/// Draw table for values val along given direction with row labels text at position {x,y}
/** String \a fnt may contain:
 *	 ‘#’ for drawing cell borders;
 *	 ‘|’ for limiting table width by subplot one (equal to option ‘value 1’);
 *	 ‘=’ for equal width of all cells;
 *	 ‘f’ for fixed format of printed numbers;
 *	 ‘E’ for using ‘E’ instead of ‘e’;
 *	 ‘F’ for printing in LaTeX format;
 *	 ‘+’ for printing ‘+’ for positive numbers;
 *	 ‘-’ for printing usual ‘-’;
 *	 ‘0123456789’ for precision at printing numbers.
 * Option value set the width of the table (default is 1).*/
void MGL_EXPORT mgl_table(HMGL gr, double x, double y, HCDT val, const char *text, const char *fnt, const char *opt);
void MGL_EXPORT mgl_table_(uintptr_t *gr, mreal *x, mreal *y, uintptr_t *val, const char *text, const char *fnt, const char *opt,int,int,int);
void MGL_EXPORT mgl_tablew(HMGL gr, double x, double y, HCDT val, const wchar_t *text, const char *fnt, const char *opt);

/// Draws bitmap (logo) which is stretched along whole axis range
void MGL_EXPORT mgl_logo(HMGL gr, long w, long h, const unsigned char *rgba, int smooth, const char *opt);
void MGL_EXPORT mgl_logo_file(HMGL gr, const char *fname, int smooth, const char *opt);
void MGL_EXPORT mgl_logo_file_(uintptr_t *gr, const char *fname, int *smooth, const char *opt,int l,int n);

/// Draws Lamerey diagram for mapping x_new = f(x_old)
/** String \a stl may contain: ‘v’ for drawing arrows; ‘~’ for disable 1st segment.
 *	Option value set the number of segments (default is 20).*/
void MGL_EXPORT mgl_lamerey(HMGL gr, double x0, double (*f)(double,void *), void *par, const char *stl, const char *opt);
void MGL_EXPORT mgl_lamerey_dat(HMGL gr, double x0, HCDT f, const char *stl, const char *opt);
void MGL_EXPORT mgl_lamerey_str(HMGL gr, double x0, const char *f, const char *stl, const char *opt);
void MGL_EXPORT mgl_lamerey_dat_(uintptr_t *gr, double *x0, uintptr_t *f, const char *stl, const char *opt, int,int);
void MGL_EXPORT mgl_lamerey_str_(uintptr_t *gr, double *x0, const char *f, const char *stl, const char *opt, int,int,int);

/// Draws Bifurcation diagram for mapping x_new = f(x_old) in x-axis range
/** Option value set the number of stationary points (default is 1024).*/
void MGL_EXPORT mgl_bifurcation(HMGL gr, double dx, double (*f)(double,double,void *), void *par, const char *stl, const char *opt);
void MGL_EXPORT mgl_bifurcation_dat(HMGL gr, double dx, HCDT f, const char *stl, const char *opt);
void MGL_EXPORT mgl_bifurcation_str(HMGL gr, double dx, const char *f, const char *stl, const char *opt);
void MGL_EXPORT mgl_bifurcation_dat_(uintptr_t *gr, double *dx, uintptr_t *f, const char *stl, const char *opt, int,int);
void MGL_EXPORT mgl_bifurcation_str_(uintptr_t *gr, double *dx, const char *f, const char *stl, const char *opt, int,int,int);

/// Draws Iris plots for determining cross-dependences of data arrays
/** NOTE: using the same ranges and empty ids will not draw axis. This will add data to existing Iris plot.
 * 	Option value set the size of data labels ids, separated by ';'.*/
void MGL_EXPORT mgl_iris(HMGL gr, HCDT dats, HCDT ranges, const char *ids, const char *stl, const char *opt);
void MGL_EXPORT mgl_iris_1(HMGL gr, HCDT dats, const char *ids, const char *stl, const char *opt);
void MGL_EXPORT mgl_irisw(HMGL gr, HCDT dats, HCDT ranges, const wchar_t *ids, const char *stl, const char *opt);
void MGL_EXPORT mgl_irisw_1(HMGL gr, HCDT dats, const wchar_t *ids, const char *stl, const char *opt);
void MGL_EXPORT mgl_iris_(uintptr_t *gr, uintptr_t *dats, uintptr_t *ranges, const char *ids, const char *stl, const char *opt,int l,int m,int n);
void MGL_EXPORT mgl_iris_1_(uintptr_t *gr, uintptr_t *dats, const char *ids, const char *stl, const char *opt,int,int,int);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
