/***************************************************************************
 * fit.h is part of Math Graphic Library
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
#ifndef _MGL_FIT_H_
#define _MGL_FIT_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif
//-----------------------------------------------------------------------------
extern int mglFitPnts;			///< Number of output points in fitting
extern char mglFitRes[1024];	///< Last fitted formula
/// Fit data along x-direction for each data row. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_1(HMGL gr, HCDT y, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_1_(uintptr_t* gr, uintptr_t* y, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
/// Fit data along x-, y-directions for each data slice. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_2(HMGL gr, HCDT z, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_2_(uintptr_t* gr, uintptr_t* z, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
/// Fit data along along all directions. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_3(HMGL gr, HCDT a, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_3_(uintptr_t* gr, uintptr_t* a, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
/// Fit data along x-direction for each data row. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_xy(HMGL gr, HCDT x, HCDT y, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xy_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
/// Fit data along x-, y-directions for each data slice. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xyz_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
/// Fit data along along all directions. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_xyza(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xyza_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
	/// Fit data with dispersion s along x-direction for each data row. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_ys(HMGL gr, HCDT y, HCDT s, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_ys_(uintptr_t* gr, uintptr_t* y, uintptr_t* ss, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
HMDT MGL_EXPORT mgl_fit_xys(HMGL gr, HCDT x, HCDT y, HCDT s, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xys_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* ss, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
	/// Fit data with dispersion s along x-, y-directions for each data slice. Return array with values for found formula.
HMDT MGL_EXPORT mgl_fit_xyzs(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT s, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xyzs_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* ss, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);
HMDT MGL_EXPORT mgl_fit_xyzas(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, HCDT s, const char *eq, const char *vars, HMDT ini, const char *opt);
uintptr_t MGL_EXPORT mgl_fit_xyzas_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, uintptr_t* ss, const char *eq, const char *vars, uintptr_t *ini, const char *opt,int, int l, int n);

/// Get last fitted formula
MGL_EXPORT_CONST const char *mgl_get_fit(HMGL gr);
int MGL_EXPORT mgl_get_fit_(uintptr_t *gr, char *out, int len);

/// Make histogram (distribution) of data. This function do not plot data.
/** Option "value" sets the size of output array (default is mglFitPnts=100). */
HMDT MGL_EXPORT mgl_hist_x(HMGL gr, HCDT x, HCDT a, const char *opt);
uintptr_t MGL_EXPORT mgl_hist_x_(uintptr_t* gr, uintptr_t* x, uintptr_t* a, const char *opt,int);
/// Make histogram (distribution) of data. This function do not plot data.
/** Option "value" sets the size of output array (default is mglFitPnts=100). */
HMDT MGL_EXPORT mgl_hist_xy(HMGL gr, HCDT x, HCDT y, HCDT a, const char *opt);
uintptr_t MGL_EXPORT mgl_hist_xy_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* a, const char *opt,int);
/// Make histogram (distribution) of data. This function do not plot data.
/** Option "value" sets the size of output array (default is mglFitPnts=100). */
HMDT MGL_EXPORT mgl_hist_xyz(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *opt);
uintptr_t MGL_EXPORT mgl_hist_xyz_(uintptr_t* gr, uintptr_t* x, uintptr_t* y, uintptr_t* z, uintptr_t* a, const char *opt,int);

/// Print fitted last formula (with coefficients)
void MGL_EXPORT mgl_puts_fit(HMGL gr, double x, double y, double z, const char *prefix, const char *font, double size);
void MGL_EXPORT mgl_puts_fit_(uintptr_t* gr, mreal *x, mreal *y, mreal *z, const char *prefix, const char *font, mreal *size, int l, int n);
/// Get chi for last fitted formula
mreal MGL_EXPORT_PURE mgl_get_fit_chi();
mreal MGL_EXPORT_PURE mgl_get_fit_chi_();
/// Get covariance matrix for last fitted formula
HCDT MGL_EXPORT_CONST mgl_get_fit_covar();
uintptr_t MGL_EXPORT_CONST mgl_get_fit_covar_();
//-----------------------------------------------------------------------------
#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
