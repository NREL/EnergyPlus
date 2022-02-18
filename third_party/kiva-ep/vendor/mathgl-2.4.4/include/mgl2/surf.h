/***************************************************************************
 * surf.h is part of Math Graphic Library
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
#ifndef _MGL_SURF_H_
#define _MGL_SURF_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Draw surface by formula with x,y in axis range
/** Option "value" set initial number of points. */
void MGL_EXPORT mgl_fsurf(HMGL graph, const char *fz, const char *stl, const char *opt);
void MGL_EXPORT mgl_fsurf_(uintptr_t *graph, const char *fz, const char *stl, const char *opt,int,int,int);
/// Draw surface by formulas parametrically depended on u,v in range [0,1]
/** Option "value" set initial number of points. */
void MGL_EXPORT mgl_fsurf_xyz(HMGL graph, const char *fx, const char *fy, const char *fz, const char *stl, const char *opt);
void MGL_EXPORT mgl_fsurf_xyz_(uintptr_t *graph, const char *fx, const char *fy, const char *fz, const char *stl, const char *opt, int, int, int, int, int);

/// Draw grid lines for density plot of 2d data specified parametrically
void MGL_EXPORT mgl_grid_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *stl, const char *opt);
void MGL_EXPORT mgl_grid_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *stl, const char *opt,int,int);
/// Draw grid lines for density plot of 2d data
void MGL_EXPORT mgl_grid(HMGL graph, HCDT a,const char *stl, const char *opt);
void MGL_EXPORT mgl_grid_(uintptr_t *graph, uintptr_t *a,const char *stl, const char *opt,int,int);

/// Draw mesh lines for 2d data specified parametrically
void MGL_EXPORT mgl_mesh_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_mesh_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw mesh lines for 2d data
void MGL_EXPORT mgl_mesh(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_mesh_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw waterfall plot for 2d data specified parametrically
/** Style 'x' draw lines in x-direction. */
void MGL_EXPORT mgl_fall_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_fall_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw waterfall plot for 2d data
/** Style 'x' draw lines in x-direction. */
void MGL_EXPORT mgl_fall(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_fall_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw belts for 2d data specified parametrically
/** Style 'x' draw belts in x-direction. */
void MGL_EXPORT mgl_belt_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_belt_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw belts for 2d data
/** Style 'x' draw belts in x-direction. */
void MGL_EXPORT mgl_belt(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_belt_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw belts for 2d data specified parametrically with color proportional to c
/** Style 'x' draw belts in x-direction. */
void MGL_EXPORT mgl_beltc_xy(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_beltc_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw belts for 2d data with color proportional to c
/** Style 'x' draw belts in x-direction. */
void MGL_EXPORT mgl_beltc(HMGL graph, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_beltc_(uintptr_t *graph, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);

/// Draw surface for 2d data specified parametrically with color proportional to z
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surf_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_surf_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw surface for 2d data with color proportional to z
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surf(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_surf_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw density plot for 2d data specified parametrically
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_dens_xy(HMGL graph, HCDT x, HCDT y, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_dens_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw density plot for 2d data
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_dens(HMGL graph, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_dens_(uintptr_t *graph, uintptr_t *c, const char *sch, const char *opt,int,int);

/// Draw vertical boxes for 2d data specified parametrically
/** Style ‘#’ draw filled boxes. */
void MGL_EXPORT mgl_boxs_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_boxs_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw vertical boxes for 2d data
/** Style ‘#’ draw filled boxes. */
void MGL_EXPORT mgl_boxs(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_boxs_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw vertical tiles with manual colors c for 2d data specified parametrically
void MGL_EXPORT mgl_tile_xyc(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tile_xyc_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw vertical tiles for 2d data specified parametrically
void MGL_EXPORT mgl_tile_xy(HMGL graph, HCDT x, HCDT y, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tile_xy_(uintptr_t *gr, uintptr_t *x, uintptr_t *y, uintptr_t *z, const char *sch, const char *opt,int,int);
/// Draw vertical tiles for 2d data
void MGL_EXPORT mgl_tile(HMGL graph, HCDT z, const char *sch, const char *opt);
void MGL_EXPORT mgl_tile_(uintptr_t *graph, uintptr_t *z, const char *sch, const char *opt,int,int);

/// Draw vertical tiles with variable size r and manual colors c for 2d data specified parametrically
void MGL_EXPORT mgl_tiles_xyc(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_tiles_xyc_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw vertical tiles with variable size r for 2d data specified parametrically
void MGL_EXPORT mgl_tiles_xy(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT r, const char *sch, const char *opt);
void MGL_EXPORT mgl_tiles_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *r, const char *sch, const char *opt,int,int);
/// Draw vertical tiles with variable size r for 2d data
void MGL_EXPORT mgl_tiles(HMGL graph, HCDT z, HCDT r, const char *sch, const char *opt);
void MGL_EXPORT mgl_tiles_(uintptr_t *graph, uintptr_t *z, uintptr_t *r, const char *sch, const char *opt,int,int);

/// Draw surface for 2d data specified parametrically with color proportional to c
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfc_xy(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfc_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw surface for 2d data with color proportional to c
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfc(HMGL graph, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfc_(uintptr_t *graph, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);

/// Draw surface for 2d data specified parametrically with alpha proportional to c
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfa_xy(HMGL graph, HCDT x, HCDT y, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfa_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);
/// Draw surface for 2d data with alpha proportional to c
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfa(HMGL graph, HCDT z, HCDT c, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfa_(uintptr_t *graph, uintptr_t *z, uintptr_t *c, const char *sch, const char *opt,int,int);

/// Draw surface for 2d data specified parametrically with  color proportional to c and alpha proportional to a
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfca_xy(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfca_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int,int);
/// Draw surface for 2d data with  color proportional to c and alpha proportional to a
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_surfca(HMGL gr, HCDT z, HCDT c, HCDT a, const char *sch, const char *opt);
void MGL_EXPORT mgl_surfca_(uintptr_t *graph, uintptr_t *z, uintptr_t *c, uintptr_t *a, const char *sch, const char *opt,int,int);

/// Draw density plot for spectra-gramm specified parametrically
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_stfa_xy(HMGL graph, HCDT x, HCDT y, HCDT re, HCDT im, int dn, const char *sch, const char *opt);
void MGL_EXPORT mgl_stfa_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *re, uintptr_t *im, int *dn, const char *sch, const char *opt,int, int);
/// Draw density plot for spectra-gramm
/** Style ‘#’ draw grid lines. Style ‘.’ produce plot by dots.*/
void MGL_EXPORT mgl_stfa(HMGL graph, HCDT re, HCDT im, int dn, const char *sch, const char *opt);
void MGL_EXPORT mgl_stfa_(uintptr_t *graph, uintptr_t *re, uintptr_t *im, int *dn, const char *sch, const char *opt,int, int);

/// Color map of matrix a to matrix b, both matrix can parametrically depend on coordinates
/** Style ‘.’ produce plot by dots. */
void MGL_EXPORT mgl_map_xy(HMGL graph, HCDT x, HCDT y, HCDT a, HCDT b, const char *sch, const char *opt);
void MGL_EXPORT mgl_map_xy_(uintptr_t *graph, uintptr_t *x, uintptr_t *y, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int,int);
/// Color map of matrix a to matrix b
/** Style ‘.’ produce plot by dots. */
void MGL_EXPORT mgl_map(HMGL graph, HCDT a, HCDT b, const char *sch, const char *opt);
void MGL_EXPORT mgl_map_(uintptr_t *graph, uintptr_t *a, uintptr_t *b, const char *sch, const char *opt,int,int);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
