/***************************************************************************
 * pde.h is part of Math Graphic Library
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
#ifndef _MGL_PDE_H_
#define _MGL_PDE_H_
//-----------------------------------------------------------------------------
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini
HADT MGL_EXPORT mgl_pde_solve_c(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0,const char *opt);
uintptr_t MGL_EXPORT mgl_pde_solve_c_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0,const char *opt,int,int);
/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini
HMDT MGL_EXPORT mgl_pde_solve(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0,const char *opt);
uintptr_t MGL_EXPORT mgl_pde_solve_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0,const char *opt,int,int);

/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini. This function use more accurate but slow algorithm.
HADT MGL_EXPORT mgl_pde_adv_c(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0,const char *opt);
uintptr_t MGL_EXPORT mgl_pde_adv_c_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0,const char *opt,int,int);
/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini. This function use more accurate but slow algorithm.
HMDT MGL_EXPORT mgl_pde_adv(HMGL gr, const char *ham, HCDT ini_re, HCDT ini_im, mreal dz, mreal k0,const char *opt);
uintptr_t MGL_EXPORT mgl_pde_adv_(uintptr_t* gr, const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, mreal *dz, mreal *k0,const char *opt,int,int);

/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
HADT MGL_EXPORT mgl_qo2d_solve_c(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy);
HADT MGL_EXPORT mgl_qo2d_func_c(mdual (*ham)(mreal u, mreal x, mreal y, mreal px, mreal py, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy);
uintptr_t MGL_EXPORT mgl_qo2d_solve_c_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, int);
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
HMDT MGL_EXPORT mgl_qo2d_solve(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy);
HMDT MGL_EXPORT mgl_qo2d_func(mdual (*ham)(mreal u, mreal x, mreal y, mreal px, mreal py, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy);
uintptr_t MGL_EXPORT mgl_qo2d_solve_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, int);

/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
HADT MGL_EXPORT mgl_qo3d_solve_c(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz);
HADT MGL_EXPORT mgl_qo3d_func_c(mdual (*ham)(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz);
uintptr_t MGL_EXPORT mgl_qo3d_solve_c_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, uintptr_t* zz, int);
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
HMDT MGL_EXPORT mgl_qo3d_solve(const char *ham, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz);
HMDT MGL_EXPORT mgl_qo3d_func(mdual (*ham)(mreal u, mreal x, mreal y, mreal z, mreal px, mreal py, mreal pz, void *par), void *par, HCDT ini_re, HCDT ini_im, HCDT ray, mreal r, mreal k0, HMDT xx, HMDT yy, HMDT zz);
uintptr_t MGL_EXPORT mgl_qo3d_solve_(const char *ham, uintptr_t* ini_re, uintptr_t* ini_im, uintptr_t* ray, mreal *r, mreal *k0, uintptr_t* xx, uintptr_t* yy, uintptr_t* zz, int);

/// Saves result of ODE solving of n equations with right part func and initial conditions x0 over time interval [0,tmax] with time step dt
HMDT MGL_EXPORT mgl_ode_solve(void (*func)(const mreal *x, mreal *dx, void *par), int n, const mreal *x0, mreal dt, mreal tmax, void *par);
/// Saves result of ODE solving for var variables with right part func (separated by ';') and initial conditions x0 over time interval [0,tmax] with time step dt
HMDT MGL_EXPORT mgl_ode_solve_str(const char *func, const char *var, HCDT x0, mreal dt, mreal tmax);
/// Saves result of ODE solving for var complex variables with right part func (separated by ';') and initial conditions x0 over time interval [0,tmax] with time step dt
HADT MGL_EXPORT mgl_ode_solve_str_c(const char *func, const char *var, HCDT x0, mreal dt, mreal tmax);
/// Saves result of ODE solving of n equations with right part func and initial conditions x0 over time interval [0,tmax] with time step dt. Function bord (if not NULL) is called each time step to apply border reflection.
HMDT MGL_EXPORT mgl_ode_solve_ex(void (*func)(const mreal *x, mreal *dx, void *par), int n, const mreal *x0, mreal dt, mreal tmax, void *par, void (*bord)(mreal *x, const mreal *xp, void *par));
/// Finds ray with starting point r0, p0 (and prepares ray data for mgl_qo2d_solve)
HMDT MGL_EXPORT mgl_ray_trace(const char *ham, mreal x0, mreal y0, mreal z0, mreal px, mreal py, mreal pz, mreal dt, mreal tmax);
uintptr_t MGL_EXPORT mgl_ray_trace_(const char *ham, mreal *x0, mreal *y0, mreal *z0, mreal *px, mreal *py, mreal *pz, mreal *dt, mreal *tmax,int);

/// Calculate Jacobian determinant for D{x(u,v), y(u,v)} = dx/du*dy/dv-dx/dv*dy/du
HMDT MGL_EXPORT mgl_jacobian_2d(HCDT x, HCDT y);
uintptr_t MGL_EXPORT mgl_jacobian_2d_(uintptr_t* x, uintptr_t* y);
/// Calculate Jacobian determinant for D{x(u,v,w), y(u,v,w), z(u,v,w)}
HMDT MGL_EXPORT mgl_jacobian_3d(HCDT x, HCDT y, HCDT z);
uintptr_t MGL_EXPORT mgl_jacobian_3d_(uintptr_t* x, uintptr_t* y, uintptr_t* z);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
