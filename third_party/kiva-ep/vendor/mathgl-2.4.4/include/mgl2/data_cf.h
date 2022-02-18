/***************************************************************************
 * data_cf.h is part of Math Graphic Library
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
#ifndef _MGL_DATA_CF_H_
#define _MGL_DATA_CF_H_
//-----------------------------------------------------------------------------
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#else
#ifdef __cplusplus
struct gsl_vector;
struct gsl_matrix;
#else
typedef void gsl_vector;
typedef void gsl_matrix;
#endif
#endif
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif
/// Get integer power of x
double MGL_EXPORT_CONST mgl_ipow(double x,int n);
double MGL_EXPORT_PURE mgl_ipow_(mreal *x,int *n);
/// Get number of seconds since 1970 for given string
double MGL_EXPORT mgl_get_time(const char *time, const char *fmt);
double MGL_EXPORT mgl_get_time_(const char *time, const char *fmt,int,int);

/// Create HMDT object
HMDT MGL_EXPORT mgl_create_data();
uintptr_t MGL_EXPORT mgl_create_data_();
/// Create HMDT object with specified sizes
HMDT MGL_EXPORT mgl_create_data_size(long nx, long ny, long nz);
uintptr_t MGL_EXPORT mgl_create_data_size_(int *nx, int *ny, int *nz);
/// Create HMDT object with data from file
HMDT MGL_EXPORT mgl_create_data_file(const char *fname);
uintptr_t MGL_EXPORT mgl_create_data_file_(const char *fname, int len);
/// Delete HMDT object
void MGL_EXPORT mgl_delete_data(HMDT dat);
void MGL_EXPORT mgl_delete_data_(uintptr_t *dat);

/// Rearange data dimensions
void MGL_EXPORT mgl_data_rearrange(HMDT dat, long mx,long my,long mz);
void MGL_EXPORT mgl_data_rearrange_(uintptr_t *dat, int *mx, int *my, int *mz);
/// Link external data array (don't delete it at exit)
void MGL_EXPORT mgl_data_link(HMDT dat, mreal *A,long mx,long my,long mz);
void MGL_EXPORT mgl_data_link_(uintptr_t *d, mreal *A, int *nx,int *ny,int *nz);
/// Allocate memory and copy the data from the (float *) array
void MGL_EXPORT mgl_data_set_float(HMDT dat, const float *A,long mx,long my,long mz);
void MGL_EXPORT mgl_data_set_float_(uintptr_t *dat, const float *A,int *NX,int *NY,int *NZ);
void MGL_EXPORT mgl_data_set_float1_(uintptr_t *d, const float *A,int *N1);
/// Allocate memory and copy the data from the (double *) array
void MGL_EXPORT mgl_data_set_double(HMDT dat, const double *A,long mx,long my,long mz);
void MGL_EXPORT mgl_data_set_double_(uintptr_t *dat, const double *A,int *NX,int *NY,int *NZ);
void MGL_EXPORT mgl_data_set_double1_(uintptr_t *d, const double *A,int *N1);
/// Allocate memory and copy the data from the (float **) array
void MGL_EXPORT mgl_data_set_float2(HMDT d, float const * const *A,long N1,long N2);
void MGL_EXPORT mgl_data_set_float2_(uintptr_t *d, const float *A,int *N1,int *N2);
/// Allocate memory and copy the data from the (double **) array
void MGL_EXPORT mgl_data_set_double2(HMDT d, double const * const *A,long N1,long N2);
void MGL_EXPORT mgl_data_set_double2_(uintptr_t *d, const double *A,int *N1,int *N2);
/// Allocate memory and copy the data from the (float ***) array
void MGL_EXPORT mgl_data_set_float3(HMDT d, float const * const * const *A,long N1,long N2,long N3);
void MGL_EXPORT mgl_data_set_float3_(uintptr_t *d, const float *A,int *N1,int *N2,int *N3);
/// Allocate memory and copy the data from the (double ***) array
void MGL_EXPORT mgl_data_set_double3(HMDT d, double const * const * const *A,long N1,long N2,long N3);
void MGL_EXPORT mgl_data_set_double3_(uintptr_t *d, const double *A,int *N1,int *N2,int *N3);
/// Import data from abstract type
void MGL_EXPORT mgl_data_set(HMDT dat, HCDT a);
void MGL_EXPORT mgl_data_set_(uintptr_t *dat, uintptr_t *a);
/// Allocate memory and copy the data from the gsl_vector
void MGL_EXPORT mgl_data_set_vector(HMDT dat, gsl_vector *v);
/// Allocate memory and copy the data from the gsl_matrix
void MGL_EXPORT mgl_data_set_matrix(HMDT dat, gsl_matrix *m);
/// Set value of data element [i,j,k]
void MGL_EXPORT mgl_data_set_value(HMDT dat, mreal v, long i, long j, long k);
void MGL_EXPORT mgl_data_set_value_(uintptr_t *d, mreal *v, int *i, int *j, int *k);
/// Get value of data element [i,j,k]
mreal MGL_EXPORT mgl_data_get_value(HCDT dat, long i, long j, long k);
mreal MGL_EXPORT mgl_data_get_value_(uintptr_t *d, int *i, int *j, int *k);
/// Allocate memory and scanf the data from the string
void MGL_EXPORT mgl_data_set_values(HMDT dat, const char *val, long nx, long ny, long nz);
void MGL_EXPORT mgl_data_set_values_(uintptr_t *d, const char *val, int *nx, int *ny, int *nz, int l);

/// Read data array from HDF file (parse HDF4 and HDF5 files)
int MGL_EXPORT mgl_data_read_hdf(HMDT d,const char *fname,const char *data);
int MGL_EXPORT mgl_data_read_hdf_(uintptr_t *d, const char *fname, const char *data,int l,int n);
/// Read data from tab-separated text file with auto determining size
int MGL_EXPORT mgl_data_read(HMDT dat, const char *fname);
int MGL_EXPORT mgl_data_read_(uintptr_t *d, const char *fname,int l);
/// Read data from text file with size specified at beginning of the file
int MGL_EXPORT mgl_data_read_mat(HMDT dat, const char *fname, long dim);
int MGL_EXPORT mgl_data_read_mat_(uintptr_t *dat, const char *fname, int *dim, int);
/// Read data from text file with specifeid size
int MGL_EXPORT mgl_data_read_dim(HMDT dat, const char *fname,long mx,long my,long mz);
int MGL_EXPORT mgl_data_read_dim_(uintptr_t *dat, const char *fname,int *mx,int *my,int *mz,int);
/// Read data from tab-separated text files with auto determining size which filenames are result of sprintf(fname,templ,t) where t=from:step:to
int MGL_EXPORT mgl_data_read_range(HMDT d, const char *templ, double n1, double n2, double step, int as_slice);
int MGL_EXPORT mgl_data_read_range_(uintptr_t *d, const char *fname, mreal *n1, mreal *n2, mreal *step, int *as_slice,int l);
/// Read data from tab-separated text files with auto determining size which filenames are satisfied to template (like "t_*.dat")
int MGL_EXPORT mgl_data_read_all(HMDT dat, const char *templ, int as_slice);
int MGL_EXPORT mgl_data_read_all_(uintptr_t *d, const char *fname, int *as_slice,int l);
/// Import data array from PNG file according color scheme
void MGL_EXPORT mgl_data_import(HMDT dat, const char *fname, const char *scheme,mreal v1,mreal v2);
void MGL_EXPORT mgl_data_import_(uintptr_t *dat, const char *fname, const char *scheme,mreal *v1,mreal *v2,int,int);
/// Scan textual file for template and fill data array
int MGL_EXPORT mgl_data_scan_file(HMDT dat,const char *fname, const char *templ);
int MGL_EXPORT mgl_data_scan_file_(uintptr_t *dat,const char *fname, const char *templ,int,int);
/// Read data array from Tektronix WFM file
/** Parse Tektronix TDS5000/B, TDS6000/B/C, TDS/CSA7000/B, MSO70000/C, DSA70000/B/C DPO70000/B/C DPO7000/ MSO/DPO5000. */
int MGL_EXPORT mgl_data_read_wfm(HMDT d,const char *fname, long num, long step, long start);
int MGL_EXPORT mgl_data_read_wfm_(uintptr_t *d, const char *fname, long *num, long *step, long *start,int l);
/// Read data array from Matlab MAT file (parse versions 4 and 5)
int MGL_EXPORT mgl_data_read_matlab(HMDT d,const char *fname,const char *data);
int MGL_EXPORT mgl_data_read_matlab_(uintptr_t *d, const char *fname, const char *data,int l,int n);

/// Create or recreate the array with specified size and fill it by zero
void MGL_EXPORT mgl_data_create(HMDT dat, long nx,long ny,long nz);
void MGL_EXPORT mgl_data_create_(uintptr_t *dat, int *nx,int *ny,int *nz);
/// Transpose dimensions of the data (generalization of Transpose)
void MGL_EXPORT mgl_data_transpose(HMDT dat, const char *dim);
void MGL_EXPORT mgl_data_transpose_(uintptr_t *dat, const char *dim,int);
/// Normalize the data to range [v1,v2]
void MGL_EXPORT mgl_data_norm(HMDT dat, mreal v1,mreal v2,int sym,long dim);
void MGL_EXPORT mgl_data_norm_(uintptr_t *dat, mreal *v1,mreal *v2,int *sym,int *dim);
/// Normalize the data to range [v1,v2] slice by slice
void MGL_EXPORT mgl_data_norm_slice(HMDT dat, mreal v1,mreal v2,char dir,long keep_en,long sym);
void MGL_EXPORT mgl_data_norm_slice_(uintptr_t *dat, mreal *v1,mreal *v2,char *dir,int *keep_en,int *sym,int l);
/// Limit the data to be inside [-v,v], keeping the original sign
void MGL_EXPORT mgl_data_limit(HMDT dat, mreal v);
void MGL_EXPORT mgl_data_limit_(uintptr_t *dat, mreal *v);
/// Project the periodical data to range [v1,v2] (like mod() function). Separate branches by NAN if sep=true.
void MGL_EXPORT mgl_data_coil(HMDT dat, mreal v1, mreal v2, int sep);
void MGL_EXPORT mgl_data_coil_(uintptr_t *dat, mreal *v1, mreal *v2, int *sep);

/// Get sub-array of the data with given fixed indexes
HMDT MGL_EXPORT mgl_data_subdata(HCDT dat, long xx,long yy,long zz);
uintptr_t MGL_EXPORT mgl_data_subdata_(uintptr_t *dat, int *xx,int *yy,int *zz);
/// Get sub-array of the data with given fixed indexes (like indirect access)
HMDT MGL_EXPORT mgl_data_subdata_ext(HCDT dat, HCDT xx, HCDT yy, HCDT zz);
uintptr_t MGL_EXPORT mgl_data_subdata_ext_(uintptr_t *dat, uintptr_t *xx,uintptr_t *yy,uintptr_t *zz);
/// Get column (or slice) of the data filled by formulas of named columns
HMDT MGL_EXPORT mgl_data_column(HCDT dat, const char *eq);
uintptr_t MGL_EXPORT mgl_data_column_(uintptr_t *dat, const char *eq,int l);
/// Get data from sections ids, separated by value val along specified direction.
/** If section id is negative then reverse order is used (i.e. -1 give last section). */
HMDT MGL_EXPORT mgl_data_section(HCDT dat, HCDT ids, char dir, mreal val);
uintptr_t MGL_EXPORT mgl_data_section_(uintptr_t *d, uintptr_t *ids, const char *dir, mreal *val,int);
/// Get data from section id, separated by value val along specified direction.
/** If section id is negative then reverse order is used (i.e. -1 give last section). */
HMDT MGL_EXPORT mgl_data_section_val(HCDT dat, long id, char dir, mreal val);
uintptr_t MGL_EXPORT mgl_data_section_val_(uintptr_t *d, int *id, const char *dir, mreal *val,int);
/// Get contour lines for dat[i,j]=val. NAN values separate the the curves
HMDT mgl_data_conts(mreal val, HCDT dat);

/// Equidistantly fill the data to range [x1,x2] in direction dir
void MGL_EXPORT mgl_data_fill(HMDT dat, mreal x1,mreal x2,char dir);
void MGL_EXPORT mgl_data_fill_(uintptr_t *dat, mreal *x1,mreal *x2,const char *dir,int);
/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
void MGL_EXPORT mgl_data_fill_eq(HMGL gr, HMDT dat, const char *eq, HCDT vdat, HCDT wdat,const char *opt);
void MGL_EXPORT mgl_data_fill_eq_(uintptr_t *gr, uintptr_t *dat, const char *eq, uintptr_t *vdat, uintptr_t *wdat,const char *opt, int, int);
/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in range [x1,x2] using global spline
void MGL_EXPORT mgl_data_refill_gs(HMDT dat, HCDT xdat, HCDT vdat, mreal x1, mreal x2, long sl);
void MGL_EXPORT mgl_data_refill_gs_(uintptr_t *dat, uintptr_t *xdat, uintptr_t *vdat, mreal *x1, mreal *x2, long *sl);
/// Fill dat by interpolated values of vdat parametrically depended on xdat for x in range [x1,x2]
void MGL_EXPORT mgl_data_refill_x(HMDT dat, HCDT xdat, HCDT vdat, mreal x1, mreal x2, long sl);
void MGL_EXPORT mgl_data_refill_x_(uintptr_t *dat, uintptr_t *xdat, uintptr_t *vdat, mreal *x1, mreal *x2, long *sl);
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat for x,y in range [x1,x2]*[y1,y2]
void MGL_EXPORT mgl_data_refill_xy(HMDT dat, HCDT xdat, HCDT ydat, HCDT vdat, mreal x1, mreal x2, mreal y1, mreal y2, long sl);
void MGL_EXPORT mgl_data_refill_xy_(uintptr_t *dat, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, long *sl);
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [x1,x2]*[y1,y2]*[z1,z2]
void MGL_EXPORT mgl_data_refill_xyz(HMDT dat, HCDT xdat, HCDT ydat, HCDT zdat, HCDT vdat, mreal x1, mreal x2, mreal y1, mreal y2, mreal z1, mreal z2);
void MGL_EXPORT mgl_data_refill_xyz_(uintptr_t *dat, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2);
/// Fill dat by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range
void MGL_EXPORT mgl_data_refill_gr(HMGL gr, HMDT dat, HCDT xdat, HCDT ydat, HCDT zdat, HCDT vdat, long sl, const char *opt);
void MGL_EXPORT mgl_data_refill_gr_(uintptr_t *gr, uintptr_t *dat, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, long *sl, const char *opt,int);
/// Set the data by triangulated surface values assuming x,y,z in range [r1,r2]
void MGL_EXPORT mgl_data_grid(HMGL gr, HMDT d, HCDT xdat, HCDT ydat, HCDT zdat,const char *opt);
void MGL_EXPORT mgl_data_grid_(uintptr_t *gr, uintptr_t *dat, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, const char *opt,int);
/// Set the data by triangulated surface values assuming x,y,z in range [x1,x2]*[y1,y2]
void MGL_EXPORT mgl_data_grid_xy(HMDT d, HCDT xdat, HCDT ydat, HCDT zdat, mreal x1, mreal x2, mreal y1, mreal y2);
void MGL_EXPORT mgl_data_grid_xy_(uintptr_t *dat, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2);
/// Put value to data element(s)
void MGL_EXPORT mgl_data_put_val(HMDT dat, mreal val, long i, long j, long k);
void MGL_EXPORT mgl_data_put_val_(uintptr_t *dat, mreal *val, int *i, int *j, int *k);
/// Put array to data element(s)
void MGL_EXPORT mgl_data_put_dat(HMDT dat, HCDT val, long i, long j, long k);
void MGL_EXPORT mgl_data_put_dat_(uintptr_t *dat, uintptr_t *val, int *i, int *j, int *k);
/// Modify the data by specified formula
void MGL_EXPORT mgl_data_modify(HMDT dat, const char *eq,long dim);
void MGL_EXPORT mgl_data_modify_(uintptr_t *dat, const char *eq,int *dim,int);
/// Modify the data by specified formula
void MGL_EXPORT mgl_data_modify_vw(HMDT dat, const char *eq,HCDT vdat,HCDT wdat);
void MGL_EXPORT mgl_data_modify_vw_(uintptr_t *dat, const char *eq, uintptr_t *vdat, uintptr_t *wdat,int);
/// Reduce size of the data
void MGL_EXPORT mgl_data_squeeze(HMDT dat, long rx,long ry,long rz,long smooth);
void MGL_EXPORT mgl_data_squeeze_(uintptr_t *dat, int *rx,int *ry,int *rz,int *smooth);

/// Get array which is n-th pairs {x[i],y[i]} for iterated function system (fractal) generated by A
/** NOTE: A.nx must be >= 7. */
HMDT MGL_EXPORT mgl_data_ifs_2d(HCDT A, long n, long skip);
uintptr_t MGL_EXPORT mgl_data_ifs_2d_(uintptr_t *A, long *n, long *skip);
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) generated by A
/** NOTE: A.nx must be >= 13. */
HMDT MGL_EXPORT mgl_data_ifs_3d(HCDT A, long n, long skip);
uintptr_t MGL_EXPORT mgl_data_ifs_3d_(uintptr_t *A, long *n, long *skip);
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) defined in *.ifs file 'fname' and named as 'name'
HMDT MGL_EXPORT mgl_data_ifs_file(const char *fname, const char *name, long n, long skip);
uintptr_t mgl_data_ifs_file_(const char *fname, const char *name, long *n, long *skip,int l,int m);
/// Codes for flame fractal functions
enum {
	mglFlame2d_linear=0,	mglFlame2d_sinusoidal,	mglFlame2d_spherical,	mglFlame2d_swirl,		mglFlame2d_horseshoe,
	mglFlame2d_polar,		mglFlame2d_handkerchief,mglFlame2d_heart,		mglFlame2d_disc,		mglFlame2d_spiral,
	mglFlame2d_hyperbolic,	mglFlame2d_diamond,		mglFlame2d_ex,			mglFlame2d_julia,		mglFlame2d_bent,
	mglFlame2d_waves,		mglFlame2d_fisheye,		mglFlame2d_popcorn,		mglFlame2d_exponential,	mglFlame2d_power,
	mglFlame2d_cosine,		mglFlame2d_rings,		mglFlame2d_fan,			mglFlame2d_blob,		mglFlame2d_pdj,
	mglFlame2d_fan2,		mglFlame2d_rings2,		mglFlame2d_eyefish,		mglFlame2d_bubble,		mglFlame2d_cylinder,
	mglFlame2d_perspective,	mglFlame2d_noise,		mglFlame2d_juliaN,		mglFlame2d_juliaScope,	mglFlame2d_blur,
	mglFlame2d_gaussian,	mglFlame2d_radialBlur,	mglFlame2d_pie,			mglFlame2d_ngon,		mglFlame2d_curl,
	mglFlame2d_rectangles,	mglFlame2d_arch,		mglFlame2d_tangent,		mglFlame2d_square,		mglFlame2d_blade,
	mglFlame2d_secant,		mglFlame2d_rays,		mglFlame2d_twintrian,	mglFlame2d_cross,		mglFlame2d_disc2,
	mglFlame2d_supershape,	mglFlame2d_flower,		mglFlame2d_conic,		mglFlame2d_parabola,	mglFlame2d_bent2,
	mglFlame2d_bipolar,		mglFlame2d_boarders,	mglFlame2d_butterfly,	mglFlame2d_cell,		mglFlame2d_cpow,
	mglFlame2d_curve,		mglFlame2d_edisc,		mglFlame2d_elliptic,	mglFlame2d_escher,		mglFlame2d_foci,
	mglFlame2d_lazySusan,	mglFlame2d_loonie,		mglFlame2d_preBlur,		mglFlame2d_modulus,		mglFlame2d_oscope,
	mglFlame2d_polar2,		mglFlame2d_popcorn2,	mglFlame2d_scry,		mglFlame2d_separation,	mglFlame2d_split,
	mglFlame2d_splits,		mglFlame2d_stripes,		mglFlame2d_wedge,		mglFlame2d_wedgeJulia,	mglFlame2d_wedgeSph,
	mglFlame2d_whorl,		mglFlame2d_waves2,		mglFlame2d_exp,			mglFlame2d_log,			mglFlame2d_sin,
	mglFlame2d_cos,			mglFlame2d_tan,			mglFlame2d_sec,			mglFlame2d_csc,			mglFlame2d_cot,
	mglFlame2d_sinh,		mglFlame2d_cosh,		mglFlame2d_tanh,		mglFlame2d_sech,		mglFlame2d_csch,
	mglFlame2d_coth,		mglFlame2d_auger,		mglFlame2d_flux,		mglFlame2dLAST
};
/// Get array which is n-th pairs {x[i],y[i]} for Flame fractal generated by A with functions F
/** NOTE: A.nx must be >= 7 and F.nx >= 2 and F.nz=A.ny.
 * F[0,i,j] denote function id. F[1,i,j] give function weight. F(2:5,i,j) provide function parameters.
 * Resulting point is {xnew,ynew} = sum_i F[1,i,j]*F[0,i,j]{IFS2d(A[j]){x,y}}. */
HMDT MGL_EXPORT mgl_data_flame_2d(HCDT A, HCDT F, long n, long skip);
uintptr_t MGL_EXPORT mgl_data_flame_2d_(uintptr_t *A, uintptr_t *F, long *n, long *skip);

/// Get curves, separated by NAN, for maximal values of array d as function of x coordinate.
/** Noises below lvl amplitude are ignored.
  * Parameter dy \in [0,ny] set the "attraction" distance of points to curve. */
HMDT MGL_EXPORT mgl_data_detect(HCDT d, mreal lvl, mreal dj, mreal di, mreal min_len);
uintptr_t MGL_EXPORT mgl_data_detect_(uintptr_t *d, mreal *lvl, mreal *dj, mreal *di, mreal *min_len);

/// Get array as solution of tridiagonal matrix solution a[i]*x[i-1]+b[i]*x[i]+c[i]*x[i+1]=d[i]
/** String \a how may contain:
 * 'x', 'y', 'z' for solving along x-,y-,z-directions, or
 * 'h' for solving along hexagonal direction at x-y plain (need nx=ny),
 * 'c' for using periodical boundary conditions,
 * 'd' for diffraction/diffuse calculation.
 * NOTE: It work for flat data model only (i.e. for a[i,j]==a[i+nx*j]) */
HMDT MGL_EXPORT mgl_data_tridmat(HCDT A, HCDT B, HCDT C, HCDT D, const char *how);
uintptr_t MGL_EXPORT mgl_data_tridmat_(uintptr_t *A, uintptr_t *B, uintptr_t *C, uintptr_t *D, const char *how, int);

/// Returns pointer to data element [i,j,k]
MGL_EXPORT mreal *mgl_data_value(HMDT dat, long i,long j,long k);
/// Returns pointer to internal data array
MGL_EXPORT_PURE mreal *mgl_data_data(HMDT dat);

/// Gets the x-size of the data.
long MGL_EXPORT mgl_data_get_nx(HCDT d);
long MGL_EXPORT mgl_data_get_nx_(uintptr_t *d);
/// Gets the y-size of the data.
long MGL_EXPORT mgl_data_get_ny(HCDT d);
long MGL_EXPORT mgl_data_get_ny_(uintptr_t *d);
/// Gets the z-size of the data.
long MGL_EXPORT mgl_data_get_nz(HCDT d);
long MGL_EXPORT mgl_data_get_nz_(uintptr_t *d);

/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
HMDT MGL_EXPORT mgl_data_combine(HCDT dat1, HCDT dat2);
uintptr_t MGL_EXPORT mgl_data_combine_(uintptr_t *dat1, uintptr_t *dat2);
/// Extend data dimensions
void MGL_EXPORT mgl_data_extend(HMDT dat, long n1, long n2);
void MGL_EXPORT mgl_data_extend_(uintptr_t *dat, int *n1, int *n2);
/// Insert data rows/columns/slices
void MGL_EXPORT mgl_data_insert(HMDT dat, char dir, long at, long num);
void MGL_EXPORT mgl_data_insert_(uintptr_t *dat, const char *dir, int *at, int *num, int);
/// Delete data rows/columns/slices
void MGL_EXPORT mgl_data_delete(HMDT dat, char dir, long at, long num);
void MGL_EXPORT mgl_data_delete_(uintptr_t *dat, const char *dir, int *at, int *num, int);
/// Joind another data array
void MGL_EXPORT mgl_data_join(HMDT dat, HCDT d);
void MGL_EXPORT mgl_data_join_(uintptr_t *dat, uintptr_t *d);

/// Smooth the data on specified direction or directions
/** String \a dir may contain:
 *  ‘x’, ‘y’, ‘z’ for 1st, 2nd or 3d dimension;
 *  ‘dN’ for linear averaging over N points;
 *  ‘3’ for linear averaging over 3 points;
 *  ‘5’ for linear averaging over 5 points.
 *  By default quadratic averaging over 5 points is used. */
void MGL_EXPORT mgl_data_smooth(HMDT d, const char *dirs, mreal delta);
void MGL_EXPORT mgl_data_smooth_(uintptr_t *dat, const char *dirs, mreal *delta,int);
/// Get array which is result of summation in given direction or directions
HMDT MGL_EXPORT mgl_data_sum(HCDT dat, const char *dir);
uintptr_t MGL_EXPORT mgl_data_sum_(uintptr_t *dat, const char *dir,int);
/// Get array which is result of maximal values in given direction or directions
HMDT MGL_EXPORT mgl_data_max_dir(HCDT dat, const char *dir);
uintptr_t MGL_EXPORT mgl_data_max_dir_(uintptr_t *dat, const char *dir,int);
/// Get array which is result of minimal values in given direction or directions
HMDT MGL_EXPORT mgl_data_min_dir(HCDT dat, const char *dir);
uintptr_t MGL_EXPORT mgl_data_min_dir_(uintptr_t *dat, const char *dir,int);
/// Get positions of local maximums and minimums
HMDT MGL_EXPORT mgl_data_minmax(HCDT dat);
uintptr_t MGL_EXPORT mgl_data_minmax_(uintptr_t *dat);
/// Get indexes of a set of connected surfaces for set of values {a_ijk,b_ijk} as dependent on j,k
/** NOTE: not optimized for general case!!! */
HMDT MGL_EXPORT mgl_data_connect(HCDT a, HCDT b);
uintptr_t MGL_EXPORT mgl_data_connect_(uintptr_t *a, uintptr_t *b);
/// Resort data values according found connected surfaces for set of values {a_ijk,b_ijk} as dependent on j,k
/** NOTE: not optimized for general case!!! */
void MGL_EXPORT mgl_data_connect_r(HMDT a, HMDT b);
void MGL_EXPORT mgl_data_connect_r_(uintptr_t *a, uintptr_t *b);

/// Cumulative summation the data in given direction or directions
void MGL_EXPORT mgl_data_cumsum(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_cumsum_(uintptr_t *dat, const char *dir,int);
/// Integrate (cumulative summation) the data in given direction or directions
void MGL_EXPORT mgl_data_integral(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_integral_(uintptr_t *dat, const char *dir,int);
/// Differentiate the data in given direction or directions
void MGL_EXPORT mgl_data_diff(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_diff_(uintptr_t *dat, const char *dir,int);
/// Differentiate the parametrically specified data along direction v1 with v2,v3=const (v3 can be NULL)
void MGL_EXPORT mgl_data_diff_par(HMDT dat, HCDT v1, HCDT v2, HCDT v3);
void MGL_EXPORT mgl_data_diff_par_(uintptr_t *dat, uintptr_t *v1, uintptr_t *v2, uintptr_t *v3);
/// Double-differentiate (like Laplace operator) the data in given direction
void MGL_EXPORT mgl_data_diff2(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_diff2_(uintptr_t *dat, const char *dir,int);
/// Swap left and right part of the data in given direction (useful for Fourier spectrum)
void MGL_EXPORT mgl_data_swap(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_swap_(uintptr_t *dat, const char *dir,int);
/// Roll data along direction dir by num slices
void MGL_EXPORT mgl_data_roll(HMDT dat, char dir, long num);
void MGL_EXPORT mgl_data_roll_(uintptr_t *dat, const char *dir, int *num, int);
/// Mirror the data in given direction (useful for Fourier spectrum)
void MGL_EXPORT mgl_data_mirror(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_mirror_(uintptr_t *dat, const char *dir,int);
/// Sort rows (or slices) by values of specified column
void MGL_EXPORT mgl_data_sort(HMDT dat, long idx, long idy);
void MGL_EXPORT mgl_data_sort_(uintptr_t *dat, int *idx, int *idy);
/// Return dilated array of 0 or 1 for data values larger val
void MGL_EXPORT mgl_data_dilate(HMDT dat, mreal val, long step);
void MGL_EXPORT mgl_data_dilate_(uintptr_t *dat, mreal *val, int *step);
/// Return eroded array of 0 or 1 for data values larger val
void MGL_EXPORT mgl_data_erode(HMDT dat, mreal val, long step);
void MGL_EXPORT mgl_data_erode_(uintptr_t *dat, mreal *val, int *step);

/// Apply Hankel transform
void MGL_EXPORT mgl_data_hankel(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_hankel_(uintptr_t *dat, const char *dir,int);
/// Apply Sin-Fourier transform
void MGL_EXPORT mgl_data_sinfft(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_sinfft_(uintptr_t *dat, const char *dir,int);
/// Apply Cos-Fourier transform
void MGL_EXPORT mgl_data_cosfft(HMDT dat, const char *dir);
void MGL_EXPORT mgl_data_cosfft_(uintptr_t *dat, const char *dir,int);
/// Fill data by coordinates/momenta samples for Hankel ('h') or Fourier ('f') transform
/** Parameter \a how may contain:
 * ‘x‘,‘y‘,‘z‘ for direction (only one will be used),
 * ‘k‘ for momenta samples,
 * ‘h‘ for Hankel samples,
 * ‘f‘ for Cartesian/Fourier samples (default). */
void MGL_EXPORT mgl_data_fill_sample(HMDT dat, const char *how);
void MGL_EXPORT mgl_data_fill_sample_(uintptr_t *dat, const char *how,int);
/// Find correlation between 2 data arrays
HMDT MGL_EXPORT mgl_data_correl(HCDT dat1, HCDT dat2, const char *dir);
uintptr_t MGL_EXPORT mgl_data_correl_(uintptr_t *dat1, uintptr_t *dat2, const char *dir,int);
/// Apply wavelet transform
/** Parameter \a dir may contain:
 * ‘x‘,‘y‘,‘z‘ for directions,
 * ‘d‘ for daubechies, ‘D‘ for centered daubechies,
 * ‘h‘ for haar, ‘H‘ for centered haar,
 * ‘b‘ for bspline, ‘B‘ for centered bspline,
 * ‘i‘ for applying inverse transform. */
void MGL_EXPORT mgl_data_wavelet(HMDT dat, const char *how, int k);
void MGL_EXPORT mgl_data_wavelet_(uintptr_t *d, const char *dir, int *k,int);

/// Allocate and prepare data for Fourier transform by nthr threads
MGL_EXPORT void *mgl_fft_alloc(long n, void **space, long nthr);
MGL_EXPORT void *mgl_fft_alloc_thr(long n);
/// Free data for Fourier transform
void MGL_EXPORT mgl_fft_free(void *wt, void **ws, long nthr);
void MGL_EXPORT mgl_fft_free_thr(void *wt);
/// Make Fourier transform of data x of size n and step s between points
void MGL_EXPORT mgl_fft(double *x, long s, long n, const void *wt, void *ws, int inv);
/// Clear internal data for speeding up FFT and Hankel transforms
void MGL_EXPORT mgl_clear_fft();

/// Interpolate by cubic spline the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
mreal MGL_EXPORT mgl_data_spline(HCDT dat, mreal x,mreal y,mreal z);
mreal MGL_EXPORT mgl_data_spline_(uintptr_t *dat, mreal *x,mreal *y,mreal *z);
/// Interpolate by cubic spline the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
mreal MGL_EXPORT mgl_data_spline_ext(HCDT dat, mreal x,mreal y,mreal z, mreal *dx,mreal *dy,mreal *dz);
mreal MGL_EXPORT mgl_data_spline_ext_(uintptr_t *dat, mreal *x,mreal *y,mreal *z, mreal *dx,mreal *dy,mreal *dz);
/// Prepare coefficients for global spline interpolation
HMDT MGL_EXPORT mgl_gspline_init(HCDT x, HCDT v);
uintptr_t MGL_EXPORT mgl_gspline_init_(uintptr_t *x, uintptr_t *v);
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
mreal MGL_EXPORT mgl_gspline(HCDT coef, mreal dx, mreal *d1, mreal *d2);
mreal MGL_EXPORT mgl_gspline_(uintptr_t *c, mreal *dx, mreal *d1, mreal *d2);
/// Return an approximated x-value (root) when dat(x) = val
mreal MGL_EXPORT mgl_data_solve_1d(HCDT dat, mreal val, int spl, long i0);
mreal MGL_EXPORT mgl_data_solve_1d_(uintptr_t *dat, mreal *val, int *spl, int *i0);
/// Return an approximated value (root) when dat(x) = val
HMDT MGL_EXPORT mgl_data_solve(HCDT dat, mreal val, char dir, HCDT i0, int norm);
uintptr_t MGL_EXPORT mgl_data_solve_(uintptr_t *dat, mreal *val, const char *dir, uintptr_t *i0, int *norm,int);

/// Get trace of the data array
HMDT MGL_EXPORT mgl_data_trace(HCDT d);
uintptr_t MGL_EXPORT mgl_data_trace_(uintptr_t *d);
/// Resize the data to new sizes
HMDT MGL_EXPORT mgl_data_resize(HCDT dat, long mx,long my,long mz);
uintptr_t MGL_EXPORT mgl_data_resize_(uintptr_t *dat, int *mx,int *my,int *mz);
/// Resize the data to new sizes of box [x1,x2]*[y1,y2]*[z1,z2]
HMDT MGL_EXPORT mgl_data_resize_box(HCDT dat, long mx,long my,long mz,mreal x1,mreal x2,mreal y1,mreal y2,mreal z1,mreal z2);
uintptr_t MGL_EXPORT mgl_data_resize_box_(uintptr_t *dat, int *mx,int *my,int *mz,mreal *x1,mreal *x2,mreal *y1,mreal *y2,mreal *z1,mreal *z2);
/// Create n-th points distribution of this data values in range [v1, v2]
HMDT MGL_EXPORT mgl_data_hist(HCDT dat, long n, mreal v1, mreal v2, long nsub);
uintptr_t MGL_EXPORT mgl_data_hist_(uintptr_t *dat, int *n, mreal *v1, mreal *v2, int *nsub);
/// Create n-th points distribution of this data values in range [v1, v2] with weight w
HMDT MGL_EXPORT mgl_data_hist_w(HCDT dat, HCDT weight, long n, mreal v1, mreal v2, long nsub);
uintptr_t MGL_EXPORT mgl_data_hist_w_(uintptr_t *dat, uintptr_t *weight, int *n, mreal *v1, mreal *v2, int *nsub);
/// Get momentum (1D-array) of data along direction 'dir'. String looks like "x1" for median in x-direction, "x2" for width in x-dir and so on.
HMDT MGL_EXPORT mgl_data_momentum(HCDT dat, char dir, const char *how);
uintptr_t MGL_EXPORT mgl_data_momentum_(uintptr_t *dat, char *dir, const char *how, int,int);
/// Get pulse properties: pulse maximum and its position, pulse duration near maximum and by half height.
HMDT MGL_EXPORT mgl_data_pulse(HCDT dat, char dir);
uintptr_t MGL_EXPORT mgl_data_pulse_(uintptr_t *dat, char *dir,int);
/// Get array which values is result of interpolation this for coordinates from other arrays
HMDT MGL_EXPORT mgl_data_evaluate(HCDT dat, HCDT idat, HCDT jdat, HCDT kdat, int norm);
uintptr_t MGL_EXPORT mgl_data_evaluate_(uintptr_t *dat, uintptr_t *idat, uintptr_t *jdat, uintptr_t *kdat, int *norm);
/// Set as the data envelop
void MGL_EXPORT mgl_data_envelop(HMDT dat, char dir);
void MGL_EXPORT mgl_data_envelop_(uintptr_t *dat, const char *dir, int);
/// Remove phase jump
void MGL_EXPORT mgl_data_sew(HMDT dat, const char *dirs, mreal da);
void MGL_EXPORT mgl_data_sew_(uintptr_t *dat, const char *dirs, mreal *da, int);
/// Crop the data
void MGL_EXPORT mgl_data_crop(HMDT dat, long n1, long n2, char dir);
void MGL_EXPORT mgl_data_crop_(uintptr_t *dat, int *n1, int *n2, const char *dir,int);
/// Crop the data to be most optimal for FFT (i.e. to closest value of 2^n*3^m*5^l)
void MGL_EXPORT mgl_data_crop_opt(HMDT dat, const char *how);
void MGL_EXPORT mgl_data_crop_opt_(uintptr_t *dat, const char *how,int);
/// Remove rows with duplicate values in column id
void MGL_EXPORT mgl_data_clean(HMDT dat, long id);
void MGL_EXPORT mgl_data_clean_(uintptr_t *dat, int *id);

/// Multiply the data by other one for each element
void MGL_EXPORT mgl_data_mul_dat(HMDT dat, HCDT d);
void MGL_EXPORT mgl_data_mul_dat_(uintptr_t *dat, uintptr_t *d);
/// Divide the data by other one for each element
void MGL_EXPORT mgl_data_div_dat(HMDT dat, HCDT d);
void MGL_EXPORT mgl_data_div_dat_(uintptr_t *dat, uintptr_t *d);
/// Add the other data
void MGL_EXPORT mgl_data_add_dat(HMDT dat, HCDT d);
void MGL_EXPORT mgl_data_add_dat_(uintptr_t *dat, uintptr_t *d);
/// Subtract the other data
void MGL_EXPORT mgl_data_sub_dat(HMDT dat, HCDT d);
void MGL_EXPORT mgl_data_sub_dat_(uintptr_t *dat, uintptr_t *d);
/// Multiply each element by the number
void MGL_EXPORT mgl_data_mul_num(HMDT dat, mreal d);
void MGL_EXPORT mgl_data_mul_num_(uintptr_t *dat, mreal *d);
/// Divide each element by the number
void MGL_EXPORT mgl_data_div_num(HMDT dat, mreal d);
void MGL_EXPORT mgl_data_div_num_(uintptr_t *dat, mreal *d);
/// Add the number
void MGL_EXPORT mgl_data_add_num(HMDT dat, mreal d);
void MGL_EXPORT mgl_data_add_num_(uintptr_t *dat, mreal *d);
/// Subtract the number
void MGL_EXPORT mgl_data_sub_num(HMDT dat, mreal d);
void MGL_EXPORT mgl_data_sub_num_(uintptr_t *dat, mreal *d);

/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for amplitude and phase
HMDT MGL_EXPORT mgl_transform_a(HCDT am, HCDT ph, const char *tr);
uintptr_t MGL_EXPORT mgl_transform_a_(uintptr_t *am, uintptr_t *ph, const char *tr, int);
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for real and imaginary parts
HMDT MGL_EXPORT mgl_transform(HCDT re, HCDT im, const char *tr);
uintptr_t MGL_EXPORT mgl_transform_(uintptr_t *re, uintptr_t *im, const char *tr, int);
/// Apply Fourier transform for the data and save result into it
void MGL_EXPORT mgl_data_fourier(HMDT re, HMDT im, const char *dir);
void MGL_EXPORT mgl_data_fourier_(uintptr_t *re, uintptr_t *im, const char *dir, int l);
/// Short time Fourier analysis for real and imaginary parts. Output is amplitude of partial Fourier (result will have size {dn, floor(nx/dn), ny} for dir='x'
HMDT MGL_EXPORT mgl_data_stfa(HCDT re, HCDT im, long dn, char dir);
uintptr_t MGL_EXPORT mgl_data_stfa_(uintptr_t *re, uintptr_t *im, int *dn, char *dir, int);

/// Do something like Delone triangulation for 3d points
HMDT MGL_EXPORT mgl_triangulation_3d(HCDT x, HCDT y, HCDT z);
uintptr_t MGL_EXPORT mgl_triangulation_3d_(uintptr_t *x, uintptr_t *y, uintptr_t *z);
/// Do Delone triangulation for 2d points
HMDT MGL_EXPORT mgl_triangulation_2d(HCDT x, HCDT y);
uintptr_t MGL_EXPORT mgl_triangulation_2d_(uintptr_t *x, uintptr_t *y);

/// Find root for nonlinear equation
mreal MGL_EXPORT mgl_find_root(mreal (*func)(mreal val, void *par), mreal ini, void *par);
/// Find root for nonlinear equation defined by textual formula
mreal MGL_EXPORT mgl_find_root_txt(const char *func, mreal ini, char var_id);
mreal MGL_EXPORT mgl_find_root_txt_(const char *func, mreal *ini, const char *var_id,int,int);
/// Find roots for nonlinear equation defined by textual formula
HMDT MGL_EXPORT mgl_data_roots(const char *func, HCDT ini, char var_id);
uintptr_t MGL_EXPORT mgl_data_roots_(const char *func, uintptr_t *ini, const char *var_id,int,int);
/// Find roots for set of nonlinear equations defined by textual formulas
HMDT MGL_EXPORT mgl_find_roots_txt(const char *func, const char *vars, HCDT ini);
uintptr_t MGL_EXPORT mgl_find_roots_txt_(const char *func, const char *vars, uintptr_t *ini,int,int);
/// Find roots for set of nonlinear equations defined by function
bool MGL_EXPORT mgl_find_roots(size_t n, void (*func)(const mreal *x, mreal *f, void *par), mreal *x0, void *par);
//-----------------------------------------------------------------------------
#ifdef __cplusplus
}
#endif
#endif
//-----------------------------------------------------------------------------
