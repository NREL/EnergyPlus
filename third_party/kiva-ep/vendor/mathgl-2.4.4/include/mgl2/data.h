/***************************************************************************
 * data.h is part of Math Graphic Library
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
#ifndef _MGL_DATA_H_
#define _MGL_DATA_H_

#include "mgl2/data_cf.h"
#include "mgl2/pde.h"
//-----------------------------------------------------------------------------
#include <stdarg.h>
//-----------------------------------------------------------------------------
mreal MGL_EXPORT_PURE mglLinear(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z);
mreal MGL_EXPORT mglSpline3(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z,mreal *dx=0, mreal *dy=0, mreal *dz=0);
mreal MGL_EXPORT_PURE mglSpline3s(const mreal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z);
//-----------------------------------------------------------------------------
/// Class for working with data array
class MGL_EXPORT mglData : public mglDataA
{
public:
using mglDataA::Momentum;
	long nx;		///< number of points in 1st dimensions ('x' dimension)
	long ny;		///< number of points in 2nd dimensions ('y' dimension)
	long nz;		///< number of points in 3d dimensions ('z' dimension)
	mreal *a;		///< data array
	bool link;		///< use external data (i.e. don't free it)

	/// Initiate by other mglData variable
	mglData(const mglData &d)	{	a=0;	mgl_data_set(this,&d);		}	// NOTE: must be constructor for mglData& to exclude copy one
#if MGL_HAVE_RVAL
	mglData(mglData &&d):nx(d.nx),ny(d.ny),nz(d.nz),a(d.a),link(d.link)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	id=d.id;	d.a=0;	d.func=0;	}
#endif
	mglData(const mglDataA *d)
	{	a=0;	if(d)	mgl_data_set(this, d);	else	mgl_data_create(this,1,1,1);		}
	mglData(bool, mglData *d)	// NOTE: Variable d will be deleted!!!
	{	if(d)
		{	nx=d->nx;	ny=d->ny;	nz=d->nz;	a=d->a;	d->a=0;
			temp=d->temp;	func=d->func;	o=d->o;	s=d->s;
			id=d->id;	link=d->link;	delete d;	}
		else	{	a=0;	Create(1);	}	}
	/// Initiate by flat array
	mglData(int size, const float *d)	{	a=0;	Set(d,size);	}
	mglData(int rows, int cols, const float *d)	{	a=0;	Set(d,cols,rows);	}
	mglData(int size, const double *d)	{	a=0;	Set(d,size);	}
	mglData(int rows, int cols, const double *d)	{	a=0;	Set(d,cols,rows);	}
	mglData(const double *d, int size)	{	a=0;	Set(d,size);	}
	mglData(const double *d, int rows, int cols)	{	a=0;	Set(d,cols,rows);	}
	mglData(const float *d, int size)	{	a=0;	Set(d,size);	}
	mglData(const float *d, int rows, int cols)	{	a=0;	Set(d,cols,rows);	}
	/// Allocate memory and copy data from std::vector<T>
	mglData(const std::vector<int> &d)		{	a=0;	Set(d);	}
	mglData(const std::vector<float> &d)	{	a=0;	Set(d);	}
	mglData(const std::vector<double> &d)	{	a=0;	Set(d);	}
	/// Read data from file
	mglData(const char *fname)			{	a=0;	Read(fname);	}
	/// Allocate the memory for data array and initialize it zero
	mglData(long xx=1,long yy=1,long zz=1)	{	a=0;	Create(xx,yy,zz);	}
	/// Delete the array
	virtual ~mglData()	{	if(!link && a)	delete []a;	}

	/// Move all data from variable d, and delete this variable.
	inline void Move(mglData *d)	// NOTE: Variable d will be deleted!!!
	{	if(d && d->GetNN()>1)
		{	bool l=link;	mreal *b=a;
			nx=d->nx;	ny=d->ny;	nz=d->nz;	a=d->a;	d->a=b;
			temp=d->temp;	func=d->func;	o=d->o;	s=d->s;
			id=d->id;	link=d->link;	d->link=l;	delete d;	}
		else if(d)	{	*this = d->a[0];	delete d;	}
	}

	inline mreal GetVal(long i, long j=0, long k=0) const
	{	return mgl_data_get_value(this,i,j,k);}
	inline void SetVal(mreal f, long i, long j=0, long k=0)
	{	mgl_data_set_value(this,f,i,j,k);	}
	/// Get sizes
	long GetNx() const	{	return nx;	}
	long GetNy() const	{	return ny;	}
	long GetNz() const	{	return nz;	}

	/// Link external data array (don't delete it at exit)
	inline void Link(mreal *A, long NX, long NY=1, long NZ=1)
	{	mgl_data_link(this,A,NX,NY,NZ);	}
	inline void Link(mglData &d)	{	Link(d.a,d.nx,d.ny,d.nz);	}
	/// Allocate memory and copy the data from the gsl_vector
	inline void Set(gsl_vector *m)	{	mgl_data_set_vector(this,m);	}
	/// Allocate memory and copy the data from the gsl_matrix
	inline void Set(gsl_matrix *m)	{	mgl_data_set_matrix(this,m);	}

	/// Allocate memory and copy the data from the (float *) array
	inline void Set(const float *A,long NX,long NY=1,long NZ=1)
	{	mgl_data_set_float(this,A,NX,NY,NZ);	}
	/// Allocate memory and copy the data from the (double *) array
	inline void Set(const double *A,long NX,long NY=1,long NZ=1)
	{	mgl_data_set_double(this,A,NX,NY,NZ);	}
	/// Allocate memory and copy the data from the (float **) array
	inline void Set(float const * const *A,long N1,long N2)
	{	mgl_data_set_float2(this,A,N1,N2);	}
	/// Allocate memory and copy the data from the (double **) array
	inline void Set(double const * const *A,long N1,long N2)
	{	mgl_data_set_double2(this,A,N1,N2);	}
	/// Allocate memory and copy the data from the (float ***) array
	inline void Set(float const * const * const *A,long N1,long N2,long N3)
	{	mgl_data_set_float3(this,A,N1,N2,N3);	}
	/// Allocate memory and copy the data from the (double ***) array
	inline void Set(double const * const * const *A,long N1,long N2,long N3)
	{	mgl_data_set_double3(this,A,N1,N2,N3);	}
	/// Allocate memory and scanf the data from the string
	inline void Set(const char *str,long NX,long NY=1,long NZ=1)
	{	mgl_data_set_values(this,str,NX,NY,NZ);	}
	/// Import data from abstract type
	inline void Set(HCDT dat)	{	mgl_data_set(this, dat);	}
	inline void Set(const mglDataA &dat)	{	mgl_data_set(this, &dat);	}
	/// Allocate memory and copy data from std::vector<T>
	inline void Set(const std::vector<int> &d)
	{	if(d.size()>0)	{	Create(d.size());	for(long i=0;i<nx;i++)	a[i] = d[i];	}
		else	Create(1);	}
	inline void Set(const std::vector<float> &d)
	{	if(d.size()>0)	Set(&(d[0]),d.size());	else	Create(1);	}
	inline void Set(const std::vector<double> &d)
	{	if(d.size()>0)	Set(&(d[0]),d.size());	else	Create(1);	}
	/// Allocate memory and set data from variable argument list of double values
	inline void SetList(long n, ...)
	{
		if(n<1)	return;
		mgl_data_create(this,n,1,1);
		va_list vl;	va_start(vl,n);
		for(long i=0;i<n;i++)	a[i] = va_arg(vl,double);
		va_end(vl);
	}

	/// Create or recreate the array with specified size and fill it by zero
	inline void Create(long mx,long my=1,long mz=1)
	{	mgl_data_create(this,mx,my,mz);	}
	/// Rearange data dimensions
	inline void Rearrange(long mx, long my=0, long mz=0)
	{	mgl_data_rearrange(this,mx,my,mz);	}
	/// Transpose dimensions of the data (generalization of Transpose)
	inline void Transpose(const char *dim="yx")
	{	mgl_data_transpose(this,dim);	}
	/// Extend data dimensions
	inline void Extend(long n1, long n2=0)
	{	mgl_data_extend(this,n1,n2);	}
	/// Reduce size of the data
	inline void Squeeze(long rx,long ry=1,long rz=1,bool smooth=false)
	{	mgl_data_squeeze(this,rx,ry,rz,smooth);	}
	/// Crop the data
	inline void Crop(long n1, long n2,char dir='x')
	{	mgl_data_crop(this,n1,n2,dir);	}
	/// Crop the data to be most optimal for FFT (i.e. to closest value of 2^n*3^m*5^l)
	inline void Crop(const char *how="235x")
	{	mgl_data_crop_opt(this, how);	}
	/// Insert data rows/columns/slices
	inline void Insert(char dir, long at=0, long num=1)
	{	mgl_data_insert(this,dir,at,num);	}
	/// Delete data rows/columns/slices
	inline void Delete(char dir, long at=0, long num=1)
	{	mgl_data_delete(this,dir,at,num);	}
	/// Remove rows with duplicate values in column clmn
	inline void Clean(long clmn)
	{	mgl_data_clean(this,clmn);	}
	/// Join with another data array
	inline void Join(const mglDataA &d)
	{	mgl_data_join(this,&d);	}

	/// Modify the data by specified formula
	inline void Modify(const char *eq,long dim=0)
	{	mgl_data_modify(this, eq, dim);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglDataA &vdat, const mglDataA &wdat)
	{	mgl_data_modify_vw(this,eq,&vdat,&wdat);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglDataA &vdat)
	{	mgl_data_modify_vw(this,eq,&vdat,0);	}
	/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
	inline void Fill(HMGL gr, const char *eq, const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,0,0,opt);	}
	inline void Fill(HMGL gr, const char *eq, const mglDataA &vdat, const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,&vdat,0,opt);	}
	inline void Fill(HMGL gr, const char *eq, const mglDataA &vdat, const mglDataA &wdat,const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,&vdat,&wdat,opt);	}
	/// Equidistantly fill the data to range [x1,x2] in direction dir
	inline void Fill(mreal x1,mreal x2=mglNaN,char dir='x')
	{	mgl_data_fill(this,x1,x2,dir);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2] using global spline
	inline void RefillGS(const mglDataA &xdat, const mglDataA &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_data_refill_gs(this,&xdat,&vdat,x1,x2,sl);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2]
	inline void Refill(const mglDataA &xdat, const mglDataA &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_data_refill_x(this,&xdat,&vdat,x1,x2,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_data_refill_x(this,&xdat,&vdat,p1.x,p2.x,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &ydat, const mglDataA &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_data_refill_xy(this,&xdat,&ydat,&vdat,p1.x,p2.x,p1.y,p2.y,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &ydat, const mglDataA &zdat, const mglDataA &vdat, mglPoint p1, mglPoint p2)
	{	mgl_data_refill_xyz(this,&xdat,&ydat,&zdat,&vdat,p1.x,p2.x,p1.y,p2.y,p1.z,p2.z);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range of gr
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,0,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &ydat, const mglDataA &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,&ydat,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &ydat, const mglDataA &zdat, const mglDataA &vdat, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,&ydat,&zdat,&vdat,-1,opt);	}
	/// Set the data by triangulated surface values assuming x,y,z in axis range of gr
	inline void Grid(HMGL gr, const mglDataA &x, const mglDataA &y, const mglDataA &z, const char *opt="")
	{	mgl_data_grid(gr,this,&x,&y,&z,opt);	}
	/// Set the data by triangulated surface values assuming x,y,z in range [p1, p2]
	inline void Grid(const mglDataA &xdat, const mglDataA &ydat, const mglDataA &vdat, mglPoint p1, mglPoint p2)
	{	mgl_data_grid_xy(this,&xdat,&ydat,&vdat,p1.x,p2.x,p1.y,p2.y);	}
	/// Put value to data element(s)
	inline void Put(mreal val, long i=-1, long j=-1, long k=-1)
	{	mgl_data_put_val(this,val,i,j,k);	}
	/// Put array to data element(s)
	inline void Put(const mglDataA &dat, long i=-1, long j=-1, long k=-1)
	{	mgl_data_put_dat(this,&dat,i,j,k);	}

	/// Read data from tab-separated text file with auto determining size
	inline bool Read(const char *fname)
	{	return mgl_data_read(this,fname); }
	/// Read data from text file with specifeid size
	inline bool Read(const char *fname,long mx,long my=1,long mz=1)
	{	return mgl_data_read_dim(this,fname,mx,my,mz);	}
	/// Import data array from PNG file according color scheme
	inline void Import(const char *fname,const char *scheme,mreal v1=0,mreal v2=1)
	{	mgl_data_import(this,fname,scheme,v1,v2);	}
	/// Read data from tab-separated text files with auto determining size which filenames are result of sprintf(fname,templ,t) where t=from:step:to
	inline bool ReadRange(const char *templ, double from, double to, double step=1, bool as_slice=false)
	{	return mgl_data_read_range(this,templ,from,to,step,as_slice);	}
	/// Read data from tab-separated text files with auto determining size which filenames are satisfied to template (like "t_*.dat")
	inline bool ReadAll(const char *templ, bool as_slice=false)
	{	return mgl_data_read_all(this, templ, as_slice);	}
	/// Read data from text file with size specified at beginning of the file
	inline bool ReadMat(const char *fname, long dim=2)
	{	return mgl_data_read_mat(this,fname,dim);	}
	/// Read data array from HDF file (parse HDF4 and HDF5 files)
	inline int ReadHDF(const char *fname,const char *data)
	{	return mgl_data_read_hdf(this,fname,data);	}
	/// Scan textual file for template and fill data array
	inline int ScanFile(const char *fname, const char *templ)
	{	return mgl_data_scan_file(this,fname, templ);	}


	/// Get column (or slice) of the data filled by formulas of named columns
	inline mglData Column(const char *eq) const
	{	return mglData(true,mgl_data_column(this,eq));	}
	/// Get momentum (1D-array) of data along direction 'dir'. String looks like "x1" for median in x-direction, "x2" for width in x-dir and so on.
	inline mglData Momentum(char dir, const char *how) const
	{	return mglData(true,mgl_data_momentum(this,dir,how));	}
	/// Get pulse properties: pulse maximum and its position, pulse duration near maximum and by half height, energy in first pulse.
	inline mglData Pulse(char dir) const
	{	return mglData(true,mgl_data_pulse(this,dir));	}
	/// Get sub-array of the data with given fixed indexes
	inline mglData SubData(long xx,long yy=-1,long zz=-1) const
	{	return mglData(true,mgl_data_subdata(this,xx,yy,zz));	}
	inline mglData SubData(const mglDataA &xx, const mglDataA &yy, const mglDataA &zz) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,&zz));	}
	inline mglData SubData(const mglDataA &xx, const mglDataA &yy) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,0));	}
	inline mglData SubData(const mglDataA &xx) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,0,0));	}
	/// Get data from sections ids, separated by value val along specified direction.
	/** If section id is negative then reverse order is used (i.e. -1 give last section). */
	inline mglData Section(const mglDataA &ids, char dir='y', mreal val=NAN) const
	{	return mglData(true,mgl_data_section(this,&ids,dir,val));	}
	inline mglData Section(long id, char dir='y', mreal val=NAN) const
	{	return mglData(true,mgl_data_section_val(this,id,dir,val));	}
	/// Get contour lines for dat[i,j]=val. NAN values separate the the curves.
	inline mglData Conts(mreal val)
	{	return mglData(true,mgl_data_conts(val,this));	}

	/// Get trace of the data array
	inline mglData Trace() const
	{	return mglData(true,mgl_data_trace(this));	}
	/// Create n-th points distribution of this data values in range [v1, v2]
	inline mglData Hist(long n,mreal v1=0,mreal v2=1, long nsub=0) const
	{	return mglData(true,mgl_data_hist(this,n,v1,v2,nsub));	}
	/// Create n-th points distribution of this data values in range [v1, v2] with weight w
	inline mglData Hist(const mglDataA &w, long n,mreal v1=0,mreal v2=1, long nsub=0) const
	{	return mglData(true,mgl_data_hist_w(this,&w,n,v1,v2,nsub));	}
	/// Get array which is result of summation in given direction or directions
	inline mglData Sum(const char *dir) const
	{	return mglData(true,mgl_data_sum(this,dir));	}
	/// Get array which is result of maximal values in given direction or directions
	inline mglData Max(const char *dir) const
	{	return mglData(true,mgl_data_max_dir(this,dir));	}
	/// Get array which is result of minimal values in given direction or directions
	inline mglData Min(const char *dir) const
	{	return mglData(true,mgl_data_min_dir(this,dir));	}
	/// Get positions of local maximums and minimums
	inline mglData MinMax() const
	{	return mglData(true,mgl_data_minmax(this));	}
	/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
	inline mglData Combine(const mglDataA &dat) const
	{	return mglData(true,mgl_data_combine(this,&dat));	}
	/// Resize the data to new size of box [x1,x2]*[y1,y2]*[z1,z2]
	inline mglData Resize(long mx,long my=0,long mz=0, mreal x1=0,mreal x2=1, mreal y1=0,mreal y2=1, mreal z1=0,mreal z2=1) const
	{	return mglData(true,mgl_data_resize_box(this,mx,my,mz,x1,x2,y1,y2,z1,z2));	}
	/// Get array which values is result of interpolation this for coordinates from other arrays
	inline mglData Evaluate(const mglData &idat, bool norm=true) const
	{	return mglData(true,mgl_data_evaluate(this,&idat,0,0,norm));	}
	inline mglData Evaluate(const mglData &idat, const mglData &jdat, bool norm=true) const
	{	return mglData(true,mgl_data_evaluate(this,&idat,&jdat,0,norm));	}
	inline mglData Evaluate(const mglData &idat, const mglData &jdat, const mglData &kdat, bool norm=true) const
	{	return mglData(true,mgl_data_evaluate(this,&idat,&jdat,&kdat,norm));	}
	/// Find roots for nonlinear equation defined by textual formula
	inline mglData Roots(const char *eq, char var='x') const
	{	return mglData(true,mgl_data_roots(eq, this, var));	}
	/// Find roots for set of nonlinear equations defined by textual formula
	inline mglData MultiRoots(const char *eq, const char *vars) const
	{	return mglData(true,mgl_find_roots_txt(eq, vars, this));	}
	/// Find correlation with another data arrays
	inline mglData Correl(const mglDataA &dat, const char *dir) const
	{	return mglData(true,mgl_data_correl(this,&dat,dir));	}
	/// Find auto correlation function
	inline mglData AutoCorrel(const char *dir) const
	{	return mglData(true,mgl_data_correl(this,this,dir));	}
	/// Get curves, separated by NAN, for maximal values of array d as function of x coordinate.
	/** Noises below lvl amplitude are ignored.
	 * Parameter dy \in [0,ny] set the "attraction" distance of points to curve. */
	inline mglData Detect(mreal lvl, mreal dj, mreal di=0, mreal min_len=0) const
	{	return mglData(true,mgl_data_detect(this,lvl,dj,di,min_len));	}

	/// Cumulative summation the data in given direction or directions
	inline void CumSum(const char *dir)	{	mgl_data_cumsum(this,dir);	}
	/// Integrate (cumulative summation) the data in given direction or directions
	inline void Integral(const char *dir)	{	mgl_data_integral(this,dir);	}
	/// Differentiate the data in given direction or directions
	inline void Diff(const char *dir)	{	mgl_data_diff(this,dir);	}
	/// Differentiate the parametrically specified data along direction v1
	inline void Diff(const mglDataA &v1)
	{	mgl_data_diff_par(this,&v1,0,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2=const
	inline void Diff(const mglDataA &v1, const mglDataA &v2)
	{	mgl_data_diff_par(this,&v1,&v2,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2,v3=const
	inline void Diff(const mglDataA &v1, const mglDataA &v2, const mglDataA &v3)
	{	mgl_data_diff_par(this,&v1,&v2,&v3);	}
	/// Double-differentiate (like Laplace operator) the data in given direction
	inline void Diff2(const char *dir)	{	mgl_data_diff2(this,dir);	}

	/// Swap left and right part of the data in given direction (useful for Fourier spectrum)
	inline void Swap(const char *dir)		{	mgl_data_swap(this,dir);	}
	/// Roll data along direction dir by num slices
	inline void Roll(char dir, long num)	{	mgl_data_roll(this,dir,num);	}
	/// Mirror the data in given direction (useful for Fourier spectrum)
	inline void Mirror(const char *dir)		{	mgl_data_mirror(this,dir);	}
	/// Sort rows (or slices) by values of specified column
	inline void Sort(long idx, long idy=-1)	{	mgl_data_sort(this,idx,idy);	}
	/// Return dilated array of 0 or 1 for data values larger val
	inline void Dilate(mreal val=1, long step=1)
	{	mgl_data_dilate(this, val, step);	}
	/// Return eroded array of 0 or 1 for data values larger val
	inline void Erode(mreal val=1, long step=1)
	{	mgl_data_erode(this, val, step);	}

	/// Set as the data envelop
	inline void Envelop(char dir='x')
	{	mgl_data_envelop(this,dir);	}
	/// Remove phase jump
	inline void Sew(const char *dirs="xyz", mreal da=2*mglPi)
	{	mgl_data_sew(this,dirs,da);	}
	/// Smooth the data on specified direction or directions
	/** String \a dir may contain:
	 *  ‘x’, ‘y’, ‘z’ for 1st, 2nd or 3d dimension;
	 *  ‘dN’ for linear averaging over N points;
	 *  ‘3’ for linear averaging over 3 points;
	 *  ‘5’ for linear averaging over 5 points;
	 *  ‘^’ for finding upper bound;
	 *  ‘_’ for finding lower bound.
	 *  By default quadratic averaging over 5 points is used. */
	inline void Smooth(const char *dirs="xyz",mreal delta=0)
	{	mgl_data_smooth(this,dirs,delta);	}
	/// Normalize the data to range [v1,v2]
	inline void Norm(mreal v1=0,mreal v2=1,bool sym=false,long dim=0)
	{	mgl_data_norm(this,v1,v2,sym,dim);	}
	/// Normalize the data to range [v1,v2] slice by slice
	inline void NormSl(mreal v1=0,mreal v2=1,char dir='z',bool keep_en=true,bool sym=false)
	{	mgl_data_norm_slice(this,v1,v2,dir,keep_en,sym);	}
	/// Limit the data to be inside [-v,v], keeping the original sign
	inline void Limit(mreal v)
	{	mgl_data_limit(this, v);	}
	/// Project the periodical data to range [v1,v2] (like mod() function). Separate branches by NAN if sep=true.
	inline void Coil(mreal v1, mreal v2, bool sep=true)
	{	mgl_data_coil(this, v1, v2, sep);	}

	/// Apply Hankel transform
	inline void Hankel(const char *dir)	{	mgl_data_hankel(this,dir);	}
	/// Apply Sin-Fourier transform
	inline void SinFFT(const char *dir)	{	mgl_data_sinfft(this,dir);	}
	/// Apply Cos-Fourier transform
	inline void CosFFT(const char *dir)	{	mgl_data_cosfft(this,dir);	}
	/// Fill data by coordinates/momenta samples for Hankel ('h') or Fourier ('f') transform
	/** Parameter \a how may contain:
	 * ‘x‘,‘y‘,‘z‘ for direction (only one will be used),
	 * ‘k‘ for momenta samples,
	 * ‘h‘ for Hankel samples,
	 * ‘f‘ for Cartesian/Fourier samples (default). */
	inline void FillSample(const char *how)
	{	mgl_data_fill_sample(this,how);	}
	/// Apply wavelet transform
	/** Parameter \a dir may contain:
	 * ‘x‘,‘y‘,‘z‘ for directions,
	 * ‘d‘ for daubechies, ‘D‘ for centered daubechies,
	 * ‘h‘ for haar, ‘H‘ for centered haar,
	 * ‘b‘ for bspline, ‘B‘ for centered bspline,
	 * ‘i‘ for applying inverse transform. */
	inline void Wavelet(const char *how, int k)	{	mgl_data_wavelet(this, how, k);	}

	/// Return an approximated x-value (root) when dat(x) = val
	inline mreal Solve(mreal val, bool use_spline=true, long i0=0) const
	{	return mgl_data_solve_1d(this, val, use_spline, i0);		}
	/// Return an approximated value (root) when dat(x) = val
	inline mglData Solve(mreal val, char dir, bool norm=true) const
	{	return mglData(true,mgl_data_solve(this, val, dir, 0, norm));	}
	inline mglData Solve(mreal val, char dir, const mglData &i0, bool norm=true) const
	{	return mglData(true,mgl_data_solve(this, val, dir, &i0, norm));	}

	/// Copy data from other mglData variable
	inline const mglDataA &operator=(const mglDataA &d)
	{	if(this!=&d)	mgl_data_set(this,&d);	return d;	}
	inline const mglData &operator=(const mglData &d)
	{	if(this!=&d)	mgl_data_set(this,&d);	return d;	}
	inline mreal operator=(mreal val)
	{	mgl_data_fill(this,val,val,'x');	return val;	}
	/// Multiply the data by other one for each element
	inline void operator*=(const mglDataA &d)	{	mgl_data_mul_dat(this,&d);	}
	/// Divide the data by other one for each element
	inline void operator/=(const mglDataA &d)	{	mgl_data_div_dat(this,&d);	}
	/// Add the other data
	inline void operator+=(const mglDataA &d)	{	mgl_data_add_dat(this,&d);	}
	/// Subtract the other data
	inline void operator-=(const mglDataA &d)	{	mgl_data_sub_dat(this,&d);	}
	/// Multiply each element by the number
	inline void operator*=(mreal d)		{	mgl_data_mul_num(this,d);	}
	/// Divide each element by the number
	inline void operator/=(mreal d)		{	mgl_data_div_num(this,d);	}
	/// Add the number
	inline void operator+=(mreal d)		{	mgl_data_add_num(this,d);	}
	/// Subtract the number
	inline void operator-=(mreal d)		{	mgl_data_sub_num(this,d);	}
#ifndef SWIG
	/// Direct access to the data cell
	inline mreal operator[](long i) const	{	return a[i];	}
	inline mreal &operator[](long i)		{	return a[i];	}
	/// Get sub-array of the data with given fixed indexes
	inline mglData operator()(long xx,long yy=-1,long zz=-1) const
	{	return mglData(true,mgl_data_subdata(this,xx,yy,zz));	}
	inline mglData operator()(const mglDataA &xx, const mglDataA &yy, const mglDataA &zz) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,&zz));	}
	inline mglData operator()(const mglDataA &xx, const mglDataA &yy) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,0));	}
	inline mglData operator()(const mglDataA &xx) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,0,0));	}
#endif

#ifndef DEBUG
	/// Get the value in given cell of the data
	mreal v(long i,long j=0,long k=0) const	{	return a[i+nx*(j+ny*k)];	}
	/// Set the value in given cell of the data
	void set_v(mreal val, long i,long j=0,long k=0)	{	a[i+nx*(j+ny*k)]=val;	}
#else
	/// Get the value in given cell of the data with border checking
	mreal v(long i,long j=0,long k=0) const	{	return mgl_data_get_value(this,i,j,k);	}
	/// Set the value in given cell of the data
	void set_v(mreal val, long i,long j=0,long k=0)	{	mgl_data_set_value(this,val,i,j,k);	}
#endif
	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal y=0,mreal z=0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	return mglSpline3(a,nx,ny,nz,x,y,z,dx,dy,dz);	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal y=0,mreal z=0) const
	{	return mglSpline3s(a,nx,ny,nz,x,y,z);	}
	mreal vthr(long i) const {	return a[i];	}
	// add for speeding up !!!
	mreal dvx(long i,long j=0,long k=0) const
	{   long i0 = size_t(i)<size_t(nx-1) ? i+nx*(j+ny*k):nx*(1+j+ny*k)-2;	return a[i0+1]-a[i0];	}
//	{   long i0=i+nx*(j+ny*k);
//		return i>0? (i<nx-1? (a[i0+1]-a[i0-1])/2:a[i0]-a[i0-1]) : a[i0+1]-a[i0];	}
	mreal dvy(long i,long j=0,long k=0) const
	{   long i0 = size_t(j)<size_t(ny-1) ? i+nx*(j+ny*k):i+nx*(ny*(k+1)-2);	return a[i0+nx]-a[i0];	}
//	{   long i0=i+nx*(j+ny*k);
//		return j>0? (j<ny-1? (a[i0+nx]-a[i0-nx])/2:a[i0]-a[i0-nx]) : a[i0+nx]-a[i0];}
	mreal dvz(long i,long j=0,long k=0) const
	{   long n=nx*ny, i0 = size_t(k)<size_t(nz-1) ? i+nx*(j+ny*k):i+nx*(j+ny*(nz-2));	return a[i0+n]-a[i0];	}
//	{   long i0=i+nx*(j+ny*k), n=nx*ny;
//		return k>0? (k<nz-1? (a[i0+n]-a[i0-n])/2:a[i0]-a[i0-n]) : a[i0+n]-a[i0];	}
};
//-----------------------------------------------------------------------------
#ifndef SWIG
inline mglData operator*(const mglDataA &b, const mglDataA &d)
{	mglData a(&b);	a*=d;	return a;	}
inline mglData operator*(mreal b, const mglDataA &d)
{	mglData a(&d);	a*=b;	return a;	}
inline mglData operator*(const mglDataA &d, mreal b)
{	mglData a(&d);	a*=b;	return a;	}
inline mglData operator-(const mglDataA &b, const mglDataA &d)
{	mglData a(&b);	a-=d;	return a;	}
inline mglData operator-(mreal b, const mglDataA &d)
{	mglData a(&d);	a-=b;	return a;	}
inline mglData operator-(const mglDataA &d, mreal b)
{	mglData a(&d);	a-=b;	return a;	}
inline mglData operator+(const mglDataA &b, const mglDataA &d)
{	mglData a(&b);	a+=d;	return a;	}
inline mglData operator+(mreal b, const mglDataA &d)
{	mglData a(&d);	a+=b;	return a;	}
inline mglData operator+(const mglDataA &d, mreal b)
{	mglData a(&d);	a+=b;	return a;	}
inline mglData operator/(const mglDataA &b, const mglDataA &d)
{	mglData a(&b);	a/=d;	return a;	}
inline mglData operator/(const mglDataA &d, mreal b)
{	mglData a(&d);	a/=b;	return a;	}
inline bool operator==(const mglData &b, const mglData &d)
{	if(b.nx!=d.nx || b.ny!=d.ny || b.nz!=d.nz)	return false;
	return !memcmp(b.a,d.a,b.nx*b.ny*b.nz*sizeof(mreal));	}
inline bool operator<(const mglDataA &b, const mglDataA &d)
{	return b.Maximal()<d.Maximal();	}
inline bool operator>(const mglDataA &b, const mglDataA &d)
{	return b.Minimal()>d.Minimal();	}
//-----------------------------------------------------------------------------
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for amplitude and phase
inline mglData mglTransformA(const mglDataA &am, const mglDataA &ph, const char *tr)
{	return mglData(true,mgl_transform_a(&am,&ph,tr));	}
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for real and imaginary parts
inline mglData mglTransform(const mglDataA &re, const mglDataA &im, const char *tr)
{	return mglData(true,mgl_transform(&re,&im,tr));	}
/// Apply Fourier transform for the data and save result into it
inline void mglFourier(mglData &re, mglData &im, const char *dir)
{	mgl_data_fourier(&re,&im,dir);	}
/// Short time Fourier analysis for real and imaginary parts. Output is amplitude of partial Fourier (result will have size {dn, floor(nx/dn), ny} for dir='x'
inline mglData mglSTFA(const mglDataA &re, const mglDataA &im, long dn, char dir='x')
{	return mglData(true, mgl_data_stfa(&re,&im,dn,dir));	}
//-----------------------------------------------------------------------------
/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini
inline mglData mglPDE(HMGL gr, const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, mreal dz=0.1, mreal k0=100,const char *opt="")
{	return mglData(true, mgl_pde_solve(gr,ham, &ini_re, &ini_im, dz, k0,opt));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglData mglQO2d(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo2d_solve(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0));	}
inline mglData mglQO2d(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mglData &xx, mglData &yy, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo2d_solve(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglData mglQO3d(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo3d_solve(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0, 0));	}
inline mglData mglQO3d(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mglData &xx, mglData &yy, mglData &zz, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo3d_solve(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy, &zz));	}
/// Finds ray with starting point r0, p0 (and prepares ray data for mglQO2d)
inline mglData mglRay(const char *ham, mglPoint r0, mglPoint p0, mreal dt=0.1, mreal tmax=10)
{	return mglData(true, mgl_ray_trace(ham, r0.x, r0.y, r0.z, p0.x, p0.y, p0.z, dt, tmax));	}
/// Saves result of ODE solving for var complex variables with right part func (separated by ';') and initial conditions x0 over time interval [0,tmax] with time step dt
inline mglData mglODE(const char *func, const char *var, const mglDataA &ini, mreal dt=0.1, mreal tmax=10)
{	return mglData(true, mgl_ode_solve_str(func,var, &ini, dt, tmax));	}
//-----------------------------------------------------------------------------
/// Get array as solution of tridiagonal system of equations a[i]*x[i-1]+b[i]*x[i]+c[i]*x[i+1]=d[i]
/** String \a how may contain:
 * 'x', 'y', 'z' for solving along x-,y-,z-directions, or
 * 'h' for solving along hexagonal direction at x-y plain (need nx=ny),
 * 'c' for using periodical boundary conditions,
 * 'd' for diffraction/diffuse calculation. */
inline mglData mglTridMat(const mglDataA &A, const mglDataA &B, const mglDataA &C, const mglDataA &D, const char *how)
{	return mglData(true, mgl_data_tridmat(&A, &B, &C, &D, how));	}
//-----------------------------------------------------------------------------
/// Calculate Jacobian determinant for D{x(u,v), y(u,v)} = dx/du*dy/dv-dx/dv*dy/du
inline mglData mglJacobian(const mglDataA &x, const mglDataA &y)
{	return mglData(true, mgl_jacobian_2d(&x, &y));	}
/// Calculate Jacobian determinant for D{x(u,v,w), y(u,v,w), z(u,v,w)}
inline mglData mglJacobian(const mglDataA &x, const mglDataA &y, const mglDataA &z)
{	return mglData(true, mgl_jacobian_3d(&x, &y, &z));	}
/// Do something like Delone triangulation
inline mglData mglTriangulation(const mglDataA &x, const mglDataA &y, const mglDataA &z)
{	return mglData(true,mgl_triangulation_3d(&x,&y,&z));	}
inline mglData mglTriangulation(const mglDataA &x, const mglDataA &y)
{	return mglData(true,mgl_triangulation_2d(&x,&y));	}
/// Get curves, separated by NAN, for maximal values of array d as function of x coordinate.
/** Noises below lvl amplitude are ignored.
 * Parameter dy \in [0,ny] set the "attraction" distance of points to curve. */
inline mglData mglDetect(const mglDataA &d, mreal lvl, mreal dj, mreal di=0, mreal min_len=0)
{	return mglData(true,mgl_data_detect(&d, lvl, dj, di, min_len));	}
//-----------------------------------------------------------------------------
/// Get array which is n-th pairs {x[i],y[i]} for iterated function system (fractal) generated by A
inline mglData mglIFS2d(const mglDataA &A, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_2d(&A,n,skip));	}
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) generated by A
inline mglData mglIFS3d(const mglDataA &A, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_3d(&A,n,skip));	}
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) defined in *.ifs file 'fname' and named as 'name'
inline mglData mglIFSfile(const char *fname, const char *name, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_file(fname,name,n,skip));	}
/// Get array which is n-th pairs {x[i],y[i]} for Flame fractal generated by A with functions F
/** NOTE: A.nx must be >= 7 and F.nx >= 2 and F.nz=A.ny.
 * F[0,i,j] denote function id. F[1,i,j] give function weight, F(2:5,i,j) provide function parameters.
 * Resulting point is {xnew,ynew} = sum_i F[1,i,j]*F[0,i,j]{IFS2d(A[j]){x,y}}. */
inline mglData mglFlame2d(const mglDataA &A, const mglDataA &F, long n, long skip=20)
{	return mglData(true,mgl_data_flame_2d(&A,&F,n,skip));	}
//-----------------------------------------------------------------------------
/// Get sub-array of the data with given fixed indexes
inline mglData mglSubData(const mglDataA &dat, long xx, long yy=-1, long zz=-1)
{	return mglData(true,mgl_data_subdata(&dat,xx,yy,zz));	}
inline mglData mglSubData(const mglDataA &dat, const mglDataA &xx, const mglDataA &yy, const mglDataA &zz)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,&yy,&zz));	}
inline mglData mglSubData(const mglDataA &dat, const mglDataA &xx, const mglDataA &yy)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,&yy,0));	}
inline mglData mglSubData(const mglDataA &dat, const mglDataA &xx)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,0,0));	}
//-----------------------------------------------------------------------------
/// Prepare coefficients for global spline interpolation
inline mglData mglGSplineInit(const mglDataA &xdat, const mglDataA &ydat)
{	return mglData(true,mgl_gspline_init(&xdat, &ydat));	}
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
inline mreal mglGSpline(const mglDataA &coef, mreal dx, mreal *d1=0, mreal *d2=0)
{	return mgl_gspline(&coef, dx, d1,d2);	}
//-----------------------------------------------------------------------------
/// Wrapper class for expression evaluating
class MGL_EXPORT mglExpr
{
	HMEX ex;
	mglExpr(const mglExpr &){}	// copying is not allowed
	const mglExpr &operator=(const mglExpr &t){return t;}	// copying is not allowed
public:
	mglExpr(const char *expr)		{	ex = mgl_create_expr(expr);	}
#if MGL_HAVE_RVAL
	mglExpr(mglExpr &&d):ex(d.ex)	{	d.ex=0;	}
#endif
	~mglExpr()	{	mgl_delete_expr(ex);	}
	/// Return value of expression for given x,y,z variables
	inline double Eval(double x, double y=0, double z=0)
	{	return mgl_expr_eval(ex,x,y,z);	}
	/// Return value of expression differentiation over variable dir for given x,y,z variables
	inline double Diff(char dir, double x, double y=0, double z=0)
	{	return mgl_expr_diff(ex,dir, x,y,z);	}
#ifndef SWIG
	/// Return value of expression for given variables
	inline double Eval(mreal var[26])
	{	return mgl_expr_eval_v(ex,var);	}
	/// Return value of expression differentiation over variable dir for given variables
	inline double Diff(char dir, mreal var[26])
	{	return mgl_expr_diff_v(ex,dir, var);	}
#endif
};
//-----------------------------------------------------------------------------
/// Class which present equidistantly distributed data
class MGL_EXPORT mglDataV : public mglDataA
{
	long nx;	///< number of points in 1st dimensions ('x' dimension)
	long ny;	///< number of points in 2nd dimensions ('y' dimension)
	long nz;	///< number of points in 3d dimensions ('z' dimension)
	mreal di, dj, dk, a0;
public:
	mglDataV(long xx=1,long yy=1,long zz=1,mreal x1=0,mreal x2=mglNaN,char dir='x'):nx(xx),ny(yy),nz(zz)
	{	Fill(x1,x2,dir);	}
	mglDataV(const mglDataV &d):nx(d.nx),ny(d.ny),nz(d.nz),di(d.di),dj(d.dj),dk(d.dk),a0(d.a0)	{}
#if MGL_HAVE_RVAL
	mglDataV(mglDataV &&d):nx(d.nx),ny(d.ny),nz(d.nz),di(d.di),dj(d.dj),dk(d.dk),a0(d.a0)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	d.func=0;	}
#endif
	virtual ~mglDataV()	{}

	/// Get sizes
	long GetNx() const	{	return nx;	}
	long GetNy() const	{	return ny;	}
	long GetNz() const	{	return nz;	}

	/// Create or recreate the array with specified size and fill it by zero
	inline void Create(long mx,long my=1,long mz=1)
	{	di=mx>1?di*(nx-1)/(mx-1):0;	dj=my>1?dj*(ny-1)/(my-1):0;
		dk=mz>1?dk*(nz-1)/(mz-1):0;	nx=mx;	ny=my;	nz=mz;	}
	/// For going throw all elements
	inline void All()	{	di=dj=dk=1;	a0=0;	}
	/// Equidistantly fill the data to range [x1,x2] in direction dir
	inline void Fill(mreal x1,mreal x2=mglNaN,char dir='x')
	{
		di=dj=dk=0;	a0=x1;
		if(mgl_isnum(x2))
		{
			if(dir=='x' && nx>1)	di=(x2-x1)/(nx-1);
			if(dir=='y' && ny>1)	dj=(x2-x1)/(ny-1);
			if(dir=='z' && nz>1)	dk=(x2-x1)/(nz-1);
		}
	}
	mreal Maximal() const
	{	return a0+mgl_max(mgl_max(di*(nx-1),dj*(ny-1)),mgl_max(dk*(nz-1),0));	}
	mreal Minimal() const
	{	return a0+mgl_min(mgl_min(di*(nx-1),dj*(ny-1)),mgl_min(dk*(nz-1),0));	}

	/// Copy data from other mglDataV variable
	inline const mglDataV &operator=(const mglDataV &d)
	{	nx=d.nx;	ny=d.ny;	nz=d.nz;	a0=d.a0;
		di=d.di;	dj=d.dj;	dk=d.dk;	return d;	}
	inline mreal operator=(mreal val)
	{	di=dj=dk=0;	a0=val;	return val;	}
	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal y=0,mreal z=0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	if(dx)	*dx=di;	if(dy)	*dy=dj;	if(dz)	*dz=dk;
		return a0+di*x+dj*y+dk*z;	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal y=0,mreal z=0) const	{	return a0+di*x+dj*y+dk*z;	}
	mreal v(long i,long j=0,long k=0) const		{	return a0+di*i+dj*j+dk*k;	}
	mreal vthr(long ii) const
	{	long i=ii%nx, j=(ii/nx)%ny, k=ii/(nx*ny);	return a0+di*i+dj*j+dk*k;	}
	// add for speeding up !!!
	mreal dvx(long ,long =0,long =0) const	{	return di;	}
	mreal dvy(long ,long =0,long =0) const	{	return dj;	}
	mreal dvz(long ,long =0,long =0) const	{	return dk;	}
};
//-----------------------------------------------------------------------------
/// Class which present FFT frequency as data array
class MGL_EXPORT mglDataW : public mglDataA
{
	long nx;	///< number of points in 1st dimensions ('x' dimension)
	long ny;	///< number of points in 2nd dimensions ('y' dimension)
	long nz;	///< number of points in 3d dimensions ('z' dimension)
	mreal di, dj, dk;
public:

	mglDataW(long xx=1,long yy=1,long zz=1,mreal dp=0,char dir='x'):nx(xx),ny(yy),nz(zz)
	{	Freq(dp,dir);	}
	mglDataW(const mglDataW &d):nx(d.nx),ny(d.ny),nz(d.nz),di(d.di),dj(d.dj),dk(d.dk)	{}
#if MGL_HAVE_RVAL
	mglDataW(mglDataW &&d):nx(d.nx),ny(d.ny),nz(d.nz),di(d.di),dj(d.dj),dk(d.dk)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	d.func=0;	}
#endif
	virtual ~mglDataW()	{}

	/// Get sizes
	long GetNx() const	{	return nx;	}
	long GetNy() const	{	return ny;	}
	long GetNz() const	{	return nz;	}

	/// Create or recreate the array with specified size and fill it by zero
	inline void Create(long mx,long my=1,long mz=1)
	{	nx=mx;	ny=my;	nz=mz;	}
	/// For going throw all elements
	inline void All()	{	di=dj=dk=1;	}
	/// Equidistantly fill the data to range [x1,x2] in direction dir
	inline void Freq(mreal dp,char dir='x')
	{
		di=dj=dk=0;
		if(dir=='x')	di=dp;
		if(dir=='y')	dj=dp;
		if(dir=='z')	dk=dp;
	}
	mreal Maximal() const
	{	return mgl_max(mgl_max(di*(nx-1),dj*(ny-1)),mgl_max(dk*(nz-1),0));	}
	mreal Minimal() const
	{	return mgl_min(mgl_min(di*(nx-1),dj*(ny-1)),mgl_min(dk*(nz-1),0));	}

	/// Copy data from other mglDataV variable
	inline const mglDataW &operator=(const mglDataW &d)
	{	nx=d.nx;	ny=d.ny;	nz=d.nz;	di=d.di;	dj=d.dj;	dk=d.dk;	return d;	}
	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal y=0,mreal z=0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	if(dx)	*dx=di;	if(dy)	*dy=dj;	if(dz)	*dz=dk;
		return di*(x<nx/2?x:x-nx)+dj*(y<ny/2?y:y-ny)+dk*(z<nz/2?z:z-nz);	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal y=0,mreal z=0) const
	{	return di*(x<nx/2?x:x-nx)+dj*(y<ny/2?y:y-ny)+dk*(z<nz/2?z:z-nz);	}
	mreal v(long i,long j=0,long k=0) const
	{	return di*(i<nx/2?i:i-nx)+dj*(j<ny/2?j:j-ny)+dk*(k<nz/2?k:k-nz);	}
	mreal vthr(long ii) const
	{	long i=ii%nx, j=(ii/nx)%ny, k=ii/(nx*ny);
		return di*(i<nx/2?i:i-nx)+dj*(j<ny/2?j:j-ny)+dk*(k<nz/2?k:k-nz);	}
	// add for speeding up !!!
	mreal dvx(long ,long =0,long =0) const	{	return di;	}
	mreal dvy(long ,long =0,long =0) const	{	return dj;	}
	mreal dvz(long ,long =0,long =0) const	{	return dk;	}
};
//-----------------------------------------------------------------------------
/// Class which present function as data array
class MGL_EXPORT mglDataF : public mglDataA
{
	long nx;	///< number of points in 1st dimensions ('x' dimension)
	long ny;	///< number of points in 2nd dimensions ('y' dimension)
	long nz;	///< number of points in 3d dimensions ('z' dimension)
	std::string str;	///< function as string
	mglPoint v1, v2;	///< ranges for coordinates
	HMEX ex;			///< parsed variant
	mreal dx,dy,dz;
	inline void setD()
	{
		dx = nx>1?(v2.x-v1.x)/(nx-1):0;
		dy = ny>1?(v2.y-v1.y)/(ny-1):0;
		dz = nz>1?(v2.z-v1.z)/(nz-1):0;
	}
	mreal (*dfunc)(mreal i, mreal j, mreal k, void *par);
	void *par;
public:

	mglDataF(long xx=1,long yy=1,long zz=1):nx(xx),ny(yy),nz(zz), dfunc(0),par(0)
	{	ex=0;	v2.Set(1,1,1);	setD();	}
	mglDataF(const mglDataF &d) : nx(d.nx), ny(d.ny), nz(d.nz), str(d.str), v1(d.v1), v2(d.v2), dx(d.dx),dy(d.dy),dz(d.dz), dfunc(d.dfunc),par(d.par)
	{	ex = mgl_create_expr(str.c_str());	}
#if MGL_HAVE_RVAL
	mglDataF(mglDataF &&d):nx(d.nx),ny(d.ny),nz(d.nz), str(d.str), v1(d.v1),v2(d.v2), ex(d.ex), dx(d.dx),dy(d.dy),dz(d.dz), dfunc(d.dfunc),par(d.par)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	d.ex=0;	d.func=0;	}
#endif
	virtual ~mglDataF()	{	mgl_delete_expr(ex);	}

	/// Get sizes
	long GetNx() const	{	return nx;	}
	long GetNy() const	{	return ny;	}
	long GetNz() const	{	return nz;	}

	/// Create or recreate the array with specified size and fill it by zero
	inline void Create(long mx,long my=1,long mz=1)	{	nx=mx;	ny=my;	nz=mz;	setD();	}
	inline void SetRanges(mglPoint p1, mglPoint p2)	{	v1=p1;	v2=p2;	setD();	}
	/// Set formula to be used as dfunction
	inline void SetFormula(const char *eq)
	{
		mgl_delete_expr(ex);	dfunc=0;	par=0;
		if(eq && *eq)	{	ex = mgl_create_expr(eq);	str=eq;	}
		else	{	ex=0;	str="";	}
	}
	/// Set function and coordinates range [r1,r2]
	inline void SetFunc(mreal (*f)(mreal,mreal,mreal,void*), void *p=NULL)
	{	mgl_delete_expr(ex);	ex=0;	dfunc=f;	par=p;	}

	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal i,mreal j=0,mreal k=0, mreal *di=0,mreal *dj=0,mreal *dk=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(di)	*di = 0;
		if(dj)	*dj = 0;
		if(dk)	*dk = 0;
		if(dfunc)
		{
			res = dfunc(x,y,z, par);
			if(di)	*di = dfunc(x+dx,y,z, par)-res;
			if(dj)	*dj = dfunc(x,y+dy,z, par)-res;
			if(dk)	*dk = dfunc(x,y,z+dz, par)-res;
		}
		else if(ex)
		{
			if(di)	*di = mgl_expr_diff(ex,'x',x,y,z)*dx;
			if(dj)	*dj = mgl_expr_diff(ex,'y',x,y,z)*dy;
			if(dk)	*dk = mgl_expr_diff(ex,'z',x,y,z)*dz;
			res = mgl_expr_eval(ex,x,y,z);
		}
		return res;
	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal i,mreal j=0,mreal k=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(dfunc)	res = dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x,y,z);
		return res;
	}
	/// Copy data from other mglDataV variable
	inline const mglDataF &operator=(const mglDataF &d)
	{	nx=d.nx;	ny=d.ny;	nz=d.nz;	v1=d.v1;	v2=d.v2;	setD();	mgl_delete_expr(ex);
		str=d.str;	ex = mgl_create_expr(str.c_str());	dfunc=d.dfunc;	par=d.par;	return d;	}
	/// Get the value in given cell of the data without border checking
	mreal v(long i,long j=0,long k=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(dfunc)	res = dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x,y,z);
		return res;
	}
	mreal vthr(long i) const
	{
		mreal res=0, x=v1.x+dx*(i%nx), y=v1.y+dy*((i/nx)%ny), z=v1.z+dz*(i/(nx*ny));
		if(dfunc)	res = dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x,y,z);
		return res;
	}
	// add for speeding up !!!
	mreal dvx(long i,long j=0,long k=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(dfunc)	res = dfunc(x+dx,y,z, par)-dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x+dx,y,z)-mgl_expr_eval(ex,x,y,z);
		return res;
	}
	mreal dvy(long i,long j=0,long k=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(dfunc)	res = dfunc(x,y+dy,z, par)-dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x,y+dy,z)-mgl_expr_eval(ex,x,y,z);
		return res;
	}
	mreal dvz(long i,long j=0,long k=0) const
	{
		mreal res=0, x=v1.x+dx*i, y=v1.y+dy*j, z=v1.z+dz*k;
		if(dfunc)	res = dfunc(x,y,z+dz, par)-dfunc(x,y,z, par);
		else if(ex)	res = mgl_expr_eval(ex,x,y,z+dz)-mgl_expr_eval(ex,x,y,z);
		return res;
	}
};
//-----------------------------------------------------------------------------
/// Class which present column of another data as data array
class MGL_EXPORT mglDataT : public mglDataA
{
	const mglDataA &dat;
	long ind;
	const mglDataT &operator=(const mglDataT &d)	{	return d;	}
public:
	mglDataT(const mglDataT &d) : dat(d.dat), ind(d.ind)	{	s = d.s;	}
	mglDataT(const mglDataA &d, long col=0) : dat(d), ind(col)	{}
	mglDataT(HCDT d, long col=0) : dat(*d), ind(col)	{}
#if MGL_HAVE_RVAL
	mglDataT(mglDataT &&d):dat(d.dat),ind(d.ind)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	d.func=0;	}
#endif
	virtual ~mglDataT()	{}

	/// Get sizes
	long GetNx() const	{	return dat.GetNy();	}
	long GetNy() const	{	return dat.GetNz();	}
	long GetNz() const	{	return 1;	}

	mreal Maximal() const
	{	return mglSubData(dat,ind).Maximal();	}
	mreal Minimal() const
	{	return mglSubData(dat,ind).Minimal();	}
	inline void SetInd(long i, const wchar_t *name)
	{	ind = i;	s = name;	}
	inline void SetInd(long i, wchar_t name)
	{	ind = i;	s = name;	}

	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal y=0,mreal =0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	if(dz)	*dz=0;	return dat.valueD(ind,x,y,0,dx,dy);	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal y=0,mreal =0) const
	{	return dat.value(ind,x,y);	}
	/// Get the value in given cell of the data without border checking
	mreal v(long i,long j=0,long =0) const
	{	return dat.v(ind,i,j);	}
	mreal vthr(long i) const
	{	return dat.vthr(ind+dat.GetNx()*i);	}
	// add for speeding up !!!
	mreal dvx(long i,long j=0,long =0) const
	{	return	dat.dvy(ind,i,j);	}
	mreal dvy(long i,long j=0,long =0) const
	{	return dat.dvz(ind,i,j);	}
	mreal dvz(long ,long =0,long =0) const
	{	return 0;	}
};
//-----------------------------------------------------------------------------
/// Class which present row of another data as data array
class MGL_EXPORT mglDataR : public mglDataA
{
	const mglDataA &dat;
	long ind;
	const mglDataR &operator=(const mglDataR &d)	{	return d;	}
public:
	mglDataR(const mglDataR &d) : dat(d.dat), ind(d.ind)	{	s = d.s;	}
	mglDataR(const mglDataA &d, long row=0) : dat(d), ind(row)	{}
	mglDataR(HCDT d, long row=0) : dat(*d), ind(row)	{}
#if MGL_HAVE_RVAL
	mglDataR(mglDataR &&d):dat(d.dat),ind(d.ind)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	d.func=0;	}
#endif
	virtual ~mglDataR()	{}

	/// Get sizes
	long GetNx() const	{	return dat.GetNx();	}
	long GetNy() const	{	return 1;	}
	long GetNz() const	{	return 1;	}

	mreal Maximal() const
	{	return mglSubData(dat,-1,ind).Maximal();	}
	mreal Minimal() const
	{	return mglSubData(dat,-1,ind).Minimal();	}
	inline void SetInd(long i, const wchar_t *name)
	{	ind = i;	s = name;	}
	inline void SetInd(long i, wchar_t name)
	{	ind = i;	s = name;	}

	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal =0,mreal =0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	if(dy)	*dy=0;	if(dz)	*dz=0;	return dat.valueD(x,ind,0,dx);	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal =0,mreal =0) const
	{	return dat.value(x,ind,0);	}
	/// Get the value in given cell of the data without border checking
	mreal v(long i,long =0,long =0) const
	{	return dat.v(i,ind,0);	}
	mreal vthr(long i) const
	{	return dat.vthr(i+dat.GetNx()*ind);	}
	// add for speeding up !!!
	mreal dvx(long i,long =0,long =0) const
	{	return	dat.dvx(i,ind,0);	}
	mreal dvy(long ,long =0,long =0) const
	{	return 0;	}
	mreal dvz(long ,long =0,long =0) const
	{	return 0;	}
};
//-----------------------------------------------------------------------------
/// Class which present std::vector as data array
class MGL_EXPORT mglDataS : public mglDataA
{
public:
	std::vector<mreal> dat;

	mglDataS(const mglDataS &st) : dat(st.dat)	{}
	mglDataS(const std::vector<mreal> &d) : dat(d)	{}
	mglDataS(size_t s=1)	{	dat.resize(s);	}
	~mglDataS()	{}
	inline void reserve(size_t num)	{	dat.reserve(num);	}
	inline void clear()	{	dat.clear();	}
	inline double operator[](size_t i)	{	return dat[i];	}
	inline void push_back(double t)	{	dat.push_back(t);	}
	inline size_t size() const	{	return dat.size();	}
	const mglDataS &operator=(const mglDataS &st)	{	dat = st.dat;	return st;	}
	const std::vector<mreal> &operator=(const std::vector<mreal> &st)	{	dat = st;	return st;	}

	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal =0,mreal =0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	return mglSpline3(dat.data(),dat.size(),1,1,x,0,0,dx,dy,dz);	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal =0,mreal =0) const
	{	return mglSpline3s(dat.data(),dat.size(),1,1,x,0,0);	}

	mreal v(long i,long =0,long =0) const		{	return dat[i];	}
	mreal vthr(long i) const	{	return dat[i];	}
	long GetNx() const	{	return dat.size();	}
	long GetNy() const	{	return 1;	}
	long GetNz() const	{	return 1;	}
	mreal dvx(long i,long =0,long =0) const
	{	return i>0? (i<long(dat.size()-1)? (dat[i+1]-dat[i-1])/2:dat[i]-dat[i-1]) : dat[i+1]-dat[i];	}
	mreal dvy(long ,long =0,long =0) const	{	return 0;	}
	mreal dvz(long ,long =0,long =0) const	{	return 0;	}
};
//-----------------------------------------------------------------------------
#endif
#endif
