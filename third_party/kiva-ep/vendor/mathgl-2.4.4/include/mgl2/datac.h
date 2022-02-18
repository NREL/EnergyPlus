/***************************************************************************
 * datac.h is part of Math Graphic Library
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
#ifndef _MGL_DATAC_H_
#define _MGL_DATAC_H_

#include "mgl2/data.h"
#include "mgl2/datac_cf.h"
//-----------------------------------------------------------------------------
#include <vector>
#include <string>
//-----------------------------------------------------------------------------
#ifndef SWIG
dual MGL_EXPORT mglLinearC(const dual *a, long nx, long ny, long nz, mreal x, mreal y, mreal z);
dual MGL_EXPORT mglSpline3C(const dual *a, long nx, long ny, long nz, mreal x, mreal y, mreal z,dual *dx=0, dual *dy=0, dual *dz=0);
dual MGL_EXPORT mglSpline3Cs(const dual *a, long nx, long ny, long nz, mreal x, mreal y, mreal z);
//-----------------------------------------------------------------------------
/// Class for working with complex data array
class MGL_EXPORT mglDataC : public mglDataA
{
public:
using mglDataA::Momentum;
	long nx;		///< number of points in 1st dimensions ('x' dimension)
	long ny;		///< number of points in 2nd dimensions ('y' dimension)
	long nz;		///< number of points in 3d dimensions ('z' dimension)
	dual *a;		///< data array
	bool link;		///< use external data (i.e. don't free it)

	/// Initiate by other mglDataC variable
	mglDataC(const mglDataC &d)	{	a=0;	mgl_datac_set(this,&d);		}	// NOTE: must be constructor for mglDataC& to exclude copy one
	mglDataC(const mglDataA &d)	{	a=0;	mgl_datac_set(this,&d);		}
#if MGL_HAVE_RVAL
	mglDataC(mglDataC &&d):nx(d.nx),ny(d.ny),nz(d.nz),a(d.a),link(d.link)
	{	s=d.s;	temp=d.temp;	func=d.func;	o=d.o;	id=d.id;	d.a=0;	d.func=0;	}
#endif
	mglDataC(const mglDataA &re, const mglDataA &im)	{	a=0;	mgl_datac_set_ri(this,&re,&im);	}
	mglDataC(HCDT d)	{	a=0;	mgl_datac_set(this, d);		}
	mglDataC(HCDT re, HCDT im)	{	a=0;	mgl_datac_set_ri(this, re, im);		}
	mglDataC(bool, mglDataC *d)	// NOTE: Variable d will be deleted!!!
	{	if(d)
		{	nx=d->nx;	ny=d->ny;	nz=d->nz;	a=d->a;	d->a=0;
			temp=d->temp;	func=d->func;	o=d->o;	s=d->s;
			id=d->id;	link=d->link;	delete d;	}
		else	{	a=0;	Create(1);	}	}
	/// Initiate by flat array
	mglDataC(int size, const dual *d)	{	a=0;	Set(d,size);	}
	mglDataC(int rows, int cols, const dual *d)	{	a=0;	Set(d,cols,rows);	}
	mglDataC(int size, const double *d)	{	a=0;	Set(d,size);	}
	mglDataC(int rows, int cols, const double *d)	{	a=0;	Set(d,cols,rows);	}
	mglDataC(int size, const float *d)	{	a=0;	Set(d,size);	}
	mglDataC(int rows, int cols, const float *d)	{	a=0;	Set(d,cols,rows);	}
	mglDataC(const dual *d, int size)	{	a=0;	Set(d,size);	}
	mglDataC(const dual *d, int rows, int cols)	{	a=0;	Set(d,cols,rows);	}
	mglDataC(const double *d, int size)	{	a=0;	Set(d,size);	}
	mglDataC(const double *d, int rows, int cols)	{	a=0;	Set(d,cols,rows);	}
	mglDataC(const float *d, int size)	{	a=0;	Set(d,size);	}
	mglDataC(const float *d, int rows, int cols)	{	a=0;	Set(d,cols,rows);	}
	/// Allocate memory and copy data from std::vector<T>
	mglDataC(const std::vector<int> &d)		{	a=0;	Set(d);	}
	mglDataC(const std::vector<float> &d)	{	a=0;	Set(d);	}
	mglDataC(const std::vector<double> &d)	{	a=0;	Set(d);	}
	mglDataC(const std::vector<std::complex<double> > &d)	{	a=0;	Set(d);	}
	mglDataC(const std::vector<std::complex<float> > &d)	{	a=0;	Set(d);	}
	/// Read data from file
	mglDataC(const char *fname)			{	a=0;	Read(fname);	}
	/// Allocate the memory for data array and initialize it zero
	mglDataC(long xx=1,long yy=1,long zz=1)	{	a=0;	Create(xx,yy,zz);	}
	/// Delete the array
	virtual ~mglDataC()	{	if(!link && a)	delete []a;	}

	/// Move all data from variable d, and delete this variable.
	inline void Move(mglDataC *d)	// NOTE: Variable d will be deleted!!!
	{	if(d && d->GetNN()>1)
		{	bool l=link;	dual *b=a;
			nx=d->nx;	ny=d->ny;	nz=d->nz;	a=d->a;	d->a=b;
			temp=d->temp;	func=d->func;	o=d->o;	s=d->s;
			id=d->id;	link=d->link;	d->link=l;	delete d;	}
		else if(d)	{	*this = d->a[0];	delete d;	}
	}

	inline dual GetVal(long i, long j=0, long k=0) const
	{	return mgl_datac_get_value(this,i,j,k);}
	inline void SetVal(dual f, long i, long j=0, long k=0)
	{	mgl_datac_set_value(this,f,i,j,k);	}
	/// Get sizes
	long GetNx() const	{	return nx;	}
	long GetNy() const	{	return ny;	}
	long GetNz() const	{	return nz;	}

	/// Link external data array (don't delete it at exit)
	inline void Link(dual *A, long NX, long NY=1, long NZ=1)
	{	mgl_datac_link(this,reinterpret_cast<mdual*>(A),NX,NY,NZ);	}
	inline void Link(mglDataC &d)	{	Link(d.a,d.nx,d.ny,d.nz);	}
	/// Allocate memory and copy the data from the gsl_vector
	inline void Set(gsl_vector *m)	{	mgl_datac_set_vector(this,m);	}
	/// Allocate memory and copy the data from the gsl_matrix
	inline void Set(gsl_matrix *m)	{	mgl_datac_set_matrix(this,m);	}

	/// Allocate memory and copy the data from the (float *) array
	inline void Set(const float *A,long NX,long NY=1,long NZ=1)
	{	mgl_datac_set_float(this,A,NX,NY,NZ);	}
	/// Allocate memory and copy the data from the (double *) array
	inline void Set(const double *A,long NX,long NY=1,long NZ=1)
	{	mgl_datac_set_double(this,A,NX,NY,NZ);	}
	/// Allocate memory and copy the data from the (complex *) array
	inline void Set(const dual *A,long NX,long NY=1,long NZ=1)
	{	mgl_datac_set_complex(this,reinterpret_cast<const mdual*>(A),NX,NY,NZ);	}
	/// Allocate memory and scanf the data from the string
	inline void Set(const char *str,long NX,long NY=1,long NZ=1)
	{	mgl_datac_set_values(this,str,NX,NY,NZ);	}
	/// Import data from abstract type
	inline void Set(HCDT dat)	{	mgl_datac_set(this, dat);	}
	inline void Set(const mglDataA &dat)	{	mgl_datac_set(this, &dat);	}
	inline void Set(const mglDataA &re, const mglDataA &im)	{	mgl_datac_set_ri(this, &re, &im);	}
	inline void Set(HCDT re, HCDT im)	{	mgl_datac_set_ri(this, re, im);	}
	inline void SetAmpl(const mglDataA &ampl, const mglDataA &phase)
	{	mgl_datac_set_ap(this, &ampl, &phase);	}
	/// Allocate memory and copy data from std::vector<T>
	inline void Set(const std::vector<int> &d)
	{	if(d.size()>0)	{	Create(d.size());	for(long i=0;i<nx;i++)	a[i] = d[i];	}
		else	Create(1);	}
	inline void Set(const std::vector<float> &d)
	{	if(d.size()>0)	Set(&(a[0]),d.size());	else	Create(1);	}
	inline void Set(const std::vector<double> &d)
	{	if(d.size()>0)	Set(&(a[0]),d.size());	else	Create(1);	}
	inline void Set(const std::vector<std::complex<double> > &d)
	{	if(d.size()>0)	{	Create(d.size());	for(long i=0;i<nx;i++)	a[i] = d[i];	}
		else	Create(1);	}
	inline void Set(const std::vector<std::complex<float> > &d)
	{	if(d.size()>0)	{	Create(d.size());	for(long i=0;i<nx;i++)	a[i] = d[i];	}
		else	Create(1);	}

	/// Create or recreate the array with specified size and fill it by zero
	inline void Create(long mx,long my=1,long mz=1)
	{	mgl_datac_create(this,mx,my,mz);	}
	/// Rearange data dimensions
	inline void Rearrange(long mx, long my=0, long mz=0)
	{	mgl_datac_rearrange(this,mx,my,mz);	}
	/// Transpose dimensions of the data (generalization of Transpose)
	inline void Transpose(const char *dim="yx")
	{	mgl_datac_transpose(this,dim);	}
	/// Extend data dimensions
	inline void Extend(long n1, long n2=0)
	{	mgl_datac_extend(this,n1,n2);	}
	/// Reduce size of the data
	inline void Squeeze(long rx,long ry=1,long rz=1,bool smooth=false)
	{	mgl_datac_squeeze(this,rx,ry,rz,smooth);	}
	/// Crop the data
	inline void Crop(long n1, long n2,char dir='x')
	{	mgl_datac_crop(this,n1,n2,dir);	}
	/// Crop the data to be most optimal for FFT (i.e. to closest value of 2^n*3^m*5^l)
	inline void Crop(const char *how="235x")
	{	mgl_datac_crop_opt(this, how);	}
	/// Insert data
	inline void Insert(char dir, long at=0, long num=1)
	{	mgl_datac_insert(this,dir,at,num);	}
	/// Delete data
	inline void Delete(char dir, long at=0, long num=1)
	{	mgl_datac_delete(this,dir,at,num);	}
	/// Join with another data array
	inline void Join(const mglDataA &d)
	{	mgl_datac_join(this,&d);	}

	/// Modify the data by specified formula
	inline void Modify(const char *eq,long dim=0)
	{	mgl_datac_modify(this, eq, dim);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglDataA &vdat, const mglDataA &wdat)
	{	mgl_datac_modify_vw(this,eq,&vdat,&wdat);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglDataA &vdat)
	{	mgl_datac_modify_vw(this,eq,&vdat,0);	}
	/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
	inline void Fill(mglBase *gr, const char *eq, const char *opt="")
	{	mgl_datac_fill_eq(gr,this,eq,0,0,opt);	}
	inline void Fill(mglBase *gr, const char *eq, const mglDataA &vdat, const char *opt="")
	{	mgl_datac_fill_eq(gr,this,eq,&vdat,0,opt);	}
	inline void Fill(mglBase *gr, const char *eq, const mglDataA &vdat, const mglDataA &wdat,const char *opt="")
	{	mgl_datac_fill_eq(gr,this,eq,&vdat,&wdat,opt);	}
	/// Equidistantly fill the data to range [x1,x2] in direction dir
	inline void Fill(dual x1,dual x2=mglNaN,char dir='x')
	{	mgl_datac_fill(this,x1,x2,dir);	}

		/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2] using global spline
	inline void RefillGS(const mglDataA &xdat, const mglDataA &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_datac_refill_gs(this,&xdat,&vdat,x1,x2,sl);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2]
	inline void Refill(const mglDataA &xdat, const mglDataA &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_datac_refill_x(this,&xdat,&vdat,x1,x2,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_datac_refill_x(this,&xdat,&vdat,p1.x,p2.x,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &ydat, const mglDataA &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_datac_refill_xy(this,&xdat,&ydat,&vdat,p1.x,p2.x,p1.y,p2.y,sl);	}
	inline void Refill(const mglDataA &xdat, const mglDataA &ydat, const mglDataA &zdat, const mglDataA &vdat, mglPoint p1, mglPoint p2)
	{	mgl_datac_refill_xyz(this,&xdat,&ydat,&zdat,&vdat,p1.x,p2.x,p1.y,p2.y,p1.z,p2.z);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range of gr
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &vdat, long sl=-1, const char *opt="")
	{	mgl_datac_refill_gr(gr,this,&xdat,0,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &ydat, const mglDataA &vdat, long sl=-1, const char *opt="")
	{	mgl_datac_refill_gr(gr,this,&xdat,&ydat,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglDataA &xdat, const mglDataA &ydat, const mglDataA &zdat, const mglDataA &vdat, const char *opt="")
	{	mgl_datac_refill_gr(gr,this,&xdat,&ydat,&zdat,&vdat,-1,opt);	}


		/// Put value to data element(s)
	inline void Put(dual val, long i=-1, long j=-1, long k=-1)
	{	mgl_datac_put_val(this,val,i,j,k);	}
	/// Put array to data element(s)
	inline void Put(const mglDataA &dat, long i=-1, long j=-1, long k=-1)
	{	mgl_datac_put_dat(this,&dat,i,j,k);	}

	/// Read data from tab-separated text file with auto determining size
	inline bool Read(const char *fname)
	{	return mgl_datac_read(this,fname); }
	/// Read data from text file with specifeid size
	inline bool Read(const char *fname,long mx,long my=1,long mz=1)
	{	return mgl_datac_read_dim(this,fname,mx,my,mz);	}
	/// Save whole data array (for ns=-1) or only ns-th slice to text file
	void Save(const char *fname,long ns=-1) const
	{	mgl_datac_save(this,fname,ns);	}
	/// Get whole data array (for ns=-1) or only ns-th slice to string
	std::string Get(long ns=-1) const
	{	return mgl_datac_to_string(this,ns);	}

	/// Read data from tab-separated text files with auto determining size which filenames are result of sprintf(fname,templ,t) where t=from:step:to
	inline bool ReadRange(const char *templ, double from, double to, double step=1, bool as_slice=false)
	{	return mgl_datac_read_range(this,templ,from,to,step,as_slice);	}
	/// Read data from tab-separated text files with auto determining size which filenames are satisfied to template (like "t_*.dat")
	inline bool ReadAll(const char *templ, bool as_slice=false)
	{	return mgl_datac_read_all(this, templ, as_slice);	}
	/// Read data from text file with size specified at beginning of the file
	inline bool ReadMat(const char *fname, long dim=2)
	{	return mgl_datac_read_mat(this,fname,dim);	}

		/// Read data array from HDF file (parse HDF4 and HDF5 files)
	inline int ReadHDF(const char *fname,const char *data)
	{	return mgl_datac_read_hdf(this,fname,data);	}
	/// Save data to HDF file
	void SaveHDF(const char *fname,const char *data,bool rewrite=false) const
	{	mgl_datac_save_hdf(this,fname,data,rewrite);	}

	/// Get real part of data values
	inline mglData Real() const
	{	return mglData(true,mgl_datac_real(this));	}
	/// Get imaginary part of data values
	inline mglData Imag() const
	{	return mglData(true,mgl_datac_imag(this));	}
	/// Get absolute value of data values, i.e. |u|
	inline mglData Abs() const
	{	return mglData(true,mgl_datac_abs(this));	}
	/// Get square of absolute value of data values, i.e. |u|^2
	inline mglData Norm() const
	{	return mglData(true,mgl_datac_norm(this));	}
	/// Get argument of data values
	inline mglData Arg() const
	{	return mglData(true,mgl_datac_arg(this));	}

	/// Get column (or slice) of the data filled by formulas of named columns
	inline mglDataC Column(const char *eq) const
	{	return mglDataC(true,mgl_datac_column(this,eq));	}
	/// Get momentum (1D-array) of data along direction 'dir'. String looks like "x1" for median in x-direction, "x2" for width in x-dir and so on.
	inline mglDataC Momentum(char dir, const char *how) const
	{	return mglDataC(true,mgl_datac_momentum(this,dir,how));	}
	/// Get sub-array of the data with given fixed indexes
	inline mglDataC SubData(long xx,long yy=-1,long zz=-1) const
	{	return mglDataC(true,mgl_datac_subdata(this,xx,yy,zz));	}
	inline mglDataC SubData(const mglDataA &xx, const mglDataA &yy, const mglDataA &zz) const
	{	return mglDataC(true,mgl_datac_subdata_ext(this,&xx,&yy,&zz));	}
	inline mglDataC SubData(const mglDataA &xx, const mglDataA &yy) const
	{	return mglDataC(true,mgl_datac_subdata_ext(this,&xx,&yy,0));	}
	inline mglDataC SubData(const mglDataA &xx) const
	{	return mglDataC(true,mgl_datac_subdata_ext(this,&xx,0,0));	}
	/// Get data from sections ids, separated by value val along specified direction.
	/** If section id is negative then reverse order is used (i.e. -1 give last section). */
	inline mglDataC Section(const mglDataA &ids, char dir='y', mreal val=NAN) const
	{	return mglDataC(true,mgl_datac_section(this,&ids,dir,val));	}
	inline mglDataC Section(long id, char dir='y', mreal val=NAN) const
	{	return mglDataC(true,mgl_datac_section_val(this,id,dir,val));	}

	/// Get trace of the data array
	inline mglDataC Trace() const
	{	return mglDataC(true,mgl_datac_trace(this));	}
	/// Get array which is result of summation in given direction or directions
	inline mglDataC Sum(const char *dir) const
	{	return mglDataC(true,mgl_datac_sum(this,dir));	}
	/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
	inline mglDataC Combine(const mglDataA &dat) const
	{	return mglDataC(true,mgl_datac_combine(this,&dat));	}
	/// Resize the data to new size of box [x1,x2]*[y1,y2]*[z1,z2]
	inline mglDataC Resize(long mx,long my=1,long mz=1, mreal x1=0,mreal x2=1, mreal y1=0,mreal y2=1, mreal z1=0,mreal z2=1) const
	{	return mglDataC(true,mgl_datac_resize_box(this,mx,my,mz,x1,x2,y1,y2,z1,z2));	}
	/// Get array which values is result of interpolation this for coordinates from other arrays
	inline mglDataC Evaluate(const mglData &idat, bool norm=true) const
	{	return mglDataC(true,mgl_datac_evaluate(this,&idat,0,0,norm));	}
	inline mglDataC Evaluate(const mglData &idat, const mglData &jdat, bool norm=true) const
	{	return mglDataC(true,mgl_datac_evaluate(this,&idat,&jdat,0,norm));	}
	inline mglDataC Evaluate(const mglData &idat, const mglData &jdat, const mglData &kdat, bool norm=true) const
	{	return mglDataC(true,mgl_datac_evaluate(this,&idat,&jdat,&kdat,norm));	}

	/// Find correlation with another data arrays
	inline mglDataC Correl(const mglData &dat, const char *dir) const
	{	return mglDataC(true,mgl_datac_correl(this,&dat,dir));	}
	/// Find auto correlation function
	inline mglDataC AutoCorrel(const char *dir) const
	{	return mglDataC(true,mgl_datac_correl(this,this,dir));	}

	/// Create n-th points distribution of this data values in range [v1, v2]
	inline mglData Hist(long n,mreal v1=0,mreal v2=1, long nsub=0) const
	{	return mglData(true,mgl_data_hist(this,n,v1,v2,nsub));	}
	/// Create n-th points distribution of this data values in range [v1, v2] with weight w
	inline mglData Hist(const mglDataA &w, long n,mreal v1=0,mreal v2=1, long nsub=0) const
	{	return mglData(true,mgl_data_hist_w(this,&w,n,v1,v2,nsub));	}
	/// Get array which is result of maximal values in given direction or directions
	inline mglData Max(const char *dir) const
	{	return mglData(true,mgl_data_max_dir(this,dir));	}
	/// Get array which is result of minimal values in given direction or directions
	inline mglData Min(const char *dir) const
	{	return mglData(true,mgl_data_min_dir(this,dir));	}
	/// Find roots for set of nonlinear equations defined by textual formula
	inline mglDataC MultiRoots(const char *eq, const char *vars) const
	{	return mglDataC(true,mgl_find_roots_txt_c(eq, vars, this));	}

	/// Cumulative summation the data in given direction or directions
	inline void CumSum(const char *dir)	{	mgl_datac_cumsum(this,dir);	}
	/// Integrate (cumulative summation) the data in given direction or directions
	inline void Integral(const char *dir)	{	mgl_datac_integral(this,dir);	}
	/// Differentiate the data in given direction or directions
	inline void Diff(const char *dir)	{	mgl_datac_diff(this,dir);	}
	/// Differentiate the parametrically specified data along direction v1
	inline void Diff(const mglDataA &v1)
	{	mgl_datac_diff_par(this,&v1,0,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2=const
	inline void Diff(const mglDataA &v1, const mglDataA &v2)
	{	mgl_datac_diff_par(this,&v1,&v2,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2,v3=const
	inline void Diff(const mglDataA &v1, const mglDataA &v2, const mglDataA &v3)
	{	mgl_datac_diff_par(this,&v1,&v2,&v3);	}
	/// Double-differentiate (like laplace operator) the data in given direction
	inline void Diff2(const char *dir)	{	mgl_datac_diff2(this,dir);	}

	/// Swap left and right part of the data in given direction (useful for fourier spectrums)
	inline void Swap(const char *dir)		{	mgl_datac_swap(this,dir);	}
	/// Roll data along direction dir by num slices
	inline void Roll(char dir, long num)	{	mgl_datac_roll(this,dir,num);	}
	/// Mirror the data in given direction (useful for fourier spectrums)
	inline void Mirror(const char *dir)		{	mgl_datac_mirror(this,dir);	}
	/// Smooth the data on specified direction or directions
	/** String \a dir may contain:
	 *  ‘x’, ‘y’, ‘z’ for 1st, 2nd or 3d dimension;
	 *  ‘dN’ for linear averaging over N points;
	 *  ‘3’ for linear averaging over 3 points;
	 *  ‘5’ for linear averaging over 5 points.
	 *  By default quadratic averaging over 5 points is used. */
	inline void Smooth(const char *dirs="xyz",mreal delta=0)
	{	mgl_datac_smooth(this,dirs,delta);	}
	/// Limit the data to be inside [-v,v], keeping the original sign
	inline void Limit(mreal v)
	{	mgl_datac_limit(this, v);	}

	/// Set as the data envelop
	inline void Envelop(char dir='x')	{	mgl_datac_envelop(this,dir);	}
	/// Hankel transform
	inline void Hankel(const char *dir)	{	mgl_datac_hankel(this,dir);	}
	/// Apply Sin-Fourier transform
	inline void SinFFT(const char *dir)	{	mgl_datac_sinfft(this,dir);	}
	/// Apply Cos-Fourier transform
	inline void CosFFT(const char *dir)	{	mgl_datac_cosfft(this,dir);	}
	/// Fourier transform
	inline void FFT(const char *dir)	{	mgl_datac_fft(this,dir);	}
	/// Calculate one step of diffraction by finite-difference method with parameter q
	inline void Diffraction(const char *how, mreal q)	{	mgl_datac_diffr(this,how,q);	}
	/// Apply wavelet transform
	/** Parameter \a dir may contain:
	 * ‘x‘,‘y‘,‘z‘ for directions,
	 * ‘d‘ for daubechies, ‘D‘ for centered daubechies,
	 * ‘h‘ for haar, ‘H‘ for centered haar,
	 * ‘b‘ for bspline, ‘B‘ for centered bspline,
	 * ‘i‘ for applying inverse transform. */
	inline void Wavelet(const char *how, int k)	{	mgl_datac_wavelet(this, how, k);	}

	/// Interpolate by cubic spline the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline dual Spline(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_datac_spline(this, x,y,z);	}
	/// Interpolate by cubic spline the data to given point x,\a y,\a z which normalized in range [0, 1]
	inline dual Spline1(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_datac_spline(this, x*(nx-1),y*(ny-1),z*(nz-1));	}
	/// Interpolate by linear function the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline dual Linear(mreal x,mreal y=0,mreal z=0)	const
	{	return mgl_datac_linear_ext(this,x,y,z,0,0,0);	}
	/// Interpolate by line the data to given point x,\a y,\a z which normalized in range [0, 1]
	inline dual Linear1(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_datac_linear_ext(this,x*(nx-1),y*(ny-1),z*(nz-1),0,0,0);	}
	/// Interpolate by linear function the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline dual Linear(mglPoint &dif, mreal x,mreal y=0,mreal z=0)	const
	{
		mdual val,dx,dy,dz;
		val = mgl_datac_linear_ext(this,x,y,z, &dx, &dy, &dz);
		dif.Set(dx.real(),dy.real(),dz.real());	return val;
	}
	/// Interpolate by line the data and return its derivatives at given point x,\a y,\a z which normalized in range [0, 1]
	inline dual Linear1(mglPoint &dif, mreal x,mreal y=0,mreal z=0) const
	{
		mdual val,dx,dy,dz;
		val = mgl_datac_linear_ext(this,x,y,z, &dx, &dy, &dz);
		dif.Set(dx.real(),dy.real(),dz.real());
		dif.x/=nx>1?nx-1:1;	dif.y/=ny>1?ny-1:1;	dif.z/=nz>1?nz-1:1;
		return val;
	}
	/// Return an approximated x-value (root) when dat(x) = val
	inline mreal Solve(mreal val, bool use_spline=true, long i0=0) const
	{	return mgl_data_solve_1d(this, val, use_spline, i0);		}
	/// Return an approximated value (root) when dat(x) = val
	inline mglData Solve(mreal val, char dir, bool norm=true) const
	{	return mglData(true,mgl_data_solve(this, val, dir, 0, norm));	}
	inline mglData Solve(mreal val, char dir, const mglData &i0, bool norm=true) const
	{	return mglData(true,mgl_data_solve(this, val, dir, &i0, norm));	}

	/// Copy data from other mglDataA variable
	inline const mglDataA &operator=(const mglDataA &d)
	{	if(this!=&d)	Set(&d);	return d;	}
	inline const mglDataC &operator=(const mglDataC &d)
	{	if(this!=&d)	Set(&d);	return d;	}
	inline dual operator=(dual val)
	{
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	a[i]=val;	return val;	}
	inline dual operator=(mreal val)
	{
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	a[i]=val;	return val;	}
	/// Multiply the data by other one for each element
	inline void operator*=(const mglDataA &d)	{	mgl_datac_mul_dat(this,&d);	}
	/// Divide the data by other one for each element
	inline void operator/=(const mglDataA &d)	{	mgl_datac_div_dat(this,&d);	}
	/// Add the other data
	inline void operator+=(const mglDataA &d)	{	mgl_datac_add_dat(this,&d);	}
	/// Subtract the other data
	inline void operator-=(const mglDataA &d)	{	mgl_datac_sub_dat(this,&d);	}
	/// Multiply each element by the number
	inline void operator*=(dual d)		{	mgl_datac_mul_num(this,d);	}
	/// Divide each element by the number
	inline void operator/=(dual d)		{	mgl_datac_div_num(this,d);	}
	/// Add the number
	inline void operator+=(dual d)		{	mgl_datac_add_num(this,d);	}
	/// Subtract the number
	inline void operator-=(dual d)		{	mgl_datac_sub_num(this,d);	}
#ifndef SWIG
	/// Direct access to the data cell
	inline dual operator[](long i) const	{	return a[i];	}
	inline dual &operator[](long i)			{	return a[i];	}
#endif


#ifndef DEBUG
	/// Get the value in given cell of the data
	mreal v(long i,long j=0,long k=0) const	{	return abs(a[i+nx*(j+ny*k)]);	}
	/// Set the value in given cell of the data
	void set_v(mreal val, long i,long j=0,long k=0)	{	a[i+nx*(j+ny*k)]=val;	}
#else
	/// Get the value in given cell of the data with border checking
	mreal v(long i,long j=0,long k=0) const	{	return mgl_abs(mgl_datac_get_value(this,i,j,k));	}
	/// Set the value in given cell of the data
	void set_v(mreal val, long i,long j=0,long k=0)	{	mgl_datac_set_value(this,val,i,j,k);	}
#endif
	/// Get the complex value in given cell of the data
	dual vc(long i,long j=0,long k=0) const	{	return a[i+nx*(j+ny*k)];	}
	dual vcthr(long i) const	{	return a[i];	}
	/// Get the interpolated value and its derivatives in given data cell without border checking
	mreal valueD(mreal x,mreal y=0,mreal z=0,mreal *dx=0,mreal *dy=0,mreal *dz=0) const
	{	dual aa,ax,ay,az;	mreal res;
		aa = mglSpline3C(a,nx,ny,nz,x,y,z,&ax,&ay,&az);	res = abs(aa);
		if(dx)	*dx = res?(real(aa)*real(ax)+imag(aa)*imag(ax))/res:0;
		if(dy)	*dy = res?(real(aa)*real(ay)+imag(aa)*imag(ay))/res:0;
		if(dz)	*dz = res?(real(aa)*real(az)+imag(aa)*imag(az))/res:0;
		return res;	}
	/// Get the interpolated value in given data cell without border checking
	mreal value(mreal x,mreal y=0,mreal z=0) const
	{	return abs(mglSpline3Cs(a,nx,ny,nz,x,y,z));	}
	mreal vthr(long i) const {	return abs(a[i]);	}
	// add for speeding up !!!
	mreal dvx(long i,long j=0,long k=0) const
	{   long i0 = size_t(i)<size_t(nx-1) ? i+nx*(j+ny*k):nx*(1+j+ny*k)-2;	return abs(a[i0+1]-a[i0]);	}
//	{   long i0=i+nx*(j+ny*k);
//		return i>0? abs(i<nx-1? (a[i0+1]-a[i0-1])/mreal(2):a[i0]-a[i0-1]) : abs(a[i0+1]-a[i0]);	}
	mreal dvy(long i,long j=0,long k=0) const
	{   long i0 = size_t(j)<size_t(ny-1) ? i+nx*(j+ny*k):i+nx*(ny*(k+1)-2);	return abs(a[i0+nx]-a[i0]);	}
//	{   long i0=i+nx*(j+ny*k);
//		return j>0? abs(j<ny-1? (a[i0+nx]-a[i0-nx])/mreal(2):a[i0]-a[i0-nx]) : abs(a[i0+nx]-a[i0]);}
	mreal dvz(long i,long j=0,long k=0) const
	{   long n=nx*ny, i0 = size_t(k)<size_t(nz-1) ? i+nx*(j+ny*k):i+nx*(j+ny*(nz-2));	return abs(a[i0+n]-a[i0]);	}
//	{   long i0=i+nx*(j+ny*k), n=nx*ny;
//		return k>0? abs(k<nz-1? (a[i0+n]-a[i0-n])/mreal(2):a[i0]-a[i0-n]) : abs(a[i0+n]-a[i0]);	}
};
//-----------------------------------------------------------------------------
/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini
inline mglDataC mglPDEc(mglBase *gr, const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, mreal dz=0.1, mreal k0=100,const char *opt="")
{	return mglDataC(true, mgl_pde_solve_c(gr,ham, &ini_re, &ini_im, dz, k0,opt));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglDataC mglQO2dc(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mreal r=1, mreal k0=100)
{	return mglDataC(true, mgl_qo2d_solve_c(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0));	}
inline mglDataC mglQO2dc(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mglData &xx, mglData &yy, mreal r=1, mreal k0=100)
{	return mglDataC(true, mgl_qo2d_solve_c(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglDataC mglQO3dc(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mreal r=1, mreal k0=100)
{	return mglDataC(true, mgl_qo3d_solve_c(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0, 0));	}
inline mglDataC mglQO3dc(const char *ham, const mglDataA &ini_re, const mglDataA &ini_im, const mglDataA &ray, mglData &xx, mglData &yy, mglData &zz, mreal r=1, mreal k0=100)
{	return mglDataC(true, mgl_qo3d_solve_c(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy, &zz));	}
/// Saves result of ODE solving for var complex variables with right part func (separated by ';') and initial conditions x0 over time interval [0,tmax] with time step dt
inline mglDataC mglODEc(const char *func, const char *var, const mglDataA &ini, mreal dt=0.1, mreal tmax=10)
{	return mglDataC(true, mgl_ode_solve_str_c(func,var, &ini, dt, tmax));	}
//-----------------------------------------------------------------------------
/// Get array as solution of tridiagonal system of equations a[i]*x[i-1]+b[i]*x[i]+c[i]*x[i+1]=d[i]
/** String \a how may contain:
 * 'x', 'y', 'z' for solving along x-,y-,z-directions, or
 * 'h' for solving along hexagonal direction at x-y plain (need nx=ny),
 * 'c' for using periodical boundary conditions,
 * 'd' for diffraction/diffuse calculation. */
inline mglDataC mglTridMatC(const mglDataA &A, const mglDataA &B, const mglDataA &C, const mglDataC &D, const char *how)
{	return mglDataC(true, mgl_datac_tridmat(&A, &B, &C, &D, how));	}
//-----------------------------------------------------------------------------
/// Get sub-array of the data with given fixed indexes
inline mglDataC mglSubDataC(const mglDataA &dat, long xx, long yy=-1, long zz=-1)
{	return mglDataC(true,mgl_datac_subdata(&dat,xx,yy,zz));	}
inline mglDataC mglSubDataC(const mglDataA &dat, const mglDataA &xx, const mglDataA &yy, const mglDataA &zz)
{	return mglDataC(true,mgl_datac_subdata_ext(&dat,&xx,&yy,&zz));	}
inline mglDataC mglSubDataC(const mglDataA &dat, const mglDataA &xx, const mglDataA &yy)
{	return mglDataC(true,mgl_datac_subdata_ext(&dat,&xx,&yy,0));	}
inline mglDataC mglSubDataC(const mglDataA &dat, const mglDataA &xx)
{	return mglDataC(true,mgl_datac_subdata_ext(&dat,&xx,0,0));	}
//-----------------------------------------------------------------------------
/// Prepare coefficients for global spline interpolation
inline mglDataC mglGSplineCInit(const mglDataA &xdat, const mglDataA &ydat)
{	return mglDataC(true,mgl_gsplinec_init(&xdat, &ydat));	}
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
inline dual mglGSplineC(const mglDataA &coef, mreal dx, dual *d1=0, dual *d2=0)
{	return mgl_gsplinec(&coef, dx, reinterpret_cast<mdual*>(d1), reinterpret_cast<mdual*>(d2));	}
//-----------------------------------------------------------------------------
#define _DN_(a)	((mglDataC *)*(a))
#define _DC_		((mglDataC *)*d)
//-----------------------------------------------------------------------------
#ifndef SWIG
/// Wrapper class for complex expression evaluating
class MGL_EXPORT mglExprC
{
	HAEX ex;
	mglExprC(const mglExprC &){}	// copying is not allowed
	const mglExprC &operator=(const mglExprC &t){return t;}	// copying is not allowed
public:
	mglExprC(const char *expr)		{	ex = mgl_create_cexpr(expr);	}
	~mglExprC()	{	mgl_delete_cexpr(ex);	}
	/// Return value of expression for given x,y,z variables
	inline dual Eval(dual x, dual y=0, dual z=0)
	{	return mgl_cexpr_eval(ex,x,y,z);	}
	/// Return value of expression for given x,y,z,u,v,w variables
	inline dual Eval(dual x, dual y, dual z, dual u, dual v, dual w)
	{
		mdual var[26];
		var['x'-'a']=x;	var['y'-'a']=y;	var['z'-'a']=z;
		var['u'-'a']=u;	var['v'-'a']=v;	var['w'-'a']=w;
		return mgl_cexpr_eval_v(ex,var);	}
	/// Return value of expression for given variables
	inline dual Eval(dual var[26])
	{	return mgl_cexpr_eval_v(ex,reinterpret_cast<mdual*>(var));	}
};
#endif
//-----------------------------------------------------------------------------
#endif
#endif
