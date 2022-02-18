/***************************************************************************
 * data.i is part of Math Graphic Library
 * Copyright (C) 2007-2012 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
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
//-----------------------------------------------------------------------------
#include <string>
class mglData
{
public:

	long nx;		///< number of points in 1st dimensions ('x' dimension)
	long ny;		///< number of points in 2nd dimensions ('y' dimension)
	long nz;		///< number of points in 3d dimensions ('z' dimension)
	mreal *a;		///< data array
	bool link;		///< use external data (i.e. don't free it)

	/// Initiate by other mglData variable
	mglData(const mglData &d)	{	a=0;	mgl_data_set(this,&d);		}	// NOTE: must be constructor for mglData& to exclude copy one
	mglData(const mglData *d)	{	a=0;	mgl_data_set(this, d);		}
	mglData(bool, mglData *d)	// NOTE: Variable d will be deleted!!!
	{	if(d)
		{	nx=d->nx;	ny=d->ny;	nz=d->nz;	a=d->a;	d->a=0;
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
	/// Read data from file
	mglData(const char *fname)			{	a=0;	Read(fname);	}
	/// Allocate the memory for data array and initialize it zero
	mglData(long xx=1,long yy=1,long zz=1)	{	a=0;	Create(xx,yy,zz);	}
	/// Delete the array
	virtual ~mglData()	{	if(!link && a)	delete []a;	}

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
	inline void Set(const mglData &dat)	{	mgl_data_set(this, &dat);	}

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
	inline void Join(const mglData &d)
	{	mgl_data_join(this,&d);	}

	/// Modify the data by specified formula
	inline void Modify(const char *eq,long dim=0)
	{	mgl_data_modify(this, eq, dim);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglData &vdat, const mglData &wdat)
	{	mgl_data_modify_vw(this,eq,&vdat,&wdat);	}
	/// Modify the data by specified formula
	inline void Modify(const char *eq,const mglData &vdat)
	{	mgl_data_modify_vw(this,eq,&vdat,0);	}
	/// Modify the data by specified formula assuming x,y,z in range [r1,r2]
	inline void Fill(HMGL gr, const char *eq, const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,0,0,opt);	}
	inline void Fill(HMGL gr, const char *eq, const mglData &vdat, const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,&vdat,0,opt);	}
	inline void Fill(HMGL gr, const char *eq, const mglData &vdat, const mglData &wdat,const char *opt="")
	{	mgl_data_fill_eq(gr,this,eq,&vdat,&wdat,opt);	}
	/// Equidistantly fill the data to range [x1,x2] in direction dir
	inline void Fill(mreal x1,mreal x2=NaN,char dir='x')
	{	mgl_data_fill(this,x1,x2,dir);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2] using global spline
	inline void RefillGS(const mglData &xdat, const mglData &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_data_refill_gs(this,&xdat,&vdat,x1,x2,sl);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in range [p1,p2]
	inline void Refill(const mglData &xdat, const mglData &vdat, mreal x1, mreal x2,long sl=-1)
	{	mgl_data_refill_x(this,&xdat,&vdat,x1,x2,sl);	}
	inline void Refill(const mglData &xdat, const mglData &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_data_refill_x(this,&xdat,&vdat,p1.x,p2.x,sl);	}
	inline void Refill(const mglData &xdat, const mglData &ydat, const mglData &vdat, mglPoint p1, mglPoint p2,long sl=-1)
	{	mgl_data_refill_xy(this,&xdat,&ydat,&vdat,p1.x,p2.x,p1.y,p2.y,sl);	}
	inline void Refill(const mglData &xdat, const mglData &ydat, const mglData &zdat, const mglData &vdat, mglPoint p1, mglPoint p2)
	{	mgl_data_refill_xyz(this,&xdat,&ydat,&zdat,&vdat,p1.x,p2.x,p1.y,p2.y,p1.z,p2.z);	}
	/// Fill the data by interpolated values of vdat parametrically depended on xdat,ydat,zdat for x,y,z in axis range of gr
	inline void Refill(HMGL gr, const mglData &xdat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,0,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglData &xdat, const mglData &ydat, const mglData &vdat, long sl=-1, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,&ydat,0,&vdat,sl,opt);	}
	inline void Refill(HMGL gr, const mglData &xdat, const mglData &ydat, const mglData &zdat, const mglData &vdat, const char *opt="")
	{	mgl_data_refill_gr(gr,this,&xdat,&ydat,&zdat,&vdat,-1,opt);	}
	/// Set the data by triangulated surface values assuming x,y,z in axis range of gr
	inline void Grid(HMGL gr, const mglData &x, const mglData &y, const mglData &z, const char *opt="")
	{	mgl_data_grid(gr,this,&x,&y,&z,opt);	}
	/// Set the data by triangulated surface values assuming x,y,z in range [p1, p2]
	inline void Grid(const mglData &xdat, const mglData &ydat, const mglData &vdat, mglPoint p1, mglPoint p2)
	{	mgl_data_grid_xy(this,&xdat,&ydat,&vdat,p1.x,p2.x,p1.y,p2.y);	}
	/// Put value to data element(s)
	inline void Put(mreal val, long i=-1, long j=-1, long k=-1)
	{	mgl_data_put_val(this,val,i,j,k);	}
	/// Put array to data element(s)
	inline void Put(const mglData &dat, long i=-1, long j=-1, long k=-1)
	{	mgl_data_put_dat(this,&dat,i,j,k);	}
	/// Set names for columns (slices)
	inline void SetColumnId(const char *ids)
	{	mgl_data_set_id(this,ids);	}
	/// Make new id
	inline void NewId()	{	id.clear();	}

	/// Read data from tab-separated text file with auto determining size
	inline bool Read(const char *fname)
	{	return mgl_data_read(this,fname); }
	/// Read data from text file with specifeid size
	inline bool Read(const char *fname,long mx,long my=1,long mz=1)
	{	return mgl_data_read_dim(this,fname,mx,my,mz);	}
	/// Save whole data array (for ns=-1) or only ns-th slice to text file
	inline void Save(const char *fname,long ns=-1) const
	{	mgl_data_save(this,fname,ns);	}
	/// Export data array (for ns=-1) or only ns-th slice to PNG file according color scheme
	inline void Export(const char *fname,const char *scheme,mreal v1=0,mreal v2=0,long ns=-1) const
	{	mgl_data_export(this,fname,scheme,v1,v2,ns);	}
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
	/// Save data to HDF file
	inline void SaveHDF(const char *fname,const char *data,bool rewrite=false) const
	{	mgl_data_save_hdf(this,fname,data,rewrite);	}
	/// Put HDF data names into buf as '\t' separated.
	inline static int DatasHDF(const char *fname, char *buf, long size)
	{	return mgl_datas_hdf(fname,buf,size);	}
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
	inline mglData SubData(const mglData &xx, const mglData &yy, const mglData &zz) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,&zz));	}
	inline mglData SubData(const mglData &xx, const mglData &yy) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,&yy,0));	}
	inline mglData SubData(const mglData &xx) const
	{	return mglData(true,mgl_data_subdata_ext(this,&xx,0,0));	}
	/// Get data from sections ids, separated by value val along specified direction.
	/** If section id is negative then reverse order is used (i.e. -1 give last section). */
	inline mglData Section(const mglData &ids, char dir='y', mreal val=NAN) const
	{	return mglData(true,mgl_data_section(this,&ids,dir,val));	}
	inline mglData Section(long id, char dir='y', mreal val=NAN) const
	{	return mglData(true,mgl_data_section_val(this,id,dir,val));	}

	/// Get trace of the data array
	inline mglData Trace() const
	{	return mglData(true,mgl_data_trace(this));	}
	/// Create n-th points distribution of this data values in range [v1, v2]
	inline mglData Hist(long n,mreal v1=0,mreal v2=1, long nsub=0) const
	{	return mglData(true,mgl_data_hist(this,n,v1,v2,nsub));	}
	/// Create n-th points distribution of this data values in range [v1, v2] with weight w
	inline mglData Hist(const mglData &w, long n,mreal v1=0,mreal v2=1, long nsub=0) const
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
	/// Get the data which is direct multiplication (like, d[i,j] = this[i]*a[j] and so on)
	inline mglData Combine(const mglData &dat) const
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
	inline mglData Correl(const mglData &dat, const char *dir) const
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
	inline void Diff(const mglData &v1)
	{	mgl_data_diff_par(this,&v1,0,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2=const
	inline void Diff(const mglData &v1, const mglData &v2)
	{	mgl_data_diff_par(this,&v1,&v2,0);	}
	/// Differentiate the parametrically specified data along direction v1 with v2,v3=const
	inline void Diff(const mglData &v1, const mglData &v2, const mglData &v3)
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
	inline void Sew(const char *dirs="xyz", mreal da=2*Pi)
	{	mgl_data_sew(this,dirs,da);	}
	/// Smooth the data on specified direction or directions
	/** String \a dir may contain:
	 *  ‘x’, ‘y’, ‘z’ for 1st, 2nd or 3d dimension;
	 *  ‘dN’ for linear averaging over N points;
	 *  ‘3’ for linear averaging over 3 points;
	 *  ‘5’ for linear averaging over 5 points.
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

	/// Interpolate by cubic spline the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline mreal Spline(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_data_spline(this, x,y,z);	}
	/// Interpolate by cubic spline the data to given point x,\a y,\a z which normalized in range [0, 1]
	inline mreal Spline1(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_data_spline(this, x*(nx-1),y*(ny-1),z*(nz-1));	}
	/// Interpolate by linear function the data to given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline mreal Linear(mreal x,mreal y=0,mreal z=0)	const
	{	return mgl_data_linear(this,x,y,z);	}
	/// Interpolate by line the data to given point x,\a y,\a z which normalized in range [0, 1]
	inline mreal Linear1(mreal x,mreal y=0,mreal z=0) const
	{	return mgl_data_linear(this,x*(nx-1),y*(ny-1),z*(nz-1));	}

	/// Interpolate by cubic spline the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline mreal Spline(mglPoint &dif, mreal x,mreal y=0,mreal z=0) const
	{	return mgl_data_spline_ext(this, x,y,z, &(dif.x),&(dif.y), &(dif.z));	}
	/// Interpolate by cubic spline the data and return its derivatives at given point x,\a y,\a z which normalized in range [0, 1]
	inline mreal Spline1(mglPoint &dif, mreal x,mreal y=0,mreal z=0) const
	{	mreal res=mgl_data_spline_ext(this, x*(nx-1),y*(ny-1),z*(nz-1), &(dif.x),&(dif.y), &(dif.z));
		dif.x*=nx>1?nx-1:1;	dif.y*=ny>1?ny-1:1;	dif.z*=nz>1?nz-1:1;	return res;	}
	/// Interpolate by linear function the data and return its derivatives at given point x=[0...nx-1], y=[0...ny-1], z=[0...nz-1]
	inline mreal Linear(mglPoint &dif, mreal x,mreal y=0,mreal z=0)	const
	{	return mgl_data_linear_ext(this,x,y,z, &(dif.x),&(dif.y), &(dif.z));	}
	/// Interpolate by line the data and return its derivatives at given point x,\a y,\a z which normalized in range [0, 1]
	inline mreal Linear1(mglPoint &dif, mreal x,mreal y=0,mreal z=0) const
	{	mreal res=mgl_data_linear_ext(this,x*(nx-1),y*(ny-1),z*(nz-1), &(dif.x),&(dif.y), &(dif.z));
		dif.x*=nx>1?nx-1:1;	dif.y*=ny>1?ny-1:1;	dif.z*=nz>1?nz-1:1;	return res;	}

	/// Get information about the data (sizes and momentum) to string
	inline const char *PrintInfo() const	{	return mgl_data_info(this);	}
	/// Print information about the data (sizes and momentum) to FILE (for example, stdout)
	inline void PrintInfo(FILE *fp) const
	{	if(fp)	{	fprintf(fp,"%s",mgl_data_info(this));	fflush(fp);	}	}
	/// Get maximal value of the data
	inline mreal Maximal() const	{	return mgl_data_max(this);	}
	/// Get minimal value of the data
	inline mreal Minimal() const	{	return mgl_data_min(this);	}
	/// Get maximal value of the data which is less than 0
	inline mreal MaximalNeg() const	{	return mgl_data_neg_max(this);	}
	/// Get minimal value of the data which is larger than 0
	inline mreal MinimalPos() const	{	return mgl_data_pos_min(this);	}
	/// Get maximal value of the data and its position
	inline mreal Maximal(long &i,long &j,long &k) const
	{	return mgl_data_max_int(this,&i,&j,&k);	}
	/// Get minimal value of the data and its position
	inline mreal Minimal(long &i,long &j,long &k) const
	{	return mgl_data_min_int(this,&i,&j,&k);	}
	/// Get maximal value of the data and its approximated position
	inline mreal Maximal(mreal &x,mreal &y,mreal &z) const
	{	return mgl_data_max_real(this,&x,&y,&z);	}
	/// Get minimal value of the data and its approximated position
	inline mreal Minimal(mreal &x,mreal &y,mreal &z) const
	{	return mgl_data_min_real(this,&x,&y,&z);	}
	/// Get "energy" and find first (median) and second (width) momenta of data
	inline mreal Momentum(char dir,mreal &m,mreal &w) const
	{	return mgl_data_momentum_val(this,dir,&m,&w,0,0);	}
	/// Get "energy and find 4 momenta of data: median, width, skewness, kurtosis
	inline mreal Momentum(char dir,mreal &m,mreal &w,mreal &s,mreal &k) const
	{	return mgl_data_momentum_val(this,dir,&m,&w,&s,&k);	}
	/// Find position (after specified in i,j,k) of first nonzero value of formula
	inline mreal Find(const char *cond, long &i, long &j, long &k) const
	{	return mgl_data_first(this,cond,&i,&j,&k);	}
	/// Find position (before specified in i,j,k) of last nonzero value of formula
	inline mreal Last(const char *cond, long &i, long &j, long &k) const
	{	return mgl_data_last(this,cond,&i,&j,&k);	}
	/// Find position of first in direction 'dir' nonzero value of formula
	inline long Find(const char *cond, char dir, long i=0, long j=0, long k=0) const
	{	return mgl_data_find(this,cond,dir,i,j,k);	}
	/// Find if any nonzero value of formula
	inline bool FindAny(const char *cond) const
	{	return mgl_data_find_any(this,cond);	}

	/// Copy data from other mglData variable
	inline const mglData &operator=(const mglData &d)
	{	if(this!=&d)	mgl_data_set(this,&d);	return d;	}
	inline mreal operator=(mreal val)
	{	mgl_data_fill(this,val,val,'x');	return val;	}
	/// Multiply the data by other one for each element
	inline void operator*=(const mglData &d)	{	mgl_data_mul_dat(this,&d);	}
	/// Divide the data by other one for each element
	inline void operator/=(const mglData &d)	{	mgl_data_div_dat(this,&d);	}
	/// Add the other data
	inline void operator+=(const mglData &d)	{	mgl_data_add_dat(this,&d);	}
	/// Subtract the other data
	inline void operator-=(const mglData &d)	{	mgl_data_sub_dat(this,&d);	}
	/// Multiply each element by the number
	inline void operator*=(mreal d)		{	mgl_data_mul_num(this,d);	}
	/// Divide each element by the number
	inline void operator/=(mreal d)		{	mgl_data_div_num(this,d);	}
	/// Add the number
	inline void operator+=(mreal d)		{	mgl_data_add_num(this,d);	}
	/// Subtract the number
	inline void operator-=(mreal d)		{	mgl_data_sub_num(this,d);	}
};
//-----------------------------------------------------------------------------
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for amplitude and phase
inline mglData mglTransformA(const mglData &am, const mglData &ph, const char *tr)
{	return mglData(true,mgl_transform_a(&am,&ph,tr));	}
/// Integral data transformation (like Fourier 'f' or 'i', Hankel 'h' or None 'n') for real and imaginary parts
inline mglData mglTransform(const mglData &re, const mglData &im, const char *tr)
{	return mglData(true,mgl_transform(&re,&im,tr));	}
/// Apply Fourier transform for the data and save result into it
inline void mglFourier(mglData &re, mglData &im, const char *dir)
{	mgl_data_fourier(&re,&im,dir);	}
/// Short time Fourier analysis for real and imaginary parts. Output is amplitude of partial Fourier (result will have size {dn, floor(nx/dn), ny} for dir='x'
inline mglData mglSTFA(const mglData &re, const mglData &im, long dn, char dir='x')
{	return mglData(true, mgl_data_stfa(&re,&im,dn,dir));	}
//-----------------------------------------------------------------------------
/// Saves result of PDE solving (|u|^2) for "Hamiltonian" ham with initial conditions ini
inline mglData mglPDE(HMGL gr, const char *ham, const mglData &ini_re, const mglData &ini_im, mreal dz=0.1, mreal k0=100,const char *opt="")
{	return mglData(true, mgl_pde_solve(gr,ham, &ini_re, &ini_im, dz, k0,opt));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglData mglQO2d(const char *ham, const mglData &ini_re, const mglData &ini_im, const mglData &ray, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo2d_solve(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0));	}
inline mglData mglQO2d(const char *ham, const mglData &ini_re, const mglData &ini_im, const mglData &ray, mglData &xx, mglData &yy, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo2d_solve(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy));	}
/// Saves result of PDE solving for "Hamiltonian" ham with initial conditions ini along a curve ray (must have nx>=7 - x,y,z,px,py,pz,tau or nx=5 - x,y,px,py,tau)
inline mglData mglQO3d(const char *ham, const mglData &ini_re, const mglData &ini_im, const mglData &ray, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo3d_solve(ham, &ini_re, &ini_im, &ray, r, k0, 0, 0, 0));	}
inline mglData mglQO3d(const char *ham, const mglData &ini_re, const mglData &ini_im, const mglData &ray, mglData &xx, mglData &yy, mglData &zz, mreal r=1, mreal k0=100)
{	return mglData(true, mgl_qo3d_solve(ham, &ini_re, &ini_im, &ray, r, k0, &xx, &yy, &zz));	}
/// Finds ray with starting point r0, p0 (and prepares ray data for mglQO2d)
inline mglData mglRay(const char *ham, mglPoint r0, mglPoint p0, mreal dt=0.1, mreal tmax=10)
{	return mglData(true, mgl_ray_trace(ham, r0.x, r0.y, r0.z, p0.x, p0.y, p0.z, dt, tmax));	}
/// Saves result of ODE solving for var complex variables with right part func (separated by ';') and initial conditions x0 over time interval [0,tmax] with time step dt
inline mglData mglODE(const char *func, const char *var, const mglData &ini, mreal dt=0.1, mreal tmax=10)
{	return mglData(true, mgl_ode_solve_str(func,var, &ini, dt, tmax));	}
//-----------------------------------------------------------------------------
/// Get array as solution of tridiagonal system of equations a[i]*x[i-1]+b[i]*x[i]+c[i]*x[i+1]=d[i]
/** String \a how may contain:
 * 'x', 'y', 'z' for solving along x-,y-,z-directions, or
 * 'h' for solving along hexagonal direction at x-y plain (need nx=ny),
 * 'c' for using periodical boundary conditions,
 * 'd' for diffraction/diffuse calculation. */
inline mglData mglTridMat(const mglData &A, const mglData &B, const mglData &C, const mglData &D, const char *how)
{	return mglData(true, mgl_data_tridmat(&A, &B, &C, &D, how));	}
//-----------------------------------------------------------------------------
/// Calculate Jacobian determinant for D{x(u,v), y(u,v)} = dx/du*dy/dv-dx/dv*dy/du
inline mglData mglJacobian(const mglData &x, const mglData &y)
{	return mglData(true, mgl_jacobian_2d(&x, &y));	}
/// Calculate Jacobian determinant for D{x(u,v,w), y(u,v,w), z(u,v,w)}
inline mglData mglJacobian(const mglData &x, const mglData &y, const mglData &z)
{	return mglData(true, mgl_jacobian_3d(&x, &y, &z));	}
/// Do something like Delone triangulation
inline mglData mglTriangulation(const mglData &x, const mglData &y, const mglData &z)
{	return mglData(true,mgl_triangulation_3d(&x,&y,&z));	}
inline mglData mglTriangulation(const mglData &x, const mglData &y)
{	return mglData(true,mgl_triangulation_2d(&x,&y));	}
/// Get curves, separated by NAN, for maximal values of array d as function of x coordinate.
/** Noises below lvl amplitude are ignored.
 * Parameter dy \in [0,ny] set the "attraction" distance of points to curve. */
inline mglData mglDetect(const mglData &d, mreal lvl, mreal dj, mreal di=0, mreal min_len=0)
{	return mglData(true,mgl_data_detect(&d, lvl, dj, di, min_len));	}
//-----------------------------------------------------------------------------
/// Get array which is n-th pairs {x[i],y[i]} for iterated function system (fractal) generated by A
inline mglData mglIFS2d(const mglData &A, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_2d(&A,n,skip));	}
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) generated by A
inline mglData mglIFS3d(const mglData &A, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_3d(&A,n,skip));	}
/// Get array which is n-th points {x[i],y[i],z[i]} for iterated function system (fractal) defined in *.ifs file 'fname' and named as 'name'
inline mglData mglIFSfile(const char *fname, const char *name, long n, long skip=20)
{	return mglData(true,mgl_data_ifs_file(fname,name,n,skip));	}
/// Get array which is n-th pairs {x[i],y[i]} for Flame fractal generated by A with functions F
/** NOTE: A.nx must be >= 7 and F.nx >= 2 and F.nz=A.ny.
 * F[0,i,j] denote function id. F[1,i,j] give function weight, F(2:5,i,j) provide function parameters.
 * Resulting point is {xnew,ynew} = sum_i F[1,i,j]*F[0,i,j]{IFS2d(A[j]){x,y}}. */
inline mglData mglFlame2d(const mglData &A, const mglData &F, long n, long skip=20)
{	return mglData(true,mgl_data_flame_2d(&A,&F,n,skip));	}
//-----------------------------------------------------------------------------
/// Get sub-array of the data with given fixed indexes
inline mglData mglSubData(const mglData &dat, long xx, long yy=-1, long zz=-1)
{	return mglData(true,mgl_data_subdata(&dat,xx,yy,zz));	}
inline mglData mglSubData(const mglData &dat, const mglData &xx, const mglData &yy, const mglData &zz)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,&yy,&zz));	}
inline mglData mglSubData(const mglData &dat, const mglData &xx, const mglData &yy)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,&yy,0));	}
inline mglData mglSubData(const mglData &dat, const mglData &xx)
{	return mglData(true,mgl_data_subdata_ext(&dat,&xx,0,0));	}
//-----------------------------------------------------------------------------
/// Prepare coefficients for global spline interpolation
inline mglData mglGSplineInit(const mglData &xdat, const mglData &ydat)
{	return mglData(true,mgl_gspline_init(&xdat, &ydat));	}
/// Evaluate global spline (and its derivatives d1, d2 if not NULL) using prepared coefficients \a coef
inline mreal mglGSpline(const mglData &coef, mreal dx, mreal *d1=0, mreal *d2=0)
{	return mgl_gspline(&coef, dx, d1,d2);	}
//-----------------------------------------------------------------------------
/// Wrapper class for expression evaluating
class mglExpr
{
	HMEX ex;
	mglExpr(const mglExpr &){}	// copying is not allowed
	const mglExpr &operator=(const mglExpr &t){return t;}	// copying is not allowed
public:
	mglExpr(const char *expr)		{	ex = mgl_create_expr(expr);	}
	~mglExpr()	{	mgl_delete_expr(ex);	}
	/// Return value of expression for given x,y,z variables
	inline double Eval(double x, double y=0, double z=0)
	{	return mgl_expr_eval(ex,x,y,z);	}
	/// Return value of expression differentiation over variable dir for given x,y,z variables
	inline double Diff(char dir, double x, double y=0, double z=0)
	{	return mgl_expr_diff(ex,dir, x,y,z);	}
};
//-----------------------------------------------------------------------------
