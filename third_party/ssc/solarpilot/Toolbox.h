/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _TOOLBOX_H_
#define _TOOLBOX_H_ 1

#include <math.h>
#include <vector>
#include <time.h>
#if defined(_WIN64)
#include <assert.h>
#else
#include <cassert>
#endif

#include "exceptions.hpp"

//Double vector - sparse
/*
some sample code for maps--

map<int, double> MM;
map<int, double>::iterator mmit, mmend;
MM[100] = 250.;
mmit = MM.find(4);
mmend = MM.end();
bool mmtest = MM.find(4)==MM.end();
*/

//-------------------------
//---- needed for the matrix_t structure
#if defined(_DEBUG) && defined(_MSC_VER) && defined(_WIN32) && !defined(_WIN64)
#define VEC_ASSERT(x) {if(!(x)) _asm{int 0x03}}
#else
#define VEC_ASSERT(X) assert(X)
#endif
//----



template< typename T >
class block_t
{
private:
	T *t_array;
	size_t n_rows, n_cols, n_layers;
public:

	block_t()
	{
		t_array = new T[1];
		n_rows = n_cols = n_layers = 1;
	}

	block_t( const block_t &B ){
		n_rows = B.nrows();
		n_cols = B.ncols();
		n_layers = B.nlayers();
		size_t nn = n_rows * n_cols * n_layers;

		t_array = new T[ nn ];

		for( size_t i=0; i<nn; i++){
			t_array[i] = B.t_array[i]; 
		}
	}
		
	block_t(size_t nr, size_t nc, size_t nl)
	{
		t_array = NULL;
		n_rows = n_cols = n_layers = 0;
		if (nl < 1) nl = 1;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		resize(nr,nc,nl);			
	}

	block_t(size_t nr, size_t nc, size_t nl, const T &val)
	{
		t_array = NULL;
		n_rows = n_cols = n_layers = 0;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		if (nl < 1) nl = 1;
		resize(nr,nc,nl);
		fill(val);
	}


	virtual ~block_t()
	{
		if (t_array) delete [] t_array;
	}
		
	void clear()
	{	//Note: when Clear() is called before resize() or resize_fill(), it can cause a memory error.
		//Do not use clear before calling these functions.
		if (t_array) delete [] t_array;
		n_layers = n_rows = n_cols = 0;
	}
		
	void copy( const block_t &rhs )
	{
		if (this != &rhs)
		{
			resize( rhs.nlayers(), rhs.nrows(), rhs.ncols() );
			size_t nn = n_layers*n_rows*n_cols;
			for (size_t i=0;i<nn;i++)
				t_array[i] = rhs.t_array[i];
		}
	}

	void assign( const T *pvalues, size_t nr, size_t nc, size_t nl )
	{
		resize( nr, nc, nl );
		if ( n_rows == nr && n_cols == nc && n_layers == nl)
		{
			size_t len = nr*nc*nl;
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
		}
	}

	block_t &operator=(const block_t &rhs)
	{
		copy( rhs );
		return *this;
	}
		
	block_t &operator=(const T &val)
	{
		resize(1,1,1);
		t_array[0] = val;
		return *this;
	}
		
	inline operator T()
	{
		return t_array[0];
	}
		
	bool equals( const block_t & rhs )
	{
		if (n_rows != rhs.n_rows || n_cols != rhs.n_cols || n_layers != rhs.n_layers)
			return false;
			
		size_t nn = n_rows*n_cols*n_layers;
		for (size_t i=0;i<nn;i++)
			if (t_array[i] != rhs.t_array[i])
				return false;
			
		return true;
	}
		
	inline bool is_single()
	{
		return (n_rows == 1 && n_cols == 1 && n_layers == 1);
	}
			
	inline bool is_array()
	{
		return (n_rows == 1 && n_layers == 1);
	}
		
	void fill( const T &val )
	{
		size_t ncells = n_rows*n_cols*n_layers;
		for (size_t i=0;i<ncells;i++)
			t_array[i] = val;
	}

	void resize(size_t nr, size_t nc, size_t nl)
	{
		if (nr < 1 || nc < 1 || nl < 1) return;
		if (nr == n_rows && nc == n_cols && n_layers) return;
			
		if (t_array) delete [] t_array;
		t_array = new T[ nr * nc * nl];
		n_rows = nr;
		n_cols = nc;
		n_layers = nl;
	}

	void resize_fill(size_t nr, size_t nc, size_t nl, const T &val)
	{
		resize( nr, nc, nl);
		fill( val );
	}
		
	void resize(size_t len)
	{
		resize( 1, len, 1);
	}
		
	void resize_fill(size_t len, const T &val)
	{
		resize_fill( 1, len, 1, val );
	}
		
	inline T &at(size_t r, size_t c, size_t l)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols && l >= 0 && l < n_layers);
#endif
		return t_array[n_layers*(c+r*n_cols)+l];	//Keep the last index close in memory
	}

	inline const T &at(size_t r, size_t c, size_t l) const
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols && l >= 0 && l < n_layers);
#endif
		return t_array[n_layers*(c+r*n_cols)+l];
	}
		
	T operator[] (size_t i) const
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
		
	T &operator[] (size_t i)
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
				
	inline size_t nrows() const
	{
		return n_rows;
	}
		
	inline size_t ncols() const
	{
		return n_cols;
	}
		
	inline size_t nlayers() const
	{
		return n_layers;
	}
		
	inline size_t ncells() const
	{
		return n_rows*n_cols*n_layers;
	}
		
	inline size_t membytes() const
	{
		return n_rows*n_cols*n_layers*sizeof(T);
	}
		
	void size(size_t &nr, size_t &nc, size_t &nl) const
	{
		nr = n_rows;
		nc = n_cols;
		nl = n_layers;
	}
		
	size_t length() const
	{
		return n_cols;
	}
		
	inline T *data()
	{
		return t_array;
	}

	inline T value() const
	{
		return t_array[0];
	}
};


template< typename T >
class matrix_t
{
private:
	T *t_array;
	size_t n_rows, n_cols;
public:

	matrix_t()
	{
		t_array = new T[1];
		n_rows = n_cols = 1;
	}

	matrix_t( const matrix_t &M){
		n_rows = M.nrows();
		n_cols = M.ncols();
		size_t nn = n_rows * n_cols;

		t_array = new T[ nn ];

		for( size_t i=0; i<nn; i++){
			t_array[i] = M.t_array[i]; 
		}

	}
			
	matrix_t(size_t len)
	{
		t_array = NULL;
		n_rows = n_cols = 0;
		if (len < 1) len = 1;
		resize( 1, len );
	}

	matrix_t(size_t nr, size_t nc)
	{
		t_array = NULL;
		n_rows = n_cols = 0;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		resize(nr,nc);
	}
		
	matrix_t(size_t nr, size_t nc, const T &val)
	{
		t_array = NULL;
		n_rows = n_cols = 0;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		resize(nr,nc);
		fill(val);
	}


	virtual ~matrix_t()
	{
		if (t_array) delete [] t_array;
	}
		
	void clear()
	{
		if (t_array) delete [] t_array;
		n_rows = n_cols = 0;
		//mjw- recreate the array
		t_array = new T[1];
	}
		
	void copy( const matrix_t &rhs )
	{
		if (this != &rhs)
		{
			resize( rhs.nrows(), rhs.ncols() );
			size_t nn = n_rows*n_cols;
			for (size_t i=0;i<nn;i++)
				t_array[i] = rhs.t_array[i];
		}
	}

	void assign( const T *pvalues, size_t len )
	{
		resize( len );
		if ( n_cols == len && n_rows == 1 )
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
	}
		
	void assign( const T *pvalues, size_t nr, size_t nc )
	{
		resize( nr, nc );
		if ( n_rows == nr && n_cols == nc )
		{
			size_t len = nr*nc;
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
		}
	}

	matrix_t &operator=(const matrix_t &rhs)
	{
		copy( rhs );
		return *this;
	}
		
	matrix_t &operator=(const T &val)
	{
		resize(1,1);
		t_array[0] = val;
		return *this;
	}
		
	inline operator T()
	{
		return t_array[0];
	}
		
	bool equals( const matrix_t & rhs )
	{
		if (n_rows != rhs.n_rows || n_cols != rhs.n_cols)
			return false;
			
		size_t nn = n_rows*n_cols;
		for (size_t i=0;i<nn;i++)
			if (t_array[i] != rhs.t_array[i])
				return false;
			
		return true;
	}
		
	inline bool is_single()
	{
		return (n_rows == 1 && n_cols == 1);
	}
			
	inline bool is_array()
	{
		return (n_rows == 1);
	}
		
	void fill( const T &val )
	{
		size_t ncells = n_rows*n_cols;
		for (size_t i=0;i<ncells;i++)
			t_array[i] = val;
	}

	void resize(size_t nr, size_t nc)
	{
		if (nr < 1 || nc < 1) return;
		if (nr == n_rows && nc == n_cols) return;
			
		if (t_array) delete [] t_array;
		t_array = new T[ nr * nc ];
		n_rows = nr;
		n_cols = nc;
	}

	void resize_fill(size_t nr, size_t nc, const T &val)
	{
		resize( nr, nc );
		fill( val );
	}
		
	void resize(size_t len)
	{
		resize( 1, len );
	}
		
	void resize_fill(size_t len, const T &val)
	{
		resize_fill( 1, len, val );
	}
		
	//also add methods for 1-D
	inline T &at(size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( n_rows==1 && c>=0 && c<n_cols );
#endif
		return t_array[c];
	}
		
	inline const T &at(size_t c) const
	{
#ifdef _DEBUG
		VEC_ASSERT( n_rows == 1 && c >= 0 && c < n_cols );
#endif
		return t_array[c];
	}
	//

	inline T &at(size_t r, size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}

	inline const T &at(size_t r, size_t c) const
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}
		
	T operator[] (size_t i) const
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
		
	T &operator[] (size_t i)
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
				
	inline size_t nrows() const
	{
		return n_rows;
	}
		
	inline size_t ncols() const
	{
		return n_cols;
	}
		
	inline size_t ncells() const
	{
		return n_rows*n_cols;
	}
		
	inline size_t membytes() const
	{
		return n_rows*n_cols*sizeof(T);
	}
		
	void size(size_t &nr, size_t &nc) const
	{
		nr = n_rows;
		nc = n_cols;
	}
		
	size_t length() const
	{
		return n_cols;
	}
		
	inline T *data()
	{
		return t_array;
	}

	inline T value() const
	{
		return t_array[0];
	}
};

struct sp_point {
	double x, y, z;

    sp_point();

    sp_point( const sp_point &P );

    sp_point(double X, double Y, double Z);

    void Set(double X, double Y, double Z);

    void Set( sp_point &P );

    void Add( sp_point &P );

    void Add(double X, double Y, double Z);

    void Subtract( sp_point &P );

    double& operator [](const int &index);

    bool operator <(const sp_point &p) const;


};
inline bool operator==(const sp_point& lhs, const sp_point& rhs)
{
    return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;   
};

struct Vect {
	double i, j, k;

    Vect(double i, double j, double k);
    Vect();
    Vect(const Vect &V);

	void Set(double _i, double _j, double _k);
	void Set( Vect &V );
	void Add( Vect &V );
	void Subtract( Vect &V );
	void Add(double _i, double _j, double _k);
    void Scale( double m );
	double &operator[](int index);
};

struct PointVect {

private:
	sp_point p;
	Vect v;

public:
	//structure to represent a mathematical vector with an originating point {x,y,z} and direction {i,j,k}
	double x, y, z, i, j, k;
	
	PointVect(const PointVect &v);
	PointVect& operator= (const PointVect &v);
	PointVect(double px=0.0, double py=0.0, double pz=0.0, double vi=0.0, double vj=0.0, double vk=1.0);

	sp_point *point();
	Vect *vect();

};


//---

struct DTobj {		//A basic structure that the larger DateTime class can be built on.
public:
	int _year, _month, _yday, _mday, _wday, _hour, _min, _sec, _ms;
	
	DTobj();
	
	void setZero();
	
	DTobj *Now();
};

//-----------------

class DateTime : public DTobj {
	//Defines date/time members and scripts
	int monthLength[12];

public:
	//Constructors
	DateTime(); //Default constructor
	DateTime(DTobj &DT);
	DateTime(double doy, double hour);	//Construct with day of the year and fractional hours (-12..12)
	~DateTime(){};

	//Methods
	//GETS
	int GetYear();
	int GetMonth();
	int GetYearDay();
	int GetMonthDay();
	int GetWeekDay();
	int GetMinute();
	int GetSecond();
	int GetMillisecond();
	// SETS
	void SetYear(const int val);
	void SetMonth(const int val);
	void SetYearDay(const int val);
	void SetMonthDay(const int val);
	void SetWeekDay(const int val);
	void SetHour(const int val);
	void SetMinute(const int val);
	void SetSecond(const int val);
	void SetMillisecond(const int val);
	void SetDate(int year, int month, int day);

	//Scripts
	void SetMonthLengths(const int year);
	void setDefaults();
	void hours_to_date(double hours, int &month, int &day_of_month);

	int GetDayOfYear();
	int GetDayOfYear(int year, int month, int mday);
	static int CalculateDayOfYear( int year, int month, int mday );	//Static version
	int GetHourOfYear();
	static std::string GetMonthName(int month /*1-12*/);

};

class WeatherData
{
	std::vector<std::vector<double>*> v_ptrs;
public:
	WeatherData();

	void initPointers();
	//Copy constructor
	WeatherData( const WeatherData &wd );

	int _N_items;
	std::vector<double>
		Day,	//Day of the month
		Hour,	//Hour of the day
		Month,	//Month of the day
		DNI,	//[W/m2] Direct normal irradiation
		T_db,	//[C] Dry bulb temperatre 
		Pres,	//[bar] Ambient atmospheric pressure
		V_wind,	//[m/s] Wind velocity
		Step_weight;	//[-]	Weighting factor for this weather step (used in layout simulations)
	
	int size(){return _N_items;}
    void clear();
	void resizeAll(int size, double val=0.);	//resizes all of the arrays
	void getStep(int step, double &day, double &hour, double &dni, double &step_weight);
	void getStep(int step, double &day, double &hour, double &month, double &dni, double &tdb, double &pres, double &vwind, double &step_weight);
	void append(double day, double hour, double dni, double step_weight);
	void append(double day, double hour, double month, double dni, double tdb, double pres, double vwind, double step_weight);
	void setStep(int step, double day, double hour, double dni, double step_weight);
	void setStep(int step, double day, double hour, double month, double dni, double tdb, double pres, double vwind, double step_weight);
	std::vector<std::vector<double>*> *getEntryPointers();
};

namespace Toolbox
 {
    
 	//Miscellaneous functions
	double round(double x);
	int factorial(int x) ;
	double factorial_d(int x) ;
	void writeMatD(std::string dir, std::string name, matrix_t<double> &mat, bool clear = false);
	void writeMatD(std::string dir, std::string name, block_t<double> &mat, bool clear = false);
	void swap(double &a, double &b);
	void swap(double *a, double *b);
	double atan3(double &x, double &y);	//my custom atan function [0..360]
	void map_profiles(double *source, int nsource, double *dest, int ndest, double *weights = (double*)NULL);

	//Vector geometry
    double dotprod(const Vect &A, const Vect &B);
	double dotprod(const Vect &A, const sp_point &B);
	Vect crossprod(const Vect &A, const Vect &B); 
	double crossprod(const sp_point &O, const sp_point &A, const sp_point &B);
	void unitvect(Vect &A); 
	double vectmag(double i, double j, double k);
	double vectmag(const Vect &A);
	double vectmag(const sp_point &P);
	double vectangle(const Vect &A, const Vect&B);      //Determine the angle between two vectors
	void rotation(double theta, int axis, sp_point &P);    //Rotation of a point about the origin
	void rotation(double theta, int axis, Vect &V);     //Rotation of a point about the origin
	
    //computational geometry 
	//Intersection of a vector on a plane
	bool plane_intersect(sp_point &pt_on_plane, Vect &vect_plane, sp_point &pt_line_start, Vect &vect_line, sp_point &int_on_plane);
	//Point on a line closest to external point
	bool line_norm_intersect(sp_point &line_p0, sp_point &line_p1, sp_point &P, sp_point &N, double &rad);
	//Intersection area of a rectangle and an ellipse
	double intersect_fuv(double U, double V);
	double intersect_ellipse_rect(double rect[4], double ellipse[2]);
	//Axis-aligned bounding box of a rotated ellipse
	void ellipse_bounding_box(double &A, double &B, double &phi, double sides[4], double cx = 0., double cy = 0.);
	//Calculate the convex hull surrounding a set of points
	void convex_hull(std::vector<sp_point> &points, std::vector<sp_point> &hull);
	//Calculate the area of an irregular polygon
	double area_polygon(std::vector<sp_point> &points);
	//Rotation of a point about an arbitrary axis centered at point 'axloc'
	sp_point rotation_arbitrary(double theta, Vect &axis, sp_point &axloc, sp_point &pt);
	//Calculate the z-rotation angle of a heliostat that's undergone a normal-vector transform
	double ZRotationTransform(double Az, double Zen);
	double ZRotationTransform(Vect &norm_vect);
    //Polygon-point hit test
	bool pointInPolygon( const std::vector<sp_point> &poly, const sp_point &pt);
	int polywind(const std::vector< sp_point> &vt, const sp_point &pt);
	std::vector<sp_point> projectPolygon(std::vector<sp_point> &poly, PointVect &plane) ; 
    std::vector<sp_point> clipPolygon(std::vector<sp_point> &A, std::vector<sp_point> &B); //construct a polygon that is the intersection of two polygons A and B
    void poly_from_svg(std::string &svg, std::vector< sp_point > &polygon, bool clear_poly=true);     //construct a polygon from an SVG path
    void BezierQ(sp_point &start, sp_point &control, sp_point &end, double t, sp_point &result);
    void BezierC(sp_point &start, sp_point &control1, sp_point &control2, sp_point &end, double t, sp_point &result);

	//File handling functions
	std::vector< std::string > split( const std::string &str, const std::string &delim, bool ret_empty=false, bool ret_delim=false );
	std::string getDelimiter(std::string &text);

};

#endif

