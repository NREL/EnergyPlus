// matrix3.h
// =================================================================================
// Copyright 1992-2009	Regents of University of California
//						Lawrence Berkeley National Laboratory

//  Authors: W.L. Carroll and R.J. Hitchcock
//           Building Technologies Department
//           Lawrence Berkeley National Laboratory

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

// NOTICE: The Government is granted for itself and others acting on its behalf 
// a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
// prepare derivative works, and perform publicly and display publicly. 
// Beginning five (5) years after (date permission to assert copyright was obtained),
// subject to two possible five year renewals, the Government is granted for itself 
// and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
// license in this data to reproduce, prepare derivative works, distribute copies to 
// the public, perform publicly and display publicly, and to permit others to do so. 
// NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
// THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
// LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
// INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
// WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
// =================================================================================

namespace BldgGeomLib {

class matrix3
{
public:

    // Constructors
    matrix3();
    matrix3(Double a, Double b, Double c,
         Double d, Double e, Double f,
         Double g, Double h, Double i);
    matrix3(const matrix3 &m);
    matrix3(ZeroOrOne k);
    matrix3(Block k);

    // Accessor functions
    Int         Rows() const { return(3); };
    Int         Cols() const { return(3); };
    vector3        &operator [] (Int i);
    const vector3  &operator [] (Int i) const;
    Double        *Ref() const;               // Return pointer to data

    // Assignment operators
    matrix3        &operator =  (const matrix3 &m);
    matrix3        &operator =  (ZeroOrOne k);
	matrix3        &operator =  (Block k);
    matrix3        &operator += (const matrix3 &m);
    matrix3        &operator -= (const matrix3 &m);
    matrix3        &operator *= (const matrix3 &m);
    matrix3        &operator *= (Double s);
    matrix3        &operator /= (Double s);

    // Comparison operators
    Bool        operator <  (const matrix3 &m) const;  // M < N?	WLC
    Bool        operator == (const matrix3 &m) const;  // M == N?
    Bool        operator != (const matrix3 &m) const;  // M != N?

    // Arithmetic operators
    matrix3        operator + (const matrix3 &m) const;   // M + N
    matrix3        operator - (const matrix3 &m) const;   // M - N
    matrix3        operator - () const;                // -M
    matrix3        operator * (const matrix3 &m) const;   // M * N
    matrix3        operator * (Double s) const;          // M * s
    matrix3        operator / (Double s) const;          // M / s


    // Initialize
    Void        MakeZero();                 // Zero matrix
    Void        MakeDiag(Double k = vl_one);  // I
    Void        MakeBlock(Double k = vl_one); // all elts = k

	// rotation
    matrix3&       MakeRot(const vector3 &axis, Double theta);

    // Private...

protected:
    vector3        row[3];
};


// --- Matrix operators -------------------------------------------------------
inline point3	&operator *= (point3 &p, const matrix3 &m);		// p *= m	// WLC
inline point3	operator * (const matrix3 &m, const point3 &p);	// m * p	// WLC
inline point3	operator * (const point3 &p, const matrix3 &m);	// p * m	// WLC


inline vector3     &operator *= (vector3 &v, const matrix3 &m);      // v *= m
inline vector3     operator * (const matrix3 &m, const vector3 &v);  // m * v
inline vector3     operator * (const vector3 &v, const matrix3 &m);  // v * m
inline matrix3     operator * (const Double s, const matrix3 &m);   // s * m

matrix3            trans(const matrix3 &m);                   // Transpose
Double            trace(const matrix3 &m);                   // Trace
matrix3            adj(const matrix3 &m);                     // Adjoint
Double            det(const matrix3 &m);                     // Determinant
matrix3            inv(const matrix3 &m);                     // Inverse
matrix3            oprod(const vector3 &a, const vector3 &b);    // Outer product
//	NOTE:  vector{3/4} "axis" MUST BE UNIT LENGTH	//	WLC
matrix3			Rot3(const vector3 &axis, Double theta);

// --- Inlines ----------------------------------------------------------------

inline matrix3::matrix3()
{}

inline vector3 &matrix3::operator [] (Int i)
{
    return(row[i]);
}

inline const vector3 &matrix3::operator [] (Int i) const
{
    return(row[i]);
}

inline Double *matrix3::Ref() const
{
    return((Double *) row);
}

inline matrix3::matrix3(ZeroOrOne k)
{
    MakeDiag(k);
}

inline matrix3::matrix3(Block k)
{
    MakeBlock((ZeroOrOne) k);
}

inline matrix3 &matrix3::operator = (ZeroOrOne k)
{
    MakeDiag(k);

    return(SELF);
}

inline matrix3 &matrix3::operator = (Block k)
{
    MakeBlock((ZeroOrOne) k);

    return(SELF);
}

inline matrix3 operator *  (const Double s, const matrix3 &m)
{
    return(m * s);
}

inline vector3 operator * (const matrix3 &m, const vector3 &v)
{
    vector3 result;

    result[0] = v[0] * m[0][0] + v[1] * m[0][1] + v[2] * m[0][2];
    result[1] = v[0] * m[1][0] + v[1] * m[1][1] + v[2] * m[1][2];
    result[2] = v[0] * m[2][0] + v[1] * m[2][1] + v[2] * m[2][2];

    return(result);
}

inline vector3 operator * (const vector3 &v, const matrix3 &m)
{
    vector3 result;

    result[0] = v[0] * m[0][0] + v[1] * m[1][0] + v[2] * m[2][0];
    result[1] = v[0] * m[0][1] + v[1] * m[1][1] + v[2] * m[2][1];
    result[2] = v[0] * m[0][2] + v[1] * m[1][2] + v[2] * m[2][2];

    return(result);
}

inline vector3 &operator *= (vector3 &v, const matrix3 &m)
{
    Double t0, t1;

    t0   = v[0] * m[0][0] + v[1] * m[1][0] + v[2] * m[2][0];
    t1   = v[0] * m[0][1] + v[1] * m[1][1] + v[2] * m[2][1];
    v[2] = v[0] * m[0][2] + v[1] * m[1][2] + v[2] * m[2][2];
    v[0] = t0;
    v[1] = t1;

    return(v);
}

// WLC Point3*Mat3 - multiple forms
inline point3 operator * (const matrix3 &m, const point3 &p)	//	WLC
{
	point3 result;
	
	result[0] = p[0] * m[0][0] + p[1] * m[0][1] + p[2] * m[0][2];
	result[1] = p[0] * m[1][0] + p[1] * m[1][1] + p[2] * m[1][2];
	result[2] = p[0] * m[2][0] + p[1] * m[2][1] + p[2] * m[2][2];
	
	return(result);
}

inline point3 operator * (const point3 &p, const matrix3 &m)	//	WLC	
{
	point3 result;
	
	result[0] = p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0];
	result[1] = p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1];
	result[2] = p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2];
	
	return(result);
}

inline point3 &operator *= (point3 &p, const matrix3 &m)	//	WLC			
{	
	Double t0, t1;
	
	t0   = p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0];
	t1   = p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1];
	p[2] = p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2];
	p[0] = t0;
	p[1] = t1;

	return(p);
}

}	//	end namespace BldgGeomLib


std::ostream         &operator << (std::ostream &s, const BldgGeomLib::matrix3 &m);
std::istream         &operator >> (std::istream &s, BldgGeomLib::matrix3 &m);

